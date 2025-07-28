library(shiny)
library(tidyverse)
library(DT)
library(rsconnect)

# Loan calculation function
calc_annual_payment <- function(principal, rate, term_years) {
  (rate * principal * (1 + rate)^term_years) /
    ((1 + rate)^term_years - 1)
}

run_scenario <- function(purchase_price, dp_pct, mortgage_rate,
                         loan_term_years, tax_rate, scenario_id) {
  dp         <- dp_pct * purchase_price
  loan_amt   <- purchase_price - dp
  pmt_annual <- calc_annual_payment(loan_amt, mortgage_rate, loan_term_years)
  pmt_month  <- pmt_annual / 12
  
  # Monthly property tax
  tax_annual <- (tax_rate / 100) * purchase_price
  tax_month  <- tax_annual / 12
  
  # Monthly home insurance (0.2% of home price annually)
  insur_annual <- purchase_price * 0.002
  insur_month  <- insur_annual / 12
  
  total_monthly <- pmt_month + tax_month + insur_month
  
  # HTML-formatted breakdown for tooltip
  breakdown <- sprintf(
    "Mortgage: $%s\nEst. property tax: $%s\nEst. home insurance: $%s",
    format(round(pmt_month), big.mark = ","),
    format(round(tax_month), big.mark = ","),
    format(round(insur_month), big.mark = ",")
  )
  
  tibble(
    `Home price` = paste0("$", format(purchase_price, big.mark = ",")),
    `DP %` = paste0(round(dp_pct * 100, 1), "%"),
    `DP $` = paste0("$", format(round(dp), big.mark = ",")),
    `Loan years` = loan_term_years,
    `Interest rate` = paste0(round(mortgage_rate * 100, 2), "%"),
    `Total loan` = sprintf(
      "<span title='Annual payments: $%s\nMonthly payments: $%s'>$%s</span>",
      format(round(pmt_annual), big.mark = ","),
      format(round(pmt_month), big.mark = ","),
      format(round(loan_amt), big.mark = ",")
    ),
    `Loan payments`  = paste0("$", format(round(pmt_month), big.mark = ",")),
    `Total monthly payments` = sprintf(
      "<span title='%s'>$%s</span>",
      breakdown,
      format(round(total_monthly), big.mark = ",")
    )
  )
}


# UI for a single scenario block
scenarioInputUI <- function(id, label) {
  ns <- NS(id)
  wellPanel(
    fluidRow(
      column(10, h5(label)),
      column(2, actionButton(ns("remove"), "✕", class = "btn-sm"))
    ),
    numericInput(ns("price"), "Home Price", value = 700000, step = 10000),
    sliderInput(
      ns("dp"), # new ID
      "Down Payment ($)",
      min   = 0,
      max   = 700000, # initial max (gets updated in server)
      value = 0.2 * 700000, # 20% default
      step  = 1000,
      pre   = "$",
      sep   = ","
    ),
    sliderInput(ns("rate"), "Mortgage Rate",
      min = 0.01, max = 0.1, value = 0.05, step = 0.001
    ),
    selectInput(ns("term"), "Loan Term",
      choices = c(10, 15, 20, 30), selected = 15
    ),
    numericInput(ns("tax_rate"), "Annual Property Tax Rate (%)",
                 value = 1.2, min = 0, max = 10, step = 0.1)
  )
}

# Server logic for one scenario block
scenarioInputServer <- function(id, remove_callback) {
  moduleServer(id, function(input, output, session) {
    # remove button
    observeEvent(input$remove, {
      remove_callback(id)
    })

    # whenever the price changes, update the dp slider max & reset to 20%
    observeEvent(input$price,
      {
        updateSliderInput(
          session, "dp",
          max = input$price,
          value = round(input$price * 0.2, -3)
        )
      },
      ignoreInit = FALSE
    )

    # expose a reactive list with dp_pct computed under the hood
    reactive({
      list(
        price  = input$price,
        dp_pct = input$dp / input$price,
        rate   = input$rate,
        term   = as.numeric(input$term),
        tax_rate = input$tax_rate
      )
    })
  })
}

# UI
ui <- fluidPage(
  fluidRow(
    column(
      width = 8,
      tags$div(
        h2("Compare Mortgage Scenarios"),
        tags$a(
          href = "https://github.com/dal211/projects/tree/main/personal/down_payment", target = "_blank",
          style = "font-size:16px; text-decoration: none;",
          icon("github"), "GitHub"
        )
      )
    ),
    column(
      width = 4, align = "right",
      downloadButton("download_table", "Export CSV", class = "btn-sm")
    )
  ),
  hr(),
  
  # ── Scenario inputs & table ──
  fluidRow(
    column(
      4,
      actionButton("add_scenario", "Add Scenario"),
      br(), br(),
      uiOutput("scenario_inputs")
    ),
    column(
      8,
      DTOutput("comparison_table")
    )
  )
)


server <- function(input, output, session) {
  # Maximum number of scenario blocks
  max_blocks <- 11
  
  # Track active scenario IDs and the next available ID
  rv      <- reactiveValues(ids = 1)
  next_id <- reactiveVal(2)
  
  # Add a new scenario when the button is pressed
  observeEvent(input$add_scenario, {
    if (length(rv$ids) < max_blocks) {
      rv$ids <- c(rv$ids, next_id())
      next_id(next_id() + 1)
    }
  })
  
  # Render UI for each active scenario
  output$scenario_inputs <- renderUI({
    lapply(rv$ids, function(i) {
      scenarioInputUI(paste0("s", i), strong(paste("Scenario", i)))
    })
  })
  
  
  # Launch a server module for each scenario, wiring up removal
  scenario_values <- reactiveValues()
  observe({
    for (i in rv$ids) {
      module_id <- paste0("s", i)
      if (is.null(scenario_values[[module_id]])) {
        scenario_values[[module_id]] <- scenarioInputServer(
          module_id,
          function(rm_id) {
            if (length(rv$ids) > 1) {
              rv$ids <- setdiff(rv$ids, as.integer(sub("s", "", rm_id)))
              scenario_values[[rm_id]] <- NULL
            }
          }
        )
      }
    }
  })
  
  # Combine all scenarios into one reactive table
  table_data <- reactive({
    req(rv$ids)
    map2_dfr(seq_along(rv$ids), rv$ids, ~ {
      vals <- scenario_values[[paste0("s", .y)]]()
      run_scenario(
        purchase_price  = vals$price,
        dp_pct          = vals$dp_pct,
        mortgage_rate   = vals$rate,
        loan_term_years = vals$term,
        tax_rate        = vals$tax_rate,
        scenario_id     = .x
      )
    })
  })
  
  # Render the comparison table
  output$comparison_table <- renderDT({
    datatable(
      table_data(),
      rownames = FALSE,
      escape   = FALSE,  # <-- allow HTML tooltips to render
      options  = list(dom = "t")
    )
  })
  
  
  # Download handler for exporting the table as CSV
  output$download_table <- downloadHandler(
    filename = function() {
      paste0("mortgage_scenarios_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(table_data(), file)
    }
  )
}


shinyApp(ui, server)
