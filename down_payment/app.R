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
                         loan_term_years, scenario_id) {
  dp <- dp_pct * purchase_price
  loan_amt <- purchase_price - dp
  pmt <- calc_annual_payment(loan_amt, mortgage_rate, loan_term_years)

  tibble(
    Scenario = paste("Scenario", scenario_id),
    `Home price` = paste0("$", format(purchase_price, big.mark = ",")),
    `DP %` = paste0(round(dp_pct * 100, 1), "%"),
    `DP $` = paste0("$", format(round(dp), big.mark = ",")),
    `Term years` = loan_term_years,
    `Mortgage interest rate` = paste0(round(mortgage_rate * 100, 2), "%"),
    `Mortgage amount` = paste0("$", format(round(loan_amt), big.mark = ",")),
    `Annual payments` = paste0("$", format(round(pmt), big.mark = ",")),
    `Monthly payments` = paste0("$", format(round(pmt / 12), big.mark = ","))
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
    )
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
        term   = as.numeric(input$term)
      )
    })
  })
}

ui <- fluidPage(
  titlePanel("Compare Mortgage Scenarios"),
  fluidRow(
    column(
      4,
      actionButton("add_scenario", "Add Scenario"), br(), br(),
      uiOutput("scenario_inputs")
    ),
    column(
      8,
      DTOutput("comparison_table")
    )
  )
)

server <- function(input, output, session) {
  max_blocks <- 11
  rv <- reactiveValues(ids = 1)
  next_id <- reactiveVal(2)

  observeEvent(input$add_scenario, {
    if (length(rv$ids) < max_blocks) {
      rv$ids <- c(rv$ids, next_id())
      next_id(next_id() + 1)
    }
  })

  output$scenario_inputs <- renderUI({
    lapply(rv$ids, function(i) {
      scenarioInputUI(paste0("s", i), paste("Scenario", i))
    })
  })

  scenario_values <- reactiveValues()
  observe({
    for (i in rv$ids) {
      module_id <- paste0("s", i)
      if (is.null(scenario_values[[module_id]])) {
        scenario_values[[module_id]] <- scenarioInputServer(
          module_id, function(rm_id) {
            if (length(rv$ids) > 1) {
              rv$ids <- setdiff(rv$ids, as.integer(sub("s", "", rm_id)))
              scenario_values[[rm_id]] <- NULL
            }
          }
        )
      }
    }
  })

  output$comparison_table <- renderDT({
    req(rv$ids)
    dat <- map2_dfr(seq_along(rv$ids), rv$ids, ~ {
      vals <- scenario_values[[paste0("s", .y)]]()
      run_scenario(
        purchase_price  = vals$price,
        dp_pct          = vals$dp_pct,
        mortgage_rate   = vals$rate,
        loan_term_years = vals$term,
        scenario_id     = .x
      )
    })

    datatable(dat, rownames = FALSE, options = list(dom = "t"))
  })
}

shinyApp(ui, server)
