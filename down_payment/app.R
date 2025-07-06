library(shiny)
library(tidyverse)
library(DT)

# Loan calculation function
calc_annual_payment <- function(principal, rate, term_years) {
  (rate * principal * (1 + rate)^term_years) / ((1 + rate)^term_years - 1)
}

run_scenario <- function(purchase_price, dp_pct, mortgage_rate, loan_term_years, scenario_id) {
  dp <- dp_pct * purchase_price
  loan_amt <- purchase_price - dp
  pmt <- calc_annual_payment(loan_amt, mortgage_rate, loan_term_years)
  
  tibble(
    Scenario = paste("Scenario", scenario_id),
    `Home price` = paste0("$",format(purchase_price, big.mark = ",")),
    `DP %` = paste0(round(dp_pct * 100, 1), "%"),
    `DP $` = paste0("$", format(round(dp), big.mark = "," )),
    `Term years` = loan_term_years,
    `Mortgage rate` = paste0(round(mortgage_rate * 100, 2),"%"),
    `Mortgage amount` = format(round(loan_amt), big.mark = ","),
    `Annual payments` = format(round(pmt), big.mark = ","),
    `Monthly payments` = format(round(pmt / 12), big.mark = ",")
  )
}

# UI for a single scenario input block, with Remove button
scenarioInputUI <- function(id, label) {
  ns <- NS(id)
  wellPanel(
    fluidRow(
      column(10, h5(label)),
      column(2, actionButton(ns("remove"), "✕", class = "btn-sm"))
    ),
    numericInput(ns("price"), "Home Price", value = 700000, step = 10000),
    sliderInput(ns("dp_pct"), "Down Payment %", min = 0, max = 1, value = 0.2, step = 0.01),
    sliderInput(ns("rate"), "Mortgage Rate", min = 0.01, max = 0.1, value = 0.05, step = 0.001),
    selectInput(ns("term"), "Loan Term", choices = c(10, 15, 20, 30), selected = 15)
  )
}

# Server logic for a single scenario; calls remove_callback(id) when its button is clicked
scenarioInputServer <- function(id, remove_callback) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$remove, {
      remove_callback(id)
    })
    reactive({
      list(
        price = input$price,
        dp_pct = input$dp_pct,
        rate = input$rate,
        term = as.numeric(input$term)
      )
    })
  })
}

ui <- fluidPage(
  titlePanel("Compare Mortgage Scenarios"),
  fluidRow(
    column(
      width = 4,
      actionButton("add_scenario", "Add Scenario"),
      br(), br(),
      uiOutput("scenario_inputs")
    ),
    column(
      width = 8,
      DTOutput("comparison_table")
    )
  )
)

server <- function(input, output, session) {
  max_blocks <- 11
  
  # Track active scenario IDs
  rv <- reactiveValues(ids = c(1))
  next_id <- reactiveVal(2)
  
  # Add scenario
  observeEvent(input$add_scenario, {
    if (length(rv$ids) < max_blocks) {
      rv$ids <- c(rv$ids, next_id())
      next_id(next_id() + 1)
    }
  })
  
  # Render UI for each active scenario
  output$scenario_inputs <- renderUI({
    lapply(rv$ids, function(i) {
      scenarioInputUI(paste0("s", i), paste("Scenario", i))
    })
  })
  
  # Launch server modules, wiring up remove callback
  scenario_values <- reactiveValues()
  observe({
    for (i in rv$ids) {
      id <- paste0("s", i)
      if (is.null(scenario_values[[id]])) {
        scenario_values[[id]] <- scenarioInputServer(id, function(rm_id) {
          # Only remove if more than one remains
          if (length(rv$ids) > 1) {
            rv$ids <- setdiff(rv$ids, as.integer(sub("s", "", rm_id)))
            scenario_values[[rm_id]] <- NULL
          }
        })
      }
    }
  })
  
  # Combine all scenarios into one table
  output$comparison_table <- renderDT({
    req(rv$ids)
    dat <- map2(seq_along(rv$ids), rv$ids, ~{
      inputs <- scenario_values[[paste0("s", .y)]]()
      run_scenario(
        purchase_price  = inputs$price,
        dp_pct          = inputs$dp_pct,
        mortgage_rate   = inputs$rate,
        loan_term_years = inputs$term,
        scenario_id     = .x
      )
    }) %>% bind_rows()
    
    datatable(dat, rownames = FALSE, options = list(dom = 't'))
  })
}

shinyApp(ui, server)
