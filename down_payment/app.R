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
    `Home price` = purchase_price,
    `Down payment %` = round(dp_pct * 100, 1),
    `Down payment amt` = round(dp),
    `Loan Term (yrs)` = loan_term_years,
    `Mortgage rate` = round(mortgage_rate * 100, 2),
    `Total mortgage` = round(loan_amt),
    `Annual mortgage pmts` = round(pmt),
    `Monthly pmts` = round(pmt / 12)
  )
}

# UI for a single scenario input block
scenarioInputUI <- function(id, label) {
  ns <- NS(id)
  wellPanel(
    h5(label),
    numericInput(ns("price"), "Home Price", value = 700000, step = 10000),
    sliderInput(ns("dp_pct"), "Down Payment %", min = 0, max = 1, value = 0.2, step = 0.01),
    sliderInput(ns("rate"), "Mortgage Rate", min = 0.01, max = 0.1, value = 0.05, step = 0.001),
    selectInput(ns("term"), "Loan Term", choices = c(10, 15, 20, 30), selected = 15)
  )
}

# Server logic for a single scenario input block
scenarioInputServer <- function(id) {
  moduleServer(id, function(input, output, session) {
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

# UI
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
      # h4("Mortgage Comparison Table"),
      DTOutput("comparison_table")
    )
  )
)

# Server
server <- function(input, output, session) {
  max_blocks <- 11
  rv <- reactiveValues(count = 1)
  
  observeEvent(input$add_scenario, {
    if (rv$count < max_blocks) rv$count <- rv$count + 1
  })
  
  output$scenario_inputs <- renderUI({
    lapply(1:rv$count, function(i) scenarioInputUI(paste0("s", i), paste("Scenario", i)))
  })
  
  scenario_data <- reactive({
    lapply(1:rv$count, function(i) {
      scenarioInputServer(paste0("s", i))()
    })
  })
  
  output$comparison_table <- renderDT({
    dat <- map2(scenario_data(), 1:rv$count, ~{
      run_scenario(
        purchase_price = .x$price,
        dp_pct = .x$dp_pct,
        mortgage_rate = .x$rate,
        loan_term_years = .x$term,
        scenario_id = .y
      )
    }) %>%
      bind_rows()
    
    datatable(dat, rownames = FALSE, options = list(dom = 't'))
  })
}

shinyApp(ui, server)
