library(shiny)
library(tidyverse)
library(DT)
library(rsconnect)

# Loan calculation function
calc_annual_payment <- function(principal, rate, term_years) {
  (rate * principal * (1 + rate)^term_years) /
    ((1 + rate)^term_years - 1)
}

# Enhanced scenario runner for two lenders
after_dp <- function(purchase_price, dp_pct) {
  purchase_price * (1 - dp_pct)
}

run_scenario <- function(purchase_price, dp_pct, rate1, term1,
                         rate2, term2, split_pct, tax_rate) {
  dp      <- dp_pct * purchase_price
  total_loan <- after_dp(purchase_price, dp_pct)
  loan1   <- total_loan * split_pct
  loan2   <- total_loan * (1 - split_pct)
  
  # Annual mortgage payments
  pmt1_a  <- calc_annual_payment(loan1, rate1, term1)
  pmt2_a  <- calc_annual_payment(loan2, rate2, term2)
  pmt_annual <- pmt1_a + pmt2_a
  pmt_month  <- pmt_annual / 12
  
  # Taxes & insurance
  tax_month   <- (tax_rate / 100) * purchase_price / 12
  insur_month <- purchase_price * 0.002 / 12
  total_monthly <- pmt_month + tax_month + insur_month
  
  # Tooltip breakdown
  breakdown <- sprintf(
    "Loan1 (%s%% @ %s%%/%dyr): $%s\nLoan2 (%s%% @ %s%%/%dyr): $%s\nTax: $%s\nIns.: $%s",
    round(split_pct * 100), round(rate1 * 100,2), term1, format(round(pmt1_a/12), big.mark=","),
    round((1-split_pct)*100), round(rate2 * 100,2), term2, format(round(pmt2_a/12), big.mark=","),
    format(round(tax_month), big.mark=","),
    format(round(insur_month), big.mark=","))
  
  tibble(
    `Home price`            = paste0("$", format(purchase_price, big.mark=",")),
    `DP %`                  = paste0(round(dp_pct * 100,1), "%"),
    `DP $`                  = paste0("$", format(round(dp), big.mark=",")),
    `Split`                 = paste0(round(split_pct*100),"% / ", round((1-split_pct)*100),"%"),
    `Loan years`            = paste0(term1, " / ", term2),
    `Interest rate`         = paste0(round(rate1*100,2), "% / ", round(rate2*100,2), "%"),
    `Total loan`            = paste0("$", format(round(total_loan), big.mark=",")),
    `Total monthly`         = sprintf("<span title='%s'>$%s</span>",
                                      breakdown, format(round(total_monthly), big.mark=","))
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
    numericInput(ns("price"),    "Home Price", value = 700000, step = 10000),
    sliderInput(ns("dp"),        "Down Payment ($)", min=0, max=700000, value=140000,
                step=1000, pre="$", sep=","),
    sliderInput(ns("split"),     "Portion to Loan #1 (%)", min=0, max=100, value=60, step=5),
    sliderInput(ns("rate"),      "Mortgage Rate #1", min=0.01, max=0.1, value=0.05, step=0.001),
    selectInput(ns("term"),      "Loan Term #1 (years)", choices=c(10,15,20,30), selected=15),
    sliderInput(ns("rate2"),     "Mortgage Rate #2", min=0.01, max=0.1, value=0.04, step=0.001),
    selectInput(ns("term2"),     "Loan Term #2 (years)", choices=c(10,15,20,30), selected=15),
    numericInput(ns("tax_rate"), "Annual Property Tax Rate (%)", value=1.2, min=0, max=10, step=0.1)
  )
}

# Server logic for one scenario block
scenarioInputServer <- function(id, remove_callback) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$remove, { remove_callback(id) })
    observeEvent(input$price, {
      updateSliderInput(session, "dp", max=input$price,
                        value=round(input$price*0.2, -3))
    }, ignoreInit=FALSE)
    
    reactive({
      list(
        price     = input$price,
        dp_pct    = input$dp / input$price,
        split_pct = input$split / 100,
        rate1     = input$rate,
        term1     = as.numeric(input$term),
        rate2     = input$rate2,
        term2     = as.numeric(input$term2),
        tax_rate  = input$tax_rate
      )
    })
  })
}

# UI
ui <- fluidPage(
  fluidRow(
    column(8, tags$div(h2("Compare Mortgage Scenarios"),
                       tags$a(href="https://github.com/...", icon("github"), "GitHub"))),
    column(4, align="right", downloadButton("download_table","Export CSV", class="btn-sm"))
  ),
  hr(),
  fluidRow(
    column(4, actionButton("add_scenario","Add Scenario"), br(), br(), uiOutput("scenario_inputs")),
    column(8, DTOutput("comparison_table"))
  )
)

# Server
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
    lapply(rv$ids, function(i) scenarioInputUI(paste0("s",i), strong(paste("Scenario",i))))
  })
  
  scenario_values <- reactiveValues()
  observe({
    for (i in rv$ids) {
      id0 <- paste0("s",i)
      if (is.null(scenario_values[[id0]])) {
        scenario_values[[id0]] <- scenarioInputServer(id0, function(rm) {
          if (length(rv$ids)>1) {
            rv$ids <- setdiff(rv$ids, i)
            scenario_values[[rm]] <- NULL
          }
        })
      }
    }
  })
  
  table_data <- reactive({
    req(rv$ids)
    map_dfr(rv$ids, function(i) {
      vals <- scenario_values[[paste0("s",i)]]()
      run_scenario(
        purchase_price=vals$price,
        dp_pct=vals$dp_pct,
        rate1=vals$rate1,
        term1=vals$term1,
        rate2=vals$rate2,
        term2=vals$term2,
        split_pct=vals$split_pct,
        tax_rate=vals$tax_rate
      )
    })
  })
  
  output$comparison_table <- renderDT({
    datatable(table_data(), rownames=FALSE, escape=FALSE, options=list(dom="t"))
  })
  
  output$download_table <- downloadHandler(
    filename = function() paste0("mortgage_scenarios_", Sys.Date(), ".csv"),
    content  = function(file) write_csv(table_data(), file)
  )
}

shinyApp(ui, server)