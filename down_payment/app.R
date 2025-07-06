# Load libraries
library(shiny)
library(shinydashboardPlus)
library(bslib)
library(DT)
library(ggplot2)
library(plotly)
library(scales)

# Scenario module UI
scenarioUI <- function(id) {
  ns <- NS(id)
  shinydashboardPlus::box(
    title = paste("Scenario", id),
    width = 12,
    solidHeader = TRUE,
    status = "primary",
    collapsible = TRUE,
    fluidRow(
      column(6,
             sliderInput(ns("price"), "Home price", min = 100000, max = 2000000, value = 700000, step = 10000, pre = "$"),
             sliderInput(ns("rate"), "Interest rate", min = 0.01, max = 0.1, step = 0.001, value = 0.05, post = "%"),
             selectInput(ns("term"), "Loan term (years)", choices = c(10, 15, 20, 30), selected = 15),
             sliderInput(ns("dp_pct"), "Down payment %", min = 0, max = 1, step = 0.01, value = 0.2, post = "%")
      ),
      column(6,
             checkboxInput(ns("opt_tax"), "Include Property Tax (% of price)", FALSE),
             conditionalPanel("input.opt_tax", ns = ns,
                              sliderInput(ns("tax_pct"), "Tax rate", min = 0, max = 0.05, step = 0.001, value = 0.012)
             ),
             checkboxInput(ns("opt_ins"), "Include Homeowners Insurance ($/year)", FALSE),
             conditionalPanel("input.opt_ins", ns = ns,
                              numericInput(ns("ins_amount"), "Insurance cost", value = 1200, step = 100)
             ),
             checkboxInput(ns("opt_hoa"), "Include HOA fees ($/month)", FALSE),
             conditionalPanel("input.opt_hoa", ns = ns,
                              numericInput(ns("hoa_amt"), "HOA monthly", value = 200, step = 50)
             )
      )
    )
  )
}

# Scenario module server logic
scenarioServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      price <- input$price
      rate <- input$rate
      term <- input$term
      dp_pct <- input$dp_pct
      dp_amt <- dp_pct * price
      loan <- price - dp_amt
      pmt_ann <- (rate * loan * (1 + rate)^term) / ((1 + rate)^term - 1)
      pmt_mon <- pmt_ann / 12
      total_interest <- pmt_ann * term - loan
      property_tax <- if (input$opt_tax) input$tax_pct * price else 0
      insurance <- if (input$opt_ins) input$ins_amount else 0
      hoa <- if (input$opt_hoa) input$hoa_amt * 12 else 0
      
      amort_schedule <- data.frame(
        Year = 1:term,
        Payment = rep(pmt_ann, term),
        Interest = numeric(term),
        Principal = numeric(term),
        Balance = numeric(term)
      )
      balance <- loan
      for (i in 1:term) {
        interest_i <- balance * rate
        principal_i <- pmt_ann - interest_i
        balance <- balance - principal_i
        amort_schedule$Interest[i] <- interest_i
        amort_schedule$Principal[i] <- principal_i
        amort_schedule$Balance[i] <- max(balance, 0)
      }
      
      list(
        price = price,
        rate = rate,
        term = term,
        dp_pct = dp_pct,
        dp_amt = dp_amt,
        loan = loan,
        pmt_ann = pmt_ann,
        pmt_mon = pmt_mon,
        total_interest = total_interest,
        annual_costs = pmt_ann + property_tax + insurance + hoa,
        monthly_costs = pmt_mon + property_tax / 12 + insurance / 12 + hoa / 12,
        amort_schedule = amort_schedule
      )
    })
  })
}

# UI
i <- fluidPage(
  theme = bs_theme(bootswatch = "flatly", base_font = font_google("Lato")),
  title = "Mortgage Dashboard",
  navbarPage(
    "Mortgage Dashboard",
    tabPanel("Single Scenario",
             fluidRow(
               column(4, scenarioUI("s1")),
               column(4,
                      shinydashboardPlus::box(
                        title = "Affordability",
                        width = 12,
                        solidHeader = TRUE,
                        status = "info",
                        numericInput("income", "Annual net income", value = 120000, step = 5000),
                        uiOutput("afford_meter")
                      ),
                      shinydashboardPlus::box(
                        title = "Amortization Charts",
                        width = 12,
                        solidHeader = TRUE,
                        status = "warning",
                        plotlyOutput("amort_plot"),
                        plotlyOutput("bal_plot")
                      )
               ),
               column(4,
                      shinydashboardPlus::box(
                        title = "Summary",
                        width = 12,
                        solidHeader = TRUE,
                        status = "success",
                        tableOutput("single_table")
                      )
               )
             )
    ),
    tabPanel("Compare Scenarios",
             fluidRow(
               shinydashboardPlus::box(
                 title = tagList("Scenarios", actionButton("add_sc", "Add Scenario", icon = icon("plus"))),
                 width = 12,
                 solidHeader = TRUE,
                 uiOutput("compare_inputs")
               ),
               shinydashboardPlus::box(
                 title = "Comparison Table",
                 width = 12,
                 solidHeader = TRUE,
                 DTOutput("compare_table"),
                 downloadButton("download_csv", "Download CSV")
               )
             )
    )
  )
)

# Server
s <- function(input, output, session) {
  scen1 <- scenarioServer("s1")
  
  output$single_table <- renderTable({
    d <- scen1()
    data.frame(
      `Down Payment` = dollar(d$dp_amt),
      `Loan Amount`  = dollar(d$loan),
      `Annual PMT`   = dollar(round(d$pmt_ann)),
      `Monthly PMT`  = dollar(round(d$pmt_mon)),
      `Total Interest` = dollar(round(d$total_interest)),
      `Monthly Total Cost` = dollar(round(d$monthly_costs))
    )
  }, striped = TRUE, hover = TRUE)
  
  output$afford_meter <- renderUI({
    d <- scen1()
    monthly_income <- input$income / 12
    pct <- d$monthly_costs / monthly_income
    shinydashboardPlus::gauge(pct * 100, min = 0, max = 100, symbol = "%",
                              gaugeSectors(success = c(0,30), warning = c(30,35), danger = c(35,100)))
  })
  
  output$amort_plot <- renderPlotly({
    d <- scen1()
    df <- d$amort_schedule
    p <- ggplot(df, aes(x = Year)) +
      geom_line(aes(y = Interest, color = "Interest")) +
      geom_line(aes(y = Principal, color = "Principal")) +
      labs(title = "Principal vs. Interest Over Time", y = "$ Amount") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$bal_plot <- renderPlotly({
    d <- scen1()
    df <- d$amort_schedule
    p <- ggplot(df, aes(x = Year, y = Balance)) +
      geom_line(color = "steelblue") +
      labs(title = "Remaining Loan Balance", y = "$ Balance") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Compare Scenarios
  rv <- reactiveValues(count = 1)
  observeEvent(input$add_sc, { rv$count <- min(rv$count + 1, 3) })
  output$compare_inputs <- renderUI({
    lapply(seq_len(rv$count), function(i) scenarioUI(paste0("cmp", i)))
  })
  compare_list <- reactive({
    lapply(seq_len(rv$count), function(i) scenarioServer(paste0("cmp", i))())
  })
  output$compare_table <- renderDT({
    comps <- compare_list()
    df <- data.frame(
      Metric = c("Down Payment", "Loan Amount", "Annual PMT", "Monthly PMT", "Total Interest", "Monthly Cost")
    )
    for (i in seq_along(comps)) {
      d <- comps[[i]]
      df[[paste0("Scenario ", i)]] <- c(
        dollar(d$dp_amt),
        dollar(d$loan),
        dollar(d$pmt_ann),
        dollar(d$pmt_mon),
        dollar(d$total_interest),
        dollar(d$monthly_costs)
      )
    }
    datatable(df)
  })
  output$download_csv <- downloadHandler(
    filename = function() paste0("mortgage_compare_", Sys.Date(), ".csv"),
    content = function(file) {
      comps <- compare_list()
      df <- data.frame(
        Metric = c("Down Payment", "Loan Amount", "Annual PMT", "Monthly PMT", "Total Interest", "Monthly Cost")
      )
      for (i in seq_along(comps)) {
        d <- comps[[i]]
        df[[paste0("Scenario ", i)]] <- c(
          d$dp_amt,
          d$loan,
          d$pmt_ann,
          d$pmt_mon,
          d$total_interest,
          d$monthly_costs
        )
      }
      write.csv(df, file, row.names = FALSE)
    }
  )
}

shinyApp(i, s)
