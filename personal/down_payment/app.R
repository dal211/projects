library(shiny)
library(tidyverse)
library(DT)
library(rsconnect)

# Loan calculation function
calc_annual_payment <- function(principal, rate, term_years) {
  (rate * principal * (1 + rate)^term_years) /
    ((1 + rate)^term_years - 1)
}

after_dp <- function(purchase_price, dp_pct) {
  purchase_price * (1 - dp_pct)
}

# Run a single scenario, optionally splitting across two lenders
run_scenario <- function(purchase_price, dp_pct,
                         rate1, term1,
                         split_enable, loan1_amt, rate2, term2,
                         tax_rate) {
  dp <- dp_pct * purchase_price
  total_loan <- after_dp(purchase_price, dp_pct)
  
  if (isTRUE(split_enable)) {
    # clamp loan1_amt
    loan1 <- pmin(pmax(loan1_amt, 0), total_loan)
    loan2 <- total_loan - loan1
    pmt1_a <- calc_annual_payment(loan1, rate1, term1)
    pmt2_a <- calc_annual_payment(loan2, rate2, term2)
    loan_years <- paste0(term1, " / ", term2)
    interest_rate <- paste0(round(rate1 * 100, 2), "% / ", round(rate2 * 100, 2), "%")
  } else {
    loan1 <- total_loan
    pmt1_a <- calc_annual_payment(loan1, rate1, term1)
    pmt2_a <- 0
    loan_years <- as.character(term1)
    interest_rate <- paste0(round(rate1 * 100, 2), "%")
  }
  
  pmt_annual <- pmt1_a + pmt2_a
  pmt_month <- pmt_annual / 12
  
  # Taxes & insurance
  tax_month <- (tax_rate / 100) * purchase_price / 12
  insur_month <- purchase_price * 0.002 / 12
  total_monthly <- pmt_month + tax_month + insur_month
  
  breakdown <- sprintf(
    "Loan1: $%s\nLoan2: $%s\nTax: $%s\nIns.: $%s",
    format(round(pmt1_a / 12), big.mark = ","),
    format(round(pmt2_a / 12), big.mark = ","),
    format(round(tax_month), big.mark = ","),
    format(round(insur_month), big.mark = ",")
  )
  
  tibble(
    `Home price` = paste0("$", format(purchase_price, big.mark = ",")),
    `DP %` = paste0(round(dp_pct * 100, 1), "%"),
    `DP $` = paste0("$", format(round(dp), big.mark = ",")),
    `Loan years` = loan_years,
    `Interest rate` = interest_rate,
    `Total loan` = paste0("$", format(round(total_loan), big.mark = ",")),
    `Monthly` = sprintf("<span title='%s'>$%s</span>", breakdown, format(round(total_monthly), big.mark = ","))
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
    sliderInput(ns("dp"), "Down Payment ($)", min = 0, max = 700000, value = 0.2 * 700000, step = 1000),
    checkboxInput(ns("split_enable"), "Split mortgage between two lenders", value = FALSE),
    sliderInput(ns("rate1"), "Mortgage Rate #1", min = 0.01, max = 0.1, value = 0.05, step = 0.001),
    selectInput(ns("term1"), "Loan Term #1 (years)", choices = c(10, 15, 20, 30), selected = 20),
    conditionalPanel(
      condition = sprintf("input['%s'] == true", ns("split_enable")),
      numericInput(ns("loan1_amt"), "Loan #1 Amount ($)", value = 0, min = 0, max = 0, step = 1000),
        tags$div(style = "margin-top: 10px;",
    strong("Loan #2 Amount ($)"),
    textOutput(ns("loan2_amt"))
  ),
      sliderInput(ns("rate2"), "Mortgage Rate #2", min = 0.01, max = 0.1, value = 0.04, step = 0.001),
      selectInput(ns("term2"), "Loan Term #2 (years)", choices = c(10, 15, 20, 30), selected = 20)
    ),
    numericInput(ns("tax_rate"), "Annual Property Tax Rate (%)", value = 1.2, min = 0, max = 10, step = 0.1)
  )
}

# Server logic for one scenario block
scenarioInputServer <- function(id, remove_callback) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$remove, { remove_callback(id) })
    
    # inside scenarioInputServer:
    observeEvent(input$price, {
      # only update the slider max; leave `value` untouched
      updateSliderInput(session, "dp",
                        max = input$price
      )
    }, ignoreInit = TRUE)
    
    # 1) Adjust DP slider **only** on price change,
    #    clamping the current dp if needed but otherwise preserving it.
    observeEvent(input$price, {
      old_dp   <- input$dp
      new_max  <- input$price
      # if they’d chosen more than the new price, drop to new price
      new_dp   <- min(old_dp, new_max)
      updateSliderInput(session, "dp",
                        max   = new_max,
                        value = new_dp
      )
    }, ignoreInit = TRUE)
    
    # enforce constraint on typed values
    observeEvent(input$loan1_amt, {
      if (isTRUE(input$split_enable)) {
        total_loan <- input$price - input$dp
        if (!is.na(input$loan1_amt)) {
          if (input$loan1_amt > total_loan) {
            updateNumericInput(session, "loan1_amt", value = total_loan)
          } else if (input$loan1_amt < 0) {
            updateNumericInput(session, "loan1_amt", value = 0)
          }
        }
      }
    })
    
    # enforce constraint on Annual Property Tax Rate: max 3%
    observeEvent(input$tax_rate, {
      if (!is.na(input$tax_rate) && input$tax_rate > 3) {
        updateNumericInput(session, "tax_rate", value = 3)
      }
    })
    
    output$loan2_amt <- renderText({
      if (!isTRUE(input$split_enable)) return(NULL)
      total_loan <- input$price - input$dp
      loan2      <- total_loan - input$loan1_amt
      paste0("$", format(round(loan2), big.mark = ","))
    })
    
    reactive({
      list(
        price = input$price,
        dp_pct = input$dp / input$price,
        split_enable = isTRUE(input$split_enable),
        loan1_amt = if (isTRUE(input$split_enable)) input$loan1_amt else NA_real_,
        rate1 = input$rate1,
        term1 = as.numeric(input$term1),
        rate2 = if (isTRUE(input$split_enable)) input$rate2 else NA_real_,
        term2 = if (isTRUE(input$split_enable)) as.numeric(input$term2) else NA_real_,
        tax_rate = input$tax_rate
      )
    })
  })
}

# App UI
ui <- fluidPage(
  fluidRow(
    column(8, tags$div(
      h2("Compare Mortgage Scenarios"),
      tags$a(
        href = "https://github.com/dal211/projects/tree/main/personal/down_payment",
        icon("github"), "GitHub", target = "_blank",
        style = "font-size:16px; text-decoration:none; margin-right: 15px;"
      ),
      tags$a(
        href = "https://richardgasquet.shinyapps.io/income_statement/",
        icon("file-invoice-dollar"), "See how your mortgage impacts your income", target = "_blank",
        style = "font-size:16px; text-decoration:none;"
      )
    )),
    column(4, align = "right", downloadButton("download_table", "Export CSV", class = "btn-sm"))
  ),
  hr(),
  fluidRow(
    column(4, actionButton("add_scenario", "Add Scenario"), br(), br(),
    div(id = "scenario_container"), uiOutput("scenario_inputs")),
    column(8, DTOutput("comparison_table"))
  )
)


# App server
server <- function(input, output, session) {
  max_blocks <- 11
  rv         <- reactiveValues(ids = character())
  next_id    <- reactiveVal(1)
  scenario_values <- reactiveValues()
  
  # 1) On app start, insert the first scenario
  observeEvent(TRUE, {
    id <- paste0("s", next_id())
    insertUI(
      selector = "#scenario_container",
      where    = "beforeEnd",
      ui       = scenarioInputUI(id, strong(paste("Scenario", next_id())))
    )
    scenario_values[[id]] <- scenarioInputServer(id, function(rm) {
      removeUI(selector = paste0("#", rm, "-row"))
      rv$ids <- setdiff(rv$ids, rm)
      scenario_values[[rm]] <- NULL
    })
    rv$ids <- c(rv$ids, id)
    next_id(next_id() + 1)
  }, once = TRUE)
  
  # 2) When “Add Scenario” is clicked, append exactly one new block
  observeEvent(input$add_scenario, {
    req(length(rv$ids) < max_blocks)
    id <- paste0("s", next_id())
    insertUI(
      selector = "#scenario_container",
      where    = "beforeEnd",
      ui       = scenarioInputUI(id, strong(paste("Scenario", next_id())))
    )
    scenario_values[[id]] <- scenarioInputServer(id, function(rm) {
      removeUI(selector = paste0("#", rm, "-row"))
      rv$ids <- setdiff(rv$ids, rm)
      scenario_values[[rm]] <- NULL
    })
    rv$ids <- c(rv$ids, id)
    next_id(next_id() + 1)
  })
  
  # 3) Build the comparison table from whatever scenarios currently exist
  table_data <- reactive({
    req(rv$ids)
    map_dfr(rv$ids, function(i) {
      vals <- scenario_values[[i]]()
      run_scenario(
        purchase_price = vals$price,
        dp_pct         = vals$dp_pct,
        rate1          = vals$rate1,
        term1          = vals$term1,
        split_enable   = vals$split_enable,
        loan1_amt      = vals$loan1_amt,
        rate2          = vals$rate2,
        term2          = vals$term2,
        tax_rate       = vals$tax_rate
      )
    })
  })
  
  # 4) Render the DT
  output$comparison_table <- renderDT({
    datatable(table_data(),
              rownames = FALSE,
              escape   = FALSE,
              options  = list(dom = "t"))
  })
  
  # 5) CSV download
  output$download_table <- downloadHandler(
    filename = function() sprintf("mortgage_scenarios_%s.csv", Sys.Date()),
    content  = function(file) write_csv(table_data(), file)
  )
}

# If this is a single-file app, end with:
shinyApp(ui, server)

