# app.R

library(shiny)
library(tidyverse)
library(DT)
library(openxlsx)
library(rsconnect)

# --- Category choices ---
income_cats     <- c("Paychecks/Salary")
deduction_cats  <- c(
  "Total Taxes",
  "Retirement Deduction",
  "Health Insurance Deduction",
  "Long-term Disability Deduction",
  "Transit Deduction"
)
expense_cats    <- c(
  "Rent",
  "Groceries",
  "Restaurants",
  "General Merchandise",
  "Travel",
  "Insurance",
  "Healthcare/Medical",
  "Taxes",
  "Entertainment",
  "Cable/Satellite",
  "Online Services",
  "Personal Care",
  "Clothing/Shoes",
  "Gasoline/Fuel",
  "Charitable Giving",
  "Dues & Subscriptions",
  "Education",
  "Electronics",
  "Automotive",
  "Utilities",
  "Home Improvement",
  "Office Supplies",
  "Gifts",
  "Printing",
  "Postage & Shipping",
  "Service Charges/Fees",
  "Telephone",
  "Hobbies"
)

# --- Per-row UI module ---
entryRowUI <- function(id) {
  ns <- NS(id)
  wellPanel(
    fluidRow(
      column(3,
             selectInput(
               ns("type"), NULL,
               choices = c("Income", "Deductions", "Expenses")
             )
      ),
      column(5,
             conditionalPanel(
               condition = sprintf("input['%s'] == 'Income'", ns("type")),
               selectInput(ns("category_inc"), NULL, choices = income_cats)
             ),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'Deductions'", ns("type")),
               selectInput(ns("category_ded"), NULL, choices = deduction_cats)
             ),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'Expenses'", ns("type")),
               selectInput(ns("category_exp"), NULL, choices = expense_cats)
             )
      ),
      column(3,
             numericInput(ns("amount"), NULL, value = 0, min = 0, step = 100)
      ),
      column(1,
             actionButton(ns("remove"), NULL, icon = icon("times"), class = "btn-sm")
      )
    )
  )
}

# --- Per-row server module ---
entryRowServer <- function(id, remove_callback) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$remove, { remove_callback(id) })
    reactive({
      list(
        type     = input$type,
        category = case_when(
          input$type == "Income"     ~ input$category_inc,
          input$type == "Deductions" ~ input$category_ded,
          TRUE                          ~ input$category_exp
        ),
        amount   = input$amount
      )
    })
  })
}

# --- UI ---
ui <- fluidPage(
  fluidRow(
    column(8,
           tags$div(
             h2("Household Income Statement"),
             tags$a(
               href = "https://github.com/dal211/projects/tree/main/personal/household_income_statement",
               icon("github"), "GitHub",
               target = "_blank",
               style = "font-size:16px; text-decoration:none;"
             )
           )
    ),
    column(4, align = "right",
           downloadButton("download_excel", "Export Excel", class = "btn-sm")
    )
  ),
  hr(),
  fluidRow(
    column(4,
           h4("Entries"),
           actionButton("add_row", "Add Entry", class = "btn-sm"),
           br(), br(),
           uiOutput("entry_rows")
    ),
    column(8,
           h4("Income Statement"),
           DTOutput("income_table")
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  rv      <- reactiveValues(ids = 1)
  next_id <- reactiveVal(2)
  entries <- reactiveValues()
  
  # Add new row
  observeEvent(input$add_row, {
    new_id <- next_id()
    rv$ids <- c(rv$ids, new_id)
    next_id(new_id + 1)
  })
  
  # Render rows
  output$entry_rows <- renderUI({
    tagList(lapply(rv$ids, function(i) entryRowUI(paste0("e", i))))
  })
  
  # Launch modules
  observe({
    for (i in rv$ids) {
      key <- paste0("e", i)
      if (is.null(entries[[key]])) {
        entries[[key]] <- entryRowServer(key, function(id_to_rm) {
          id_num <- as.numeric(sub("^e", "", id_to_rm))
          rv$ids <- setdiff(rv$ids, id_num)
          entries[[id_to_rm]] <- NULL
        })
      }
    }
  })
  
  # Build table
  table_data <- reactive({
    df <- map_dfr(rv$ids, function(i) {
      vals <- entries[[paste0("e", i)]]()
      tibble(
        Type     = vals$type,
        Category = vals$category,
        Amount   = vals$amount
      )
    })
    
    total_inc <- df %>%
      filter(Type == "Income") %>%
      pull(Amount) %>% sum(na.rm = TRUE)
    
    total_ded <- df %>%
      filter(Type == "Deductions") %>%
      pull(Amount) %>% sum(na.rm = TRUE)
    
    total_exp <- df %>%
      filter(Type == "Expenses") %>%
      pull(Amount) %>% sum(na.rm = TRUE)
    
    net_inc <- total_inc - total_ded - total_exp
    
    # Summary rows in desired order
    bind_rows(
      df,
      tibble(Type = "","Category" = "—",                      Amount = NA_real_),
      tibble(Type = "","Category" = "Total Income",           Amount = total_inc),
      tibble(Type = "","Category" = "Total Deductions",       Amount = total_ded),
      tibble(Type = "","Category" = "Total Expenses",         Amount = total_exp),
      tibble(Type = "","Category" = "Net Income",            Amount = net_inc)
    )
  })
  
  output$income_table <- renderDT({
    datatable(
      table_data(),
      rownames = FALSE,
      options  = list(dom = "t", paging = FALSE),
      colnames = c("Type", "Line Item", "Annual $")
    ) %>% formatCurrency("Amount")
  })
  
  output$download_excel <- downloadHandler(
    filename = function() sprintf("household_income_%s.xlsx", Sys.Date()),
    content  = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "Income Statement")
      writeData(wb, "Income Statement", table_data())
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

# --- Run App ---
shinyApp(ui, server)
