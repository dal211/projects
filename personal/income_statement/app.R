# app.R

library(shiny)
library(tidyverse)
library(DT)
library(openxlsx)
library(rsconnect)

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
           h4("Enter Annual Amounts"),
           numericInput("salary",       "Salary ($)",              value = 50000, step = 1000),
           numericInput("other_inc",    "Other Income ($)",        value =  5000, step =  500),
           numericInput("mort_pi",      "Mortgage P&I ($)",        value = 12000, step =  500),
           numericInput("prop_tax",     "Property Tax ($)",        value =  3600, step =  100),
           numericInput("insurance",    "Insurance ($)",           value =   600, step =   50)
    ),
    column(8,
           h4("Income Statement"),
           DTOutput("income_table")
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  table_data <- reactive({
    # pull inputs
    salary    <- input$salary
    other_inc <- input$other_inc
    mort_pi   <- input$mort_pi
    prop_tax  <- input$prop_tax
    insurance <- input$insurance
    
    total_inc <- salary + other_inc
    total_exp <- mort_pi + prop_tax + insurance
    net_inc   <- total_inc - total_exp
    
    tibble(
      Category       = c(
        "Salary",
        "Other Income",
        "Mortgage P&I",
        "Property Tax",
        "Insurance",
        "—",
        "Total Income",
        "Total Expenses",
        "Net Income"
      ),
      Amount = c(
        salary,
        other_inc,
        mort_pi,
        prop_tax,
        insurance,
        NA,
        total_inc,
        total_exp,
        net_inc
      )
    )
  })
  
  output$income_table <- renderDT({
    datatable(
      table_data(),
      rownames = FALSE,
      options  = list(dom = "t", paging = FALSE),
      colnames = c("Line Item", "Annual $"),
      escape   = FALSE
    ) %>% 
      formatCurrency("Amount")
  })
  
  output$download_excel <- downloadHandler(
    filename = function() {
      sprintf("household_income_%s.xlsx", Sys.Date())
    },
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "Income Statement")
      writeData(wb, "Income Statement", table_data())
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

# --- Run App ---
shinyApp(ui, server)
