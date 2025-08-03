# app.R

library(shiny)
library(tidyverse)
library(DT)
library(openxlsx)
library(rsconnect)

# --- Category choices ---
income_cats    <- c(
  "Paychecks/Salary",
  "Investment Income",
  "Rental Income",
  "Government Benefits"
)
deduction_cats <- c(
  "Total Taxes",
  "Retirement Deduction",
  "Health Insurance Deduction",
  "Long-term Disability Deduction",
  "Transit Deduction"
)

expense_cats   <- c(
  # Housing & Maintenance
  "Rent/Mortgage",
  "Renter Insurance",
  "Home Maintenance",
  
  # Food & Dining
  "Groceries",
  "Restaurants/Takeout",
  
  # Transportation & Auto
  "Transit pass",
  "Transit parking",
  "EZ-Pass",
  "Gasoline/Fuel",
  "Car Insurance",
  "Automotive - Car Registration",
  "Automotive - State Inspectation",
  "Automotive - Oil Change",
  "Automotive - Motor Excise Tax",
  
  # Utilities & Services
  "Utilities - Gas",
  "Utilities - Electric",
  "Utilities - WIFI",
  "Cable/Satellite",
  "Telephone",
  
  # Subscriptions & Personal
  "Entertainment/Subscriptions",
  "Personal Care",
  "Clothing/Shoes",
  
  # Healthcare & Taxes
  "Healthcare/Medical",
  "Tax Return Payment",
  
  # Leisure & Gifts
  "Travel/Vacation",
  "Gifts",
  
  # Charitable & Misc
  "Charitable Giving",
  "Misc."
)

# --- Per-row UI module (inline rows) ---
entryRowUI <- function(id) {
  ns <- NS(id)
  tags$div(
    id = ns("row"),
    fluidRow(
      style = "margin-bottom:5px;",
      column(3,
             selectInput(ns("type"), NULL,
                         choices = c("Income","Deductions","Expenses"),
                         width = "100%")
      ),
      column(5,
             conditionalPanel(
               condition = sprintf("input['%s']=='Income'",ns("type")),
               selectInput(ns("category_inc"), NULL, income_cats, width="100%")
             ),
             conditionalPanel(
               condition = sprintf("input['%s']=='Deductions'",ns("type")),
               selectInput(ns("category_ded"), NULL, deduction_cats, width="100%")
             ),
             conditionalPanel(
               condition = sprintf("input['%s']=='Expenses'",ns("type")),
               selectInput(ns("category_exp"), NULL, expense_cats, width="100%")
             )
      ),
      column(3,
             numericInput(ns("amount"), NULL, value=0, min=0, step=100, width="100%")
      ),
      column(1,
             actionButton(ns("remove"), NULL, icon=icon("times"), class="btn-sm")
      )
    )
  )
}

# --- Per-row server module ---
entryRowServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$remove, {
      removeUI(selector = paste0("#", session$ns("row")))
    })
    reactive({
      list(
        Type     = input$type,
        Category = case_when(
          input$type=="Income"     ~ input$category_inc,
          input$type=="Deductions" ~ input$category_ded,
          TRUE                        ~ input$category_exp
        ),
        Amount   = input$amount
      )
    })
  })
}

# --- UI ---
ui <- fluidPage(
  ui <- fluidPage(
    tags$head(
      tags$style(HTML("
      /* zoom out the whole app to 85% */
      body {
        zoom: 0.85;
      }
    "))
    ),
    
    # … the rest of your UI …
  ),
  
  fluidRow(
    column(8,
           tags$div(
             h2("Household Income Statement"),
             tags$a(href="https://github.com/dal211/projects/tree/main/personal/household_income_statement",
                    icon("github"),"GitHub",target="_blank",
                    style="font-size:16px;text-decoration:none;")
           )
    ),
    column(4,align="right",
           downloadButton("download_excel","Export Excel",class="btn-sm")
    )
  ),
  hr(),
  fluidRow(
    column(4,
           h4("Entries"),
           actionButton("add_row","Add Entry",class="btn-sm"),
           br(),br(),
           wellPanel(
             div(id="entry_container")
           )
    ),
    column(8,
           h4("Income Statement"),
           DTOutput("income_table")
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  next_id <- reactiveVal(1)
  entries <- reactiveValues()
  
  # initialize first row
  observe({
    if (next_id()==1) {
      insertUI(selector="#entry_container", where="beforeEnd",
               ui = entryRowUI(paste0("e",next_id())))
      entries[[paste0("e",next_id())]] <- entryRowServer(paste0("e",next_id()))
      next_id(next_id()+1)
    }
  })
  
  # add new rows
  observeEvent(input$add_row, {
    id <- paste0("e", next_id())
    insertUI(selector="#entry_container", where="beforeEnd",
             ui = entryRowUI(id))
    entries[[id]] <- entryRowServer(id)
    next_id(next_id()+1)
  })
  
  # build table
  table_data <- reactive({
    df_list <- map(names(entries), ~ entries[[.x]]())
    df <- bind_rows(df_list)
    df <- df %>% mutate(
      Amount = if_else(Type %in% c("Deductions","Expenses"), -Amount, Amount),
      Category = case_when(
        Type=="Deductions" ~ paste0("\u00A0\u00A0",Category),
        Type=="Expenses"   ~ paste0("\u00A0\u00A0\u00A0\u00A0",Category),
        TRUE ~ Category
      )
    )
    
    total_inc <- sum(df$Amount[df$Type=="Income"],na.rm=TRUE)
    total_ded <- sum(df$Amount[df$Type=="Deductions"],na.rm=TRUE)
    subtotal  <- total_inc + total_ded
    total_exp <- sum(df$Amount[df$Type=="Expenses"],na.rm=TRUE)
    net_inc   <- subtotal + total_exp
    
    out <- bind_rows(
      df,
      tibble(Type="", Category="—",                      Amount=NA_real_),
      tibble(Type="Total Income", Category="",             Amount=total_inc),
      tibble(Type="Total Deductions", Category="",         Amount=total_ded),
      tibble(Type="Total Take-Home-Pay", Category="",    Amount=subtotal),
      tibble(Type="Total Expenses", Category="",           Amount=total_exp),
      tibble(Type="Total Savings", Category="",               Amount=net_inc)
    )
    # add monthly column
    out %>% mutate(
      `Monthly $` = round(Amount / 12, 2)
    )
  })
  
  output$income_table <- renderDT({
    datatable(
      table_data(), rownames=FALSE, escape=FALSE,
      options=list(dom="t", paging=FALSE,
                   rowCallback=DT::JS(
                     "function(row, data){",
                     " var labels=['Total Income','Total Deductions',",
                     " 'Total Take-Home-Pay','Total Expenses','Total Savings'];",
                     " if(labels.includes(data[0])){",
                     " $('td', row).css({'border':'2px solid black'}); }",
                     "}"
                   )
      ),
      colnames=c("Type","Line Item","Annual $","Monthly $")
    ) %>% formatCurrency(c("Amount","Monthly $"), digits=2)
  })
  
  output$download_excel <- downloadHandler(
    filename=function() sprintf("household_income_%s.xlsx",Sys.Date()),
    content=function(file){
      wb <- createWorkbook()
      addWorksheet(wb, "Income Statement")
      writeData(wb, "Income Statement", table_data())
      saveWorkbook(wb, file, overwrite=TRUE)
    }
  )
}

# run app
shinyApp(ui, server)
