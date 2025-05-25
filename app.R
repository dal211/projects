cat("===== LOADED UPDATED APP.R @", Sys.time(), "=====\n")

library(shiny)
library(leaflet)
library(dplyr)
library(tigris)
library(readr)
library(sf)
library(rsconnect)
library(tidyverse)
library(styler)
library(tidycensus)
library(ggplot2)
library(usethis)
library(scales)
library(flexdashboard)
library(readxl)
library(openxlsx)
library(fuzzyjoin)
library(tidytransit)

towns_sf <- readRDS("data/towns_sf.rds")
towns_sf <- st_simplify(towns_sf, dTolerance = 100) 
commuter_shapes_sf <- readRDS("data/shapes_sf.rds")

# Color palette for exceed percentile
pal_bin <- colorFactor(palette = c("transparent", "#ffc107"), domain = c(0, 1), na.color = "transparent")

# ---- UI ----
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .spaced > * {
        margin-bottom: 2rem;
      }
      .spaced > *:last-child {
        margin-bottom: 0;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      style = "
        display: flex;
        flex-direction: column;
        justify-content: space-between;
        height: 100vh;
        padding-top: 2rem;
        padding-bottom: 2rem;
      ",
      
      # Top section
      tags$div(
        class = "spaced",
        tags$h4("Where should I live?"),
        selectInput(
          "town_sel", "Pick a town:",
          choices  = sort(unique(towns_sf$town_name)),
          selected = "Boston"
        ),
        tags$p("Click on a town for detailed information."),
        br()
      ),
      
      # Bottom section
      tags$div(
        style = "margin: 0; padding: 0; text-align: left;",
        
        # GitHub link centered
        tags$div(
          style = "text-align: center;",
          tags$a("Visit my GitHub Repo",
                 href   = "https://github.com/dal211/projects",
                 target = "_blank",
                 style  = "font-size:14px;"
          )
        )
      )
    ),
    
    mainPanel(
      width = 10,
      style = "padding:0; margin:0; height:100vh;",
      leafletOutput("townMap", width = "100%", height = "100%")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  message("🚀 app starting — reaching server()")
  # Render the full map initially
  output$townMap <- renderLeaflet({
  leaflet(towns_sf) %>%
    addProviderTiles("OpenStreetMap") %>%
    setView(lng = -71.7, lat = 42.2, zoom = 8.49) %>%      
    addPolylines(
      data   = commuter_shapes_sf,
      color  = "purple",
      weight = 2,
      label  = ~shape_id
    ) %>%
    addPolygons(
      group = "towns",
      label = ~town_name,
      fillColor = ~ pal_bin(school_color),
      fillOpacity = 0.15,
      color = "black",
      weight = 1,
      popup = ~ paste0(
        "<strong>Town:</strong> ", town_name, "<br/>",
        "<strong>School District:</strong> ", DIST_NAME, "<br/>",
        "<strong>Home Price (3 bed):</strong> $", round(current_typ_home_value / 1000), "K", "<br/>",
        "<strong>Price Δ (YoY):</strong> ",
        ifelse(one_year_price_change > 0, paste0("+", one_year_price_change), one_year_price_change), "%<br/>",
        "<strong>High School Size:</strong> ", school_size_est, "<br/>",
        "<strong>School Rating:</strong> ",
        ifelse(is.na(normalized_school_score), "NA", paste0(normalized_school_score, "%")), "<br/>",
        "<strong>To Croton (NY):</strong> ",
        paste(round(dist_mi), "miles", "(", round((dist_mi/65)*60), "min)")
      )
    ) %>%
    addLegend(
      position = "bottomleft",
      colors = "#ffc107",
      labels = "At least 70th percentile MCAS and AP scores",
      title = "<div style='font-size:13px;'>School Quality</div>",
      opacity = 0.9,
      labFormat = labelFormat(textsize = "10px")
    )
})
  
  # Zoom and highlight selected town
  observeEvent(input$town_sel, {
    sf_sel <- filter(towns_sf, town_name == input$town_sel)
    bb     <- st_bbox(sf_sel)
    leafletProxy("townMap") %>%
      clearGroup("highlight") %>%
      addPolygons(
        data   = sf_sel,
        group  = "highlight",
        color  = "red",
        weight = 3,
        fill   = FALSE
      ) %>%
      flyToBounds(bb$xmin, bb$ymin, bb$xmax, bb$ymax)
  })
}

# ---- Run App ----
shinyApp(ui, server)
