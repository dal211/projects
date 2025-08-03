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
library(readxl)
library(openxlsx)
library(fuzzyjoin)
library(tidytransit)

towns_sf <- readRDS("data/towns_sf.rds")
towns_sf <- st_simplify(towns_sf, dTolerance = 100)
commuter_shapes_sf <- readRDS("data/shapes_sf.rds")

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
  
  # ── Title + GitHub link ──
  fluidRow(
    column(
      width = 8,
      h2("Where Should I Live?"),
      tags$div(
        style = "margin-top: -10px;",
        tags$a(
          href   = "https://github.com/dal211/projects/tree/main/personal/town_map",
          target = "_blank",
          style  = "font-size:16px; text-decoration: none; margin-right: 20px;",
          icon("github"), "GitHub"
        ),
        tags$a(
          href   = "https://richardgasquet.shinyapps.io/down_payment/",
          target = "_blank",
          style  = "font-size:16px; text-decoration: none;",
          icon("house"), "Mortgage Calculator"
        )
      )
    )
  ),
  hr(),
  
  # ── Sidebar and map layout ──
  sidebarLayout(
    sidebarPanel(
      width = 2,
      style = "
        display: flex;
        flex-direction: column;
        justify-content: flex-start;
        height: auto;
        padding-top: 1rem;
        padding-bottom: 1rem;
        overflow-y: auto;
      ",
      
      # Top section
      tags$div(
        class = "spaced",
        selectInput(
          "town_sel", "Pick a town:",
          choices = sort(unique(towns_sf$town_name)),
          selected = "Holliston"
        ),
        tags$p("Click on a town for detailed information."),
        tags$p(strong("Note:"), "This for initial exploration, you should visit the town and speak to real estate agents about your preferences."),
      ),
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
        fillColor = ~ fill_color,
        fillOpacity = 0.15,
        color = "grey",
        weight = 1,
        popup = ~ paste0(
          "<strong>Town:</strong> ", town_name, "<br/>",
          "<strong>School District:</strong> ", DIST_NAME, "<br/>",
          "<strong>Home Price (3 bed):</strong> $", round(current_typ_home_value / 1000), "K", "<br/>",
          "<strong>Property Tax Rate: </strong>", percent(prop_rate, accuracy = .01), "<br/>",
          "<strong>High School Size:</strong> ", school_size_est, "<br/>",
          "<strong>School Rating:</strong> ",
          ifelse(is.na(normalized_school_score), "NA", paste0(normalized_school_score, "%")), "<br/>",
          "<strong>To Croton (NY):</strong> ",
          paste0(round(dist_mi), " miles", " (", round((dist_mi / 65) * 60), "min)")
        )
      ) %>%
      addLegend(
        position = "topright",
        colors  = c("#AB47BC", "#ffc107"),
        labels  = c("50–60th percentile", ">70th percentile"),
        title   = "<div style='font-size:13px;'>School Quality</div>",
        opacity = 0.9,
        labFormat = labelFormat(textsize = "10px")
      )
  })

  # Zoom and highlight selected town
  observeEvent(input$town_sel, {
    sf_sel <- filter(towns_sf, town_name == input$town_sel)
    bb <- st_bbox(sf_sel)
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
