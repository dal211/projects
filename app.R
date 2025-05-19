cat("===== LOADED UPDATED APP.R @", Sys.time(), "=====\n")

library(shiny)
library(leaflet)
# library(mapboxapi)
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
library(leaflet.extras)
library(leaflet.mapboxgl)
library(tidytransit)

# usethis::edit_r_environ()
# mapbox_public_token <- Sys.getenv("MAPBOX_PUBLIC_TOKEN")
# mapbox_local_token  <- Sys.getenv("MAPBOX_TOKEN_LOCAL")
# mapbox_token  <- Sys.getenv("MAPBOX_TOKEN_SHINY")

# Sys.setenv(MAPBOX_TOKEN_SHINY = "pk.eyJ1IjoiZGFsMjExMSIsImEiOiJjbWF0MXplNGowcnA4MmtweWY2cmZ2eHZ1In0.voikowvEC3PN932ab59quA")
# message("→ MAPBOX_TOKEN_SHINY is: ", Sys.getenv("MAPBOX_TOKEN_SHINY"))

towns_sf <- readRDS("data/towns_sf.rds")
towns_sf <- st_simplify(towns_sf, dTolerance = 100) 
commuter_shapes_sf <- readRDS("data/shapes_sf.rds")

# Color palette for exceed percentile
pal_bin <- colorFactor(palette = c("transparent", "#ffc107"), domain = c(0, 1), na.color = "transparent")

# ---- UI ----
ui <- fluidPage(
  titlePanel("Massachusetts Town Explorer"),
  sidebarLayout(
    sidebarPanel(
      # Use flexbox to push GitHub link to bottom
      style = "display: flex; flex-direction: column; justify-content: space-between; height: 100vh;",
      # Top: selector
      selectInput(
        inputId = "town_sel",
        label = "Pick a town:",
        choices = sort(unique(towns_sf$town_name)),
        selected = "Boston"
      ),
      # Bottom: GitHub link, moved further up
      tags$div(
        tags$a("Visit my GitHub Repo", href = "https://github.com/dal211/projects", target = "_blank", style = "font-size:14px;"),
        style = "text-align: center; margin-bottom: 60px;"
      )
    ),
    mainPanel(
      leafletOutput("townMap", height = "650px")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  message("🚀 app starting — reaching server()")
  # Render the full map initially
  output$townMap <- renderLeaflet({
    leaflet(towns_sf) %>%
      # addMapboxTiles(style_id = "streets-v11",
      #                username = "mapbox",
      #                access_token = Sys.getenv("MAPBOX_PUBLIC_TOKEN")) %>%
      # addMapboxGL(
      #   style       = "mapbox://styles/mapbox/streets-v11",
      #   accessToken = Sys.getenv("MAPBOX_PUBLIC_TOKEN")
      # ) %>%      
      # addTiles(
      #   urlTemplate = paste0(
      #     "https://api.mapbox.com/styles/v1/mapbox/streets-v11/tiles/{z}/{x}/{y}?",
      #     access_token = Sys.getenv("MAPBOX_TOKEN_SHINY")
      #   ),
      #   options = tileOptions(tileSize = 512, zoomOffset = -1)) %>%
      addProviderTiles("OpenStreetMap") %>%
      addPolylines(
        data   = shapes_sf,
        color  = "purple",
        weight = 2,
        label  = ~shape_id
      ) %>%
      addPolygons(
        group       = "towns",
        label       = ~ town_name,                 
        fillColor   = ~ pal_bin(mcas_color),
        fillOpacity = 0.15,
        color       = "black",
        weight      = 1,
        popup       = ~ paste0(
          "<strong>Town:</strong> ", town_name, "<br/>",
          "<strong>District:</strong> ", DIST_NAME, "<br/>",
          "<strong>Home Price (3 bed):</strong> $", round(current_typ_home_value/1000),"K","<br/>",
          "<strong>Price Δ:</strong> ",
          ifelse(one_year_price_change>0, paste0("+",one_year_price_change), one_year_price_change),"%<br/>",
          "<strong>School Size:</strong> ", school_size_est, "<br/>",
          "<strong>MCAS Percentile:</strong> ",
          ifelse(is.na(exceed_mcas_percentile),"NA",paste0(exceed_mcas_percentile,"%"))
        )
      ) %>%
      addLegend(
        position = "bottomleft",
        colors   = "#ffc107",
        labels   = "Top 20% MCAS Districts",
        title    = "<div style='font-size:13px;'>School Quality</div>",
        opacity  = 0.9,
        labFormat= labelFormat(textsize = "10px")
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
