# app.R: Massachusetts Town Explorer Shiny App

# ---- Load libraries ----
library(shiny)
library(leaflet)
library(dplyr)
library(tigris)
library(readr)
library(sf)
library(rsconnect)

# pull in both tokens
mapbox_public <- Sys.getenv("MAPBOX_TOKEN_PUBLIC")
# mapbox_shiny <- Sys.getenv("MAPBOX_TOKEN_SHINY")

# ---- Data Preparation ----n# Ensure caching of tigris shapes\options(tigris_use_cache = TRUE)

# MCAS data
mcas <- read_csv("data/MCAS_Achievement_Results_20250415.csv") %>%
  filter(
    SY == 2024,
    STUGRP == "White",
    ORG_NAME != "State",
    TEST_GRADE == "10",
    ORG_TYPE == "Public School District"
  ) %>%
  select(SY:STUGRP, STU_CNT, E_CNT, AVG_SCALED_SCORE)

mcas_agg <- mcas %>%
  group_by(DIST_NAME) %>%
  summarize(
    stu_cnt         = median(STU_CNT),
    exceed_cnt      = round(weighted.mean(E_CNT, w = STU_CNT), 0),
    avg_score       = round(weighted.mean(AVG_SCALED_SCORE, w = STU_CNT), 0)
  ) %>%
  mutate(
    exceed_perct            = round(exceed_cnt / stu_cnt, 3),
    school_size_est         = stu_cnt * 4,
    exceed_mcas_percentile  = round(percent_rank(exceed_perct) * 100, 1)
  )

# School district crosswalk
town_school_dist_xwalk <- read_csv("data/final_school_districts_mapping_v1.csv") %>%
  distinct() %>%
  arrange(town_name) %>%
  group_by(town_name) %>%
  slice(1)

# Zillow three-bedroom price change
price_town_mapping <- read_csv("data/Mapping_Table_with_Exact_Match_Column2.csv")
three_bed_home_price_zil <- read_csv("data/City_zhvi_bdrmcnt_3_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv") %>%
  filter(State == "MA") %>%
  select(RegionName, last_col(offset = 12), last_col()) %>%
  mutate(one_year_price_change = round((`3/31/2025` - `3/31/2024`) / `3/31/2024` * 100, 1)) %>%
  rename(
    current_typ_home_value = `3/31/2025`,
    lst_yr_typ_home_value = `3/31/2024`
  )

# Town geometry and joins
towns_sf <- tigris::county_subdivisions(state = "MA", cb = TRUE, year = 2023) %>%
  rename(town_name = NAME) %>%
  left_join(town_school_dist_xwalk, by = "town_name") %>%
  left_join(mcas_agg,              by = c("DIST_NAME")) %>%
  left_join(price_town_mapping,    by = "town_name") %>%
  left_join(three_bed_home_price_zil, by = c("region_name" = "RegionName")) %>%
  st_transform(4326) %>%                      # ensure WGS84
  mutate(mcas_color = if_else(exceed_mcas_percentile > 70, 1, 0))

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
  # Render the full map initially
  output$townMap <- renderLeaflet({
    leaflet(towns_sf) %>%
      addMapboxTiles(style_id = "streets-v11", username = "mapbox") %>%
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


