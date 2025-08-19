cat("===== LOADED UPDATED APP.R @", Sys.time(), "=====\n")

# ---- Packages ----
library(shiny)
library(dplyr)
library(sf)
library(tidyverse)
library(scales)
library(mapgl)
library(tidygeocoder)

# ---- Data ----
towns_sf <- readRDS("data/towns_sf.rds") |>
  st_transform(4326) |>
  st_simplify(dTolerance = 100) |>
  st_make_valid()

commuter_shapes_sf <- readRDS("data/shapes_sf.rds") |>
  st_transform(4326) |>
  st_make_valid()

# Build popup HTML and keep only needed columns
towns_map <- towns_sf |>
  mutate(
    popup_html = paste0(
      "<strong>Town:</strong> ", town_name, "<br/>",
      "<strong>School District:</strong> ", DIST_NAME, "<br/>",
      "<strong>Home Price (3 bed):</strong> $", round(current_typ_home_value / 1000), "K<br/>",
      "<strong>Property Tax Rate: </strong>", percent(prop_rate, accuracy = .01), "<br/>",
      "<strong>High School Size:</strong> ", school_size_est, "<br/>",
      "<strong>School Rating:</strong> ",
      ifelse(is.na(normalized_school_score), "NA", paste0(normalized_school_score, "%")), "<br/>",
      "<strong>To Croton (NY):</strong> ",
      paste0(round(dist_mi), " miles (", round((dist_mi / 65) * 60), " min)")
    )
  ) |>
  mutate(across(where(is.factor), as.character)) |>
  select(town_name, fill_color, popup_html, geometry)

# ---- UI ----
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .spaced > * { margin-bottom: 0.9rem; }
      .or-divider {
        display:flex; align-items:center; gap:8px; margin:0.6rem 0 0.3rem 0; color:#666;
        font-weight:600; letter-spacing:0.02em;
      }
      .or-divider::before, .or-divider::after {
        content:\"\"; flex:1; height:1px; background:#ddd;
      }
      body, html { height: 100%; }
    "))
  ),
  
  # Title + links
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
  
  # Sidebar + Map
  sidebarLayout(
    sidebarPanel(
      width = 2,
      style = "
        display:flex; flex-direction:column; justify-content:flex-start;
        height:auto; padding-top:1rem; padding-bottom:1rem; overflow-y:auto;
      ",
      tags$div(
        class = "spaced",
        
        # 1) Town picker FIRST — starts unselected
        selectInput(
          "town_sel", "Pick a town:",
          choices  = c("— Select a town —" = "", sort(unique(towns_sf$town_name))),
          selected = ""
        ),
        
        div(class = "or-divider", "OR"),
        
        # 2) Address entry (street + MA town dropdown)
        textInput("addr_street", "Street address:",
                  placeholder = "e.g., 24 Beacon St"),
        selectInput("addr_town", "Town:",
                    choices  = c("— Select a town —" = "", sort(unique(towns_sf$town_name))),
                    selected = ""),
        actionButton("addr_go", "Find address", class = "btn btn-primary"),
        
        tags$p("Pick a town for details, or search a specific address.")
      )
    ),
    mainPanel(
      width = 10,
      style = "padding:0; margin:0; height:100vh;",
      maplibreOutput("townMap", width = "100%", height = "100%")
    )
  )
)

maptiler_api_key <- Sys.getenv("MAPTILER_API_KEY")
style_key <- paste0("https://api.maptiler.com/maps/streets-v2/style.json?key=", maptiler_api_key)

# ---- Server ----
server <- function(input, output, session) {
  message("🚀 app starting — reaching server()")
  
  # Initial Map
  output$townMap <- renderMaplibre({
    maplibre(style = style_key) |>
      fit_bounds(towns_map) |>
      add_line_layer(
        id     = "commuter",
        source = commuter_shapes_sf,
        line_color = "purple",
        line_width = 2,
        tooltip = "shape_id"
      ) |>
      add_fill_layer(
        id     = "towns",
        source = towns_map,
        fill_color = get_column("fill_color"),
        fill_opacity = 0.15,
        fill_outline_color = "grey",
        tooltip = "town_name",
        popup   = "popup_html"
      ) |>
      add_categorical_legend(
        legend_title = "School Quality",
        values = c("50–60th percentile", ">70th percentile"),
        colors = c("#AB47BC", "#ffc107"),
        position = "top-right"
      )
  })
  
  # Town selection -> highlight + zoom
  observeEvent(input$town_sel, {
    if (is.null(input$town_sel) || input$town_sel == "") return(invisible(NULL))
    
    sf_sel <- towns_map |> filter(town_name == input$town_sel)
    if (nrow(sf_sel) == 0) return(invisible(NULL))
    
    sf_sel <- sf_sel |>
      st_zm(drop = TRUE, what = "ZM") |>
      suppressWarnings(st_cast("MULTIPOLYGON")) |>
      st_make_valid() |>
      select(town_name, popup_html, geometry)
    
    maplibre_proxy("townMap") |>
      clear_layer("highlight") |>
      add_fill_layer(
        id     = "highlight",
        source = sf_sel,
        fill_color = "transparent",
        fill_opacity = 0,
        fill_outline_color = "red"
      ) |>
      fit_bounds(sf_sel, animate = TRUE)
  })
  
  # Address lookup (street + MA town dropdown) -> pin + zoom
  observeEvent(input$addr_go, {
    street <- trimws(input$addr_street %||% "")
    town   <- trimws(input$addr_town   %||% "")
    if (!nzchar(street) || !nzchar(town)) {
      showNotification("Please enter a street and select a town.", type = "warning")
      return(invisible(NULL))
    }
    
    # Keep search strictly in Massachusetts
    query <- paste(street, town, "MA, USA", sep = ", ")
    
    res <- try(
      tidygeocoder::geocode(address = query, method = "osm", limit = 1, mode = "single"),
      silent = TRUE
    )
    ok <- !(inherits(res, "try-error") || nrow(res) == 0 ||
              any(is.na(res[, c("long","lat")])))
    if (!ok) {
      showNotification("Address not found.", type = "error")
      return(invisible(NULL))
    }
    
    pt <- st_as_sf(res, coords = c("long", "lat"), crs = 4326)
    # Buffer (~2 km) to give context around the point
    view_win <- pt |>
      st_transform(3857) |>
      st_buffer(2000) |>
      st_transform(4326)
    
    maplibre_proxy("townMap") |>
      clear_layer("search_pt") |>
      add_circle_layer(
        id = "search_pt",
        source = pt,
        circle_color = "#2962FF",
        circle_radius = 6,
        circle_stroke_color = "white",
        circle_stroke_width = 2,
        tooltip = query,
        popup   = query
      ) |>
      fit_bounds(view_win, animate = TRUE)
  })
}

# small helper
`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- Run App ----
shinyApp(ui, server)
