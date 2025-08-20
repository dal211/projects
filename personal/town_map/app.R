cat("===== LOADED UPDATED APP.R @", Sys.time(), "=====\n")

# ---- Packages ----
library(shiny)
library(dplyr)
library(sf)
library(tidyverse)
library(scales)
library(mapgl)
library(httr)         # <-- use httr instead of httr2

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
          href   = "https://www.mortgagecalculator.org/",
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
        selectInput(
          "town_sel", "Pick a town:",
          choices  = c("— Select a town —" = "", sort(unique(towns_sf$town_name))),
          selected = ""
        ),
        div(class = "or-divider", "OR"),
        textInput("addr_street", "Street address:", placeholder = "e.g., 24 Beacon St"),
        selectInput("addr_town", "Town:",
                    choices  = c("— Select a town —" = "", sort(unique(towns_sf$town_name))),
                    selected = ""),
        actionButton("addr_go", "Find address", class = "btn btn-primary"),
        actionButton("reset_view", "Reset map", class = "btn btn-outline-secondary"),
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

# Basemap
maptiler_api_key <- Sys.getenv("MAPTILER_API_KEY")
style_key <- paste0("https://api.maptiler.com/maps/streets-v2/style.json?key=", maptiler_api_key)

# ---- MapTiler geocoder (server-side via httr) ----
maptiler_key <- Sys.getenv("MAPTILER_API_KEY", unset = "")

geocode_maptiler <- function(query, key = maptiler_key) {
  if (!nzchar(key)) return(NULL)
  base <- "https://api.maptiler.com/geocoding/"
  qenc <- utils::URLencode(query, reserved = TRUE)
  url  <- paste0(
    base, qenc, ".json",
    "?key=", key,
    "&country=US",
    "&bbox=-73.508,41.237,-69.927,42.886",
    "&limit=1"
  )
  resp <- try(httr::RETRY("GET", url, times = 2, pause_min = 0.2), silent = TRUE)
  if (inherits(resp, "try-error") || httr::http_error(resp)) return(NULL)
  js <- httr::content(resp, as = "parsed", type = "application/json", encoding = "UTF-8")
  if (is.null(js$features) || length(js$features) == 0) return(NULL)
  feat <- js$features[[1]]
  c(lon = feat$geometry$coordinates[[1]],
    lat = feat$geometry$coordinates[[2]],
    place = feat$place_name %||% query)
}

# ---- Server ----
server <- function(input, output, session) {
  message("🚀 app starting — reaching server()")
  
  # Initial Map
  output$townMap <- renderMaplibre({
    maplibre(style = style_key) |>
      add_navigation_control(position = "top-left") |>  # ← Zoom + compass buttons
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
  
  # Address lookup (MapTiler only) -> pin + zoom
  observeEvent(input$addr_go, {
    street <- trimws(input$addr_street %||% "")
    town   <- trimws(input$addr_town   %||% "")
    if (!nzchar(street) || !nzchar(town)) {
      showNotification("Please enter a street and select a town.", type = "warning")
      return(invisible(NULL))
    }
    
    query  <- paste(street, town, "MA, USA", sep = ", ")
    coords <- geocode_maptiler(query)
    if (is.null(coords) || any(is.na(coords[c("lon","lat")]))) {
      showNotification("Address not found (MapTiler).", type = "error")
      return(invisible(NULL))
    }
    
    pt <- st_as_sf(
      data.frame(long = as.numeric(coords["lon"]),
                 lat  = as.numeric(coords["lat"]),
                 label = as.character(coords["place"])),
      coords = c("long", "lat"), crs = 4326
    )
    
    # Buffer (~2 km) for context
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
        tooltip = pt$label[1],
        popup   = pt$label[1]
      ) |>
      fit_bounds(view_win, animate = TRUE)
  })
  
  observeEvent(input$reset_view, {
    # Clear highlight & search point, then zoom back to full bounds
    maplibre_proxy("townMap") |>
      clear_layer("highlight") |>
      clear_layer("search_pt") |>
      fit_bounds(towns_map, animate = TRUE)
    
    # Reset inputs (comment out if you prefer to keep selections)
    updateSelectInput(session, "town_sel",   selected = "")
    updateTextInput( session, "addr_street", value    = "")
    updateSelectInput(session, "addr_town",  selected = "")
  })
  
}

# small helper
`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- Run App ----
shinyApp(ui, server)
