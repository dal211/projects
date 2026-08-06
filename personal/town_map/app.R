cat("===== LOADED UPDATED APP.R @", Sys.time(), "=====\n")

# ---- Packages ----
library(shiny)
library(dplyr)
library(sf)
library(mapgl)
library(httr)

# ---- Data ----
towns_map <- readRDS("data/towns_map.rds")
towns_sf <- readRDS("data/towns_sf.rds")
commuter_shapes_sf <- readRDS("data/shapes_sf.rds") |>
  st_transform(4326) |>
  st_simplify(dTolerance = 100) |>
  st_make_valid()
commuter_stations_sf <- readRDS("data/commuter_stations_sf.rds") |>
  mutate(label = paste0(stop_name, " — ", municipality))

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
    ")),
    tags$style(HTML("
  .tip-btn {
    width: 34px; height: 34px; padding: 0;
    border-radius: 9999px;
    display: inline-flex; align-items: center; justify-content: center;
    box-shadow: 0 2px 6px rgba(0,0,0,.15);
  }
  .tip-btn .fa { margin: 0; } /* keep the FA icon centered */
")),
    # --- add this style (optional spacing & button look to match MapLibre) ---
    tags$style(HTML("
  .custom-tip-ctrl { margin-top: 4px; position: relative; }   /* space under nav control */
  .custom-tip-ctrl button {
    width: 28px; height: 28px; background:#fff; border:0; cursor:pointer;
  }
  .custom-tip-ctrl button:hover { background:#f0f0f0; }
  .custom-tip-ctrl .info-panel {
    display: none;
    position: absolute; top: 32px; left: 0;
    width: 220px;
    background: #fff;
    border-radius: 8px;
    box-shadow: 0 2px 10px rgba(0,0,0,.2);
    padding: 10px 14px;
    font-size: 12.5px;
    color: #1c2024;
    z-index: 10;
  }
  .custom-tip-ctrl .info-panel.open { display: block; }
  .custom-tip-ctrl .info-panel h5 { margin: 0 0 6px 0; font-size: 12.5px; font-weight: 700; }
  .custom-tip-ctrl .legend-row { display: flex; align-items: center; gap: 8px; margin-bottom: 4px; }
  .custom-tip-ctrl .legend-swatch { width: 14px; height: 14px; border-radius: 3px; flex: none; }
  .custom-tip-ctrl .legend-swatch.green { background: #009688; }
  .custom-tip-ctrl .legend-swatch.purple { background: #AB47BC; }
  .custom-tip-ctrl .tip-text {
    margin-top: 8px; padding-top: 6px; border-top: 1px solid #eee;
    color: #667380; font-size: 11.3px;
  }
")),
    tags$style(HTML("
  .mobile-search-btn {
    display: none;
    position: fixed;
    bottom: 20px;
    right: 20px;
    z-index: 1000;
    width: 52px; height: 52px;
    border-radius: 9999px;
    background: #2962FF;
    color: #fff;
    border: none;
    box-shadow: 0 2px 10px rgba(0,0,0,.3);
    align-items: center;
    justify-content: center;
    font-size: 20px;
  }
  @media (max-width: 767px) {
    .mobile-search-btn { display: flex; }
  }
")),
    tags$style(HTML("
  .maplibregl-popup-content {
    padding: 10px 12px;
    border-radius: 8px;
  }
  .tp { font-size: 12.5px; line-height: 1.55; color: #1c2024; }
  .tp-head { display: flex; align-items: center; gap: 6px; font-weight: 700; font-size: 13.5px; margin-bottom: 3px; }
  .tp-dot { width: 7px; height: 7px; border-radius: 50%; flex: none; }
  .tp-sub { color: #667380; font-size: 11.3px; margin-bottom: 7px; }
  .tp-grid {
    display: grid;
    grid-template-columns: auto 1fr auto 1fr;
    column-gap: 6px;
    row-gap: 4px;
    align-items: baseline;
  }
  .tp-l { color: #667380; font-size: 11px; }
  .tp-v { font-weight: 600; font-variant-numeric: tabular-nums; }
  .tp-rating { margin-top: 8px; padding-top: 6px; border-top: 1px solid #dde3e8; }
  .tp-score { font-weight: 700; font-variant-numeric: tabular-nums; }
  .tp-score .tp-pct { color: #009688; }
  .tp-breakdown { color: #667380; font-size: 10.6px; font-variant-numeric: tabular-nums; margin-top: 2px; }
")),
    tags$style(HTML("
  #map-loading-overlay {
    position: absolute; inset: 0;
    display: flex; align-items: center; justify-content: center;
    flex-direction: column; gap: 10px;
    background: rgba(255,255,255,0.85);
    font-size: 16px; color: #333;
    z-index: 500;
  }
")),

    # --- add this script (creates an 'info' control under the nav controls) ---
    tags$script(HTML("
  Shiny.addCustomMessageHandler('attach-tip', function(id){
    var root = document.getElementById(id);
    if (!root) return;

    // MapLibre's top-left control corner (same one that holds zoom/compass)
    var corner = root.querySelector('.maplibregl-ctrl-top-left');
    if (!corner) return;

    // Avoid duplicates if hot-reloading
    if (corner.querySelector('.custom-tip-ctrl')) return;

    // Create a control group with one button
    var group = document.createElement('div');
    group.className = 'maplibregl-ctrl maplibregl-ctrl-group custom-tip-ctrl';

    var btn = document.createElement('button');
    btn.type  = 'button';
    btn.title = 'Map info & legend';   // native hover tooltip
    btn.innerHTML = '<i class=\"fa fa-info-circle\"></i>';

    var panel = document.createElement('div');
    panel.className = 'info-panel';
    panel.innerHTML =
      '<h5>School Quality</h5>' +
      '<div class=\"legend-row\"><span class=\"legend-swatch green\"></span>&gt;70th percentile (Tier 1)</div>' +
      '<div class=\"legend-row\"><span class=\"legend-swatch purple\"></span>50&ndash;69th percentile (Tier 2)</div>' +
      '<div class=\"tip-text\">Tip: Hold Ctrl + drag to tilt &amp; rotate.</div>';

    btn.addEventListener('click', function(e){
      e.stopPropagation();
      panel.classList.toggle('open');
    });
    document.addEventListener('click', function(){
      panel.classList.remove('open');
    });
    panel.addEventListener('click', function(e){ e.stopPropagation(); });

    group.appendChild(btn);
    group.appendChild(panel);
    corner.appendChild(group);
  });

  Shiny.addCustomMessageHandler('map-ready', function(x){
    var overlay = document.getElementById('map-loading-overlay');
    if (overlay) overlay.style.display = 'none';
  });
"))
  ),
  fluidRow(
    column(
      width = 8,
      h2("Where Should I Live?"),
      tags$div(
        style = "
        display: flex;
        gap: 24px;          /* equal space between links */
        margin-top: -10px;
        align-items: center;
        flex-wrap: wrap;    /* wrap to new line on small screens */
      ",
        # tags$a(
        #   href   = "https://github.com/dal211/projects/tree/main/personal/town_map",
        #   target = "_blank",
        #   style  = "font-size:16px; text-decoration: none; display:flex; align-items:center; gap:6px;",
        #   icon("github"), "GitHub"
        # ),
        tags$a(
          href = "https://www.redfin.com/",
          target = "_blank",
          style = "font-size:16px; text-decoration: none; display:flex; align-items:center; gap:6px;",
          icon("house"), "Redfin"
        ),
        tags$a(
          href = "https://www.niche.com/places-to-live/search/best-places-to-live/",
          target = "_blank",
          style = "font-size:16px; text-decoration: none; display:flex; align-items:center; gap:6px;",
          icon("map-marker-alt"), "Niche"
        )
      )
    )
  ),
  hr(),

  tags$button(
    class = "mobile-search-btn",
    title = "Back to search",
    onclick = "window.scrollTo({top: 0, behavior: 'smooth'});",
    icon("arrow-up")
  ),

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

        # --- Section 1: Town ---
        tags$h4("Explore a town", style = "font-weight:600; font-size:16px; margin:0 0 .25rem 0;"),
        selectInput(
          "town_sel", NULL, # label hidden; subtitle above serves as the label
          choices = c("— Select a town —" = "", sort(unique(towns_sf$town_name))),
          selected = ""
        ),
        div(class = "or-divider", "OR"),

        # --- Section 2: Address ---
        tags$h4("Explore an address", style = "font-weight:600; font-size:16px; margin:0 0 .25rem 0;"),
        textInput("addr_query", "Address:", placeholder = "e.g., 24 Beacon St, Newton"),
        actionButton("addr_go", "Find address", icon = icon("magnifying-glass"), class = "btn btn-primary"),
        actionButton("reset_view", "Reset map", class = "btn btn-outline-secondary"),
        tags$p("Click on the map to see info about the town.")
      )
    ),
    mainPanel(
      width = 10,
      style = "padding:0; margin:0; height:100vh; position:relative;",
      maplibreOutput("townMap", width = "100%", height = "100%"),
      tags$div(
        id = "map-loading-overlay",
        icon("spinner", class = "fa-spin", style = "font-size:32px; color:#2962FF;"),
        tags$span("Loading map…")
      )
    )
  )
)

# Basemap
# Hardcoded: shinyapps.io has no custom-env-var support, and this key is
# inherently client-visible in map tile/style requests anyway — the actual
# protection is the "Allowed HTTP Origins" restriction on the MapTiler key itself.
maptiler_api_key <- "XukbtwhZN33k7aCdvTkA"
style_key <- paste0("https://api.maptiler.com/maps/streets-v2/style.json?key=", maptiler_api_key)

# Built once per R process (data + style are identical for every session), rather
# than rebuilt and re-serialized from scratch on every single browser connection.
base_map <- maplibre(style = style_key) |>
  add_navigation_control(position = "top-left") |> # ← Zoom + compass buttons
  fit_bounds(towns_map) |>
  add_line_layer(
    id = "commuter",
    source = commuter_shapes_sf,
    line_color = "purple",
    line_width = 2,
    tooltip = "shape_id"
  ) |>
  add_fill_layer(
    id = "towns",
    source = towns_map,
    fill_color = get_column("fill_color"),
    fill_opacity = 0.15,
    fill_outline_color = "grey",
    tooltip = "town_name",
    popup = "popup_html"
  ) |>
  add_circle_layer(
    id = "commuter_stations",
    source = commuter_stations_sf,
    circle_radius = interpolate(
      property = "zoom",
      values = c(8, 14),
      stops = c(3, 8)
    ),
    circle_color = "white",
    circle_stroke_color = "purple",
    circle_stroke_width = 2,
    tooltip = "label",
    popup = "label"
  )

# ---- MapTiler geocoder (server-side via httr) ----
maptiler_key <- maptiler_api_key

geocode_maptiler <- function(query, key = maptiler_key) {
  if (!nzchar(key)) {
    return(NULL)
  }
  base <- "https://api.maptiler.com/geocoding/"
  qenc <- utils::URLencode(query, reserved = TRUE)
  url <- paste0(
    base, qenc, ".json",
    "?key=", key,
    "&country=US",
    "&bbox=-73.508,41.237,-69.927,42.886",
    "&limit=1"
  )
  resp <- try(httr::RETRY("GET", url, times = 2, pause_min = 0.2), silent = TRUE)
  if (inherits(resp, "try-error") || httr::http_error(resp)) {
    return(NULL)
  }
  js <- httr::content(resp, as = "parsed", type = "application/json", encoding = "UTF-8")
  if (is.null(js$features) || length(js$features) == 0) {
    return(NULL)
  }
  feat <- js$features[[1]]
  c(
    lon = feat$geometry$coordinates[[1]],
    lat = feat$geometry$coordinates[[2]],
    place = feat$place_name %||% query
  )
}

# ---- Server ----
server <- function(input, output, session) {
  message("🚀 app starting — reaching server()")
  message("MAPTILER_API_KEY loaded? ", substr(Sys.getenv("MAPTILER_API_KEY"), 1, 6))

  # Initial Map (pre-built once at app scope; see `base_map` above)
  output$townMap <- renderMaplibre({
    base_map
  })

  # Town selection -> highlight + zoom
  observeEvent(input$town_sel, {
    if (is.null(input$town_sel) || input$town_sel == "") {
      return(invisible(NULL))
    }

    sf_sel <- towns_map |> filter(town_name == input$town_sel)
    if (nrow(sf_sel) == 0) {
      return(invisible(NULL))
    }

    sf_sel <- sf_sel |>
      st_zm(drop = TRUE, what = "ZM") |>
      suppressWarnings(st_cast("MULTIPOLYGON")) |>
      st_make_valid() |>
      select(town_name, popup_html, geometry)

    maplibre_proxy("townMap") |>
      clear_layer("highlight") |>
      add_fill_layer(
        id = "highlight",
        source = sf_sel,
        fill_color = "transparent",
        fill_opacity = 0,
        fill_outline_color = "red"
      ) |>
      fit_bounds(sf_sel, animate = TRUE)
  })

  # Address lookup (MapTiler only) -> pin + zoom
  searching <- reactiveVal(FALSE)

  observe({
    if (isTRUE(searching())) {
      updateActionButton(session, "addr_go", label = "Searching…", icon = icon("spinner", class = "fa-spin"))
    } else {
      updateActionButton(session, "addr_go", label = "Find address", icon = icon("magnifying-glass"))
    }
  })

  observeEvent(input$addr_go, {
    if (isTRUE(searching())) {
      return(invisible(NULL))
    }

    addr_query <- trimws(input$addr_query %||% "")
    if (!nzchar(addr_query)) {
      showNotification("Please enter an address.", type = "warning")
      return(invisible(NULL))
    }

    # Flip the button to a busy state now, and defer the actual (blocking)
    # network call until after that state has been flushed to the browser.
    searching(TRUE)
    session$onFlushed(function() {
      tryCatch(
        {
          query <- paste(addr_query, "MA, USA", sep = ", ")
          coords <- geocode_maptiler(query)
          if (is.null(coords) || any(is.na(coords[c("lon", "lat")]))) {
            showNotification("Address not found (MapTiler).", type = "error")
            return(invisible(NULL))
          }

          pt <- st_as_sf(
            data.frame(
              long = as.numeric(coords["lon"]),
              lat = as.numeric(coords["lat"]),
              label = as.character(coords["place"])
            ),
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
              popup = pt$label[1]
            ) |>
            fit_bounds(view_win, animate = TRUE)
        },
        finally = searching(FALSE)
      )
    }, once = TRUE)
  })

  observeEvent(input$reset_view, {
    # Clear highlight & search point, then zoom back to full bounds
    maplibre_proxy("townMap") |>
      clear_layer("highlight") |>
      clear_layer("search_pt") |>
      fit_bounds(towns_map, animate = TRUE)

    # Reset inputs (comment out if you prefer to keep selections)
    updateSelectInput(session, "town_sel", selected = "")
    updateTextInput(session, "addr_query", value = "")
  })

  session$onFlushed(function() {
    session$sendCustomMessage("attach-tip", "townMap") # 'townMap' = your map output id
    session$sendCustomMessage("map-ready", TRUE)
  }, once = TRUE)
}

# small helper
`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- Run App ----
shinyApp(ui, server)
