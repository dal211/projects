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
# T lines (subway + light rail), built by 03_mbta.R
subway_shapes_sf <- readRDS("data/subway_shapes_sf.rds") |>
  mutate(line = sub(" Line.*$", "", route_name)) |> # "Green Line B" -> "Green"; used by the line chips
  st_transform(4326) |>
  st_simplify(dTolerance = 10) |>
  st_make_valid()
subway_stations_sf <- readRDS("data/subway_stations_sf.rds") |>
  mutate(label = paste0(stop_name, " — ", lines, if_else(grepl(",", lines), " Lines", " Line")))

# Town attributes for the details card (one row per town, no geometry)
town_info <- towns_sf |>
  st_drop_geometry() |>
  select(
    town_name, DIST_NAME, dens_cat, density, fill_color,
    current_typ_home_value, one_year_price_change, prop_rate,
    normalized_school_score, mcas_rank, ap_rank, sat_rank,
    college_bound_rate, school_size_est
  )

redfin_url <- "https://www.redfin.com/"
niche_url <- "https://www.niche.com/places-to-live/search/best-places-to-live/"
search_icon <- HTML('<svg viewBox="0 0 24 24"><circle cx="11" cy="11" r="7"/><path d="M21 21l-4.3-4.3"/></svg>')
settings_icon <- HTML(paste0(
  '<svg viewBox="0 0 24 24"><circle cx="12" cy="12" r="3"/>',
  '<path d="M19.4 15a1.65 1.65 0 0 0 .33 1.82l.06.06a2 2 0 1 1-2.83 2.83l-.06-.06a1.65 1.65 0 0 0-1.82-.33',
  ' 1.65 1.65 0 0 0-1 1.51V21a2 2 0 1 1-4 0v-.09A1.65 1.65 0 0 0 9 19.4a1.65 1.65 0 0 0-1.82.33l-.06.06',
  'a2 2 0 1 1-2.83-2.83l.06-.06A1.65 1.65 0 0 0 4.68 15a1.65 1.65 0 0 0-1.51-1H3a2 2 0 1 1 0-4h.09',
  'A1.65 1.65 0 0 0 4.6 9a1.65 1.65 0 0 0-.33-1.82l-.06-.06a2 2 0 1 1 2.83-2.83l.06.06A1.65 1.65 0 0 0 9 4.68',
  ' 1.65 1.65 0 0 0 10 3.17V3a2 2 0 1 1 4 0v.09a1.65 1.65 0 0 0 1 1.51 1.65 1.65 0 0 0 1.82-.33l.06-.06',
  'a2 2 0 1 1 2.83 2.83l-.06.06A1.65 1.65 0 0 0 19.4 9a1.65 1.65 0 0 0 1.51 1H21a2 2 0 1 1 0 4h-.09',
  'a1.65 1.65 0 0 0-1.51 1z"/></svg>'
))

# ---- About tab (methodology) ----
# Keep in sync with 01_pop_density.R / 02_preprocessing.R if the methods change
src_link <- function(label, href, ...) tags$a(href = href, target = "_blank", rel = "noopener", label, ...)

about_ui <- function() {
  tagList(
    tags$section(
      class = "box about",
      tags$h4("Finding a town to settle down"),
      tags$p(
        "Built for those deciding where to settle down in Massachusetts"
      ),
      tags$p(
        tags$b("Schools come first."),
        " They matter if you have or plan to have kids, and strong schools tend to go with",
        " nice neighborhoods, good amenities and lower crime. Treat them as a signal, not a guarantee."
      ),
      tags$p(class = "about-sub", "How to use it"),
      tags$ol(
        tags$li("Shortlist towns with strong schools (shaded green or purple). Tap the star on a town's details to save it, then compare or download your list in the Saved tab."),
        tags$li("Check the commute: tap Transit on the map to show commuter rail and T lines, or use the ruler for driving time."),
        tags$li(
          "Check home prices and taxes in the town's details, then search listings on ",
          src_link("Redfin", redfin_url, .noWS = "after"), "."
        )
      ),
      tags$div(
        class = "about-legend",
        tags$div(tags$span(class = "set-swatch tier1"), tags$b("Tier 1"), " school score 70+"),
        tags$div(tags$span(class = "set-swatch tier2"), tags$b("Tier 2"), " school score 50–69"),
        tags$div(tags$span(class = "about-line"), "Commuter rail lines and stations"),
        tags$div(tags$span(class = "set-swatch subway"), "T lines (subway and light rail)")
      )
    ),
    tags$section(
      class = "box about",
      tags$h4("School score"),
      tags$p(
        "The average of three rankings for the town's public school district. Each one ranks",
        " the district against every Massachusetts town from 0 to 100:"
      ),
      tags$ul(
        tags$li(tags$b("MCAS:"), " grade 10 average scaled score across subjects"),
        tags$li(tags$b("AP:"), " share of AP exams scored 3 or higher"),
        tags$li(tags$b("SAT:"), " average reading/writing + math score")
      ),
      tags$p(
        class = "about-note",
        "Scores are relative, not absolute: 70 means better than about 70% of towns. Towns in a",
        " regional district share its score. Towns with missing test data have no score."
      )
    ),
    tags$section(
      class = "box about",
      tags$h4("Other numbers"),
      tags$dl(
        tags$dt("Typical 3-bed home"),
        tags$dd("Zillow Home Value Index for 3-bedroom homes (middle third of the market), latest month. The change compares it with the same month a year earlier."),
        tags$dt("Property tax rate"),
        tags$dd("Residential tax rate for the latest fiscal year each town has set, shown as a percent of assessed value."),
        tags$dt("College-bound"),
        tags$dd("Share of the district's high school graduates enrolled in college the fall after graduating."),
        tags$dt("High school size"),
        tags$dd("An estimate: the district's grade 10 MCAS test-takers × 4."),
        tags$dt("Density"),
        tags$dd("Population ÷ town area. Rural under 200 people/sq mi, sparse suburban 200–499, mid-dense 500–999, dense suburb 1,000–3,000, urban city over 3,000.")
      )
    ),
    tags$section(
      class = "box about",
      tags$h4("Sources"),
      tags$ul(
        class = "about-sources",
        tags$li(src_link("MA DESE Education-to-Career data hub", "https://educationtocareer.data.mass.gov/"), " (MCAS, AP, SAT, college enrollment)"),
        tags$li(src_link("Zillow Research", "https://www.zillow.com/research/data/"), " (home values)"),
        tags$li(src_link("MA Division of Local Services", "https://dls-gw.dor.state.ma.us/reports/rdPage.aspx?rdReport=PropertyTaxInformation.taxratesbyclass.taxratesbyclass_main"), " (tax rates)"),
        tags$li(src_link("U.S. Census ACS 5-year", "https://data.census.gov/"), " (population)"),
        tags$li(src_link("MBTA GTFS", "https://www.mbta.com/developers/gtfs"), " (commuter rail and T lines)"),
        tags$li(src_link("OSRM", "https://project-osrm.org/"), " (driving distances for the ruler)")
      ),
      tags$p(class = "about-note", "Each source is pulled at its latest available release whenever the data is rebuilt.")
    )
  )
}

star_icon <- HTML('<svg viewBox="0 0 24 24"><path d="M12 3.5l2.6 5.3 5.9.9-4.3 4.1 1 5.8-5.2-2.7-5.2 2.7 1-5.8-4.3-4.1 5.9-.9z"/></svg>')
explore_icon <- HTML('<svg viewBox="0 0 24 24"><path d="M9 4 3 6v14l6-2 6 2 6-2V4l-6 2-6-2z"/><path d="M9 4v14M15 6v14"/></svg>')
about_icon <- HTML('<svg viewBox="0 0 24 24"><path d="M4 5.5A1.5 1.5 0 0 1 5.5 4H11v16H5.5A1.5 1.5 0 0 1 4 18.5z"/><path d="M20 5.5A1.5 1.5 0 0 0 18.5 4H13v16h5.5a1.5 1.5 0 0 0 1.5-1.5z"/></svg>')

# ---- UI ----
# Serve www/ explicitly: Shiny only does this automatically for runApp(<dir>),
# not when app.R is sourced (e.g. Positron's Run App button)
addResourcePath("assets", normalizePath("www"))

# Version stamp so browsers re-download a file after it changes instead of
# reusing a cached copy
asset_url <- function(file) {
  paste0("assets/", file, "?v=", as.integer(file.mtime(file.path("www", file))))
}

# Map-first layout (planning/ui_redesign.md): the map fills the screen and one
# panel floats over it — a bottom sheet in mobile mode, a collapsible sidebar
# in web mode. Styling lives in www/styles.css, behavior in www/layout.js.
ui <- bootstrapPage(
  title = "Where Should I Live?",
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, viewport-fit=cover"),
    tags$link(rel = "stylesheet", href = asset_url("styles.css")),
    tags$script(src = asset_url("layout.js"))
  ),
  tags$div(
    id = "app", class = "mode-web",
    tags$div(
      id = "stage", class = "stage",
      maplibreOutput("townMap", width = "100%", height = "100%"),
      tags$div(
        id = "map-loading-overlay",
        tags$span(class = "spin big"),
        tags$span("Loading map…")
      ),
      tags$button(
        id = "m-search", class = "m-search", type = "button",
        search_icon, tags$span("Search town or address")
      ),
      tags$button(id = "expand-tab", class = "expand-tab", type = "button", title = "Show panel", HTML("&rsaquo;")),
      tags$aside(
        id = "panel", class = "panel", `data-view` = "explore",
        tags$div(id = "grip", class = "grip", tags$span()),
        tags$header(
          id = "phead", class = "p-head",
          tags$h1("Where Should I Live?"),
          tags$button(
            id = "settings-btn", class = "head-btn settings-btn", type = "button", title = "Settings",
            `aria-haspopup` = "true", `aria-expanded` = "false", settings_icon
          ),
          tags$button(id = "collapse-btn", class = "head-btn collapse-btn", type = "button", title = "Hide panel", HTML("&lsaquo;"))
        ),
        tags$div(
          id = "pbody", class = "p-body", `data-view` = "explore",
          uiOutput("town_card", class = "box"),
          tags$section(
            class = "box",
            tags$h4("Explore a town"),
            selectInput(
              "town_sel", NULL,
              choices = c("— Select a town —" = "", sort(unique(towns_sf$town_name))),
              selected = "",
              selectize = FALSE
            ),
            tags$div(class = "or-divider", "OR"),
            tags$h4("Explore an address"),
            textInput("addr_query", NULL, placeholder = "e.g., 24 Beacon St, Newton"),
            tags$div(
              class = "btns",
              actionButton("addr_go", "Find address", class = "btn-primary"),
              actionButton("reset_view", "Reset map")
            ),
            tags$p(class = "hint", "Tap a town on the map to see its details here.")
          )
        ),
        # Starred towns: the list is kept in the browser (layout.js) and sent
        # here as input$saved_towns
        tags$div(
          id = "saved-view", class = "p-body is-empty", `data-view` = "saved",
          tags$section(
            class = "box saved",
            tags$h4("Saved towns"),
            uiOutput("saved_table"),
            tags$div(
              class = "btns saved-actions",
              downloadButton("saved_csv", "Download CSV", class = "btn-primary", icon = NULL),
              tags$button(id = "saved-share", type = "button", class = "btn btn-default", "Copy share link")
            ),
            tags$p(
              class = "hint",
              "Saved in this browser only. Open a share link on another device to bring your list along."
            )
          )
        ),
        tags$div(id = "about-view", class = "p-body", `data-view` = "about", about_ui()),
        # Explore / Saved / About switch (styled like the MBTA app's tab bar)
        tags$nav(
          id = "tabbar", class = "tabbar", role = "tablist",
          tags$button(
            type = "button", role = "tab", class = "active", `aria-selected` = "true", `data-tab` = "explore",
            explore_icon, tags$span("Explore")
          ),
          tags$button(
            type = "button", role = "tab", `aria-selected` = "false", `data-tab` = "saved",
            tags$span(class = "tab-icon", star_icon, tags$span(id = "saved-count", class = "tab-count", style = "display:none")),
            tags$span("Saved")
          ),
          tags$button(
            type = "button", role = "tab", `aria-selected` = "false", `data-tab` = "about",
            about_icon, tags$span("About")
          )
        ),
        # Map credits, copied in from the map's attribution by layout.js
        tags$footer(id = "map-credit", class = "p-foot")
      ),
      # Settings menu: lives outside the panel (which clips its contents) and is
      # positioned next to the gear by layout.js
      tags$div(
        id = "settings-pop", class = "settings-pop", role = "dialog", `aria-label` = "Settings",
        tags$div(class = "set-label", "Appearance"),
        tags$div(
          class = "seg", role = "group",
          tags$button(type = "button", `data-theme-choice` = "light", "Light"),
          tags$button(type = "button", `data-theme-choice` = "dark", "Dark")
        ),
        tags$div(class = "set-label set-gap", "Layout"),
        tags$div(
          class = "seg", role = "group",
          tags$button(type = "button", `data-mode-choice` = "web", "Desktop"),
          tags$button(type = "button", `data-mode-choice` = "mobile", "Mobile")
        ),
        tags$div(class = "set-label set-gap", "School shading on map"),
        tags$label(
          class = "set-toggle",
          tags$span(class = "set-swatch tier1"),
          tags$span(class = "set-text", tags$b("Tier 1 schools"), tags$small("score 70+")),
          tags$input(type = "checkbox", `data-tier` = "1", checked = NA)
        ),
        tags$label(
          class = "set-toggle",
          tags$span(class = "set-swatch tier2"),
          tags$span(class = "set-text", tags$b("Tier 2 schools"), tags$small("score 50–69")),
          tags$input(type = "checkbox", `data-tier` = "2", checked = NA)
        )
      ),
      # Transit panel: opened by the "Transit" button in the map's control stack
      # (added by layout.js); positioned next to that button
      tags$div(
        id = "transit-pop", class = "settings-pop transit-pop", role = "dialog", `aria-label` = "Transit layers",
        tags$div(class = "set-label", "Show on map"),
        tags$label(
          class = "set-toggle",
          tags$span(class = "set-swatch rail"),
          tags$span(class = "set-text", tags$b("Commuter rail"), tags$small("MBTA lines and stations")),
          tags$input(type = "checkbox", `data-layer` = "rail", checked = NA)
        ),
        tags$div(
          class = "set-tlines",
          tags$div(class = "set-text", tags$b("T lines"), tags$small("subway and light rail: tap to show or hide each")),
          tags$div(
            class = "chips",
            lapply(
              list(
                list("Red", "#DA291C"), list("Orange", "#ED8B00"), list("Blue", "#003DA5"),
                list("Green", "#00843D"), list("Mattapan", "#DA291C")
              ),
              function(l) {
                tags$button(
                  type = "button", class = "tchip", `data-tline` = l[[1]], `aria-pressed` = "false", # T lines start off
                  style = paste0("--c:", l[[2]]), l[[1]]
                )
              }
            )
          )
        )
      ),
      tags$div(id = "toast")
    )
  )
)

# Basemap
# Hardcoded: shinyapps.io has no custom-env-var support, and this key is
# inherently client-visible in map tile/style requests anyway — the actual
# protection is the "Allowed HTTP Origins" restriction on the MapTiler key itself.
maptiler_api_key <- "XukbtwhZN33k7aCdvTkA"
map_styles <- list(
  dark = paste0("https://api.maptiler.com/maps/streets-v2-dark/style.json?key=", maptiler_api_key),
  light = paste0("https://api.maptiler.com/maps/streets-v2/style.json?key=", maptiler_api_key)
)
style_key <- map_styles$light # light is the default; layout.js asks for dark if chosen

# Built once per R process (data + style are identical for every session), rather
# than rebuilt and re-serialized from scratch on every single browser connection.
# Town details open in the panel (layout.js sends input$town_click), so the town
# layer has a hover tooltip but no popup.
base_map <- maplibre(style = style_key) |>
  add_navigation_control(position = "top-right") |> # ← Zoom + compass (hidden on mobile)
  fit_bounds(towns_map) |>
  add_line_layer(
    id = "commuter",
    source = commuter_shapes_sf,
    line_color = "#C264D6",
    line_width = 2,
    tooltip = "shape_id"
  ) |>
  add_line_layer(
    id = "subway",
    source = subway_shapes_sf,
    line_color = get_column("route_color"), # official MBTA line colors
    line_width = 3,
    visibility = "none", # T lines start off; layout.js shows them once a line chip is on
    tooltip = "route_name"
  ) |>
  add_fill_layer(
    id = "towns",
    source = towns_map,
    fill_color = get_column("fill_color"),
    fill_opacity = 0.2, # light-theme tint; layout.js re-tints for dark
    fill_outline_color = "#8C96A3",
    tooltip = "town_name"
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
    circle_stroke_color = "#C264D6",
    circle_stroke_width = 2,
    tooltip = "label",
    popup = "label"
  ) |>
  add_circle_layer(
    id = "subway_stations",
    source = subway_stations_sf,
    circle_radius = interpolate(
      property = "zoom",
      values = c(8, 14),
      stops = c(2.5, 7)
    ),
    circle_color = "white",
    circle_stroke_color = get_column("station_color"), # gray at transfer stations
    circle_stroke_width = 2,
    visibility = "none", # see subway layer above
    tooltip = "label",
    popup = "label"
  )

full_bbox <- unname(as.numeric(st_bbox(towns_map)))

# ---- Town details card ----
fmt_or_na <- function(x, f) if (is.null(x) || length(x) == 0 || is.na(x)) "N/A" else f(x)
fmt_home <- function(v) if (v >= 1e6) sprintf("$%.2fM", v / 1e6) else paste0("$", round(v / 1000), "K")

# School tier from the town's fill color (set in 02_preprocessing.R): 1, 2 or 0
tier_of <- function(fill_color) {
  dplyr::case_when(
    fill_color == "#009688" ~ 1L,
    fill_color == "#AB47BC" ~ 2L,
    TRUE ~ 0L
  )
}

town_card_ui <- function(t, saved = FALSE) {
  tier <- tier_of(t$fill_color)
  tier_txt <- c("Below Tier 2", "Tier 1 · above 70th percentile", "Tier 2 · 50–69th percentile")[tier + 1]
  tier_col <- c("#6B7585", "#009688", "#AB47BC")[tier + 1]

  chg <- t$one_year_price_change
  chg_tag <- if (is.na(chg)) NULL else {
    tags$em(class = if (chg >= 0) "up" else "down", sprintf("%+.1f%%", chg))
  }
  score <- t$normalized_school_score
  row <- function(label, ...) tags$div(class = "c-row", tags$span(label), tags$b(...))
  pct <- function(x) fmt_or_na(x, function(v) scales::percent(v, accuracy = 1))

  tagList(
    tags$div(
      class = "c-head",
      tags$span(class = "c-dot", style = paste0("background:", tier_col)),
      tags$div(
        tags$h2(t$town_name),
        tags$p(fmt_or_na(t$DIST_NAME, function(d) paste0(d, " schools"))),
        tags$p(paste0(
          fmt_or_na(t$dens_cat, identity),
          fmt_or_na(t$density, function(d) paste0(" · ", scales::comma(d), "/sq mi"))
        )),
        tags$span(class = paste0("chip t", tier), tier_txt)
      ),
      tags$div(
        class = "c-actions",
        # Handled by layout.js, which keeps the saved list
        tags$button(
          class = if (saved) "c-star saved" else "c-star", type = "button", `data-star` = t$town_name,
          title = if (saved) "Remove from saved towns" else "Save this town",
          `aria-pressed` = if (saved) "true" else "false",
          star_icon
        ),
        tags$button(
          class = "c-close", type = "button", title = "Close",
          onclick = "Shiny.setInputValue('card_close', Date.now(), {priority: 'event'})",
          HTML("&times;")
        )
      )
    ),
    tags$div(
      class = "c-sec",
      tags$h3("Housing"),
      row(
        "Typical 3-bed home",
        fmt_or_na(t$current_typ_home_value, fmt_home),
        chg_tag
      ),
      row("Property tax rate", fmt_or_na(t$prop_rate, function(v) scales::percent(v, accuracy = 0.01)))
    ),
    tags$div(
      class = "c-sec",
      tags$h3("Schools"),
      tags$div(
        class = "c-score",
        tags$b(fmt_or_na(score, function(s) round(s))),
        tags$span("school score out of 100")
      ),
      tags$div(
        class = "c-bar",
        tags$i(style = sprintf("width:%s%%;background:%s", fmt_or_na(score, function(s) round(s)), tier_col))
      ),
      tags$div(class = "c-note", "Average of the district's MCAS, AP and SAT percentile ranks"),
      row("MCAS · AP · SAT rank", paste(pct(t$mcas_rank), pct(t$ap_rank), pct(t$sat_rank), sep = " · ")),
      row("College-bound", pct(t$college_bound_rate)),
      row("High school size", fmt_or_na(t$school_size_est, scales::comma))
    ),
    tags$div(
      class = "c-links",
      tags$a(href = redfin_url, target = "_blank", rel = "noopener", "Redfin"),
      tags$a(href = niche_url, target = "_blank", rel = "noopener", "Niche")
    )
  )
}

# ---- Saved towns (Saved tab table + CSV) ----
# Rows are clickable (layout.js opens the town); the × un-stars it
saved_table_ui <- function(s) {
  if (nrow(s) == 0) {
    return(tags$p(class = "saved-empty", "No saved towns yet. Tap the star on a town's details to add it here."))
  }
  tier_col <- c("#6B7585", "#009688", "#AB47BC")
  tags$table(
    class = "saved-table",
    tags$thead(tags$tr(
      tags$th("Town"), tags$th("Score"), tags$th("Home"), tags$th("Tax"), tags$th()
    )),
    tags$tbody(lapply(seq_len(nrow(s)), function(i) {
      t <- s[i, ]
      tags$tr(
        `data-saved-town` = t$town_name, title = paste("Show", t$town_name),
        tags$td(
          class = "s-town",
          tags$span(class = "c-dot", style = paste0("background:", tier_col[tier_of(t$fill_color) + 1])),
          t$town_name
        ),
        tags$td(fmt_or_na(t$normalized_school_score, function(v) round(v))),
        tags$td(fmt_or_na(t$current_typ_home_value, fmt_home)),
        tags$td(fmt_or_na(t$prop_rate, function(v) scales::percent(v, accuracy = 0.01))),
        tags$td(tags$button(
          class = "s-remove", type = "button", `data-star` = t$town_name,
          title = paste("Remove", t$town_name), HTML("&times;")
        ))
      )
    }))
  )
}

# Every attribute for the CSV, with readable column names
saved_csv_data <- function(s) {
  data.frame(
    `Town` = s$town_name,
    `School district` = s$DIST_NAME,
    `School tier` = c("Below Tier 2", "Tier 1", "Tier 2")[tier_of(s$fill_color) + 1],
    `School score (0-100)` = round(s$normalized_school_score, 1),
    `MCAS percentile` = round(s$mcas_rank * 100),
    `AP percentile` = round(s$ap_rank * 100),
    `SAT percentile` = round(s$sat_rank * 100),
    `College-bound (%)` = round(s$college_bound_rate * 100, 1),
    `High school size (est.)` = s$school_size_est,
    `Typical 3-bed home value ($)` = round(s$current_typ_home_value),
    `1-year price change (%)` = round(s$one_year_price_change, 1),
    `Property tax rate (%)` = round(s$prop_rate * 100, 2),
    `Density (people/sq mi)` = round(s$density),
    `Density category` = s$dens_cat,
    check.names = FALSE
  )
}

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

  toast <- function(msg) session$sendCustomMessage("toast", msg)

  # Fit the map to a bbox; layout.js pads it so the result clears the panel
  focus_map <- function(bbox, sheet = NULL, panel = FALSE, max_zoom = 13, instant = FALSE) {
    session$sendCustomMessage("focus", list(
      bbox = as.numeric(bbox), sheet = sheet, panel = panel,
      maxZoom = max_zoom, instant = instant
    ))
  }

  # Initial Map (pre-built once at app scope; see `base_map` above)
  output$townMap <- renderMaplibre({
    base_map
  })

  # ---- Light / dark basemap ----
  # layout.js sends input$theme once the map has loaded and on every toggle.
  # set_style() keeps the town, rail and ruler layers across the swap; layout.js
  # re-tints the town fills once the new style has loaded.
  map_theme <- "light" # matches style_key above
  observeEvent(input$theme, {
    theme <- if (identical(input$theme, "light")) "light" else "dark"
    if (identical(theme, map_theme)) {
      return(invisible(NULL))
    }
    map_theme <<- theme
    maplibre_proxy("townMap") |>
      set_style(map_styles[[theme]], diff = FALSE, preserve_layers = TRUE)
  })

  # ---- Town selection (dropdown or map click) -> highlight, zoom, details card ----
  current_town <- reactiveVal("")

  select_town <- function(name) {
    sf_sel <- towns_map |> filter(town_name == name)
    if (nrow(sf_sel) == 0) {
      return(invisible(NULL))
    }

    sf_sel <- sf_sel |>
      st_zm(drop = TRUE, what = "ZM") |>
      suppressWarnings(st_cast("MULTIPOLYGON")) |>
      st_make_valid() |>
      select(town_name, geometry)

    current_town(name)
    if (!identical(input$town_sel, name)) updateSelectInput(session, "town_sel", selected = name)

    maplibre_proxy("townMap") |>
      clear_layer("highlight") |>
      add_line_layer(
        id = "highlight",
        source = sf_sel,
        line_color = "#FF5A4F",
        line_width = 3
      )

    # Card opens in the panel: half-height sheet on mobile, sidebar on web
    focus_map(st_bbox(sf_sel), sheet = "half", panel = TRUE)
  }

  clear_town <- function() {
    current_town("")
    if (!identical(input$town_sel, "")) updateSelectInput(session, "town_sel", selected = "")
    maplibre_proxy("townMap") |> clear_layer("highlight")
  }

  observeEvent(input$town_sel, {
    if (identical(input$town_sel, current_town())) {
      return(invisible(NULL)) # already showing (set by a map click)
    }
    if (is.null(input$town_sel) || input$town_sel == "") clear_town() else select_town(input$town_sel)
  })

  # Clicking a town re-selects it even if it's already the current one, so the
  # map re-centres on it after the user has panned away
  observeEvent(input$town_click, {
    select_town(input$town_click$name)
  })

  observeEvent(input$card_close, {
    clear_town()
    focus_map(full_bbox, sheet = "collapsed")
  })

  output$town_card <- renderUI({
    name <- current_town()
    if (!nzchar(name)) {
      return(NULL)
    }
    t <- town_info |> filter(town_name == name)
    if (nrow(t) == 0) {
      return(NULL)
    }
    town_card_ui(t[1, ], saved = name %in% saved_towns())
  })
  # The card is hidden by CSS while empty; keep rendering it anyway, or Shiny
  # would suspend the hidden output and it could never appear
  outputOptions(output, "town_card", suspendWhenHidden = FALSE)

  # ---- Saved towns ----
  # layout.js sends the browser's list on connect and after every change.
  # Names that aren't towns (e.g. a mistyped share link) are dropped.
  saved_towns <- reactive({
    intersect(as.character(unlist(input$saved_towns)), town_info$town_name)
  })
  # Best schools first
  saved_info <- reactive({
    town_info |>
      filter(town_name %in% saved_towns()) |>
      arrange(desc(normalized_school_score), town_name)
  })

  output$saved_table <- renderUI(saved_table_ui(saved_info()))
  outputOptions(output, "saved_table", suspendWhenHidden = FALSE)

  output$saved_csv <- downloadHandler(
    filename = function() paste0("saved_towns_", Sys.Date(), ".csv"),
    content = function(file) utils::write.csv(saved_csv_data(saved_info()), file, row.names = FALSE, na = "")
  )

  # ---- Address lookup (MapTiler only) -> pin + zoom ----
  searching <- reactiveVal(FALSE)

  observe({
    if (isTRUE(searching())) {
      updateActionButton(session, "addr_go", label = tagList(tags$span(class = "spin"), "Searching…"))
    } else {
      updateActionButton(session, "addr_go", label = "Find address")
    }
  })

  observeEvent(input$addr_go, {
    if (isTRUE(searching())) {
      return(invisible(NULL))
    }

    addr_query <- trimws(input$addr_query %||% "")
    if (!nzchar(addr_query)) {
      toast("Please enter an address.")
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
            toast("Address not found.")
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
              circle_color = "#4C8DFF",
              circle_radius = 7,
              circle_stroke_color = "white",
              circle_stroke_width = 2.5,
              tooltip = pt$label[1],
              popup = pt$label[1]
            )

          # Drop the sheet so the pin is visible
          focus_map(st_bbox(view_win), sheet = "collapsed", max_zoom = 15)
        },
        finally = searching(FALSE)
      )
    }, once = TRUE)
  })

  # Driving distance for the ruler tool, via the public OSRM demo server
  # (no API key). The client has already drawn the straight-line figure, so a
  # slow or failed lookup only means the label never upgrades.
  observeEvent(input$ruler_ab, {
    ab <- input$ruler_ab
    if (is.null(ab)) {
      return(invisible(NULL))
    }

    route <- try(
      osrm::osrmRoute(
        src = c(ab$ax, ab$ay),
        dst = c(ab$bx, ab$by),
        overview = "full"
      ),
      silent = TRUE
    )

    if (inherits(route, "try-error") || is.null(route) || nrow(route) == 0) {
      session$sendCustomMessage("ruler-route", list(ok = FALSE, token = ab$token))
      return(invisible(NULL))
    }

    # Thin the drawn path — a full OSRM route runs to well over a thousand
    # vertices. dTolerance is in metres here (s2 is on), and 5 m keeps the
    # road shape while cutting the payload ~10x. The reported mileage comes
    # from OSRM's own distance field, so this never affects the number shown.
    route_geom <- try(
      suppressWarnings(st_simplify(route, dTolerance = 5)),
      silent = TRUE
    )
    if (inherits(route_geom, "try-error")) route_geom <- route

    m <- unname(st_coordinates(route_geom)[, 1:2, drop = FALSE])

    session$sendCustomMessage("ruler-route", list(
      ok = TRUE,
      token = ab$token,
      miles = as.numeric(route$distance[1]) * 0.621371,
      minutes = as.numeric(route$duration[1]),
      coords = lapply(seq_len(nrow(m)), function(i) unname(m[i, ]))
    ))
  })

  observeEvent(input$reset_view, {
    # Clear highlight, card & search point, then zoom back to full bounds
    clear_town()
    maplibre_proxy("townMap") |> clear_layer("search_pt")
    updateTextInput(session, "addr_query", value = "")
    focus_map(full_bbox, sheet = "collapsed")
  })

  # Custom controls under the zoom/compass group: locate-me, ruler, then
  # transit and the legend (kept last by CSS `order`)
  session$onFlushed(function() {
    session$sendCustomMessage("attach-layout", "townMap")
    session$sendCustomMessage("attach-distance-tool", "townMap")
    session$sendCustomMessage("attach-tip", "townMap")
    focus_map(full_bbox, instant = TRUE) # re-fit with the panel accounted for
    session$sendCustomMessage("map-ready", TRUE)
  }, once = TRUE)
}

# small helper
`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- Run App ----
shinyApp(ui, server)
