library(dplyr)
library(sf)
library(jsonlite)

# Writes the static site's data files (web/data/) from the .rds files built by
# 01_pop_density.R, 02_preprocessing.R and 03_mbta.R. Re-run after any of those.
# Shapes are simplified and validated here, once, instead of on every page load.

out_dir <- "web/data"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# GeoJSON (RFC 7946) with coordinates rounded to 5 decimals (~1 m)
write_geojson <- function(x, name) {
  path <- file.path(out_dir, paste0(name, ".geojson"))
  if (file.exists(path)) file.remove(path)
  st_write(
    st_transform(x, 4326), path,
    driver = "GeoJSON", quiet = TRUE,
    layer_options = c("RFC7946=YES", "COORDINATE_PRECISION=5", "WRITE_BBOX=NO")
  )
  message(sprintf("%-28s %6.0f KB", basename(path), file.size(path) / 1024))
}

# ---- Towns: shapes (map) + attributes (card, Saved table, CSV) ----
towns_map <- readRDS("data/towns_map.rds")
towns_sf <- readRDS("data/towns_sf.rds")

towns_map |>
  select(town_name, fill_color) |> # popup_html isn't used by the site
  st_make_valid() |>
  write_geojson("towns")

town_info <- towns_sf |>
  st_drop_geometry() |>
  select(
    town_name, DIST_NAME, dens_cat, density, fill_color,
    current_typ_home_value, one_year_price_change, prop_rate,
    normalized_school_score, mcas_rank, ap_rank, sat_rank,
    college_bound_rate, school_size_est
  ) |>
  arrange(town_name)

write_json(
  list(
    bbox = unname(as.numeric(st_bbox(towns_map))), # initial / "Reset map" view
    towns = town_info
  ),
  file.path(out_dir, "towns.json"),
  dataframe = "rows", na = "null", digits = 6, auto_unbox = TRUE
)
message(sprintf("%-28s %6.0f KB", "towns.json", file.size(file.path(out_dir, "towns.json")) / 1024))

# ---- Commuter rail ----
readRDS("data/shapes_sf.rds") |>
  st_transform(4326) |>
  st_simplify(dTolerance = 100) |>
  st_make_valid() |>
  select(shape_id) |>
  write_geojson("commuter_lines")

readRDS("data/commuter_stations_sf.rds") |>
  transmute(label = paste0(stop_name, " — ", municipality)) |>
  write_geojson("commuter_stations")

# ---- T lines (subway + light rail) ----
readRDS("data/subway_shapes_sf.rds") |>
  st_transform(4326) |>
  st_simplify(dTolerance = 10) |>
  st_make_valid() |>
  transmute(
    route_name, route_color,
    line = sub(" Line.*$", "", route_name) # "Green Line B" -> "Green"; used by the line chips
  ) |>
  write_geojson("subway_lines")

readRDS("data/subway_stations_sf.rds") |>
  transmute(
    label = paste0(stop_name, " — ", lines, if_else(grepl(",", lines), " Lines", " Line")),
    lines, station_color
  ) |>
  write_geojson("subway_stations")
