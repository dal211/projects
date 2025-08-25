library(tidycensus)
library(dplyr)
library(stringr)
library(sf)

# All MA counties for county-subdivision requests
ma_counties <- tidycensus::fips_codes %>%
  filter(state == "MA") %>%
  distinct(county) %>%
  pull()

# 1) Pull total population for county subdivisions (= towns & cities) + geometry
mcd <- get_acs(
  geography = "county subdivision",   # <-- this is the key change
  variables = "B01003_001",           # total population
  state = "MA",
  county = ma_counties,               # all counties in MA
  year = 2023,
  geometry = TRUE
)

# 2) Compute land area (sq mi) using an equal-area projection for accurate areas
mcd <- mcd %>%
  st_transform(5070) %>%                        # NAD83 / Conus Albers (equal-area)
  mutate(
    land_sqmi = as.numeric(st_area(geometry)) / 2.59e+6,
    density   = estimate / land_sqmi
  ) %>%
  st_drop_geometry()

# 3) Clean a readable town/city name
mcd <- mcd %>%
  mutate(
    town_clean = str_remove(NAME, ", Massachusetts$"),
    town_clean = sub("( town| city),.*$", "", town_clean)  # "Amherst town, Hampshire County, MA" -> "Amherst"
  ) %>%
  select(town_clean, estimate, land_sqmi, density)

mcd <- mcd %>%
  mutate(
    dens_cat = case_when(
      density > 3000                      ~ "Urban City",
      density >= 1000 & density <= 3000   ~ "Dense Suburb",
      density >= 500  & density < 1000    ~ "Mid-Dense Suburban",
      density >= 200  & density < 500     ~ "Sparse Suburban",
      density < 200                       ~ "Rural Town",
      TRUE                                ~ NA_character_
    )
  )

saveRDS(mcd, "data/pop_density.rds")
