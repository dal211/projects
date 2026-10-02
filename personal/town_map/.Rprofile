source("renv/activate.R")

# Positron on Windows doesn't load ~/.Renviron at startup, so load it here
# (only fills in variables that aren't already set, e.g. CENSUS_API_KEY)
if (!nzchar(Sys.getenv("CENSUS_API_KEY")) && file.exists("~/.Renviron")) {
  readRenviron("~/.Renviron")
}
