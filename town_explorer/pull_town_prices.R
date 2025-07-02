library(httr)
library(jsonlite)
library(dplyr)

pull_town_prices <- function(town_name) {
  api_key <- Sys.getenv("RAPIDAPI_KEY")
  
  response <- GET(
    url = "https://realtor.p.rapidapi.com/search/forsale",
    add_headers(
      `X-RapidAPI-Key` = api_key,
      `X-RapidAPI-Host` = "realtor.p.rapidapi.com"
    ),
    query = list(
      location = paste0(town_name, ", MA"),
      limit = 100 # Max 200 per call
    )
  )
  
  if (status_code(response) != 200) {
    message(paste("Failed for:", town_name))
    return(NULL)
  }
  
  content <- content(response, as = "text")
  data <- fromJSON(content, flatten = TRUE)
  
  if (length(data$properties) == 0) {
    return(NULL)
  }
  
  listings <- data$properties %>% 
    as_tibble() %>% 
    summarize(
      avg_price = mean(price, na.rm = TRUE),
      num_listings = n()
    ) %>% 
    mutate(town_name = town_name)
  
  return(listings)
}
