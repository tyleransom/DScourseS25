library(tidyverse)
library(jsonlite)
library(httr)

endpoint <- "series/observations"
params <- list(
    api_key= Sys.getenv("FRED_API_KEY"), ## Change to your own key
    file_type="json", 
    series_id="GNPCA"
)

fred <- 
    httr::GET(
        url = "https://api.stlouisfed.org/", ## Base URL
        path = paste0("fred/", endpoint),    ## The API endpoint
        query = params                       ## Our parameter list
    )

fred <-
    fred %>% 
    httr::content("text") %>% ## Extract the reponse content (i.e. text)
    jsonlite::fromJSON() %>%
    purrr::pluck("observations") %>%
    as_tibble()


library(lubridate) ## Already loaded above

fred <-
    fred %>%
    mutate(across(realtime_start:date, ymd)) %>%
    mutate(value = as.numeric(value)) 

fred %>%
    ggplot(aes(date, value)) +
    geom_line() +
    scale_y_continuous(labels = scales::comma) +
    labs(
        x="Date", y="2012 USD (Billions)",
        title="US Real Gross National Product", caption="Source: FRED"
    ) +
    theme_minimal()

