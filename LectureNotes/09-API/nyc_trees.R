library(tidyverse)
library(jsonlite)

endpoint <- "https://data.cityofnewyork.us/resource/uvpi-gqnh.json?$limit=68000"

nyc_trees <- 
    fromJSON(endpoint) %>%
    as_tibble()

# map of trees
nyc_trees %>% 
    select(longitude, latitude, stump_diam, spc_common, spc_latin, tree_id) %>% 
    mutate_at(vars(longitude:stump_diam), as.numeric) %>% 
    ggplot(aes(x=longitude, y=latitude, size=stump_diam)) + 
    geom_point(alpha=0.5) +
    scale_size_continuous(name = "Stump diameter") +
    labs(
        x = "Longitude", y = "Latitude",
        title = "Sample of New York City trees",
        caption = "Source: NYC Open Data"
    ) +
    theme_minimal()


