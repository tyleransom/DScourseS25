library(tidyverse)
library(rvest)

url <- "https://en.wikipedia.org/wiki/Men%27s_100_metres_world_record_progression"

# the hard way
webpage <- read_html(url)

# extract the table from the selector below:

data1 <- webpage %>%
    html_element("#mw-content-text > div.mw-content-ltr.mw-parser-output > table:nth-child(11)") %>% ## select table element
    html_table()                                      ## convert to data frame

data2 <- webpage %>%
    html_element("#mw-content-text > div.mw-content-ltr.mw-parser-output > table:nth-child(17)") %>% ## select table element
    html_text()                                      ## convert to text


# the easy way
d1 <- webpage %>%
    html_elements(".wikitable") %>%
    html_table()

dataE1 <- d1[[1]]
dataE2 <- d1[[3]]

dataE <- bind_rows(dataE1, dataE2)


