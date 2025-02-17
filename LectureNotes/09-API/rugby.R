library(tidyverse)
library(jsonlite)

m_endpoint <- "https://api.wr-rims-prod.pulselive.com/rugby/v3/rankings/mru?language=en"
m_rugby <- fromJSON(m_endpoint)
str(m_rugby)

w_endpoint <- "https://api.wr-rims-prod.pulselive.com/rugby/v3/rankings/wru?language=en"
w_rugby <- fromJSON(w_endpoint)
str(w_rugby)

m_rugby <- m_rugby$entries %>% as_tibble()

