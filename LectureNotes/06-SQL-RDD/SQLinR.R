library(tidyverse)
library(sqldf)
df <- iris %>% as_tibble()
sqldf('SELECT count(*) FROM df WHERE Species = "virginica"')
# store the output:
counted <- sqldf('SELECT count(*) FROM df WHERE Species = "virginica"')
# dplyr way:
counted.dplyr <- df %>% filter(Species=="virginica") %>% count %>% print

# check if the results are the same:
identical(counted, counted.dplyr)

