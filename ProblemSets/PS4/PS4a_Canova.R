library(jsonlite)
library(dplyr)
system('wget -O date.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en"')
cat dates.json
mylist <- fromJSON('dates.json')
mydf <- bind_rows(mylist$result[-1])
class(mydf$date)
head(mydf)
