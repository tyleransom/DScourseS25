library(rvest)
library(tidyverse)

# Define the URL
url <- "https://en.wikipedia.org/wiki/List_of_deserts_by_area"

# Read the webpage
page <- read_html(url)

# Extract the table
desert_data <- page %>%
    html_element(".wikitable") %>%
    html_table()



###################

library(rvest)
library(tidyverse)

# First, let's inspect what tables are available
h <- read_html("https://en.wikipedia.org/wiki/Current_members_of_the_United_States_House_of_Representatives")

# Print info about each table
tables <- h %>% html_elements(".wikitable")

# Examine each table's structure
for(i in seq_along(tables)) {
    cat(sprintf("\nTable %d:\n", i))
    print(dim(html_table(tables[[i]])))
    # Print first few column names
    print(names(html_table(tables[[i]]))[1:3])
}

# Modified scraping function with more detailed error handling
scrape_representatives <- function(url) {
    h <- read_html(url)
    
    tables <- h %>% html_elements(".wikitable")
    
    message(sprintf("Found %d tables", length(tables)))
    
    # Look for the table with the right structure
    # We can identify it by checking column names or dimensions
    for(i in seq_along(tables)) {
        table <- try(html_table(tables[[i]]))
        if(inherits(table, "try-error")) next
        
        # Print diagnostic info
        message(sprintf("Table %d dimensions: %d x %d", i, nrow(table), ncol(table)))
        message("First few column names: ", paste(names(table)[1:3], collapse=", "))
        
        # Add your logic here to identify the correct table
        # For example, if you know it should have specific columns:
        if("District" %in% names(table) && "Member" %in% names(table)) {
            message("Found target table!")
            reps <- table
            return(as_tibble(reps[,c(1:2,4:9)]))
        }
    }
    
    stop("Could not find table with expected structure")
}

# Try the scraping with verbose error handling
tryCatch({
    reps <- scrape_representatives("https://en.wikipedia.org/wiki/Current_members_of_the_United_States_House_of_Representatives")
    print(head(reps))
}, error = function(e) {
    message("Error scraping data: ", e$message)
})