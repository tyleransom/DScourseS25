library(rvest)
library(dplyr)
library(stringr)
library(httr)


#Web Scrape
{
# Web Scraping Script for Extracting List Items

# URL of the page to scrape
url <- "https://dallasmakerspace.org/wiki/Meeting_20100218"

# Function to scrape list items using specific CSS selector
scrape_list_items <- function(url) {
  tryCatch({
    # Fetch the webpage
    response <- GET(url)
    
    # Check if the request was successful
    if (status_code(response) == 200) {
      # Parse the HTML content
      page <- read_html(response)
      
      # Extract list items using the specific CSS selector
      list_items <- page %>% 
        html_element("#mw-content-text > div > ul") %>%
        html_elements("li") %>% 
        html_text(trim = TRUE)
      
      # Return the list of items
      return(list_items)
      
    } else {
      stop(paste("Failed to retrieve the page. Status code:", status_code(response)))
    }
  }, 
  error = function(e) {
    message("An error occurred during web scraping:")
    message(e$message)
    return(NULL)
  })
}

# Execute the scraping function
attendees <- scrape_list_items(url)

# Process and display the results
if (!is.null(attendees)) {
  # Print the list of attendees
  cat("Meeting Attendees:\n")
  print(attendees)
  
  # Create a data frame
  attendees_df <- data.frame(
    name = attendees,
    stringsAsFactors = FALSE
  )
  
  # Display as a data frame
  print(attendees_df)
  
  # Write to a CSV file
  write.csv(attendees_df, "meeting_attendees.csv", row.names = FALSE)
}
}

#API Call
{
  # Load required libraries
  library(httr)
  library(jsonlite)
  library(dplyr)
  
  # URL for ZBP 2018 variables
  variables_url <- "https://api.census.gov/data/2018/zbp/variables.json"
  
  # Function to retrieve and process variables
  get_census_variables <- function(url) {
    # Make the API request
    response <- GET(url)
    
    # Check if the request was successful
    if (status_code(response) == 200) {
      # Parse the JSON content
      variables_content <- fromJSON(content(response, "text", encoding = "UTF-8"))
      
      # Extract variables from the nested structure
      variables_df <- lapply(names(variables_content$variables), function(var_name) {
        var_info <- variables_content$variables[[var_name]]
        data.frame(
          variable = var_name,
          concept = if(!is.null(var_info$concept)) var_info$concept else NA_character_,
          predicateType = if(!is.null(var_info$predicateType)) var_info$predicateType else NA_character_,
          group = if(!is.null(var_info$group)) var_info$group else NA_character_,
          limit = if(!is.null(var_info$limit)) var_info$limit else NA_character_,
          stringsAsFactors = FALSE
        )
      }) %>% do.call(rbind, .)
      
      return(variables_df)
    } else {
      stop(paste("API request failed with status code:", status_code(response)))
    }
  }
  
  # Retrieve variables
  zbp_variables <- get_census_variables(variables_url)
  
  # Display summary of variables
  print(paste("Total number of variables:", nrow(zbp_variables)))
  
  # Print first few rows
  print(head(zbp_variables))
  
  # Optional: More detailed exploration
  # Filter variables by concept or group
  concept_summary <- zbp_variables %>%
    group_by(concept) %>%
    summarise(variable_count = n()) %>%
    arrange(desc(variable_count))
  
  print("Variables by Concept:")
  print(concept_summary)
  
  # Save variables to CSV
  write.csv(zbp_variables, "zbp_2018_variables.csv", row.names = FALSE)
  cat("Variables saved to zbp_2018_variables.csv\n")
}
