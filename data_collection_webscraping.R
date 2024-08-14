############################################## DATA COLLECTION BY WEB SCRAPING #########################################

# Install necessary packages
install.packages("rvest")
install.packages("httr")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("purrr")

# Load necessary libraries
library(rvest)
library(dplyr)
library(tidyverse)
library(purrr)

# Define the main URL listing the speeches
base_url <- "https://www.tccb.gov.tr/receptayyiperdogan/konusmalar/?&page="

# Function to scrape a single page
scrape_page <- function(page_number) {
  page_url <- paste0(base_url, page_number)
  page <- read_html(page_url)
  dates <- page %>% html_elements("dt.date") %>% html_text()
  titles <- page %>% html_elements("dd a") %>% html_text()
  links <- page %>% html_elements("dd a") %>% html_attr("href") %>% map_chr(~ paste0("https://www.tccb.gov.tr", .))
  
  if (length(dates) == length(titles) && length(titles) == length(links)) {
    return(tibble(Date = dates, Title = titles, Link = links))
  } else {
    return(tibble(Date = character(), Title = character(), Link = character()))
  }
}

# Loop through the pages and scrape data
all_speeches <- map_df(1:40, ~ {
  Sys.sleep(5) # Polite scraping by waiting 5 second
  scrape_page(.x)
})

# Save the initial scrape to CSV
write.csv(all_speeches, "all_speeches.csv", row.names = FALSE)

# Function to extract speech text
extract_speech_text <- function(link) {
  Sys.sleep(1) # Prevent server overload
  page <- tryCatch(read_html(link), error = function(e) NA)
  if (!is.na(page)) {
    text <- page %>% html_element('div#divContentArea') %>% html_text(trim = TRUE)
    return(text)
  } else {
    return(NA)
  }
}

# Scrape speech text for each link and add to the dataframe
speech_texts <- map_chr(all_speeches$Link, extract_speech_text)

# Add speech texts to the dataframe
all_speeches$SpeechText <- speech_texts

# Check results and save to CSV
print(head(all_speeches))
write.csv(all_speeches, "all_speeches_with_text.csv", row.names = FALSE)
write.csv(all_speeches, "C:/Users/User/Desktop/Thesis_LU/all_speeches.csv", row.names = FALSE)


