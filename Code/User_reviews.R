## Webscraping users reviews on The Last of Us Part I using R's rvest and polite functions

# Load libraries
library(rvest)
library(polite)
library(tidyverse)
library(magrittr)


# Provide html
html <- "https://www.metacritic.com/game/playstation-5/the-last-of-us-part-i/user-reviews?page=0"


# Function to use in a webscrape loop
user_reviews <- function(i) {
  bow(paste0(str_remove(html, "0"), i)) %>% 
  scrape() %>% 
  html_elements(".review_body span") %>% html_text2()
}


# Read how many pages of user comments there are on the metacritic page
pages_num <- bow(html) %>% 
  scrape() %>% 
  html_elements(".page_num") %>% 
  html_text() %>% 
  as.numeric() - 1


# Run a for loop to webscrape from the metacritic sites
scraped_reviews <- lapply(pages_num, user_reviews)


# Row bind reviews
df <- plyr::ldply(scraped_reviews, rbind)


# Stopwords vector
stopwords <- c("… expand|expand|…|click expand to view| el | la | il | de | بناموست | é | и | aaaa")


# Keep words vector
keepwords <- c(" a | the | is | game | but | no | so | best | you | you.| and | not | to ")


# Delete wrong lines 
df %<>% 
  set_colnames(paste0("Text_", 1:length(df))) %>% 
  pivot_longer(Text_1:paste0("Text_", (length(df)))) %>% 
  mutate(value = tolower(value)) %>% 
  filter(str_count(value) > 0) %>% 
  filter(!str_detect(value, 
                     stopwords)) %>% 
  mutate(count = str_count(value),
         duplicate = str_sub(value, 1, 25) == str_sub(lag(value), 1, 25)) %>%
  replace(is.na(.), FALSE) %>% 
  # # Check what word caused the classification to filter
  # mutate(string_removed = str_extract(value, stopwords))
  filter(duplicate == FALSE) %>% 
  filter(str_detect(value, 
                     keepwords)) %>% 
  filter(count > 74) %>% 
  select(-duplicate)


# Write data frame
write_csv(df, "Processed_data/users_reviews.csv")
