## Webscraping critics reviews on The Last of Us Part I using R's rvest and polite functions

# Load libraries
library(rvest)
library(polite)
library(tidyverse)
library(magrittr)
library(tm)


# Provide html 
html <- "https://www.metacritic.com/game/playstation-5/the-last-of-us-part-i/critic-reviews"


# Process html to get review body 
review_body <- read_html(html) %>% 
  html_elements(".review_body") %>% 
  html_text2()


# Get ratings from the html. Replace empty strings with 'NA'
review_ratings <- read_html(html) %>% 
  html_elements("#main .indiv") %>% 
  html_text2() %>% 
  str_replace("^$", "NA")

# Remove 2 last ratings because they are the result of metacritic internal website design and thus are not important for us
# (These deleted html elements appear in the explanation in the question mark in website design)
review_ratings <- review_ratings[1:123]


# Get a critic name
review_critics <- read_html(html) %>% 
  html_elements(".source") %>% 
  html_text2()


# Create a data frame consisting of ratings and critics
df_review <- data.frame(review_ratings, review_critics)


## Webscraping full reviews

# Get url links to full reviews
review_htmls <- read_html(html) %>% 
  html_elements(".source .external") %>% 
  html_attr("href")


# Get review critics again this time only with a url
review_critics_w_url <- read_html(html) %>% 
  html_elements(".source .external") %>% 
  html_text2()


# Make a data frame. Filter PLAY magazine as they do not provide link on the metacritic website to the review
df_critics_ratings <- data.frame(
  htmls = review_htmls, 
  critics = review_critics_w_url, ratings = df_review %>% filter(review_critics != "PLAY") %>% pull(review_ratings))


# Filter for websites with british, american domain
df_critics_ratings %<>% 
  filter(
    htmls %in% c(str_subset(htmls, "com/"), 
                 str_subset(htmls, "net/"), 
                 str_subset(htmls, "co.uk/")))


# Read reviews
review_htmls <- read_html(html) %>% 
  html_elements(".external") %>% 
  html_attr('href') %>% 
  unique()


# Keep only websites with british, american domain
review_htmls_1 <- c(
  str_subset(review_htmls, "com/"), 
  str_subset(review_htmls, "net/"),
  str_subset(review_htmls, "co.uk/")
)


# Function to use in a webscrape loop
function_bow <- function(i) {
  review_htmls_1[i] %>% 
    bow(delay = 5, force = TRUE) %>% 
    scrape() %>% 
    html_elements("p") %>% html_text2() %>% 
    str_subset(., ".+")
}

# Function that deals with errors. I tried using 'safely' function but with no success. Possibly 'nod' could also work but
# I did not look into this option. The problem with used solution is that it takes a while to go through the whole process
scrape_reviews <- function (i) {
  # Detect if there is an error or not
  if_error <- tryCatch(function_bow(i), error = function(i) i)
  
  # If there is not an error in running function_bow run the function. Else apply 'FALSE'
  if(any(class(if_error) == "error") == FALSE) {
    review <- function_bow(i)
  } else {
    review <- "FALSE"
  }
}


# Run a for loop to webscrape from all the websites provided by review_htmls_1 data
scraped_reviews <- lapply(1:length(review_htmls_1), scrape_reviews) 


# Row bind reviews
df <- plyr::ldply(scraped_reviews, rbind)


# Modify data frame. Set column names, add urls, ratings and remove websites which could not be scrapped
df %<>% 
  rowid_to_column() %>% 
  select(1:41) %>% 
  set_colnames(c("rowid", paste0("V_", 1:40))) %>% 
  cbind(data.frame(url = review_htmls_1)) %>% 
  # Join with df_critics_ratings data frame to look at ratings, critics, count per review etc
  left_join(df_critics_ratings, by = c("url" = "htmls")) %>% 
  # Filter 
  filter(V_1 != "FALSE")


# Delete lines consisting of non-english words
df %<>% 
  filter(!str_detect(V_2, " il | el |在| de | da | ja")) %>% 
  filter(!str_detect(V_3, " il | el |在| de | da | ja |ア|こ"))


# Stopwords vector. They were slowly expanded to facilitate broad web scraping only on 'p' element. This caused that many wrong
# lines were cought into the bin
stopwords <- c("gas station simulator|nintendo switch|riot games|email|crazyaejay|sammy's covered|create my|cookies|android central|formats:|more:|world’s defining voice in|gamebyte|today, we have millions|outlet|striking distance studios|early access version|trusted reviews|independent’s|cgmagazine|vg274|gamesbeat's|more :|with code provided|dan and jeff grubb visit|Fallout|
                     twitter|t3 is supported|platform:|editorial independence|public relations|video chums|©|nintendo|binge-watcher|videogamer|gamesradar|join gaming|twitter|tags:|impulse gamer|founded|further reading|provided a code|sign up|find out more|developer:|michael goroff|tj denzer|want to know what|gaming deals|verifyerrors|amazon|mailing|concert|getelementbyid|sign up|xbox series|new joe & mac|witcher|disclaimer|subscribe|registration|privacy policy|»|fifa|inbox|silent hill|bookmark|plague tale|bomb team|deals for only|features, news, tips|affiliate links")


# Delete wrong lines 
df %<>% 
  pivot_longer(cols = V_1:V_40) %>% 
  mutate(count = str_count(value),
         value = tolower(value)) %>% 
  filter(!count < 70) %>% 
  arrange(desc(count)) %>% 
  # # What stopwords should be added or changed
  # filter(str_detect(value, "more :"))
  filter(!str_detect(value, 
                     stopwords)) 
  # # Check if the lines were correctly identified as non-reviews
  # filter(str_detect(value,
  #                    stopwords)) %>%
  # # Check what word caused the classification to filter
  # mutate(string_removed = str_extract(value, stopwords))


# Put data frame in organized form to futher text mining
df %<>% 
  select(-count) %>%
  pivot_wider(values_from = value, names_from = name) %>% 
  arrange(rowid) %>% 
  select(rowid, url, critics, ratings, paste0("V_", 1:39)) %>% 
  group_by(rowid) %>% 
  replace(is.na(.), "") %>% 
  mutate(full_text = paste(across(contains("V_")), collapse = " ")) %>% 
  select(rowid, url, critics, ratings, full_text) %>% 
  mutate(full_text = stripWhitespace(full_text),
         count = str_count(full_text))


# Write data frame
write_csv(df, "Processed_data/critics_reviews.csv")
