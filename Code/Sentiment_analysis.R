# Sentiment analysis of The Last of Us Part I

# Load libraries
library(tidyverse)
library(tidytext)
library(tm)
library(qdap)
library(magrittr)
library(ggplot2)
library(tokenizers)

# Load data on critics reviews 
critics_corpus <- read_csv("Processed_data/critics_reviews.csv")
users_corpus <- read_csv("Processed_data/users_reviews.csv")

# Create a VectorSource
critics_vector <- VectorSource(critics_corpus[[5]])
users_vector <- VectorSource(users_corpus[[2]])

# Apply VCorpus
critics_vector <- VCorpus(critics_vector)
users_vector <- VCorpus(users_vector)

# Examine the first document's contents
content(critics_vector[[1]])

# Clean the text. The clean_corpus was introduced earlier:
clean_corpus <- function(corpus) {
  corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "...", "last", "game", "part", "joel", "naughty", "time", "like", "even", "also", "much", "just", "still", "will", "games", "make", "ellie", "playstation", "original", "version", "remake"))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Clean corpus
critics_clean <- clean_corpus(critics_vector)
users_clean <- clean_corpus(users_vector)

# Get back to data frame format
# Code inspired by 'Sentiment Analysis in R' Martin Schweinberger, https://ladal.edu.au/sentiment.html#Identifying_important_emotives
tokkenization <- function(x, source) {
  data.frame(x) %>% 
    select(word = 2) %>%
    stringr::str_squish() %>%
    stringr::str_split(" ") %>% 
    unlist() %>%
    tibble::tibble() %>% 
    set_colnames("word") %>%
    filter(!word %in% c('c("', '"', ")", "c", "")) %>% 
    mutate(
      word = str_remove_all(word, '"'),
      source = source
    )
}

critics_token <- tokkenization(critics_clean, "critics")
users_token <- tokkenization(users_clean, "users")


# Comparison analysis
palette <- wesanderson::wes_palette("Zissou1", n = 5)[c(5,1)]

sentiment <- rbind(critics_token, users_token) %>% 
  group_by(source) %>% 
  dplyr::mutate(words = n()) %>%
  dplyr::left_join(tidytext::get_sentiments("nrc")) %>%
  dplyr::mutate(
    source = source,
    source = factor(source),
    sentiment = factor(sentiment)
  ) %>% 
  filter(!is.na(sentiment)) %>% 
  dplyr::group_by(sentiment, source) %>%
  dplyr::summarise(sentiment = unique(sentiment),
                   sentiment_freq = n(),
                   words = unique(words)) %>%
  dplyr::mutate(percentage = round(sentiment_freq / words * 100, 1))


sentiment %>%
  dplyr::filter(sentiment != "positive",
                sentiment != "negative") %>%
  ggplot(aes(sentiment, percentage, fill = source, group = source)) +   
  geom_bar(stat="identity",   
           position=position_dodge(), fill = "white") + 
  geom_bar(stat="identity",   
           position=position_dodge(), alpha = 0.7) + 
  scale_fill_manual(values = palette, name = "") +
  scale_y_continuous(breaks = seq(0, 6, 2), labels = paste0(seq(0, 6, 2), "%")) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text = element_text(color = "black"),
    panel.grid.major.x  = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.subtitle = element_text(hjust = 0, size = 6)
  ) +
  labs(
    x = "",
    y = "Percentage",
    title = "Presence of sentiments among user and critics reviews of The Last of Us Part I"
  )
ggsave("Plots/Sentiments_presence.png", dpi = 600, width = 11, height = 7)

{
  png("Plots/Polarity_difference.png", res = 600, width = 7, height = 7, units = "in")
  sentiment %>%
    dplyr::filter(sentiment == "positive" | sentiment == "negative") %>%
    dplyr::select(-percentage, -words) %>%
    dplyr::mutate(sentiment_sum = sum(sentiment_freq),
                  positive = sentiment_sum-sentiment_freq) %>%
    dplyr::filter(sentiment != "positive") %>%
    dplyr::rename(negative = sentiment_freq) %>%
    dplyr::select(source, positive, negative) %>%
    dplyr::group_by(source) %>%
    dplyr::summarise(polarity = positive/negative - 1) %>%
    ggplot(aes(reorder(source, polarity, mean), polarity, fill = source)) +    
    geom_bar(stat = "identity") +
    geom_bar(fill = "white", alpha = 0.3, stat = "identity") +
    geom_text(aes(y = polarity-0.1, label = round(polarity, 2)), 
              color = "white", size = 4) + 
    geom_text(aes(y = polarity+0.1, label = round(polarity, 2)), 
              color = "white", size = 4) + 
    scale_fill_manual(values = palette) +
    theme_minimal() +
    labs(
      y = "",
      x = "",
      title = "Polarity between words used by users and critics in their reviews\nof The Last of Us Part I",
      subtitle = "Ratio of positive to negative words"
    ) +
    scale_y_continuous(
      breaks = c(-1, 0, 1),
      limits = c(-1, 2.5),
      labels = c("more negative", "neutral", "more positive")
    ) +
    theme(
      legend.position = "none",
      axis.text = element_text(color = "black"),
      panel.grid.major.x  = element_blank(),
      panel.grid.minor.y = element_blank(),
      plot.subtitle = element_text(hjust = 0, size = 6)
    )
  dev.off()
}

# Which words appeared more often in users reviews
comparison <- rbind(critics_token, users_token) %>% 
  filter(!word %in% c(",")) %>% 
  group_by(source) %>% 
  mutate(count = n()) %>% 
  group_by(source, word) %>% 
  summarize(percentage = 100 * n() / count) %>% 
  filter(row_number() == 1) %>% 
  arrange(desc(percentage)) %>% 
  group_by(source) %>% 
  mutate(percentage_source = ifelse(source == "users", percentage, 0)) %>% 
  mutate(word = fct_reorder(word, percentage_source))

# Take top 10 words
top_10_words <- comparison %>% 
  filter(source == "users") %>% 
  top_n(n = 10, wt = percentage) %>% 
  pull(word)

# Plot the most frequent words in user and critics reviews
comparison %>% 
  filter(word %in% top_10_words) %>% 
  ggplot(aes(x = percentage, y = fct_reorder(word, percentage_source), fill = source, group = source)) + 
  geom_bar(stat = "identity", position = position_dodge(), fill = "white") +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.7) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text = element_text(color = "black"),
    panel.grid.major.y  = element_blank(),
    plot.caption = element_text(hjust = 0, size = 6)
  ) +
  scale_fill_manual(values = palette, name = "") +
  scale_x_continuous(limits = c(0, 1.6), expand = c(0, 0), breaks = seq(0, 1.5, 0.5), labels = paste0(seq(0, 1.5, 0.5), "%")) +
  labs(
    x = "",
    y = "",
    title = "The most frequent words in user and critics reviews of The Last of Us Part I",
    subtitle = "Percentage presence in all words"
  )

ggsave("Plots/Frequency_comparison_users.png", dpi = 600, width = 11, height = 7)
