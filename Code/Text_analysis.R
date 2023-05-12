# Text analysis

# Load libraries
library(tidyverse)
library(tidytext)
library(tm)
library(qdap)
library(magrittr)
library(ggplot2)
library(tokenizers)
library(RWeka)
library(ldatuning)
library(slam)
library(topicmodels)
library(reshape2)
library(stringr)

# Load data on critics reviews 
critics_corpus <- read_csv("/Users/mr.fox/Desktop/Github/Sentiment_analysis/Processed_data/critics_reviews.csv")

# Create a VectorSource
critics_vector <- VectorSource(critics_corpus[[5]])

# Apply VCorpus
critics_vector <- VCorpus(critics_vector)

# Examine the first document's contents
content(critics_vector[[1]])

# Clean the text. The clean_corpus was introduced earlier:
clean_corpus <- function(corpus) {
  corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "...", "last", "game", "part", "joel", "naughty", "time", "like", "even", "also", "much", "just", "still", "will", "games", "make", "ellie", "playstation", "original", "version", "remake"))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Clean corpus
critics_clean <- clean_corpus(critics_vector)

# Create TDM from corpus
critics_tdm <- TermDocumentMatrix(critics_clean)

critics_tdm

critics_m_sums <- as.matrix(critics_tdm) %>% 
  rowSums() %>% 
  sort(decreasing = T)

# Plot
df_critics <- data_frame(
  word = critics_m_sums[1:15] %>% as.data.frame() %>% rownames(),
  count = critics_m_sums[1:15] %>% as.data.frame() %>% pull(.)
  )

ggplot(df_critics, aes(y = fct_reorder(word,count), x = count, group = word)) +
  geom_col(fill = "black", width = 0.1) +
  geom_point(fill = "black", size = 3.25) +
  scale_x_continuous(limits = c(0, max(df_critics$count)* 1.03), expand = c(0, 0)) +
  theme_minimal() +
  theme(
    axis.text = element_text(color = "black", vjust = 0.4),
    panel.grid.major.y  = element_blank(),
    plot.caption = element_text(hjust = 0, size = 8)
  ) +
  labs(
    x = "",
    y = "",
    title = "Count of the top 10 most common words in The Last of Us Part I reviews",
    caption = "Source: Data gathered from sites that published their reviews on www.metacritics.com"
  )

ggsave("Plots/Most_common_words_unigrams.png", dpi = 600, width = 11, height = 7, bg = "white")

# Analysing bigrams
tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

# Clean the text. The clean_corpus was introduced earlier:
clean_corpus <- function(corpus) {
  corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
  corpus <- tm_map(corpus, removePunctuation)
  # corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"))) #, "...", "last", "part", "joel", "naughty", "like", "also", "will", "games", "ellie", "playstation"))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Clean corpus
critics_clean <- clean_corpus(critics_vector)


# Create bigram_tdm
bigram_tdm <- TermDocumentMatrix(
  critics_clean, 
  control = list(tokenize = tokenizer)
)

bigram_tdm

bigram_m_sums <- as.matrix(bigram_tdm) %>% 
  rowSums() %>% 
  sort(decreasing = T)

# Create word, count data frame
df_critics_bi <- data_frame(
  word = bigram_m_sums %>% as.data.frame() %>% rownames(),
  count = bigram_m_sums %>% as.data.frame() %>% pull(.)
) %>% 
  # Worsd deletion
  filter(!word %in% "feels like") %>%
  filter(!word %in% str_match(word, "playstation.*|^part.*|joel.*|naughty.*|^last.*|us.*|s.*|game .*|^years.*|things.*|played.*"))

# Plot most common bigrams
ggplot(df_critics_bi[1:15,], aes(y = fct_reorder(word,count), x = count, group = word)) +
  geom_col(fill = "black", width = 0.1) +
  geom_point(fill = "black", size = 3.25) +
  scale_x_continuous(limits = c(0, max(df_critics_bi$count)* 1.03), expand = c(0, 0)) +
  theme_minimal() +
  theme(
    axis.text = element_text(color = "black", vjust = 0.4),
    panel.grid.major.y  = element_blank(),
    plot.caption = element_text(hjust = 0, size = 8)
  ) +
  labs(
    x = "",
    y = "",
    title = "Count of the top 10 most common bigrams in The Last of Us Part I reviews",
    caption = "Source: Data gathered from sites that published their reviews on www.metacritics.com"
  )

ggsave("Plots/Most_common_words_bigrams.png", dpi = 600, width = 11, height = 7, bg = "white")

# Finding association with a word 'remake'
# Clean the text. The clean_corpus was introduced earlier:
clean_corpus <- function(corpus) {
  corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Clean corpus
critics_clean <- clean_corpus(critics_vector)

# Create TDM from corpus
critics_tdm <- TermDocumentMatrix(critics_clean)

# Create associations with 'game' word 
associations <- findAssocs(critics_tdm, "game", 0.3)

# Extract only the strongest associations
associations_df <- tail(list_vect2df(associations)[, 2:3], n = 10)

# Plot word associations
ggplot(associations_df, aes(y = associations_df[, 1])) + 
  geom_point(
    aes(x = associations_df[, 2]), 
    data = associations_df, size = 3
    ) + 
  theme_minimal() +
  theme(
    axis.text = element_text(color = "black"),
    panel.grid.major.y  = element_line(color = "lightgrey"),
    panel.grid.minor.x = element_blank(),
    plot.subtitle = element_text(size = 10)
  ) +
  labs(
    x = "",
    y = "",
    title = "Word associations to 'game'",
    subtitle = "The closer to 1 the stronger the association"
  )

ggsave("Plots/Word_association_unigram.png", dpi = 600, width = 11, height = 7)


# Clean the text. The clean_corpus was introduced earlier:
clean_corpus <- function(corpus) {
  corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Clean corpus
critics_clean <- clean_corpus(critics_vector)

# Create bigram_tdm
bigram_tdm <- TermDocumentMatrix(
  critics_clean, 
  control = list(tokenize = tokenizer)
)

# Create associations with 'original game' bigram
associations <- findAssocs(bigram_tdm, "original game", 0.3)

# Extract only the strongest associations
associations_df <- tail(list_vect2df(associations)[, 2:3], n = 10)

# Plot word associations
ggplot(associations_df, aes(y = associations_df[, 1])) + 
  geom_point(
    aes(x = associations_df[, 2]), 
    data = associations_df, size = 3
    ) + 
  theme_minimal() +
  scale_x_continuous(
    breaks = seq(0.475, 0.6, 0.025), 
    limits = c(0.465, 0.61), 
    expand = c(0, 0)
    ) +
  theme(
    axis.text = element_text(color = "black"),
    panel.grid.major.y  = element_line(color = "lightgrey"),
    panel.grid.minor.x = element_blank(),
    plot.subtitle = element_text(size = 10)
  ) +
  labs(
    x = "",
    y = "",
    title = "Word associations to 'original game'",
    subtitle = "The closer to 1 the stronger the association"
  )

ggsave("Plots/Word_association_bigram.png", dpi = 600, width = 11, height = 7)

# Cluster dendogram
# Clean the text. The clean_corpus was introduced earlier:
clean_corpus <- function(corpus) {
  corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "...", "last", "part", "joel", "naughty", "like", "also", "will", "games", "ellie", "playstation"))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Clean corpus
critics_clean <- clean_corpus(critics_vector)

# Create bigram_tdm
bigram_tdm <- TermDocumentMatrix(
  critics_clean, 
  control = list(tokenize = tokenizer)
)

# Create bigram_sparse by removing sparse terms 
bigram_sparse <- removeSparseTerms(bigram_tdm, sparse = .8) # It means that those terms appear in 80% of the documents

# Create hc as a cluster of distance values
hc <- hclust(
  d = dist(bigram_sparse, method = "euclidean"), 
  method = "complete")

# Produce a plot of hc
png("Plots/Hierarchical_clustering_bigram.png", res = 600, width = 7, height = 7, unit = "in")
plot(hc)
dev.off()


# Topic creation
minimumFrequency <- 5 # Compute document term matrix with terms above or equal to 5

dtm_critics_clean <- DocumentTermMatrix(
  critics_clean, 
  control = list(
    bounds = list(
      global = c(minimumFrequency, Inf)
      )
    )
  )

# Dimensions of the matrix
dim(dtm_critics_clean)

# Create models with different number of topics
result <- FindTopicsNumber(
  dtm_critics_clean,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)

# Save the plot
png("Plots/Topic_number.png")
FindTopicsNumber_plot(result)
dev.off()

# Determine the number of topics
k <- 4

# Set random number generator seed
set.seed(99)

# Compute the LDA model
topicModel <- LDA(dtm_critics_clean, k, method="Gibbs", control=list(iter = 500, verbose = 25))

# Have a look a some of the results (posterior distributions)
tmResult <- posterior(topicModel)

# Check a format of the resulting object
attributes(tmResult)

# Topics are probability distributions over the entire vocabulary
beta <- tmResult$terms

# For every document we have a probability of its contained topics
theta <- tmResult$topics 

# Choose 5 most frequent terms from the topics
top_5_terms_per_topic <- terms(topicModel, 5)

# Make a name for those topics
topic_names <- apply(top_5_terms_per_topic, 2, paste, collapse=", ")

# Create a color palette for 4 topics
palette <- wesanderson::wes_palette("Darjeeling1", n = 4)

# Plot a theta distribution of topics over all reviews
theta %>% 
  set_colnames(topic_names) %>% 
  as.data.frame() %>% 
  pivot_longer(cols = 1:4, names_to = "topic", values_to = "value") %>% 
  ggplot(aes(x = value, y = topic, colour = topic)) +
    geom_violin() +
    geom_violin(aes(fill = topic), alpha = 0.2) +
    scale_color_manual(values = palette) +
    scale_fill_manual(values = palette) +
    scale_x_continuous(limits = c(0, 1)) +
    theme_minimal() +
    labs(
      x = "",
      y = "",
      title = "Distribution of topics in critics reviews of The Last of Us Part I"
    ) +
    theme(
      legend.position = "none",
      axis.text = element_text(color = "black"),
      panel.grid.major.y  = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0, size = 8)
    )

ggsave("Plots/Distribution_of_topics.png", dpi = 600, width = 15, height = 7, bg = "white")

# Load data on users reviews 
users_corpus <- read.csv("Processed_data/users_reviews.csv")


# Do the same process for user reviews

# Comparison of sites and user reviews