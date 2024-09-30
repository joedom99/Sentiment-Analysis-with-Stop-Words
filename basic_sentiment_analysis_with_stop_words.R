# Basic sentiment analysis with stop words
# Load necessary libraries
library(tidytext)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(stringr)

# 1. Read in the yelp_reviews_bouchon_bakery_las_vegas.csv file
reviews_data <- read_csv("yelp_reviews_bouchon_bakery_las_vegas.csv")

# 2. Clean the data: remove rows without reviews and set NA for missing ratings
clean_reviews <- reviews_data %>%
  filter(!is.na(review)) %>%
  mutate(rating = ifelse(rating == "", NA, rating), id = row_number())

# 3. Calculate word counts with and without stop words and retain them
word_counts <- clean_reviews %>%
  mutate(word_count_with_stop = str_count(review, boundary("word"))) %>%
  unnest_tokens(word, review) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(id) %>%
  summarise(word_count_no_stop = n(), word_count_with_stop = first(word_count_with_stop)) %>%
  ungroup() %>%
  mutate(compression_rate = word_count_no_stop / word_count_with_stop)

# Function to perform sentiment analysis, keeping the review column intact
perform_sentiment_analysis <- function(data, remove_stop_words = FALSE) {
  tokenized_data <- if (remove_stop_words) {
    data %>%
      unnest_tokens(word, review) %>%
      anti_join(stop_words, by = "word")
  } else {
    data %>%
      unnest_tokens(word, review)
  }
  
  tokenized_data %>%
    distinct(word, .keep_all = TRUE) %>%
    inner_join(get_sentiments("bing"), by = "word", relationship = "many-to-many") %>%
    count(id, sentiment) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>%
    mutate(sentiment_score = positive - negative) %>%
    right_join(data, by = "id") %>%  # Keep the review column
    mutate(
      sentiment_rating = case_when(
        sentiment_score > 0 ~ "positive",
        sentiment_score < 0 ~ "negative",
        TRUE ~ "neutral"
      )
    )
}

# 4. Perform sentiment analysis with all words and without stop words
sentiment_data_all <- perform_sentiment_analysis(clean_reviews, remove_stop_words = FALSE) %>%
  select(id, review, sentiment_score_all = sentiment_score, sentiment_rating_all = sentiment_rating)

sentiment_data_no_stop <- perform_sentiment_analysis(clean_reviews, remove_stop_words = TRUE) %>%
  select(id, review, sentiment_score_no_stop = sentiment_score, sentiment_rating_no_stop = sentiment_rating)

# 5. Compare sentiment ratings and scores
comparison <- sentiment_data_all %>%
  left_join(sentiment_data_no_stop, by = c("id", "review")) %>%
  mutate(
    score_difference = sentiment_score_all - sentiment_score_no_stop,
    rating_consistency = ifelse(sentiment_rating_all == sentiment_rating_no_stop, "consistent", "inconsistent")
  )

# 6. Filter and display reviews with score differences
reviews_with_difference <- comparison %>%
  filter(!is.na(score_difference) & score_difference != 0)

if (nrow(reviews_with_difference) > 0) {
  print("Reviews with a score difference:")
  print(reviews_with_difference %>% select(id, review, score_difference, sentiment_score_all, sentiment_score_no_stop))
} else {
  print("No reviews with a score difference found.")
}

# 7. Visualize the score differences
reviews_with_difference <- reviews_with_difference %>%
  mutate(score_change_type = case_when(
    score_difference > 0 ~ "Increase",
    score_difference < 0 ~ "Decrease",
    TRUE ~ "No Change"
  ))

ggplot(reviews_with_difference, aes(x = score_change_type, fill = score_change_type)) +
  geom_bar() +
  labs(title = "Distribution of Sentiment Score Changes", x = "Type of Change", y = "Count") +
  scale_fill_manual(values = c("Increase" = "blue", "Decrease" = "red"))

# 8. Visualize the word count and compression rate
ggplot(word_counts, aes(x = word_count_with_stop, y = word_count_no_stop)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Word Count: With vs. Without Stop Words", x = "Word Count (With Stop Words)", y = "Word Count (No Stop Words)") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")

ggplot(word_counts, aes(x = compression_rate)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "white") +
  labs(title = "Compression Rate", x = "Compression Rate", y = "Count")

# 9. Print compression rate summary
compression_summary <- word_counts %>%
  summarise(avg_compression = mean(compression_rate, na.rm = TRUE),
            min_compression = min(compression_rate, na.rm = TRUE),
            max_compression = max(compression_rate, na.rm = TRUE))

print("Compression Rate Summary:")
print(compression_summary)
