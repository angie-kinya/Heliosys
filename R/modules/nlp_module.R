library(tidytext)
library(textclean)
library(tm)
library(SnowballC)
library(syuzhet)

# Text preprocessing
preprocess_text <- function(text) {
  # Clean text
  text <- textclean::replace_contraction(text)
  text <- textclean::replace_emoticon(text)
  text <- textclean::replace_internet_slang(text)
  text <- str_to_lower(text)
  text <- str_replace_all(text, "[^a-zA-Z\\s]", "")
  text <- str_replace_all(text, "\\s+", " ")
  text <- str_trim(text)
  
  # Tokenization
  tokens <- unlist(strsplit(text, " "))
  
  # Remove stopwords
  tokens <- tokens[!tokens %in% stopwords("english")]
  
  # Stemming
  tokens <- wordStem(tokens, language = "english")
  
  return(paste(tokens, collapse = " "))
}

# Sentiment analysis
analyze_sentiment <- function(text) {
  # Multiple sentiment methods
  afinn_score <- get_sentiment(text, method = "afinn")
  bing_score <- get_sentiment(text, method = "bing")
  nrc_score <- get_sentiment(text, method = "nrc")
  
  # Combine scores
  combined_score <- (afinn_score + bing_score + nrc_score) / 3
  
  # Normalize to [-1, 1]
  normalized_score <- tanh(combined_score / 5)
  
  return(normalized_score)
}

# Feature extraction for depression detection
extract_features <- function(text) {
  # Linguistic features
  word_count <- str_count(text, "\\w+")
  char_count <- nchar(text)
  avg_word_length <- char_count / word_count
  
  # Sentiment features
  sentiment_score <- analyze_sentiment(text)
  
  # Depression-specific features
  depression_keywords <- load_depression_lexicon()
  depression_matches <- sum(str_detect(text, paste(depression_keywords$word, collapse = "|")))
  
  # Emotional features
  emotions <- get_nrc_sentiment(text)
  
  # Combine features
  features <- data.frame(
    word_count = word_count,
    char_count = char_count,
    avg_word_length = avg_word_length,
    sentiment_score = sentiment_score,
    depression_keywords = depression_matches,
    anger = emotions$anger,
    fear = emotions$fear,
    sadness = emotions$sadness,
    joy = emotions$joy,
    trust = emotions$trust,
    stringsAsFactors = FALSE
  )
  
  return(features)
}

# Load depression lexicon
load_depression_lexicon <- function() {
  # Create or load depression-specific keywords
  depression_words <- data.frame(
    word = c("sad", "depressed", "hopeless", "worthless", "empty", "lonely", 
             "tired", "exhausted", "numb", "dark", "heavy", "burden",
             "suicide", "death", "end", "give up", "pointless", "meaningless"),
    weight = c(0.8, 0.9, 0.85, 0.8, 0.7, 0.75, 0.6, 0.65, 0.7, 0.6, 0.6, 0.7,
               1.0, 0.9, 0.8, 0.8, 0.7, 0.7),
    stringsAsFactors = FALSE
  )
  
  return(depression_words)
}