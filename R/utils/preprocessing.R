library(tidytext)
library(textclean)
library(tm)
library(SnowballC)
library(stringr)

# Advanced text preprocessing
preprocess_text <- function(text) {
  if (is.na(text) || length(text) == 0) return("")
  
  # Handle contractions and internet language
  text <- textclean::replace_contraction(text)
  text <- textclean::replace_internet_slang(text)
  text <- textclean::replace_emoticon(text, replace = " EMOTICON ")
  text <- textclean::replace_emoji(text, replace = " EMOJI ")
  
  # Basic cleaning
  text <- str_to_lower(text)
  text <- str_replace_all(text, "[^a-zA-Z\\s]", " ")
  text <- str_replace_all(text, "\\s+", " ")
  text <- str_trim(text)
  
  # Tokenization and stemming
  tokens <- unlist(strsplit(text, " "))
  tokens <- tokens[tokens != ""]
  tokens <- tokens[!tokens %in% stopwords("english")]
  tokens <- wordStem(tokens, language = "english")
  
  return(paste(tokens, collapse = " "))
}

# Extract comprehensive features
extract_text_features <- function(text) {
  if (is.na(text) || length(text) == 0) {
    return(data.frame(
      word_count = 0, char_count = 0, avg_word_length = 0,
      sentiment_score = 0, depression_keywords = 0,
      negative_words = 0, positive_words = 0,
      question_marks = 0, exclamation_marks = 0,
      first_person_pronouns = 0, temporal_references = 0
    ))
  }
  
  # Basic metrics
  word_count <- str_count(text, "\\w+")
  char_count <- nchar(text)
  avg_word_length <- ifelse(word_count > 0, char_count / word_count, 0)
  
  # Sentiment analysis
  sentiment_score <- analyze_sentiment(text)
  
  # Depression keywords
  depression_lexicon <- load_depression_lexicon()
  depression_matches <- sum(str_detect(text, paste(depression_lexicon$word, collapse = "|")))
  
  # Positive/negative words
  positive_words <- sum(str_detect(text, "\\b(good|great|happy|joy|love|excellent|wonderful|amazing)\\b"))
  negative_words <- sum(str_detect(text, "\\b(bad|terrible|awful|hate|horrible|disgusting|worst)\\b"))
  
  # Punctuation patterns
  question_marks <- str_count(text, "\\?")
  exclamation_marks <- str_count(text, "!")
  
  # Linguistic patterns
  first_person_pronouns <- str_count(text, "\\b(i|me|my|myself|mine)\\b")
  temporal_references <- str_count(text, "\\b(never|always|forever|today|tomorrow|yesterday)\\b")
  
  return(data.frame(
    word_count = word_count,
    char_count = char_count,
    avg_word_length = avg_word_length,
    sentiment_score = sentiment_score,
    depression_keywords = depression_matches,
    negative_words = negative_words,
    positive_words = positive_words,
    question_marks = question_marks,
    exclamation_marks = exclamation_marks,
    first_person_pronouns = first_person_pronouns,
    temporal_references = temporal_references
  ))
}