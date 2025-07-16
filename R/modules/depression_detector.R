library(caret)
library(randomForest)
library(e1071)
library(ROCR)2

# Main depression detection function
detect_depression_indicators <- function(text) {
  # Extract features
  features <- extract_features(text)
  
  # Load trained model
  model <- load_depression_model()
  
  # Predict depression score
  prediction <- predict(model, features, type = "prob")
  
  # Return probability of depression
  return(prediction[, "depression"])
}

# Train depression detection model
train_depression_model <- function() {
  # Load training data
  training_data <- load_training_data()
  
  # Prepare features
  features_list <- lapply(training_data$text, extract_features)
  features_df <- do.call(rbind, features_list)
  features_df$label <- training_data$label
  
  # Split data
  set.seed(42)
  train_index <- createDataPartition(features_df$label, p = 0.8, list = FALSE)
  train_data <- features_df[train_index, ]
  test_data <- features_df[-train_index, ]
  
  # Train Random Forest model
  rf_model <- randomForest(
    label ~ ., 
    data = train_data,
    ntree = 500,
    mtry = sqrt(ncol(train_data) - 1),
    importance = TRUE
  )
  
  # Evaluate model
  predictions <- predict(rf_model, test_data)
  accuracy <- mean(predictions == test_data$label)
  
  cat("Model Accuracy:", accuracy, "\n")
  
  # Save model
  saveRDS(rf_model, "models/depression_model.rds")
  
  return(rf_model)
}

# Load trained model
load_depression_model <- function() {
  if (file.exists("models/depression_model.rds")) {
    return(readRDS("models/depression_model.rds"))
  } else {
    # Train new model if doesn't exist
    return(train_depression_model())
  }
}

# Risk assessment
assess_risk_level <- function(conversation_history) {
  if (nrow(conversation_history) == 0) return(list(level = "low", score = 0))
  
  user_messages <- conversation_history[conversation_history$speaker == "user", ]
  
  # Calculate metrics
  avg_depression_score <- mean(user_messages$depression_score, na.rm = TRUE)
  avg_sentiment <- mean(user_messages$sentiment, na.rm = TRUE)
  trend_depression <- calculate_trend(user_messages$depression_score)
  
  # Determine risk level
  if (avg_depression_score > 0.7 || trend_depression > 0.3) {
    risk_level <- "high"
  } else if (avg_depression_score > 0.4 || trend_depression > 0.1) {
    risk_level <- "moderate"
  } else {
    risk_level <- "low"
  }
  
  return(list(
    level = risk_level,
    score = avg_depression_score,
    trend = trend_depression,
    sentiment = avg_sentiment
  ))
}

# Calculate trend
calculate_trend <- function(scores) {
  if (length(scores) < 2) return(0)
  
  # Simple linear regression slope
  x <- 1:length(scores)
  trend <- lm(scores ~ x)$coefficients[2]
  
  return(as.numeric(trend))
}