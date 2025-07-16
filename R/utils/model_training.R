library(caret)
library(randomForest)
library(e1071)
library(ROCR)
library(pROC)

# Main training function
train_all_models <- function() {
  cat("Starting model training process...\n")
  
  # Load and prepare data
  training_data <- load_training_data()
  if (nrow(training_data) == 0) {
    stop("No training data found. Please ensure training_data.csv exists.")
  }
  
  # Train depression model
  depression_model <- train_depression_model(training_data)
  
  # Train sentiment model
  sentiment_model <- train_sentiment_model(training_data)
  
  cat("Model training completed successfully!\n")
  return(list(depression = depression_model, sentiment = sentiment_model))
}

# Train depression detection model
train_depression_model <- function(training_data = NULL) {
  if (is.null(training_data)) {
    training_data <- load_training_data()
  }
  
  cat("Training depression detection model...\n")
  
  # Extract features for all texts
  features_list <- lapply(training_data$text, extract_text_features)
  features_df <- do.call(rbind, features_list)
  features_df$label <- as.factor(training_data$depression_label)
  
  # Remove rows with missing values
  features_df <- features_df[complete.cases(features_df), ]
  
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
    importance = TRUE,
    proximity = TRUE
  )
  
  # Evaluate model
  predictions <- predict(rf_model, test_data)
  accuracy <- mean(predictions == test_data$label)
  
  # ROC analysis
  pred_probs <- predict(rf_model, test_data, type = "prob")
  roc_obj <- roc(test_data$label, pred_probs[, 2])
  auc_score <- auc(roc_obj)
  
  cat("Depression Model Performance:\n")
  cat("Accuracy:", round(accuracy, 4), "\n")
  cat("AUC:", round(auc_score, 4), "\n")
  
  # Save model
  if (!dir.exists("models")) dir.create("models")
  saveRDS(rf_model, "models/depression_model.rds")
  
  return(rf_model)
}

# Train sentiment analysis model
train_sentiment_model <- function(training_data = NULL) {
  if (is.null(training_data)) {
    training_data <- load_training_data()
  }
  
  cat("Training sentiment analysis model...\n")
  
  # Filter data with sentiment labels
  sentiment_data <- training_data[!is.na(training_data$sentiment_score), ]
  
  if (nrow(sentiment_data) == 0) {
    cat("No sentiment data found, using rule-based sentiment analysis.\n")
    return(NULL)
  }
  
  # Extract features
  features_list <- lapply(sentiment_data$text, extract_text_features)
  features_df <- do.call(rbind, features_list)
  features_df$sentiment <- sentiment_data$sentiment_score
  
  # Split data
  set.seed(42)
  train_index <- createDataPartition(features_df$sentiment, p = 0.8, list = FALSE)
  train_data <- features_df[train_index, ]
  test_data <- features_df[-train_index, ]
  
  # Train SVM model for sentiment
  svm_model <- svm(
    sentiment ~ .,
    data = train_data,
    kernel = "radial",
    cost = 1,
    gamma = 0.1
  )
  
  # Evaluate model
  predictions <- predict(svm_model, test_data)
  rmse <- sqrt(mean((predictions - test_data$sentiment)^2))
  
  cat("Sentiment Model RMSE:", round(rmse, 4), "\n")
  
  # Save model
  saveRDS(svm_model, "models/sentiment_model.rds")
  
  return(svm_model)
}

# Load training data
load_training_data <- function() {
  if (file.exists("R/data/training_data.csv")) {
    data <- read.csv("R/data/training_data.csv", stringsAsFactors = FALSE)
    return(data)
  } else {
    # Create sample data if doesn't exist
    create_sample_training_data()
    return(read.csv("R/data/training_data.csv", stringsAsFactors = FALSE))
  }
}