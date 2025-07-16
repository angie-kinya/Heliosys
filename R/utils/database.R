library(DBI)
library(RSQLite)

# Initialize database
init_database <- function() {
  con <- dbConnect(SQLite(), "heliosys.db")
  
  # Create tables
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS conversations (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      session_id TEXT,
      timestamp DATETIME,
      speaker TEXT,
      message TEXT,
      sentiment REAL,
      depression_score REAL,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS user_profiles (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      session_id TEXT UNIQUE,
      risk_level TEXT,
      total_messages INTEGER,
      avg_sentiment REAL,
      avg_depression_score REAL,
      last_interaction DATETIME,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  dbDisconnect(con)
}

# Store conversation
store_conversation <- function(conversation_df) {
  con <- dbConnect(SQLite(), "heliosys.db")
  
  # Add session ID
  conversation_df$session_id <- get_session_id()
  
  # Insert data
  dbWriteTable(con, "conversations", conversation_df, append = TRUE)
  
  # Update user profile
  update_user_profile(conversation_df)
  
  dbDisconnect(con)
}

# Get session ID
get_session_id <- function() {
  # Generate or retrieve session ID
  if (!exists("session_id", envir = .GlobalEnv)) {
    .GlobalEnv$session_id <- paste0("session_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }
  return(.GlobalEnv$session_id)
}

# Update user profile
update_user_profile <- function(conversation_df) {
  con <- dbConnect(SQLite(), "heliosys.db")
  
  session_id <- unique(conversation_df$session_id)[1]
  user_messages <- conversation_df[conversation_df$speaker == "user", ]
  
  if (nrow(user_messages) > 0) {
    profile_data <- data.frame(
      session_id = session_id,
      risk_level = assess_risk_level(conversation_df)$level,
      total_messages = nrow(user_messages),
      avg_sentiment = mean(user_messages$sentiment, na.rm = TRUE),
      avg_depression_score = mean(user_messages$depression_score, na.rm = TRUE),
      last_interaction = max(conversation_df$timestamp)
    )
    
    # Upsert profile
    dbExecute(con, "
      INSERT OR REPLACE INTO user_profiles 
      (session_id, risk_level, total_messages, avg_sentiment, avg_depression_score, last_interaction)
      VALUES (?, ?, ?, ?, ?, ?)
    ", params = list(
      profile_data$session_id,
      profile_data$risk_level,
      profile_data$total_messages,
      profile_data$avg_sentiment,
      profile_data$avg_depression_score,
      profile_data$last_interaction
    ))
  }
  
  dbDisconnect(con)
}