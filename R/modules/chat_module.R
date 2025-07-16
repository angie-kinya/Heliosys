# chat UI
chat_ui <- function(id) {
    ns <- NS(id)

    fluidRow(
        column(width = 8,
            box(
                title = "Chat with Heliosys", status = "primary", solidHeader = TRUE,
                width = NULL, height = "500px",
                div(id = ns("chat_container"), class = "chat_container"),
                fluidRow(
                    column(width = 10,
                        textInput(ns("user_input"), "", placeholder = "Type your message here...")
                    ),
                    column(width = 2,
                        actionButton(ns("send_btn"), "Send", class = "btn-primary")
                    )
                )
            )
        ),
        column(width = 4,
            box(
                title = "Session Insights", status = "info", solidHeader = TRUE,
                width = NULL,
                verbatimTextOutput(ns("session_summary"))
            )
        )
    )
}

# Chat Server
chat_server <- function(input, output, session, values) {
  ns <- session$ns
  
  # Initialize conversation
  conversation_history <- reactiveVal(data.frame(
    timestamp = as.POSIXct(character()),
    speaker = character(),
    message = character(),
    sentiment = numeric(),
    depression_score = numeric(),
    stringsAsFactors = FALSE
  ))

  # Handle user input
  observeEvent(input$send_btn, {
    req(input$user_input)
    
    # Process user message
    user_message <- input$user_input
    timestamp <- Sys.time()
    
    # NLP Analysis
    processed_text <- preprocess_text(user_message)
    sentiment_score <- analyze_sentiment(processed_text)
    depression_score <- detect_depression_indicators(processed_text)
    
    # Add to conversation history
    new_entry <- data.frame(
      timestamp = timestamp,
      speaker = "user",
      message = user_message,
      sentiment = sentiment_score,
      depression_score = depression_score,
      stringsAsFactors = FALSE
    )
    
    history <- rbind(conversation_history(), new_entry)
    
    # Generate bot response
    bot_response <- generate_response(user_message, depression_score, sentiment_score)
    
    bot_entry <- data.frame(
      timestamp = timestamp + 1,
      speaker = "bot",
      message = bot_response,
      sentiment = NA,
      depression_score = NA,
      stringsAsFactors = FALSE
    )
    
    history <- rbind(history, bot_entry)
    conversation_history(history)
    
    # Store in database
    store_conversation(history)

    # Store in database
    store_conversation(history)
    
    # Update UI
    updateTextInput(session, "user_input", value = "")
    
    # Update chat display
    output$chat_display <- renderUI({
      create_chat_display(history)
    })
  })
  
  # Session summary
  output$session_summary <- renderText({
    history <- conversation_history()
    if (nrow(history) > 0) {
      user_messages <- history[history$speaker == "user", ]
      avg_sentiment <- mean(user_messages$sentiment, na.rm = TRUE)
      avg_depression <- mean(user_messages$depression_score, na.rm = TRUE)
      
      paste0(
        "Messages: ", nrow(user_messages), "\n",
        "Avg Sentiment: ", round(avg_sentiment, 2), "\n",
        "Depression Risk: ", round(avg_depression * 100, 1), "%\n",
        "Session Duration: ", format(Sys.time() - min(history$timestamp), digits = 2)
      )
    } else {
      "No messages yet"
    }
  })
}

# Helper functions
create_chat_display <- function(history) {
  if (nrow(history) == 0) return(NULL)
  
  messages <- lapply(1:nrow(history), function(i) {
    row <- history[i, ]
    class_name <- ifelse(row$speaker == "user", "user-message", "bot-message")
    
    div(class = class_name,
      div(class = "message-header",
        span(class = "speaker", ifelse(row$speaker == "user", "You", "Heliosys")),
        span(class = "timestamp", format(row$timestamp, "%H:%M"))
      ),
      div(class = "message-content", row$message)
    )
  })
  
  do.call(tagList, messages)
}

generate_response <- function(message, depression_score, sentiment_score) {
  # Load response templates
  responses <- get_response_templates()
  
  # Determine response type based on scores
  if (depression_score > 0.7) {
    response_type <- "high_risk"
  } else if (depression_score > 0.4) {
    response_type <- "moderate_risk"
  } else if (sentiment_score < -0.5) {
    response_type <- "negative_sentiment"
  } else {
    response_type <- "supportive"
  }
  
  # Select appropriate response
  template <- sample(responses[[response_type]], 1)
  
  # Personalize response
  personalized_response <- personalize_response(template, message)
  
  return(personalized_response)
}