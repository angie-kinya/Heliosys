library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(tidytext)
library(stringr)
library(lubridate)
library(textclean)
library(tm)
library(SnowballC)
library(syuzhet)
library(DBI)
library(RSQLite)
library(httr)
library(jsonlite)
library(future)
library(promises)
library(later)

openai_key <- Sys.getenv("OPENAI_API_KEY")

# Helper functions
preprocess_text <- function(text) {
  if (is.na(text) || length(text) == 0) return("")
  
  text <- str_to_lower(text)
  text <- str_replace_all(text, "[^a-zA-Z\\s]", " ")
  text <- str_replace_all(text, "\\s+", " ")
  text <- str_trim(text)
  
  tokens <- unlist(strsplit(text, " "))
  tokens <- tokens[tokens != ""]
  tokens <- tokens[!tokens %in% stopwords("english")]
  
  return(paste(tokens, collapse = " "))
}

analyze_sentiment <- function(text) {
  if (is.na(text) || nchar(text) == 0) return(0)
  
  tryCatch({
    score <- get_sentiment(text, method = "afinn")
    return(tanh(score / 5))
  }, error = function(e) {
    return(0)
  })
}

detect_depression_indicators <- function(text) {
  if (is.na(text) || nchar(text) == 0) return(0)
  
  depression_words <- c("sad", "depressed", "hopeless", "worthless", "empty", 
                       "lonely", "tired", "exhausted", "numb", "dark", "heavy",
                       "suicide", "death", "end", "give up", "pointless")
  
  word_count <- str_count(text, "\\w+")
  if (word_count == 0) return(0)
  
  matches <- sum(str_detect(str_to_lower(text), paste(depression_words, collapse = "|")))
  return(min(matches / word_count * 5, 1))
}

generate_response <- function(message, depression_score, sentiment_score) {
  if (depression_score > 0.7) {
    responses <- c(
      "I'm concerned about what you're sharing. Have you considered speaking with a mental health professional?",
      "It sounds like you're going through a really difficult time. Would you like information about crisis resources?",
      "Your feelings are valid, and you don't have to face this alone. Professional support can be very helpful."
    )
  } else if (depression_score > 0.4) {
    responses <- c(
      "Thank you for sharing that with me. It takes courage to express difficult feelings.",
      "I hear that you're struggling. What usually helps you feel a bit better?",
      "It's okay to not be okay. Small steps forward can make a difference."
    )
  } else if (sentiment_score < -0.5) {
    responses <- c(
      "I can sense you're having a tough time. What's been on your mind lately?",
      "Sometimes talking about what's bothering us can help. I'm here to listen.",
      "It sounds like you're dealing with some challenges. How are you taking care of yourself?"
    )
  } else {
    responses <- c(
      "Thank you for sharing that with me. How are you feeling overall today?",
      "I appreciate you opening up. What's been going well for you recently?",
      "It's good to hear from you. What would you like to talk about?"
    )
  }
  
  return(sample(responses, 1))
}

init_database <- function() {
  con <- dbConnect(SQLite(), "heliosys.db")
  
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
  
  dbDisconnect(con)
}

# Initialize database
tryCatch(init_database(), error = function(e) cat("Warning: Could not initialize database\n"))

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Heliosys: Mental Health Assistant"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Chat", tabName = "chat", icon = icon("comments")),
      menuItem("Analytics", tabName = "analytics", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .content-wrapper, .main-header, .main-sidebar {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        }

        .main-header .navbar {
            background: rgba(255,255,255,0.1);
            backdrop-filter: blur(10px);
            border: none;
        }

        .main-header .navbar-brand {
            color: white !important;
            font-weight: 600;
        }

        .main-sidebar {
            background: rgba(255,255,255,0.05);
            backdrop-filter: blur(15px);
        }

        .sidebar-menu > li > a {
            color: rgba(255,255,255,0.9);
            border-left: 3px solid transparent;
            transition: all 0.3s ease;
        }

        .sidebar-menu > li > a:hover,
        .sidebar-menu > li.active > a {
            background: rgba(255,255,255,0.1);
            border-left-color: #fff;
            color: white;
        }

        .content-wrapper {
            background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
            min-height: 100vh;
        }

        .chat-container {
            height: 500px;
            overflow-y: auto;
            border: none;
            border-radius: 20px;
            padding: 20px;
            background: linear-gradient(145deg, #ffffff 0%, #f8f9fa 100%);
            box-shadow: 0 20px 40px rgba(0,0,0,0.1);
            display: flex;
            flex-direction: column;
            position: relative;
        }

        .chat-container::before {
            content: '';
            position: absolute;
            top: 0;
            left: 0;
            right: 0;
            height: 60px;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            border-radius: 20px 20px 0 0;
            z-index: 1;
        }

        .chat-header {
            position: relative;
            z-index: 2;
            text-align: center;
            margin-bottom: 20px;
            padding: 15px 0;
        }

        .chat-header h3 {
            color: white;
            margin: 0;
            font-weight: 300;
            font-size: 24px;
            text-shadow: 0 2px 4px rgba(0,0,0,0.2);
        }

        .chat-messages {
            flex: 1;
            padding: 10px 0;
            overflow-y: auto;
        }

        .user-message, .bot-message {
            display: flex;
            align-items: flex-end;
            margin: 15px 0;
            animation: slideIn 0.6s cubic-bezier(0.25, 0.46, 0.45, 0.94);
        }

        .user-message { 
            flex-direction: row-reverse; 
        }

        .bot-message { 
            flex-direction: row; 
        }

        .avatar {
            width: 45px;
            height: 45px;
            border-radius: 50%;
            margin: 0 12px;
            background-size: cover;
            background-position: center;
            box-shadow: 0 4px 12px rgba(0,0,0,0.15);
            border: 3px solid white;
            transition: transform 0.2s ease;
        }

        .avatar:hover {
            transform: scale(1.05);
        }

        .user-avatar {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            display: flex;
            align-items: center;
            justify-content: center;
            color: white;
            font-weight: bold;
            font-size: 18px;
        }

        .bot-avatar {
            background: linear-gradient(135deg, #ffecd2 0%, #fcb69f 100%);
            display: flex;
            align-items: center;
            justify-content: center;
            color: #333;
            font-weight: bold;
            font-size: 18px;
        }

        .bubble {
            padding: 16px 20px;
            border-radius: 25px;
            max-width: 75%;
            font-size: 16px;
            line-height: 1.5;
            position: relative;
            word-break: break-word;
            backdrop-filter: blur(10px);
            transition: all 0.3s ease;
        }

        .bubble:hover {
            transform: translateY(-2px);
            box-shadow: 0 8px 25px rgba(0,0,0,0.1);
        }

        .user-message .bubble {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            border-bottom-right-radius: 8px;
            box-shadow: 0 4px 15px rgba(102, 126, 234, 0.3);
        }

        .bot-message .bubble {
            background: rgba(255,255,255,0.9);
            color: #333;
            border-bottom-left-radius: 8px;
            box-shadow: 0 4px 15px rgba(0,0,0,0.1);
            border: 1px solid rgba(255,255,255,0.2);
        }

        .message-header {
            font-size: 12px;
            font-weight: 600;
            margin-bottom: 6px;
            opacity: 0.8;
            text-transform: uppercase;
            letter-spacing: 0.5px;
        }

        .chat-input-area {
            margin-top: 20px;
            position: relative;
        }

        .input-container {
            display: flex;
            gap: 10px;
            background: white;
            padding: 10px;
            border-radius: 50px;
            box-shadow: 0 4px 20px rgba(0,0,0,0.1);
            border: 2px solid transparent;
            transition: all 0.3s ease;
        }

        .input-container:focus-within {
            border-color: #667eea;
            box-shadow: 0 4px 25px rgba(102, 126, 234, 0.2);
        }

        #user_input {
            flex: 1;
            border: none;
            outline: none;
            background: transparent;
            font-size: 16px;
            padding: 8px 15px;
        }

        .send-btn {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            border: none;
            border-radius: 50px;
            padding: 10px 25px;
            color: white;
            font-weight: 600;
            cursor: pointer;
            transition: all 0.3s ease;
            box-shadow: 0 4px 15px rgba(102, 126, 234, 0.3);
        }

        .send-btn:hover {
            transform: translateY(-2px);
            box-shadow: 0 6px 20px rgba(102, 126, 234, 0.4);
        }

        .typing-indicator {
            display: flex;
            align-items: center;
            margin: 15px 0 15px 12px;
        }

        .typing-dot {
            width: 10px;
            height: 10px;
            margin: 0 3px;
            background: #667eea;
            border-radius: 50%;
            display: inline-block;
            animation: typing-bounce 1.4s infinite both;
        }

        .typing-dot:nth-child(2) { animation-delay: 0.2s; }
        .typing-dot:nth-child(3) { animation-delay: 0.4s; }

        .box {
            border-radius: 20px;
            box-shadow: 0 10px 30px rgba(0,0,0,0.1);
            border: none;
            overflow: hidden;
        }

        .box-header {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            border-radius: 20px 20px 0 0;
        }

        .insights-card {
            padding: 20px;
        }

        .insight-item {
            display: flex;
            justify-content: space-between;
            align-items: center;
            padding: 15px 0;
            border-bottom: 1px solid rgba(0,0,0,0.1);
            transition: all 0.3s ease;
        }

        .insight-item:hover {
            background: rgba(102, 126, 234, 0.05);
            margin: 0 -20px;
            padding-left: 20px;
            padding-right: 20px;
            border-radius: 10px;
        }

        .insight-item:last-child {
            border-bottom: none;
        }

        .insight-label {
            font-weight: 600;
            color: #555;
            font-size: 14px;
        }

        .insight-value {
            font-weight: 700;
            font-size: 16px;
            padding: 5px 12px;
            border-radius: 20px;
            background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        }

        .risk-high { 
            background: linear-gradient(135deg, #ff6b6b 0%, #ee5a52 100%);
            color: white;
        }

        .risk-medium { 
            background: linear-gradient(135deg, #feca57 0%, #ff9ff3 100%);
            color: white;
        }

        .risk-low { 
            background: linear-gradient(135deg, #48cab2 0%, #2dd4bf 100%);
            color: white;
        }

        @keyframes slideIn {
        from { 
            opacity: 0; 
            transform: translateY(20px) scale(0.95); 
        }
        to { 
            opacity: 1; 
            transform: translateY(0) scale(1); 
        }
        }

        @keyframes typing-bounce {
        0%, 80%, 100% { 
            transform: scale(0.8); 
            opacity: 0.5; 
        }
        40% { 
            transform: scale(1.2); 
            opacity: 1; 
        }
        }

        /* Scrollbar styling */
        .chat-messages::-webkit-scrollbar {
            width: 6px;
        }

        .chat-messages::-webkit-scrollbar-track {
            background: rgba(0,0,0,0.1);
            border-radius: 10px;
        }

        .chat-messages::-webkit-scrollbar-thumb {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            border-radius: 10px;
        }

        .chat-messages::-webkit-scrollbar-thumb:hover {
            background: linear-gradient(135deg, #764ba2 0%, #667eea 100%);
        }
      ")),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('setupJS', function(message) {
            $(document).on('keypress', '#user_input', function(e) {
            if(e.which === 13 && !e.shiftKey) {
                e.preventDefault();
                $('#send_btn').click();
            }
            });
            
            $(document).on('click', '#send_btn', function() {
            setTimeout(function() {
                $('#user_input').focus();
            }, 100);
            });
            
            $(document).ready(function() {
            $('#user_input').focus();
            });
        });
      ")),
    ),
    tabItems(
      tabItem(tabName = "chat",
        fluidRow(
          column(width = 8,
            box(
              title = NULL,
              status = "primary", 
              solidHeader = FALSE,
              width = NULL, 
              height = "600px",
              div(class = "chat-container",
                div(class = "chat-header",
                  h3("Chat with Heliosys")
                ),
                div(class = "chat-messages",
                  uiOutput("chat_display")
                ),
                div(class = "chat-input-area",
                    div(class = "input-container",
                        textInput("user_input", "", 
                                placeholder = "Share what's on your mind...", 
                                width = "100%"),
                        actionButton("send_btn", "💬 Send", class = "btn send-btn")
                    )
                )
              )
            )
          ),
          column(width = 4,
            box(
              title = "Session Insights", status = "info", solidHeader = TRUE,
              width = NULL,
              div(class = "insights-card",
                h4("Real-time Analysis", style = "margin-bottom: 20px; font-weight: 600;"),
                uiOutput("session_insights")
              )
            )
          )
        )
      ),
      tabItem(tabName = "analytics",
        fluidRow(
          column(width = 6,
            box(
              title = "Sentiment Trends", status = "primary", solidHeader = TRUE,
              width = NULL, height = "400px",
              plotlyOutput("sentiment_plot")
            )
          ),
          column(width = 6,
            box(
              title = "Depression Risk", status = "warning", solidHeader = TRUE,
              width = NULL, height = "400px",
              plotlyOutput("risk_plot")
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values
  conversation_history <- reactiveVal(data.frame(
    timestamp = as.POSIXct(character()),
    speaker = character(),
    message = character(),
    sentiment = numeric(),
    depression_score = numeric(),
    stringsAsFactors = FALSE
  ))

  bot_typing <- reactiveVal(FALSE)

  # Handle send button
  observeEvent(input$send_btn, {
    req(input$user_input)
    if (nchar(trimws(input$user_input)) == 0) return()

    user_message <- input$user_input
    timestamp <- Sys.time()

    # Process message
    processed_text <- preprocess_text(user_message)
    sentiment_score <- analyze_sentiment(processed_text)
    depression_score <- detect_depression_indicators(processed_text)

    # Add user message
    new_entry <- data.frame(
      timestamp = timestamp,
      speaker = "user",
      message = user_message,
      sentiment = sentiment_score,
      depression_score = depression_score,
      stringsAsFactors = FALSE
    )

    history <- rbind(conversation_history(), new_entry)
    conversation_history(history)

    # Show typing indicator
    bot_typing(TRUE)

    # Clear input
    updateTextInput(session, "user_input", value = "")

    # Capture values for async operation
    user_msg <- user_message
    dep_score <- depression_score
    sent_score <- sentiment_score

    # Show typing indicator briefly, then respond
    later::later(function() {
      bot_response <- generate_response(user_msg, dep_score, sent_score)
      session$onFlushed(function() {
        bot_entry <- data.frame(
          timestamp = Sys.time(),
          speaker = "bot",
          message = bot_response,
          sentiment = NA,
          depression_score = NA,
          stringsAsFactors = FALSE
        )
        current_history <- isolate(conversation_history())
        updated_history <- rbind(current_history, bot_entry)
        conversation_history(updated_history)
        bot_typing(FALSE)
      }, once = TRUE)
    }, delay = 0.1)
  })
  
  # UI initialization
  session$onFlushed(function() {
    session$sendCustomMessage("setupJS", "")
  }, once = TRUE)
  
  # Update chat display with typing indicator and auto-scroll
  output$chat_display <- renderUI({
    history <- conversation_history()
    tagList(
      create_chat_display(history),
      if (bot_typing()) div(class = "bot-message typing-indicator",
        div(class = "bot-avatar avatar"),
        div(class = "bubble",
          span(class = "typing-dot"),
          span(class = "typing-dot"),
          span(class = "typing-dot")
        )
      ),
      tags$script(HTML('setTimeout(function(){
        var chat = document.getElementsByClassName("chat-container")[0];
        if(chat){ chat.scrollTop = chat.scrollHeight; }
      }, 100);'))
    )
  })
  
  # Update create_chat_display to use avatars and modern bubbles
  create_chat_display <- function(history) {
    if (nrow(history) == 0) return(div("Start a conversation..."))
    
    messages <- lapply(1:nrow(history), function(i) {
      row <- history[i, ]
      is_user <- row$speaker == "user"
      class_name <- ifelse(is_user, "user-message", "bot-message")
      avatar_class <- ifelse(is_user, "avatar user-avatar", "avatar bot-avatar")
      speaker_label <- ifelse(is_user, "You", "Heliosys")
      
      div(class = class_name,
        div(class = avatar_class),
        div(
          class = "bubble",
          div(class = "message-header",
            span(speaker_label),
            span(style = "float: right; font-style: italic; margin-left: 10px;", 
                 format(row$timestamp, "%H:%M"))
          ),
          div(row$message)
        )
      )
    })
    
    do.call(tagList, messages)
  }
  
  # Initial chat display
#   output$chat_display <- renderUI({
#     create_chat_display(conversation_history())
#   })
  
  # Enhanced session insights with modern styling
  output$session_insights <- renderUI({
    history <- conversation_history()
    
    if (nrow(history) == 0) {
      return(div(
        class = "insight-item",
        div(class = "insight-label", "Status"),
        div(class = "insight-value", "Ready to chat")
      ))
    }
    
    user_messages <- history[history$speaker == "user", ]
    
    if (nrow(user_messages) > 0) {
      avg_sentiment <- mean(user_messages$sentiment, na.rm = TRUE)
      avg_depression <- mean(user_messages$depression_score, na.rm = TRUE)
      duration <- difftime(Sys.time(), min(history$timestamp), units = "mins")
      
      # Risk level styling
      risk_class <- if (avg_depression > 0.6) "risk-high" 
                   else if (avg_depression > 0.3) "risk-medium" 
                   else "risk-low"
      
      sentiment_class <- if (avg_sentiment > 0) "risk-low" 
                        else if (avg_sentiment > -0.3) "risk-medium" 
                        else "risk-high"
      
      tagList(
        div(class = "insight-item",
          div(class = "insight-label", "Messages Exchanged"),
          div(class = "insight-value", nrow(user_messages))
        ),
        div(class = "insight-item",
          div(class = "insight-label", "Sentiment Score"),
          div(class = paste("insight-value", sentiment_class), 
              paste0(round(avg_sentiment, 2), " ", if(avg_sentiment > 0) "😊" else if(avg_sentiment > -0.3) "😐" else "😔"))
        ),
        div(class = "insight-item",
          div(class = "insight-label", "Wellbeing Risk"),
          div(class = paste("insight-value", risk_class), 
              paste0(round(avg_depression * 100, 1), "%"))
        ),
        div(class = "insight-item",
          div(class = "insight-label", "Session Duration"),
          div(class = "insight-value", paste(round(as.numeric(duration), 1), "min"))
        )
      )
    } else {
      return(div(
        class = "insight-item",
        div(class = "insight-label", "Status"),
        div(class = "insight-value", "Listening...")
      ))
    }
  })
  
  # Analytics plots
  output$sentiment_plot <- renderPlotly({
    history <- conversation_history()
    if (nrow(history) == 0) return(NULL)
    
    user_messages <- history[history$speaker == "user", ]
    if (nrow(user_messages) == 0) return(NULL)
    
    plot_ly(
      data = user_messages,
      x = ~timestamp,
      y = ~sentiment,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "blue")
    ) %>%
      layout(
        title = "Sentiment Over Time",
        xaxis = list(title = "Time"),
        yaxis = list(title = "Sentiment Score")
      )
  })
  
  output$risk_plot <- renderPlotly({
    history <- conversation_history()
    if (nrow(history) == 0) return(NULL)
    
    user_messages <- history[history$speaker == "user", ]
    if (nrow(user_messages) == 0) return(NULL)
    
    plot_ly(
      data = user_messages,
      x = ~timestamp,
      y = ~depression_score,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "red")
    ) %>%
      layout(
        title = "Depression Risk Over Time",
        xaxis = list(title = "Time"),
        yaxis = list(title = "Depression Risk")
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)