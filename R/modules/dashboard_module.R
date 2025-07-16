library(plotly)
library(DT)

# Dashboard UI
analytics_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(width = 6,
      box(
        title = "Sentiment Trends", status = "primary", solidHeader = TRUE,
        width = NULL, height = "400px",
        plotlyOutput(ns("sentiment_plot"))
      )
    ),
    column(width = 6,
      box(
        title = "Depression Risk Levels", status = "warning", solidHeader = TRUE,
        width = NULL, height = "400px",
        plotlyOutput(ns("risk_plot"))
      )
    )
  ),
  fluidRow(
    column(width = 12,
      box(
        title = "Conversation History", status = "info", solidHeader = TRUE,
        width = NULL,
        DTOutput(ns("conversation_table"))
      )
    )
  )
}

 Risk Assessment UI
risk_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(width = 4,
      valueBoxOutput(ns("risk_level"), width = NULL)
    ),
    column(width = 4,
      valueBoxOutput(ns("avg_sentiment"), width = NULL)
    ),
    column(width = 4,
      valueBoxOutput(ns("message_count"), width = NULL)
    )
  ),
  fluidRow(
    column(width = 12,
      box(
        title = "Risk Assessment Details", status = "danger", solidHeader = TRUE,
        width = NULL,
        verbatimTextOutput(ns("risk_details"))
      )
    )
  )
}

 Analytics Server
analytics_server <- function(input, output, session, values) {
  # Sentiment trend plot
  output$sentiment_plot <- renderPlotly({
    if (nrow(values$messages) == 0) return(NULL)
    
    user_messages <- values$messages[values$messages$speaker == "user", ]
    if (nrow(user_messages) == 0) return(NULL)
    
    p <- plot_ly(
      data = user_messages,
      x = ~timestamp,
      y = ~sentiment,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "blue"),
      marker = list(color = "blue")
    ) %>%
      layout(
        title = "Sentiment Over Time",
        xaxis = list(title = "Time"),
        yaxis = list(title = "Sentiment Score")
      )
    
    return(p)
  })
  
  # Risk level plot
  output$risk_plot <- renderPlotly({
    if (nrow(values$messages) == 0) return(NULL)
    
    user_messages <- values$messages[values$messages$speaker == "user", ]
    if (nrow(user_messages) == 0) return(NULL)
    
    p <- plot_ly(
      data = user_messages,
      x = ~timestamp,
      y = ~depression_score,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "red"),
      marker = list(color = "red")
    ) %>%
      layout(
        title = "Depression Risk Over Time",
        xaxis = list(title = "Time"),
        yaxis = list(title = "Depression Score")
      )
    
    return(p)
  })
  
  # Conversation table
  output$conversation_table <- renderDT({
    if (nrow(values$messages) == 0) return(NULL)
    
    display_data <- values$messages[, c("timestamp", "speaker", "message", "sentiment", "depression_score")]
    display_data$timestamp <- format(display_data$timestamp, "%Y-%m-%d %H:%M:%S")
    display_data$sentiment <- round(display_data$sentiment, 3)
    display_data$depression_score <- round(display_data$depression_score, 3)
    
    datatable(display_data, options = list(pageLength = 10, scrollX = TRUE))
  })
}

# Risk Assessment Server
risk_server <- function(input, output, session, values) {
  # Risk level value box
  output$risk_level <- renderValueBox({
    risk_assessment <- assess_risk_level(values$messages)
    
    color <- switch(risk_assessment$level,
      "low" = "green",
      "moderate" = "yellow",
      "high" = "red"
    )
    
    valueBox(
      value = toupper(risk_assessment$level),
      subtitle = "Risk Level",
      color = color,
      icon = icon("exclamation-triangle")
    )
  })
  
  # Average sentiment value box
  output$avg_sentiment <- renderValueBox({
    if (nrow(values$messages) == 0) {
      sentiment_val <- 0
    } else {
      user_messages <- values$messages[values$messages$speaker == "user", ]
      sentiment_val <- mean(user_messages$sentiment, na.rm = TRUE)
    }
    
    valueBox(
      value = round(sentiment_val, 2),
      subtitle = "Average Sentiment",
      color = if (sentiment_val > 0) "green" else "red",
      icon = icon("smile")
    )
  })
  
  # Message count value box
  output$message_count <- renderValueBox({
    if (nrow(values$messages) == 0) {
      count <- 0
    } else {
      count <- nrow(values$messages[values$messages$speaker == "user", ])
    }
    
    valueBox(
      value = count,
      subtitle = "Messages Sent",
      color = "blue",
      icon = icon("comments")
    )
  })
  
  # Risk details
  output$risk_details <- renderText({
    risk_assessment <- assess_risk_level(values$messages)
    
    paste0(
      "Risk Assessment Summary:\n",
      "- Overall Risk Level: ", risk_assessment$level, "\n",
      "- Risk Score: ", round(risk_assessment$score, 3), "\n",
      "- Trend: ", round(risk_assessment$trend, 3), "\n",
      "- Average Sentiment: ", round(risk_assessment$sentiment, 3), "\n\n",
      "Recommendations:\n",
      if (risk_assessment$level == "high") {
        "- Consider immediate professional consultation\n- Monitor closely\n- Provide crisis resources"
      } else if (risk_assessment$level == "moderate") {
        "- Continue supportive conversation\n- Encourage professional help\n- Monitor for changes"
      } else {
        "- Maintain supportive dialogue\n- Regular check-ins\n- Positive reinforcement"
      }
    )
  })
}