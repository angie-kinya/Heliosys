# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(tidytext)
library(stringr)
library(lubridate)  # Fixed typo

# Source modules (with error handling)
tryCatch(source("modules/chat_module.R"), error = function(e) cat("Warning: chat_module.R not found\n"))
tryCatch(source("modules/nlp_module.R"), error = function(e) cat("Warning: nlp_module.R not found\n"))
tryCatch(source("modules/depression_detector.R"), error = function(e) cat("Warning: depression_detector.R not found\n"))
tryCatch(source("modules/dashboard_module.R"), error = function(e) cat("Warning: dashboard_module.R not found\n"))
tryCatch(source("utils/database.R"), error = function(e) cat("Warning: database.R not found\n"))
tryCatch(source("utils/preprocessing.R"), error = function(e) cat("Warning: preprocessing.R not found\n"))

# Initialize database (with error handling)
tryCatch(init_database(), error = function(e) cat("Warning: Could not initialize database\n"))

# Minimal UI for testing
ui <- dashboardPage(
    dashboardHeader(title = "Heliosys: Mental Health Assistant"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Chat", tabName = "chat", icon = icon("comments")),
            menuItem("Analytics", tabName = "analytics", icon = icon("chart-line")),
            menuItem("Risk Assessment", tabName = "risk_assessment", icon = icon("exclamation-triangle"))
        )
    ),
    dashboardBody(
        h1("Heliosys is loading..."),
        p("If you see this message, the basic app structure is working.")
    )
)

# Minimal server for testing
server <- function(input, output, session) {
    # Basic reactive values
    values <- reactiveValues(
        messages = data.frame(),
        user_profile = list(),
        session_data = list()
    )
}

# Run the application
shinyApp(ui = ui, server = server)