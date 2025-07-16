# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(tidytext)
library(stringr)
library(lubricate)

# Source modules
source("modules/chat_module.R")
source("modules/nlp_module.R")
source("modules/depression_detector.R")
source("modules/dashboard_module.R")
source("utils/database.R")
source("utils/preprocessing.R")

# Initialize database
init_database()

# UI
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
        tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
        tags$script(src = "custom.js")
        ),
        
        tabItems(
        tabItem(tabName = "chat", chat_ui("chat")),
        tabItem(tabName = "analytics", analytics_ui("analytics")),
        tabItem(tabName = "risk", risk_ui("risk"))
        )
    )
)

# Server
server <- function(input, output, session) {
    # reactive values
    values <- reactiveValues(
        messages = data.frame(),
        user_profile = list(),
        session_data = list()
    )

    # call modules
    callModule(chat_server, "chat", values)
    callModule(analytics_server, "analytics", values)
    callModule(risk_server, "risk", values)
}

# Run the application
shinyApp(ui = ui, server = server)