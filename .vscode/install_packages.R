# Core packages
core_packages <- c(
  "devtools",
  "remotes",
  "renv"
)

# Install core packages first
install.packages(core_packages, dependencies = TRUE)

main_packages <- c(
  # Shiny and UI
  "shiny",
  "shinydashboard",
  "shinyWidgets",
  "DT",
  "plotly",
  "htmlwidgets",
  
  # Data manipulation
  "dplyr",
  "tidyr",
  "stringr",
  "lubridate",
  "jsonlite",
  
  # NLP and text processing
  "tidytext",
  "textclean",
  "tm",
  "SnowballC",
  "wordcloud",
  "syuzhet",
  "quanteda",
  "spacyr",
  
  # Machine learning
  "caret",
  "randomForest",
  "e1071",
  "ROCR",
  "pROC",
  "glmnet",
  "xgboost",
  
  # Database
  "DBI",
  "RSQLite",
  "pool",
  
  # Utilities
  "httr",
  "config",
  "log4r",
  "golem",
  "testthat",
  "memoise"
)

# Install main packages
install.packages(main_packages, dependencies = TRUE)