FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev

# Install R packages
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'DT', 'plotly', 'tidytext', 'dplyr', 'stringr', 'lubridate', 'textclean', 'tm', 'SnowballC', 'wordcloud', 'syuzhet', 'caret', 'randomForest', 'e1071', 'ROCR', 'pROC', 'jsonlite', 'httr', 'DBI', 'RSQLite', 'golem', 'config', 'log4r'))"

# Copy app
COPY . /srv/shiny-server/heliosys/

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server/heliosys

# Expose port
EXPOSE 3838

# Run app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/heliosys/R/app.R', host='0.0.0.0', port=3838)"]