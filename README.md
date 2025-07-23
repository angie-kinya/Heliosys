# Heliosys
This project is of an AI-powered mental health chatbot that detects early signs of depression through natural language processing and conversational analysis.

The app is a simple chat tool that listens to what you say and tries to understand your feelings. It looks for signs of sadness or depression in your words and gives you friendly replies. You can also see some charts that show how your mood changes over time.

I kept things simple by not using some extra modules in the folders because they caused some build problems. But you can still use those modules if you want to try.

Before you run the app, you need to install some R packages. It's best to install them from a CRAN mirror. You can do this by running:

```bash
install.packages(c("shiny", "shinyjs", "shinydashboard", "DT", "plotly", "dplyr", "tidytext", "stringr", "lubridate", "textclean", "tm", "SnowballC", "syuzhet", "DBI", "RSQLite", "httr", "jsonlite", "future", "promises", "later"), repos = "https://cran.r-project.org")
```

After installing the packages, you can start the app by running:

```bash
Rscript -e "shiny::runApp('R/app.R', host='0.0.0.0', port=3838)"
```

**DISCLAIMER: This is a research project and not a professional mental health tool. If you are experiencing signs of depression or other mental health issues, please seek help from a qualified professional. This app is not a substitute for professional help.**

**Remember to take care of yourself each day!** ♥

That's it! Just chat and see how the app responds and tracks your mood. 

**UPCOMING CHANGE:** Running docker image for deployment and live demo link for easy access. More versions of the app will be released with new features and improvements too.
