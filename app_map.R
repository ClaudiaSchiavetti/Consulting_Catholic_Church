# Define the data file path and set it as your working directory.
#path_outputs <- "C:/Users/soffi/Documents/Consulting_Catholic_Church"
# Use /app when inside Docker; fall back to your Windows path locally
app_dir <- if (dir.exists("/app")) "/app" else "C:/Users/schia/Documents/GitHub/Consulting_Catholic_Church"

# Create an outputs folder (no working-dir changes)
outputs_dir <- file.path(app_dir, "outputs")
if (!dir.exists(outputs_dir)) dir.create(outputs_dir, recursive = TRUE)

# app.R (nella stessa cartella)
source("map_short.R", local = TRUE)   
source("ui.R",     local = TRUE)   # definisce 'ui'
source("server.R", local = TRUE)   # definisce 'server'


shinyApp(ui = ui, server = server)
