# Define the data file path and set it as your working directory.
path_outputs <- "C:/Users/schia/Documents/GitHub/Consulting_Catholic_Church"
#path_outputs <- "C:\\Users\\soffi\\Desktop\\CONSULTING"
setwd(path_outputs)

# app.R (nella stessa cartella)
source("map_short.R", local = TRUE)   
source("ui.R",     local = TRUE)   # definisce 'ui'
source("server.R", local = TRUE)   # definisce 'server'


shinyApp(ui = ui, server = server)
