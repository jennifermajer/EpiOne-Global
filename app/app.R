library(shiny)
source("../R/data_loader.R")
source("../R/utils_dhis2_api.R")

source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)