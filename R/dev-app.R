#rm(list=ls())

source("global.R")
source("gs_ui.R")
source("gs_server.R")

shinyApp(gs_ui, gs_server)
