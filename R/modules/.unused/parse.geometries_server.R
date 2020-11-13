#' parse.geometries_server UI Function
#'
#' @description Module to define UI for manipulating the output dataset used
#'   throughout most of the app.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
parse.geometries_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    geos <- reactive(geo.list[[input$region_type]])

    return(geos)
  })
}

