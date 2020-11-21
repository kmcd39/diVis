#' mod_gs_UI_control_flow server Function
#'
#' @description Module to wrap all other modules' UI. Built with server module
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_gs_UI_control_flow <- function(id) {

  moduleServer(id, function(input, output, session) {


    cat("control ui")


  })


}





# control-flow app -------------------------------------------------------------

ui.control.app <- function() {

  # ui ---------------------------------------------------------------------------
  # (uses ui for full app)

  # server -----------------------------------------------------------------------
  server <- function(input, output, session) {

    c(v, pal) %<-%
      mod_geoseg("gs")

    # output$out <- renderPrint(v())
    output$out <- DT::renderDataTable({
      req(!is.null(v()))
      v(select(tibble(v()), setdiff(colnames(v()), c("x", "geometry")), "geometry"))
      DT::datatable(v())
    })
  }
  shinyApp(gs_ui, server)
}


# run (test) --------------------------------------------------------------------

# ui.control.app()

