
# server module ----------------------------------------------------------------

#' mod_div_overlay_server Function
#'
#' @description Module to define server-side leaflet rendering
#'
#'
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import sf leaflet
mod_div_overlay_server <- function(id, show_CTs, proxy) {

  moduleServer(id, function(input, output, session) {

    # connect to overlay DB (or just use divDat)

    # update UI visibility
    observeEvent(show_CTs(), {
      showing_CTs <- !is.null(show_CTs())

      # hide if zoomed out; show if zoomed in
      if( !showing_CTs )
          updateSelectizeInput(xxx)
      else if(showing_CTs)
        updateSelectizeInput(xxx)
      })

    # update div Overlays


    })
}



# ui module --------------------------------------------------------------------
div.opts = data(package="divDat")$result[,"Item"]
div.opts = div.opts[!grepl("(cz|count|cbsa|cts)", div.opts)]
#' mod_div_overlay_ui UI Function
#'
#' @description Module to define UI for manipulating the output dataset used
#'   throughout most of the app.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param div.opts Selectable options
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_div_overlay_ui <- function(id, div.opts){
  ns <- NS(id)

  tagList(

    # divisions --
    fluidRow(
    selectizeInput(inputId = ns("div.overlay"),
                   label = strong("Select overlay features "),
                   choices = c("none",
                               div.opts),
                   multiple = T),
    actionButton(ns("render.divs"), "Render Selections",
                 width = "80%")
    ),
    #actionBttn(inputId = ns("aggregate2div.polys"), )

    # RESET TO DEFAULT
    fluidRow(height = "10px", HTML("<br>")),
    actionButton(ns("clear.divs"), "Clear Selection",
                 width = "80%")
  )
}

