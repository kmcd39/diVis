
# server module ----------------------------------------------------------------

#' mod_div_overlay_server Function
#'
#' @description Module to define server-side leaflet rendering
#'
#' @param show_CTs reactive either NULL or a subset of census tracts
#' @param proxy a leaflet proxy object to add to map
#'
#' @noRd
#'
#' @import dblinkr
#' @export mod_div_overlay_server
mod_div_overlay_server <- function(id, show_CTs, proxy) {

  moduleServer(id, function(input, output, session) {

    # connect to overlay DB (or ensure divDat)  ----------------------------------

    # manage db connections
    con <- dblinkr::db.connect(
      Sys.getenv("PRINCETON_LOGIN"), #db.usr,
      Sys.getenv("PRINCETON_PW"), #db.pw,
      pool = F)

    sessionId <- as.integer(runif(1, 1, 100000))
    #output$sessionId <- renderText(paste0("Session id: ", sessionId))
    session$onSessionEnded(function() {
      DBI::dbDisconnect(con)
      cat(paste0("\nEnded: ", sessionId))
    })

    # update UI visibility & clear on zoom out --------------------------
    observeEvent(show_CTs(), ignoreNULL = F, {
      showing_CTs <- !is.null(show_CTs())

      # hide ui & clear div layer if zoomed out; show ui if zoomed in
      if( !showing_CTs ) {
        shinyjs::hide("all_div_ui")
        proxy %>% clearGroup("divs")
      } else if(showing_CTs)
        shinyjs::show("all_div_ui")
    })


    # lazy update div overlays -------------------------------------------
    div.overlays <- isolate(
      reactive({
        divs <- map(input$div.overlays,
                    ~dblinkr::persistent.query.division(con, show_CTs(),
                                                        paste0("divs.", .)))
        names(divs) <- input$div.overlays
        return(divs)
      }))

    # "render" button events -- call update ------------------------
    observeEvent(input$render.divs, {

      # clear old layers
      proxy %>% clearGroup("divs")

      divs <- div.overlays()

      map2(divs, names(divs),
          ~add.div_index.wrapper(proxy, .x, .y))
    })
  })
}


# ui module --------------------------------------------------------------------

#' mod_div_overlay_ui UI Function
#'
#' @description Module to define UI for manipulating the output dataset used
#'   throughout most of the app.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param div.opts Selectable options. In dif-fcns/mapping script for now.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @export mod_div_overlay_ui
mod_div_overlay_ui <- function(id, div.opts){
  ns <- NS(id)
  shinyjs::useShinyjs()

  tagList(
    # divisions --
    shinyjs::hidden(div(
      id = ns("all_div_ui"),
      fluidRow(
        # select divisions
        selectizeInput(inputId = ns("div.overlays"),
                       label = strong("Select overlay features "),
                       choices = div.opts,
                       multiple = T),
        # render divisions button
        actionButton(ns("render.divs"), "Render Selections",
                     width = "50%")
      ),
      #actionBttn(inputId = ns("aggregate2div.polys"), )

      # RESET TO DEFAULT
      fluidRow(
        actionButton(ns("clear.divs"), "Clear Selection",
                     width = "80%"))
    ))
  )
}


# helper fcns in processing-fcns/div-fcns/  ----------------------------------------------------------------------

#' get_sf_from.divDat
#'
#' Quick helper fcn that transforms string 'x' to 'divDat::x' and evaluates.
#' Not used if getting divs from database.
get_sf_from.divDat <- function(div) {
  eval(parse(text = paste0("divDat::",div)))
}





