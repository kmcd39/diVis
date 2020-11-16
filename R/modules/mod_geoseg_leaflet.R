
# helpers ----------------------------------------------------------------------

#' choropleth_wrapper
#'
#' Wraps \code{iterative_choropleth_draw} with appropriate arguments based on whether
#' CTs or larger regions are being mapped. Assumes relevant datasets are present in
#' the environment; should only be called from module below.
#choropleth_wrapper <- function() {}


# server module ----------------------------------------------------------------

#' mod_geoseg_leaflet server Function
#'
#' @description Module to define server-side leaflet rendering
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param to.map Dataset prepped to map. Output of \code{mod_geoseg} module.
#' @param map.palette Leaflet fcn as colorFactor to interpolate to colors
#' @param proxy leaflet proxy object.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import sf leaflet
mod_geoseg_leaflet <- function(id, gs.dat, show_CTs, map.palette, proxy) {

  moduleServer(id,
               function(input, output, session) {

                 observeEvent( list(gs.dat(), show_CTs()), {

                   req(!is.null(gs.dat()))

                   browser()
                   # If not showing CTs, clear CTs and set params to map larger areas.
                   if( is.null(show_CTs()) ) {
                     proxy %>% clearGroup("cts")

                     to.map <- gs.dat()
                     fully_zoomed <- F
                     opacities <- 0.6
                     grp.name <- "dat"

                   } else if( !is.null(show_CTs()) ) {
                     proxy %>% clearGroup("dat")

                     to.map <- show_CTs()
                     fully_zoomed <- T
                     opacities <- appHelpers::col.to.opacity(to.map$population)
                     grp.name <- "cts"

                   }

                   # populate map -----------------------------------------------------------------
                   tooltips <- make_tooltips(input, to.map, click2zoom_enabled = !fully_zoomed)
                   #appHelpers::add_legend( to.map, map.palette(), legend.title = make_display_label(input) )

                   # browser()
                   proxy %>%
                     iterative_choropleth_draw(to.map, grp.name,
                                               tooltips,
                                               pal = map.palette(),
                                               fillOpacity = opacities,
                                               weight = 1.1,
                                               color = "white",
                                               smoothFactor = 1.1)
                   # ?leaflet::addPolygons
                   # getting events from leaflet interaction:
                   # https://rstudio.github.io/leaflet/shiny.html
                   # i.e., input$MAPID_OBJCATEGORY_EVENTNAME
                 })
               })
}



# minimalist geoseg leaflet app ------------------------------------------------

leaflet.app <- function() {

  # ui ---------------------------------------------------------------------------
  ui <- fluidPage(
    geoseg_ui("gs", selectables),
    leafletOutput("map", height = 560)
    #verbatimTextOutput("out")
  )

  # server -----------------------------------------------------------------
  server <- function(input, output, session) {

    # render base leaflet & define proxy -------------------------------------------
    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Voyager,
                         options = providerTileOptions(noWrap = TRUE))
    })
    # set proxy
    prox <- leafletProxy("map")
    # set initial zoom (lower 48 states)
    do.call('fitBounds',
            c(list(map = prox), l48bbox))

    # call modules ------------------------------------------------------------

    # parse core input using geoseg module
    c(gs.out, gs.palette) %<-%
      geoseg_server("gs" )#, gs.out, gs.palette)

    # send to map using leaflet module
    mod_geoseg_leaflet("gs", gs.out, show_CTs = reactiveVal(NULL), gs.palette, prox)
  }

  shinyApp(ui, server)
}


# launch -----------------------------------------------------------------------
leaflet.app()
