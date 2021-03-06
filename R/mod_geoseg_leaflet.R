
# server module ----------------------------------------------------------------

#' mod_geoseg_leaflet server Function
#'
#' @description Module to define server-side leaflet rendering
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param to.map Dataset prepped to map. Maybe output of \code{mod_geoseg} module.
#' @param map.palette Leaflet fcn as colorFactor to interpolate to colors
#' @param proxy leaflet proxy object.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import sf leaflet
#' @export mod_geoseg_leaflet
mod_geoseg_leaflet <- function(id, gs.dat, show_CTs, gs.palette, proxy) {

  moduleServer(id, function(input, output, session) {

    observeEvent( list(gs.dat(), show_CTs()), {

      req(!is.null(gs.dat()))

      # If not showing CTs, clear CTs and set args for mapping larger areas.
      is_showing_cts <- !is.null(show_CTs())
      if( !is_showing_cts ) {
        #proxy %>% clearGroup("cts")

        to.map <- gs.dat() %>% st_set_crs(4326)
        fully_zoomed <- F
        opacities <- 0.6
        #grp.name <- "dat"
        map.pal <- gs.palette()

      } else if( is_showing_cts ) {
        #proxy %>% clearGroup("dat")

        to.map <- show_CTs() %>% st_set_crs(4326) # %>% st_transform(4326)
        fully_zoomed <- T
        #opacities <- appHelpers::col.to.opacity(to.map$pop.dens)
        #grp.name <- "cts"
        map.pal <- colorFactor(viridis::plasma(7),
                               domain = to.map$binned_x)
      }

      # whether to use population density to set layer opacity.
      opacity_from_pop.dens <- is_showing_cts

      # populate map -----------------------------------------------------------------
      tooltips <- make_tooltips(input, to.map, click2zoom_enabled = !fully_zoomed)

      # add legend
      proxy %>% add_legend( to.map, map.pal,
                            legend.title =
                              make_display_label(input,
                                                 just_outcome = is_showing_cts)
                            )

      # clear old shapes
      proxy %>% clearGroup("gs.dat")

      # add shapes
      proxy %>%  # iterative_choropleth_draw
        choropleth_draw(to.map, grp.name = "gs.dat",
                        tooltips = tooltips,
                        pal = map.pal,
                        opacity_from_pop.dens = opacity_from_pop.dens,
                        #fillOpacity = opacities,
                        color = "white",
                        weight = .5
        )
      # ?leaflet::addPolygons
      # for getting events from leaflet interaction:
      # https://rstudio.github.io/leaflet/shiny.html
      # i.e., input$MAPID_OBJCATEGORY_EVENTNAME
    })

    observeEvent(input$reset_to_defaults, {
      do.call('fitBounds',
              c(list(map = proxy), l48bbox))
    })

  })
}


# minimalist geoseg leaflet app ------------------------------------------------

leaflet.app <- function() {

  # ui ---------------------------------------------------------------------------
  ui <- fluidPage(
    mod_geoseg_ui("gs", selectables),
    leafletOutput("map", height = 560)
    #verbatimTextOutput("out")
  )

  # server -----------------------------------------------------------------
  server <- function(input, output, session) {

    # render base leaflet & define proxy -------------------------------------------
    output$map <- renderLeaflet({
      create_leaflet_base()
    })
    # set proxy
    prox <- leafletProxy("map")
    # set initial zoom (lower 48 states)
    do.call('fitBounds',
            c(list(map = prox), l48bbox))

    # call modules ------------------------------------------------------------

    # parse core input using geoseg module
    c(gs.out, gs.palette) %<-%
      mod_geoseg("gs" )

    # send to map using leaflet module
    mod_geoseg_leaflet("gs", gs.out,
                       show_CTs = reactiveVal(NULL),
                       gs.palette, prox)
  }

  shinyApp(ui, server)
}


# run (test) -----------------------------------------------------------------------
# leaflet.app()
