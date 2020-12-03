
# helper -----------------------------------------------------------------------


#' click.hover_2region
#'
#' Translates a click or hover on a leaflet polygon to the row
#' representing the clicked region.
#' @param cursor_data list as returned from input$map_shape_click or hover.
#' @param map.layer sf object from leaflet map.
#' @export
click.hover_2region <- function(cursor_data, map.layer){

  if(is.null(cursor_data) || is.null(map.layer))
    return(NULL)

  interaction.point <- st_sfc(
    st_point(c(cursor_data$lng,
               cursor_data$lat))
    , crs = 4326)

  sbgp <- suppressMessages( st_intersects(map.layer
                                          ,interaction.point) )

  out <- map.layer[lengths(sbgp) > 0, ]

  if(nrow(out) == 0)
    return(NULL)
  else
    return(out)
}



# server module ----------------------------------------------------------------

#' mod_parse_leaflet.interaction server Function
#'
#' @description Parses leaflet interaction to get clicked region (to zoom into) and
#'   clears clicked region when zoomed out. Returns a 1 row dataframe representing
#'   the clicked region, to pass on to other modules.
#'
#' @inheritParams mod_geoseg_leaflet
#' @param leaflet_interaction reactiveValues containing zoom
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import sf leaflet
mod_parse_leaflet.interaction <- function(id,
                                          gs.out, show_CTs,
                                          leaflet_interaction,
                                          selection.reactive, # = reactiveVal(NULL),
                                          proxy) {

  moduleServer(id, function(input, output, session) {

    # module params
    minimum_ct_zoom <- 8

    # zoom observer ----------------------------------------------------------------
    observeEvent( leaflet_interaction$zoom_level, {
      #cat("zoom level", leaflet_interaction$zoom_level, "\n")

      # if you ~were~ showing CTs and you zoom out, clear region to return
      if( !is.null(show_CTs()) && leaflet_interaction$zoom_level < minimum_ct_zoom ) {
        selection.reactive(NULL)
        #show_CTs(NULL)
      }
    })

    # click observer ---------------------------------------------------------------
    observeEvent(leaflet_interaction$click_info, {

      clicked.region <- click.hover_2region(leaflet_interaction$click_info,
                                            map.layer = gs.out())

      # print(clicked.region)

      # if you weren't previously showing CTs, do so for clicked region, and zoom in.
      if( is.null(show_CTs()) && !is.null(clicked.region) ) {
        #leaflet::flyTo(proxy,
        #               lng = leaflet_interaction$click_info$lng,
        #               lat = leaflet_interaction$click_info$lat,
        #               zoom = minimum_ct_zoom,
        #               options = list(duration = .5)
        #               )
        selection.reactive( clicked.region )
      }
    })
    #return(return.region)

  })
}


# ui module --------------------------------------------------------------------

#' mod_parse_CT_ui UI Function
#'
#' @description Module to define UI for manipulating the output dataset used
#'   throughout most of the app.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param selectables Selectable options. Likely as
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_parse_CT_ui <- function(id){
  ns <- NS(id)

  tagList(
    textOutput(ns("zoomin.region"))
  )
}



# helper -----------------------------------------------------------------------
# (not used)
fast.approx.centroid <- function(region) {
  bbox = as.list(st_bbox(region))
  lon = mean(bbox$xmax, bbox$xmin)
  lat = mean(bbox$ymax, bbox$ymin)
  #matrix(c(lon, lat), ncol = 2, dimnames = list(c(), c("X", "Y")))
  return(list(lon = lon,
              lat = lat))
}
