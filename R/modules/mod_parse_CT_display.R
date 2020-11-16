
# server module ----------------------------------------------------------------

#' mod_parse_CT_display server Function
#'
#' @description Module to define server-side leaflet rendering
#'
#' @inheritParams mod_geoseg_leaflet
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import sf leaflet
mod_parse_CT_display <- function(id, leaflet_interaction, show_CTs, proxy) {

  moduleServer(id, function(input, output, session) {

    # new show_CTs status
    new.show_CTs <- reactiveVal(NULL)

    # module params
    minimum_ct_zoom <- 8

    observeEvent(leaflet_interaction$zoom_level, {

      #cat("zoom level", leaflet_interaction$zoom_level, "\n")

      # if you ~were~ showing CTs and you zoom out <=6(?), start showing larger areas
      if( !is.null(show_CTs()) && leaflet_interaction$zoom_level < minimum_ct_zoom) {
        new.show_CTs(NULL)
        # ...and reset last clicked region
        leaflet_interaction$clicked_region <- NULL
      }

    })


    observeEvent(leaflet_interaction$clicked_region, {

      region = leaflet_interaction$clicked_region
      #cat(region$region.name,"\n")

      # if you weren't previously showing CTs, do so for clicked region, and zoom in.
      if( is.null(show_CTs()) && !is.null(region) ) {

        zoom.coords <- fast.approx.centroid(region)
        leaflet::flyTo(proxy, zoom.coords$lon, zoom.coords$lat,
                       zoom = minimum_ct_zoom)

        mapCTs <- get_CTs_by_region(region) %>% rename("x" = !!rlang::sym(input$outcome))

        mapCTs <- bin_and_format(mapCTs)
        mapCTs <- st_sf(mapCTs)

        new.show_CTs(mapCTs)
        }
    })

    return(new.show_CTs)
    #return(list(new.show_CTs,
    #            reactive(create_CT_color.fcn(
    #              map.cts = new.show_CTs(),
    #              x = input$outcome
    #            ))))
  })
}

# helper -----------------------------------------------------------------------

fast.approx.centroid <- function(region) {
  bbox = as.list(st_bbox(region))
  lon = mean(bbox$xmax, bbox$xmin)
  lat = mean(bbox$ymax, bbox$ymin)
  return(list(lon = lon,
              lat = lat))
}
