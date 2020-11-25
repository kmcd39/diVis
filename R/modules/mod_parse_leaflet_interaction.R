
# server module ----------------------------------------------------------------

#' mod_parse_leaflet.interaction server Function
#'
#' @description Parses leaflet interaction to get clicked region (to zoom into) and
#'   clears clicked region when zoomed out. Returns a 1 row dataframe representing
#'   the clicked region, to pass on to other modules.
#'
#' @inheritParams mod_geoseg_leaflet
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import sf leaflet
mod_parse_leaflet.interaction <- function(id, leaflet_interaction, gs.out, show_CTs, proxy) {

  moduleServer(id, function(input, output, session) {

    # module params
    minimum_ct_zoom <- 8

    # module reactive
    return.region <- reactiveVal(NULL)

    # zoom observer ----------------------------------------------------------------
    observeEvent(leaflet_interaction$zoom_level, {
      #cat("zoom level", leaflet_interaction$zoom_level, "\n")

      # if you ~were~ showing CTs and you zoom out, start showing larger areas
      if( !is.null(show_CTs()) && leaflet_interaction$zoom_level < minimum_ct_zoom)
        return.region(NULL)
    })


    # click observer ---------------------------------------------------------------
    observeEvent(leaflet_interaction$click_info, {

      clicked.region <- click.hover_2region(leaflet_interaction$click_info,
                                            map.layer = gs.out())

      # if you weren't previously showing CTs, do so for clicked region, and zoom in.
      if( is.null(show_CTs()) && !is.null(region()) )
        return.region( clicked.region )
    })

    return(return.region)

    # move to region -> CTs helper, called from main wrapper ------------------------
'
    # update zoom to click location
    leaflet::flyTo(proxy,
                   lng = leaflet_interaction$click_info$lng,
                   lat = leaflet_interaction$click_info$lat,
                   zoom = minimum_ct_zoom
                   ,options = list(duration = .5)
    )

    region <- reactive({
      click.hover_2region(leaflet_interaction$click_info, map.layer = gs.out)
    })


    # new show_CTs status
    gen_CTs <- isolate(reactive({
      mapCTs <- get_CTs_by_region(region()) %>% rename("x" = !!rlang::sym(input$outcome))

      mapCTs <- bin_and_format(mapCTs)
      mapCTs <- st_sf(mapCTs)
      return(mapCTs)
    }))


    # observer for gs.inputs that affect mapped CTs
    observeEvent( list(input$outcome, input$pop_weighted), {
      req(show_CTs())
      new.show_CTs(gen_CTs())
    })

    # send region name to text box
    output$zoomin.region <- renderText({
      req(leaflet_interaction$zoom_level)
      if(leaflet_interaction$zoom_level >= minimum_ct_zoom)
        region()$region.name
      else
        ""
      })

    return(new.show_CTs)
    '
    #return(list(new.show_CTs,
    #            reactive(create_CT_color.fcn(
    #              map.cts = new.show_CTs(),
    #              x = input$outcome
    #            ))))
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
