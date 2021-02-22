
# helper fcn ---------------------------------------------------------------------

#' get_CTs_by_region
#'
#' Given single row representing a larger region, get all corresponding census
#' tracts. Bundles other steps applied before mapping, i.e., \code{bin_and_format}
#' and \code{st_sf}.
#' @param region single-row sf object in region.id/region.type cols.
#' @param outcome Which column to show, from input$outcome
#' @param ... add'l arguments passed onto \code{bin_and_format}.
get_CTs_by_region <- function(region, outcome, ...) {

  if(is.null(region))
    return(NULL)

  region.type <- region$region.type
  region.id <- region$region.id

  region.cts <- cts %>%
    filter(!!rlang::sym(region.type) == region.id) %>%
    rename("x" = !!rlang::sym(outcome))

  region.cts <- bin_and_format(region.cts, ...)
  region.cts <- st_sf(region.cts)
  st_crs(region.cts) <- 4326 # explicit crs for shinyapp.io hosting

  return(region.cts)
}

# server module ----------------------------------------------------------------

#' mod_region2CTs server Function
#'
#' @description Takes a 1-row object showing a single larger-region, and returns all
#'   CTs within that region. Also fills in a text output that shows name of larger
#'   region
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param region.selection Single-row df in region.id/region.type form.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import sf leaflet
#' @export
mod_region2CTs <- function(id,
                           region.reactive,
                           CT.reactive,
                           proxy,
                           minimum_ct_zoom = 8
) {

  moduleServer(id, function(input, output, session) {

    # update CT reactive when region or outcome changes ----------------------------
    observeEvent( list(region.reactive(), input$outcome), {



      # & set ct reactive with helper fcn if available -- otherwise reset to null
      if(input$outcome %in% seln.rules$ct.vars) {

        CT.reactive(
          get_CTs_by_region( region.reactive(), input$outcome )
        )

      } else {

        CT.reactive(NULL)
        region.reactive(NULL)

      }

    }, ignoreNULL = F, ignoreInit = T)


    # update zoom when CT reactive changes -----------------------------------------
    observeEvent( region.reactive(), {

      if(!is.null(region.reactive())) {
        # zoom to region
        region.coords <-
          suppressWarnings(
            st_centroid(region.reactive())$geometry) %>% st_coordinates()

        leaflet::flyTo(proxy,
                       lng = region.coords[,"X"],
                       lat = region.coords[,"Y"],
                       zoom = minimum_ct_zoom,
                       options = list(duration = .5)
        )
      }

    })

    # send region name to text box  ------------------------------------------
    output$zoom.in_region <- renderText({
      req(region.reactive())

      region.reactive()$region.name
    })
  })
}



# input (text box) -------------------------------------------------------------

#' mod_region2CTs_ui
#'
#' Server-side equivalent does work of querying all CTs and prepping them to map,
#' updating leaflet zoom etc., to facillitate leaflet interaction and zooming in to
#' small areas. This UI module just does a text box with the larger region name when
#' zoomed in
#' @export
mod_region2CTs_ui <- function(id) {

  ns <- NS(id)

  tagList(
    textOutput(ns("zoom.in_region"))
  )
}
