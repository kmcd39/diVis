
# work fcn ---------------------------------------------------------------------

#' get_CTs_by_region
#'
#' Given single row representing a larger region, get all corresponding CTs. Bundles
#' other steps applied before mapping, i.e., \code{bin_and_format} and \code{st_sf}.
#' @param region single-row sf object in region.id/region.type cols.
#' @param outcome Which column to show, from input$outcome
#' @param ... add'l argumets passed onto \code{bin_and_format}.
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
mod_region2CTs <- function(id,
                           region.reactive,
                           CT.reactive,
                           proxy,
                           minimum_ct_zoom = 8
) {

  moduleServer(id, function(input, output, session) {

    # update CT reactive when region changes ---------------------------------
    observeEvent(region.reactive(), {

      # set reactive
      CT.reactive(
        get_CTs_by_region( region.reactive(), input$outcome )
      )
      # if not NULL, also zoom to region
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

    }, ignoreNULL = F)

    # send region name to text box  ------------------------------------------
    output$zoom.in_region <- renderText({
      req(region.reactive())

      region.reactive()$region.name
    })
  })
}



# input (text box) -------------------------------------------------------------
mod_region2CTs_ui <- function(id) {

  ns <- NS(id)

  tagList(
    textOutput(ns("zoom.in_region"))
  )
}
