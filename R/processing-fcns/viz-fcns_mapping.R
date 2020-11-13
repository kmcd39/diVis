
# versions of these fcns in appHelpers.... but they should prolly just be here


# functions to update leafletProxy ----------------------------------

#' iterative_leaflet_draw
#'
#' clear data, get number of iterations, draw first one. Return 0 if there's
#' only one, otherwise loop through the rest. Idea is that it can smooth out a
#' render to make loading feel nicer. Passes tooltips and other graphic parameters
#' to \code{leaflet_draw}. viz-fcns/mapping.
#' @inheritDotParams leaflet_draw
#' @param bkdwn_size Number of rows of sf object to draw at a time.
#' @export
iterative_choropleth_draw <- function(leaflet_proxy,
                                      st_df, poly_group = "dat",
                                      tooltips, pal, ...,
                                      bkdwn_size = 50) {

  leaflet_proxy %>% clearGroup(poly_group)

  n_iterations <- ceiling(nrow(st_df) / bkdwn_size )

  # first pass
  leaflet_proxy %>%
    choropleth_draw(st_df[1:bkdwn_size, ], poly_group
                    ,tooltips = tooltips[1:bkdwn_size]
                    ,pal, ...)
  # check2end
  if(n_iterations == 1) return()

  # loop thru.
  for ( i in 1:(n_iterations - 1)) {
    step <- i * bkdwn_size
    subset_index <- (step + 1):(step + bkdwn_size)

    leaflet_proxy %>%
      choropleth_draw(st_df[subset_index, ], poly_group
                      ,tooltips = tooltips[subset_index]
                      ,pal , ...)
  }
}

#' leaflet_draw
#'
#' Wraps leaflet::addPolygons call to create a choropleth from sf object.
#' viz-fcns/mapping.
#' @param st_df sf object to map
#' @param tooltips A list of strings with html
#' @param pal A function to create color palette from; i.e., a all to
#'   leaflet::colorFactor.
#' @param ... additional arguments passed onto leaflet::addPolygons
#' @param var.name Title of column to color map based on, possibly created with
#'   bin.var_format or bin_from_breaks.
#' @opacity_from_pop Whether to interpolate fillOpacities from population column.
#' @export
choropleth_draw <- function(leaflet_proxy,
                            st_df, poly_group,
                            tooltips, pal,
                            var.name = "binned_x", outlines = NULL,
                            popup_btns = NULL,
                            opacity_from_pop = FALSE
                            , fillOpacity = .75
                            , ...) {

  # drop empty geometries.
  # (especially imp. if called from iterative draw b/c extra rows at final call)
  st_df <- st_df[!st_is_empty(st_df$geometry), ]
  tooltips <- lapply(tooltips[!is.na(tooltips)], shiny::HTML)

  # pull supplied column as vector
  vv <- pull(st_df, !!var.name)

  # parse fillOpacity parameter --
  if(opacity_from_pop)
    opacities <- appHelpers::col.to.opacity(st_df$population)
  else
    opacities <- fillOpacity

  leaflet_proxy %>%
    addPolygons(group = poly_group,
                data = st_df,
                fillColor = pal(vv),
                label = tooltips,
                popup = popup_btns,
                fillOpacity = opacities,
                ...,
                highlightOptions = highlightOptions(fillColor = "#EE77A0"
                                                    ,weight=2
                                                    ,fillOpacity = 0.6
                                                    ,bringToFront = TRUE))
}

