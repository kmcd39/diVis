
# versions of these fcns in appHelpers.... but they should prolly just be here



# render base leaflet + tiles --------------------------------------------------

#' create_leaflet_base
#'
#' Creates base leaflet and sets options for app. See discussion
#' https://stackoverflow.com/questions/54667968/controlling-the-z-index-of-a-leaflet-heatmap-in-r/54676391
#' for addMapPane/zIndex. Or also https://leafletjs.com/examples/map-panes/
#' @import leaflet
#' @export
create_leaflet_base <- function() {

  leaflet(options = leafletOptions()) %>%
    addMapPane("gs", zIndex = 400) %>%
    addMapPane("div", zIndex = 430) %>%
    addMapPane("tileLabels", zIndex = 599) %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels,
                     options = providerTileOptions(noWrap = TRUE,
                                                   pane = "tilePane")
    ) %>%
    addProviderTiles(providers$CartoDB.PositronOnlyLabels,
                     options = providerTileOptions(noWrap = TRUE,
                                                   pane = "tileLabels")
    )
  }


# functions to map base geoseg data ----------------------------------


#' choropleth_draw
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
#' @param opacity_from_pop.dens Whether to interpolate fillOpacities from `pop.dens` column.
#' @export
choropleth_draw <- function(leaflet_proxy,
                            st_df, grp.name,
                            tooltips, pal,
                            var.name = "binned_x", outlines = NULL,
                            popup_btns = NULL,
                            opacity_from_pop.dens = FALSE
                            , fillOpacity = .7
                            , ...) {

  # drop empty geometries.
  # (especially imp. if called from iterative draw b/c extra rows at final call)
  st_df <- st_df[!st_is_empty(st_df$geometry), ]
  tooltips <- lapply(tooltips[!is.na(tooltips)], shiny::HTML)

  # pull supplied column as vector
  vv <- pull(st_df, !!var.name)

  # parse fillOpacity parameter --
  if(opacity_from_pop.dens)
    opacities <- appHelpers::col.to.opacity(st_df$pop.dens)
  else
    opacities <- fillOpacity

  leaflet_proxy %>%
    addPolygons(group = grp.name,
                data = st_df,
                fillColor = pal(vv),
                label = tooltips,
                popup = popup_btns,
                fillOpacity = opacities,
                ...,
                options = pathOptions(pane = "gs"),
                highlightOptions = highlightOptions(color = "#277F8E"
                                                    ,opacity = .8
                                                    #fillColor = "#EE77A0"
                                                    ,weight=2
                                                    ,fillOpacity = 0.2
                                                    ,bringToFront = F
                                                    ,sendToBack = T)
                )
}


