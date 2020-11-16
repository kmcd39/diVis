
#' click.hover_2region
#'
#' Translates a click or hover on a leaflet polygon to the row
#' representing the clicked region.
#' @param cursor_data list as returned from input$map_shape_click or hover.
#' @param map.layer sf object on leaflet map.
click.hover_2region <- function(cursor_data, map.layer){

  if(is.null(cursor_data) | is.null(map.layer()))
    return(NULL)

  interaction.point <- st_sfc(
    st_point(c(cursor_data$lng,
               cursor_data$lat))
    , crs = 4326)

  sbgp <- suppressMessages( st_intersects(map.layer()
                                          ,interaction.point) )

  out <- map.layer()[lengths(sbgp) > 0, ]
  if(nrow(out) == 0)
    return(NULL)
  else
    return(out)
}



#' get_CTs_by_region
#'
#' Given single row representing a larger region, get all corresponding CTs
#' @param region single-row sf object in region.id/region.type cols.
get_CTs_by_region <- function(region) {

  region.type <- region$region.type
  region.id <- region$region.id

  region.cts <- cts %>%
    filter(!!rlang::sym(region.type) == region.id)

  return(region.cts)
}
