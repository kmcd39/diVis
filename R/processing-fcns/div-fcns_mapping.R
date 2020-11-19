
# color palettes ---------------------------------------------------------------

darken.amt = .4

# redlining ---------------------------------------------
# extracts colors from map used here:
# https://steppingintothemap.com/mappinghistory/category/practicum/page/10/
redlining.colors <-
    c(green = rgb(0, 255, 34, maxColorValue=255),
      blue = rgb(4, 178, 254, maxColorValue=255),
      yellow = rgb(231, 234, 58, maxColorValue=255),
      red = rgb(225, 16, 2, maxColorValue=255)) %>%
  colorspace::darken(amount = darken.amt)


# other div colors -----

#Polychrome::swatch(Polychrome::kelly.colors()[3:22])
#Polychrome::swatch(Polychrome::dark.colors())
Polychrome::swatch(RColorBrewer::brewer.pal(8, "Dark2"))

# used if no color_col specified, but cycled thru per-division-layer
div.colors <- RColorBrewer::brewer.pal(8, "Dark2")[c(1:3,5:8)] %>%
  colorspace::darken(darken.amt)

# cycled through when a color col is specified for a division
other.div.interpolation <- RColorBrewer::brewer.pal(8, "Accent") %>%
  colorspace::darken(darken.amt)

Polychrome::swatch(other.div.interpolation)
Polychrome::swatch(div.colors)


# mapping div fcns ------------------------------------------------------------------

#' long.lat_buffer
#'
#' \code{st_buffer} expects non long/lat geometries. This is a wrapper to convert an sf
#' object to a metered projection, apply \code{st_buffer}, and then transform back into long/lat
#' @param sf object to buffer
#' @param buffer amount.
#' @param ... other arguments to pass to \code{st_buffer}.
long.lat_buffer <- function(sf, buffer, ...) {
  sf %>%
    divFcns::conic.transform() %>%
    st_buffer(buffer, ...) %>%
    st_transform(4326)
}

#' leaflet.add_division_layer
#'
#' Adds a layer representing a division to the leaflet map
#' @param proxy existing leaflet or proxy to layer on top of
#' @param div.sf an sf object representing a division
#' @param div.name,color_col How to color the division layer. Either supplied name of
#'   layer will be used (uniform for layer) or a column bundled with the div sf,
#'   i.e., HOLC grades for redlining.
#' @param weight base weight for line thickness. Made explicit so that bkgd line can
#'   be a larger version
#' @param shrink.divs divs to shrink slightly before mapping, in order to map more
#'   legible if they tend to be co-terminous, like places.
#' @param ... additional arguments passed onto addPolygons or addPolylines
leaflet.add_division_layer <- function(proxy, div.sf,
                                       div.name = NULL, color_col = NA,
                                       weight = 6, shrink.divs = c("redlining", "plc", "places", "school_dists"),
                                       ...) {

  # get colors based on combination of division name or column to color by
  colors <- get.div.colors(div.sf, div.name, color_col)

  # make tooltips
  if(!is.na(color_col))
    div.tooltips <- pull(div.sf, !!color_col)
  else
    div.tooltips <- div.name

  # div layers to negative buffer
  if(div.name %in% shrink.divs)
    div.sf <- long.lat_buffer(div.sf, -25)

  #add white outline to distinguish from choropleth
  out <- proxy %>% div.outline(div.sf, weight + 5)

  # add main div representation
  out <- out %>%
    addPolylines(group = "divs",
                 data = div.sf,
                 weight = weight,
                 label = div.tooltips,
                 opacity = 1,
                 color = colors,
                 options = pathOptions(pane = "div")
                 )
  return(out)
}


#' get.div.colors
#'
#' Get a single hex color or vector of colors, based on a information on a division
#' dataset. Will color by the divname (i.e., 'school.districts') or a column in
#' divdat; i.e, holc grade.
get.div.colors <- function(df, div.name = NULL, color_col = NA, div.palette = colorFactor(div.colors,
                                                                                           div.opts)) {
  if(!is.na(color_col) & !is.null(div.name)) {
    colors <- case_when(div.name == "redlining" ~ colorFactor(redlining.colors,
                                                              domain = pull(df, !!color_col))(pull(df, !!color_col))
                        # fill in to add other specialized color layers....
                        ,TRUE ~ colorFactor(other.div.interpolation,
                                            domain = pull(df, !!color_col))(pull(df, !!color_col))
                        )
  } else if(!is.na(div.name)) {
    colors <- div.palette(div.name)
  } else
    stop("At least 1 of name or color column must be non-NULL to get.div.colors")

  return(colors)

}
# get.div.colors(tmp.plc, "plc", "NAME")
# get.div.colors(tmp.redlining, "redlining", "holc_grade")



#' div.outline
#'
#' Adds a white outline under a division layer to set it apart from underlying
#' choropleth.
#' @inheritParams leaflet.add_division_layer
div.outline <- function(proxy, div.sf, weight) {

  proxy %>%
    addPolylines(group = "divs",
                 data = div.sf,
                 color =  "#FFFFFF",
                 weight = weight,
                 opacity = 1,
                 options = pathOptions(pane = "div")
                 )
}



# sample plots -----------------------------------------------------------------
'
base.gs.choropleth %>%
  leaflet.add_division_layer(tmp.plc, div.name = "plc", color_col = "NAME")


base.gs.choropleth %>% leaflet.add_division_layer(tmp.redlining,
                             div.name = "redlining",
                             color_col = "holc_grade")
'
