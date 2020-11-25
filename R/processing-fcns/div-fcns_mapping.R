
# color/column index ------------------------------------------------------------------

# set options for overlays and parameters to be used for mapping each one.
(div.param.Index <- tibble(
  div.name = c("redlining", "rails_bts", "hwys", "places", "school_dists", "hwyPlan1947"),
  id_col = c("holc_grade", NA, "SIGN1", "NAME", "NAME", "SIGN1"),
  color_by_column = c(T, F, F, F, F, F)
))

# options for UI
div.opts <- div.param.Index$div.name



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


# used if no color_col specified, but cycled thru per-division-layer
cols <- c("purple", "lightblue", "green", "violet", "reddishbrown", "olivegreen")
Polychrome::swatch(Polychrome::kelly.colors()[3:22]) %>%
    `[`(cols)

  #  `[`(c(2,4,9,11,13,15,22))

div.colors <- (Polychrome::kelly.colors()[3:22]) %>%
  `[`(cols)
# Polychrome::swatch(div.colors)

#RColorBrewer::brewer.pal(8, "Dark2")[c(1:3,5:8)] %>%
#  colorspace::darken(darken.amt)

# cycled through when a color col is specified for a division
other.div.interpolation <- RColorBrewer::brewer.pal(8, "Accent") %>%
  colorspace::darken(darken.amt)

# Polychrome::swatch(other.div.interpolation)

# create color fcn -------------------------------------------------------------

divs_needing_color <-
  div.param.Index[div.param.Index$div.name %in% div.opts &
                    !div.param.Index$color_by_column,]$div.name

div.pal <-
  colorFactor(div.colors,
              divs_needing_color)

# Polychrome::swatch(div.pal(divs_needing_color))


# mapping div fcns ------------------------------------------------------------------

#' long.lat_buffer
#'
#' \code{st_buffer} expects non-long/lat geometries. This is a wrapper to convert an sf
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
                                       div.name = NULL, id_col = NA,
                                       color_by_column = F,
                                       weight = 5, shrink.divs = c("redlining", "plc", "places", "school_dists"),
                                       ...) {

  # get colors based on combination of division name or column to color by
  colors <- get.div.colors(div.sf, div.name, id_col, color_by_column)

  # make tooltips
  if(!is.na(id_col))
    div.tooltips <- pull(div.sf, !!id_col)
  else
    div.tooltips <- div.name

  # div layers to negative buffer
  if(div.name %in% shrink.divs)
    div.sf <- long.lat_buffer(div.sf, -25)

  #add white outline to distinguish from choropleth
  out <- proxy %>% div.outline(div.sf, weight + 3)

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
get.div.colors <- function(df, div.name = NULL, id_col = NA,
                           color_by_column = F, pal = div.pal) {

  if(color_by_column & !is.null(div.name)) {
    colors <- case_when(div.name == "redlining" ~ colorFactor(redlining.colors,
                                                              domain = pull(df, !!id_col))(pull(df, !!id_col))
                        # fill in to add other specialized color layers....
                        ,TRUE ~ colorFactor(other.div.interpolation,
                                            domain = pull(df, !!id_col))(pull(df, !!id_col))
                        )
  } else if(!is.na(div.name)) {
    colors <- pal(div.name)
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
div.outline <- function(proxy, div.sf, weight, color = "#FFFFFF") {

  proxy %>%
    addPolylines(group = "divs",
                 data = div.sf,
                 color = color,
                 weight = weight,
                 opacity = 1,
                 options = pathOptions(pane = "div")
    )
}


#' add.div_index.wrapper
#'
#' Wrapper fcn that just identifies what/if any column to color by for each division
#' layer based on div.param.Index, and passes arguments onto mapping fcn.
add.div_index.wrapper <- function(proxy, div, div.str) {

  # escape if no divs
  if(nrow(div) == 0) return()

  div.param.index <- div.param.Index[div.param.Index$div.name == div.str, ]

  proxy %>%
    leaflet.add_division_layer(div,
                               div.name = div.str,
                               id_col = div.param.index$id_col,
                               color_by_column = div.param.index$color_by_column)
}




# sample plots -----------------------------------------------------------------
'
base.gs.choropleth %>%
  add.div_index.wrapper(tmp.plc, div.str = "places")


base.gs.choropleth %>%
  add.div_index.wrapper(tmp.redlining,
                             div.str = "redlining")

'



