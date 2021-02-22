# front-end/legend labels ------------------------------------------

#' make_display_label
#'
#' Get display/label name based on selected outcome / indicator. prefixes change.in
#' when necessary. viz-fcns/tooltip
make_display_label <- function(input, just_outcome = F, outcome.opts = selectables$outcomes) {

  # index to display name for selected outcome
  ui_outcome_string <-
    unlist(unname(selectables$outcomes))[unlist(unname(selectables$outcomes)) == input$outcome] %>% names()

  # combine w/ indicator if appropriate
  if(just_outcome || input$indicator == "outcome")
    ui_indicator_string <- ""
  else
    ui_indicator_string <- paste0(input$indicator, " - ")

  display_name <- paste0(ui_indicator_string, ui_outcome_string)

  # prefix "Change in" if appropriate
  if (input$change_in & input$outcome %in% seln.rules$ts_vars)
    display_name <- paste0("Change in ", display_name)

  return(display_name)
}


# leaflet tooltips -------------------------------------------------------------


add_legend <- function(proxy, to.map, pal, legend.title) {
  proxy %>%
    clearControls() %>%
    addLegend(pal = pal,
              values = to.map$binned_x[!is.na(to.map$binned_x)],
              opacity = 0.6,
              title = legend.title, position = "bottomleft")
}

#' make_tooltips
#'
#' Makes hover tooltips for leaflet. Concatenates and formats relevant information.
#' Outcome/indicator, population, and names for larger areas. Viz-fcns/tooltip.
#' @param click2zoom_enabled If the leaflet layer can be zoomed in by clicking on a
#'   region; will also show region names if T
make_tooltips <- function(input, to.map, click2zoom_enabled = TRUE) {

  # make outcome/indicator & population
  tooltip <- paste0("<b>",make_display_label(input, !click2zoom_enabled), ":</b> ",to.map$formatted_x,
                    "<br><b>population:</b> ", appHelpers::q.format(to.map$population, digits = 0))

  # area names header & zoom info if not CTs
  if(click2zoom_enabled & input$outcome %in% seln.rules$ct.vars)
    tooltip <- paste0("<b>",to.map$region.name,"</b><br>",
                      tooltip,
                      "<br>(click to zoom)")

  return(tooltip)
}



# formatting numbers for readability --------------------------------------------

#' apply_rounding
#'
#' Takes numeric; if > 10k, rounds nearest thousand and formats; otherwise, rounds to
#' \code{digits}. viz-fcns/tooltip
apply_rounding <- function(x, digits = 2) {
  if( any(x > 1e4, na.rm = T) ) {
    x <- round(x, digits = -3)
    x <- appHelpers::q.format(x, digits = 0)
    return(x)
  }

  x <- appHelpers::q.format(x, digits = digits)
  return(x)
}


# css for ggiraph tooltips ------------------------------------------------
# previous css was mostly cursor position. base css in ggiraph is pretty good already.
# appHelpers::get_tooltip_css
# gg_tooltip_css <-
