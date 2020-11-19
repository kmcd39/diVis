# front-end/legend labels ------------------------------------------

selectables$outcomes
unlist(unname(selectables$outcomes))
#' make_display_label
#'
#' Get display/label name based on selected outcome / indicator. prefixes change.in
#' when necessary. viz-fcns/tooltip
make_display_label <- function(input, change_in = F, showing_cts = F, outcome.opts = selectables$outcomes) {
  # get UI name for selected outcome; appends indicator if appropriate
  ui_outcome_string <- unlist(unname(outcome.opts))[unlist(unname(outcome.opts)) == input$outcome] %>% names()
  ui_indicator_string <- { if(showing_cts || input$indicator == "outcome")
    ""
    else paste0(input$indicator, " - ")
    }

  ui_name <- paste0(ui_indicator_string, ui_outcome_string)
  if (change_in) ui_name <- paste0("Change in ", ui_name)

  return(ui_name)
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
  tooltip <- paste0("<b>",make_display_label(input, showing_cts = !click2zoom_enabled), ":</b> ",to.map$formatted_x,
                    "<br><b>population:</b> ", appHelpers::q.format(to.map$population, digits = 0))

  # area names header & zoom info if not CTs
  if(click2zoom_enabled)
    tooltip <- paste0("<b>",to.map$region.name,"</b><br>",
                      tooltip,
                      "<br>(click to zoom)")

  return(tooltip)
}


# buttons to zoom to map for datatable -----------------------------------


#' shinyInput
#'
#' boilerplate code to create unique buttons for shiny. Used for zoom_to_map action
#' buttons on data table (I THINK IT WASN"T DOCUMENTED BEFORE).
#' viz-fcns/tooltips-etc.
shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)

  for (i in seq_len(len)) {  # change this to map....
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  return( inputs )
}


# formatting numbers for readability --------------------------------------------

#' apply_rounding
#'
#' Takes numeric; if > 10k, rounds nearest thousand and formats; otherwise, rounds to
#' nearest hundredth. viz-fcns/tooltip
apply_rounding <- function(x) {
  if( any(x > 1e4, na.rm = T) ) {
    x <- round(x, digits = -3)
    x <- appHelpers::q.format(x, digits = 0)
    return(x)
  }
  x <- appHelpers::q.format(x, digits = 3)
  return(x)
}


# css for ggiraph tooltips ------------------------------------------------
# previous css was mostly cursor position. base css in ggiraph is pretty good already.
# appHelpers::get_tooltip_css
# gg_tooltip_css <-
