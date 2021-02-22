library(DT)
# helpers ----------------------------------------------------------------------


# slider range -----------------------------------------------------------------
# used by ui and filter fcn
# 1million as upper cap for slider filter..
count.to.million <- function(by = 5000) seq.int( 0, 1e6, by = by)

pop_filter_range <-
  c(trimws(format(count.to.million(), big.mark = ",", digits = 0, scientific = F)),
    ">1 million")


# helper fcns ------------------------------------------------------------------

#' apply_population_filter
#'
#' In app use designed for histograms; takes a character slider and selection,
#' converts slider selection to numeric, and filters input df to within bounds
#' @param x df with a 'population' column
#' @param pop_range_selection Character vector that represents a formated number;
#'   i.e., "1,000". Probably user input from \code{shinyWidgets::sliderTextInput}
apply_population_filter <- function( x, pop_range_selection, slider_range = pop_filter_range) {

  # if(is.null(x) || !("population" %in% colnames(x)))  return(NULL)

  # if slider range is at maximum (i.e. upper end is ">1 million"), only filter at bottom
  if (pop_range_selection[2] == slider_range[length(slider_range)]) {

    x %>% filter( population >=
                    appHelpers::format_as.numeric(pop_range_selection[1]) )
  } else {
    # otherwise, filter at both ends
    x %>% filter( population <=
                    appHelpers::format_as.numeric(pop_range_selection[2]) &
                    population >=
                    appHelpers::format_as.numeric(pop_range_selection[1]) )
  }
}

# module server ----------------------------------------------------------------

#' geoseg_server UI Function
#'
#' @description Module to define UI for manipulating the output dataset used
#'   throughout most of the app.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param x Dataframe with a 'population' column to filter by
#'
#' @noRd
#' @export
mod_population.filter <- function(id, x) {

  moduleServer(id, function(input, output, session) {

    pop.filtered <- reactiveVal(NULL)

    observeEvent(list(x(), input$population_slider), {

      req(x())
      # parse input and set to reactive
      pop.filtered( apply_population_filter(x(), input$population_slider) )

    })

    return(pop.filtered)
  })
}

# module ui --------------------------------------------------------------------

#' mod_population.filter_ui UI Function
#'
#' @description Population slider to filter an input reactive dataset with a
#'   'population' column.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param range A character vector of formated numerics representing selectable
#'   points along numeric range.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @export
mod_population.filter_ui <- function(id, range = pop_filter_range){
  ns <- NS(id)

  tagList(
    sliderTextInput(ns("population_slider"),
                    label = strong("Filter by Population"),
                    choices = pop_filter_range,
                    selected = c(pop_filter_range[1], pop_filter_range[length(pop_filter_range)]),
                    from_max = pop_filter_range[length(pop_filter_range)-1],
                    to_min = pop_filter_range[2], grid = FALSE,
                    dragRange = T , width = "100%")
  )
}



# modularized app --------------------------------------------------------------

pop.filter.app <- function() {

  # ui ---------------------------------------------------------------------------
  ui <- fluidPage(
    mod_geoseg_ui("gs", selectables),
    mod_population.filter_ui("gs", pop_filter_range),
    dataTableOutput("out")
  )

  # server -----------------------------------------------------------------------
  server <- function(input, output, session) {
    c(v, pal) %<-%
      mod_geoseg("gs")

    vp <- mod_population.filter("gs", v)

    # output$out <- renderPrint(v())
    output$out <- DT::renderDataTable({
      req(!is.null(vp()))
      vp(select(tibble(vp()), setdiff(colnames(vp()), c("x", "geometry")), "geometry"))
      DT::datatable(vp())
    })
  }
  shinyApp(ui, server)
}


# run (test) --------------------------------------------------------------------

# pop.filter.app()
