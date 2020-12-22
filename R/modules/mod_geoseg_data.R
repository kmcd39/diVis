library(DT)
# helpers ----------------------------------------------------------------------

#' parse.geoseg.data
#'
#' Umbrella fcn to call all steps necessary to parse output for core
#' larger-region-level functionality. Called from mod_geoseg_data server module.
#' flow-fcns/data-processing.
#' @param input input passed on from shiny, as from \code{geoseg_ui}
#' @inheritParams bin_and_format
parse.geoseg.data <- function(input, ...) {

  browser()
  # get attr -- subsets from appro
  out <- flexi.attr_subset(input)

  # get change-in if necessary
  if( input$change_in & input$outcome %in% seln.rules$ts_vars )
    out <- get_change_in(out, input)

  out <- bin_and_format(out, ...)

  # trim by year if necessary
  if( !input$change_in & input$outcome %in% seln.rules$ts_vars )
    out <- out %>% filter(year == input$year)

  # join w/ geos
  out <- inner_join(out,
                    geo.list[[input$region_type]],
                    by = c("region.id"))

  # To make bins round to nearest thousand if vector contains values >10,000
  if(any(out$x  > 1e4, na.rm = T) )
    out$x <- round(out$x, digits = -3)

  out <- st_sf(out)

  return(out)
}

#' flexi.attr_subset
#'
#' Parse data. Will depend on setup of bundled datasets and selectability rules (all
#' generated in data-raw/)
flexi.attr_subset <- function(input) {

  if(input$outcome %in% seln.rules$gs_vars)
    out <- gs_attr_subset(input)
  if(input$outcome %in% seln.rules$dod_vars)
    out <- dod_attr_subset(input)

  return(out)
}

#' gs_attr_subset
#'
#' Subsets based on input from metrics, adapted dataset exported from \code{geoseg}
gs_attr_subset <- function(input) {

  # subset based on agg_level and outcome/var.name
  out <- metrics[metrics$region.type == input$region_type &
                   metrics$var.name %in% input$outcome, ] %>%
    select(c(1:2, "x" = input$indicator,
             "var.name", "outcome", "population", "year")) # , "year"))
  return(out)

}

#' dod_attr_subset
#'
#' Subset attribute data based on user input from prepped deaths of despair dataset.
dod_attr_subset <- function(input) {

  # cdc data compliance fcns defined w/ flow-fcns
  keep_vars <- c(seln.rules$dod_vars, "population")

  # get df based on which dataset
  out <- dod %>%
    select(c(contains("region"),
             year,
             tidyselect::all_of(keep_vars))) %>%
    filter(region.type == input$region_type) %>%
    rename(x = input$outcome)

  # suppress <10 counts if necessary & save for time-series before year-trim
  out <- suppress.low.CDC.counts(out, input)

  return(out)
}



# module server ----------------------------------------------------------------

#' mod_geoseg server Function
#'
#' @description Module to define server for manipulating the output dataset used
#'   throughout most of the app.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param gs.colors avail colors to generate color function from.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_geoseg <- function(id, gs.colors = viridis::viridis(7)) {

  moduleServer(id, function(input, output, session) {

    # module reactives
    gs.out <- reactiveVal(NULL)
    gs.palette <- reactiveVal(NULL)


    # "reset to defaults" button  -----------------------------------------
    observeEvent(input$reset_to_defaults, {

      # defaults defined along w/ other options in params/selectables.
      purrr::map(names(defaults),
                 ~updateSelectizeInput(session, .,
                                       selected = defaults[[.]])
                 )
    })


    # update choices / selectability when outcomes changes ------------------

    # (options such as possible years, region levels, indicators, are dependent
    # on selected outcome)

    observeEvent( input$outcome, {
      # seln.matrix defined in data-raw/selectabilities

      new_choices <- seln.matrix[seln.matrix$outcome %in% input$outcome,] %>%
        purrr::map(unlist)

      # ensure only options that are available with given outcome selection are displayed
      # Keep previously selected options when possible
      update_ui_keep_selected_if_possible <- function(ui_element, new_choices) {

        old_selected <- input[[ui_element]]

        if( all(old_selected %in% new_choices) ) {
          updateSelectizeInput(session, ui_element,
                               choices = new_choices,
                               selected = old_selected)
        } else
          updateSelectizeInput(session, ui_element,
                               choices = new_choices)
        }

      # update indicators
      update_ui_keep_selected_if_possible("indicator",
                                          new_choices$avail_indicators)
      # update region types
      update_ui_keep_selected_if_possible("region_type",
                                          new_choices$avail_region.types)

      # update TS options
      if( input$outcome %in% seln.rules$ts_vars ) {

        shinyjs::show("time_trim")

        update_ui_keep_selected_if_possible("year",
                                            new_choices$avail_years)
        update_ui_keep_selected_if_possible("year_range",
                                            new_choices$avail_years)
        if( !length(new_choices$avail_years) > 1 ) # if only 1 year available
          updateSelectizeInput(session, "change_in",
                               selected = FALSE)
      } else {
        shinyjs::hide("time_trim")
      }


    })


    # update returned reactives ----------------------------------------------------
    observeEvent( list(
      input$outcome,
      input$indicator,
      input$region_type,
      input$pop_weighted,
      input$year,
      input$year_range,
      input$change_in), {

        req(input)

        # call wrapper fcn (defined above); set to reactive
        gs.out(
          parse.geoseg.data(input)
          )

        # generate color fcn
        pal <- colorFactor(gs.colors,
                           domain = gs.out()$binned_x)
        gs.palette(pal)
      })

    return(list(
      dat = gs.out,
      palette = gs.palette)
    )

  })
}


# module ui --------------------------------------------------------------------

#' mod_geoseg_ui UI Function
#'
#' @description Module to define UI for manipulating the output dataset used
#'   throughout most of the app.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param selectables Selectable options, probably as generated from
#'   params/selectables script.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_geoseg_ui <- function(id, selectables){
  ns <- NS(id)

  require(shinyWidgets)
  shinyjs::useShinyjs()

  tagList(
    # OUTCOME --
    selectInput(inputId = ns("outcome"),
                label = strong("Outcome"),
                choices = selectables$outcomes,
                selected = defaults$outcome
    ),

    # INDICATOR --
    selectizeInput(inputId = ns("indicator"),
                   label = strong("Inequality Indicator"),
                   choices = selectables$indicators,
                   selected = defaults$indicator),

    # REGION TYPE --
    selectizeInput(inputId = ns("region_type"),
                   label = strong("Region type"),
                   choices = selectables$region_types,
                   selected = defaults$region_type),

    # TIME SLIDER --
    shinyjs::hidden(div(id = "time_trim",
                        sliderTextInput(ns("year"), label = strong("Year:") # single year
                                        , choices = selectables$years
                                        , selected = selectables$years[length(selectables$years)]),
                        sliderTextInput(ns("year_range"), label = strong("Change over period:") # multi-year range
                                        , choices = selectables$years
                                        # **initialization is important for this one so that the slider is created as a range slider
                                        , selected = c(selectables$years[1], # first..
                                                       selectables$years[length(selectables$years)])), # ..& last
                        # CHANGE-IN
                        materialSwitch(ns("change_in"),
                                       status = "primary", label = strong("show change"),
                                       value = FALSE))),

    # WEIGHED_BY_POP
    materialSwitch(inputId = ns("pop_weighted"), status = "primary",
                   label = strong("Weigh Outcomes by Population"),
                   value = FALSE),

    # RESET TO DEFAULT
    fluidRow(height = "10px", HTML("<br>")),
    actionButton(ns("reset_to_defaults"), "Reset Selections",
                 width = "80%")
  )
}


# modularized app --------------------------------------------------------------

base.app <- function() {

  # ui ---------------------------------------------------------------------------
  ui <- fluidPage(
    mod_geoseg_ui("gs", selectables),
    DT::dataTableOutput("out")
  )

  # server -----------------------------------------------------------------------
  server <- function(input, output, session) {

    c(v, pal) %<-%
      mod_geoseg("gs")

    # datatable for straightforward test output
    output$out <- DT::renderDataTable({
      req(!is.null(v()))
      v(select(tibble(v()), setdiff(colnames(v()), c("x", "geometry")), "geometry"))
      DT::datatable(v())
    })
  }
  shinyApp(ui, server)
}


# run (test) --------------------------------------------------------------------

#base.app()
