# Variation of `mod_geoseg_data.` Safegraph data and the segregation measures don't
# have the same "outcome/indicator" structure, and the small-area measures don't
# co-align in the same ways with large-area equivalents. Therefore the parsing
# mechanisms are different


# helper fcns ------------------------------------------------------------------

#' parse.sfm
#'
#' Parse safegraph region-level input based on selected outcome
parse.sfm <-


# server-side ------------------------------------------------------------------


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
#' @export
mod_sfg <- function(id, gs.colors = viridis::viridis(7)) {

  moduleServer(id, function(input, output, session) {

    # module reactives -- returned from module
    sfg.out <- reactiveVal(NULL) # parsed dataset
    sfg.palette <- reactiveVal(NULL) # matching color-interpolation function

    # "reset to defaults" button  -----------------------------------------
    observeEvent(input$reset_to_defaults, {
      # defaults defined along w/ other options in params/selectables.
      map(names(defaults),
                 ~updateSelectizeInput(session, .,
                                       selected = defaults[[.]])
      )
    })


    # update choices / selectability when outcomes change ------------------

    # not necessary for pared down v...


    # update returned reactives ----------------------------------------------------
    observeEvent( list(
      input$outcome,
      input$region_type), {


        # call wrapper fcn (defined above); set to reactive
        gs.out(
          parse.geoseg.data(input)
        )

        # generate color fcn
        pal <- colorFactor(gs.colors,
                           domain = gs.out()$binned_x)
        gs.palette(pal)
      })

    # module retruns parsed dataset and color fcn
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
    shinyjs::hidden(div(id = ns("time_trim"),
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
                                       status = "primary", label = strong("Change over time"),
                                       value = FALSE))),

    # WEIGHED_BY_POP
    materialSwitch(inputId = ns("pop_weighted"), status = "primary",
                   label = strong("Weigh indicator by population"),
                   value = FALSE),

    # RESET TO DEFAULT
    fluidRow(height = "10px", HTML("<br>")),
    actionButton(ns("reset_to_defaults"), "Reset Selections",
                 width = "80%")
  )
}


# modularized app --------------------------------------------------------------

base.app <- function() {

  require(shinyjs)
  # ui ---------------------------------------------------------------------------
  ui <- fluidPage(
    shinyjs::useShinyjs(),
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
      v(select(tibble(v()),
               setdiff(colnames(v()), c("x", "geometry"))
      ))
      DT::datatable(v())
    })
  }
  shinyApp(ui, server)
}


# run (test) --------------------------------------------------------------------

# base.app()
