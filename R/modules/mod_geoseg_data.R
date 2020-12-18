library(DT)
# helpers ----------------------------------------------------------------------


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


    # reset to defaults observer
    observeEvent(input$reset_to_defaults, {

      # defaults defined along w/ other options in params/selectables.
      purrr::map(names(defaults),
                 ~updateSelectizeInput(session, .,
                                       selected = defaults[[.]])
      )

    })

    # update returned reactives
    observeEvent( list(input$outcome, input$indicator,
                       input$region_type, input$pop_weighted), {

      req(input)

      # call wrapper fcn to parse data; set output to reactive
      gs.out( parse.geoseg.data(input) )

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
                   choices = selectables$region_type,
                   selected = defaults$region_type),

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

base.app()
