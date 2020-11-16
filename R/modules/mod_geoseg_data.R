library(DT)
# helpers ----------------------------------------------------------------------


# module ui --------------------------------------------------------------------

#' geoseg_ui UI Function
#'
#' @description Module to define UI for manipulating the output dataset used
#'   throughout most of the app.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param selectables Selectable options. Likely as
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
geoseg_ui <- function(id, selectables){
  ns <- NS(id)

  tagList(
    # OUTCOME --
    selectInput(inputId = ns("outcome"),
                label = strong("Outcome"),
                choices = selectables$outcomes,
                selected = defaults$outcome # selection is initialized to default on server side so that av_agg and av_ind can also intitialize.
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


# module server ----------------------------------------------------------------

#' geoseg_server UI Function
#'
#' @description Module to define UI for manipulating the output dataset used
#'   throughout most of the app.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param gui_inputs If add'l options from user to adjust visual
#'   settings from user are provided, pass on those settings.
#' @param gs.colors avail colors to generate color function from.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
geoseg_server <- function(id, gs.colors = viridis::viridis(7)) {

  moduleServer(id, function(input, output, session) {

    # module reactives
    gs.out <- reactiveVal(NULL)
    gs.palette <- reactiveVal(NULL)

    observeEvent( list(input$outcome, input$indicator,
                       input$region_type, input$pop_weighted), {


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

# modularized app --------------------------------------------------------------

base.app <- function() {

  # ui ---------------------------------------------------------------------------
  ui <- fluidPage(
    geoseg_ui("gs", selectables),
    DT::dataTableOutput("out")
    #verbatimTextOutput("out")
  )

  # server -----------------------------------------------------------------------
  server <- function(input, output, session) {

    c(v, pal) %<-%
      geoseg_server("gs")


    # output$out <- renderPrint(v())
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
