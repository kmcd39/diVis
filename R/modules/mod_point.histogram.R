#' module that bundles a point histogram with a data.table


# helpers ----------------------------------------------------------------------

#' gen_zoomButtons
#'
#' Creates a vector of actionButtons to be nested as column of datatable.
#' @param n number of buttons to create; nrow of df
#' @param ns function to prefix shiny namespace
#' @param label display text, passed to shiny::actionButtona
gen_zoomButtons <- function(n, ns, id = "button_") {

  # create n buttons
  buttons <- purrr::map_chr(
    seq_len(n),
    ~as.character(
      shiny::actionButton(inputId = paste0(id, .),
                          label = "Zoom on map",
                          onclick = sprintf("Shiny.onInputChange('%s', this.id)",
                                            ns("select_button")))) # interaction to be retrieved with input$select_button
  )
  return( buttons )
}

#' add.hist.hilite
#'
#' Adds hilight circle to ggplot
add.hist.hilite <- function(hilite.pt, ...) {

  if(is.null(hilite.pt) || nrow(hilite.pt) == 0)
    return(NULL)

  geom_point(data = hilite.pt,
             aes(x = interval_numeric, y = ypos),
             shape = 1, stroke = 2.4,
             color = "#9EC3FF", size = 4, position = "identity",
             ...)
}

#' add.change_in.line
#'
#' Adds a vertical line to ggplot if param is TRUE
add.change_in.line <- function(change_in, ...) {

  if(!change_in)
    return(NULL)

  geom_vline(xintercept=0,
             linetype="dotted",
             color = "#802020",# "#235e66",
             ...)
}


# point.hist SERVER------------------------------------------------------------


#' mod_point.histogram server Function
#'
#' @description Module to define server-side leaflet rendering
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param to.plot Dataset prepped to map. Output of \code{mod_geoseg}, or
#'   \code{mod_population.filter} after that
#' @param palette Color fcn to use for interpolation.
#' @param selection.reactive server-wide reactive to set to selected region.
#' @param hilite.point To pass info from outside module to highlight a point.
#' @param change_in Changes histogram visualize to emphasize incr/decr if true.
#'
#' @noRd
#'
#' @importFrom htmltools HTML
#' @export
mod_point.histogram <- function(id,
                                gs.dat, palette,
                                selection.reactive = reactiveVal(NULL),
                                hilite.point = reactiveVal(NULL),
                                change_in = FALSE,
                                zoom.to.map.enabled = TRUE) {

  moduleServer(id, function(input, output, session) {

    # global reactives -------------------------------------------------------------
    # called in multiple places or eventually returned
    hist.dat <- reactiveVal(NULL)
    hist.plot <- reactiveVal(NULL)

    display.label <- reactive({
      #cat("making display label..\n")
      make_display_label(input)
    })

    # Prep data when input changes. --------------------
    observeEvent(gs.dat(), {

      # no hilite after redraw
      hilite.point(NULL)

      # update hist.dat reactive - Note rows won't algin with input gs.dat b/c NA's
      # are removed
      hist.dat({
        # cat("transforming data for hist..\n")

        gs.dat() %>%
          appHelpers::prep_for_point_hist( bin_denom = 10 ) %>%
          mutate(color = palette()(binned_x)) %>%
          arrange(desc(x)) %>% # to show in descending order in table
          rename(!!display.label() := x)
      })
    })

    # send to plot
    output$point.hist <- renderPlot({

      #ensure data has finished prepping
      req(display.label() %in% colnames(hist.dat()))

      draw_point_hist( hist.dat(),
                       var.name = display.label()) +
        add.hist.hilite(hilite.point()) +
        add.change_in.line(input$change_in)
    })

    # ------------------------------------------------------------------------------

    # Histogram interactivity -----------------------------------------------------

    # tooltip on hover
    output$point.hist_tooltip <- renderUI({

      # this solution is heavily indebted to example here:
      # https://gitlab.com/snippets/16220
      mouse_loc <- input$plot_hover
      point <- shiny::nearPoints(hist.dat(), mouse_loc, threshold = 10, maxpoints = 1)

      # end if nothing clicked
      if(nrow(point) == 0) return(NULL)

      cursor_coords <- appHelpers::get_cursor_coordinates(mouse_loc)
      tooltip_css <- appHelpers::get_tooltip_css(cursor_coords,
                                                 bkgd_color =  "rgba(30,30,30,0.85)",
                                                 text_color = "#FFF")
      # actual tooltip created as wellPanel
      shiny::wellPanel(
        style = tooltip_css,
        p(HTML(paste0("<b>", point$region.name, "<br>",
                      "<b>", display.label(),": </b>", point$formatted_x)))
      )
    })

    # double-click to zoom & switch tab to map --------------------------------
    observeEvent(input$plot_dbl.click, {

      req(zoom.to.map.enabled)

      clicked.point <- shiny::nearPoints(hist.dat(), input$plot_dbl.click,
                                         threshold = 10, maxpoints = 1, addDist = T)

      if (nrow(clicked.point) != 0) {
        region <- gs.dat() %>%
          filter(region.type %in% clicked.point$region.type &
                   region.id %in% clicked.point$region.id)

        selection.reactive(region)
      }

    })


    # highlight circle on single-click on plot -------------------------------
    observeEvent( input$plot_click, {

      point <- shiny::nearPoints(hist.dat(), input$plot_click,
                                 threshold = 10, maxpoints = 1, addDist = T)
      hilite.point(point)

      selectRows(dataTableProxy("dt"), NULL) # de-select row on datatable
    })


    # -----------------------------------------------------------------------

    # Datatable ---------------------------------------------------------------

    ns <- session$ns
    # prep for table display
    gs.dat_forTable <- reactive({

      dat.with.buttons <- tibble(
        hist.dat(),
        "zoom_buttons" = gen_zoomButtons(nrow(hist.dat()), ns),
        stringsAsFactors = FALSE)

      dat.with.buttons %>%
        select(c("Region" = region.name,
                 formatted_x,
                 "outcome",
                 "Population" = population,
                 " " = "zoom_buttons")) %>%
        rename(!!input$indicator := formatted_x,
               !!make_display_label(input, just_outcome = T) := outcome) %>%
        mutate(Population = format(Population, big.mark = ",", digits = 0, scientific = FALSE))
    })

    # render dt
    output$dt <- renderDataTable({

      req(input$indicator %in% colnames(gs.dat_forTable()))

      DT::datatable(gs.dat_forTable(),
                    options = list(bLengthChange = FALSE,
                                   Filter = FALSE, info = FALSE
                                   #stripeClasses = "strip1",
                                   ,pagingType = "numbers" # https://datatables.net/reference/option/pagingType
                                   #, dom = '<"top"irt><"bottom"flp><"clear">'
                    ),
                    escape = FALSE,
                    class = "compact",
                    rownames = FALSE,
                    selection = "single")

    } #, server = FALSE   # i don't know but am getting "unused argument" error for this. Seems like it shouldn't be case from documentation
    )

    # datatable interactivity ------------------------------------------------------

    # Link histogram & DT -- highlight point based on row selection
    observeEvent( input$dt_rows_selected, {

      #print(input$dt_rows_selected)
      selected_row <- hist.dat()[ input$dt_rows_selected, ]
      hilite.point( selected_row )
    })


    # "Zoom on Map" buttons getting pressed
    observeEvent(input$select_button, {
      req(zoom.to.map.enabled)

      # extract id from button that was pressed, which corresponds to table row
      row.id <- unlist(strsplit(input$select_button, "_"))[2]
      # set selected_region
      selection.reactive(
        hist.dat()[row.id,]
      )
    })

  })
}


# ------------------------------------------------------------------------------

# point.hist UI ----------------------------------------------------------------

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
#' @export
mod_point.histogram_ui <- function(id){
  ns <- NS(id)

  require(DT)
  tagList(
    #fluidRow(
    #column(
    div(
      style = "position: relative; cursor: default;",
      plotOutput(ns("point.hist"),
                 height = "450px",
                 # The following defines tags to capture user interaction, which can
                 # be retrieved i.e. through input$plot_hover:
                 hover = hoverOpts(ns("plot_hover"), delay = 1, delayType = "throttle"),
                 dblclick = ns("plot_dbl.click"),
                 click = ns("plot_click")),
      uiOutput(ns("point.hist_tooltip"))),
    #width = 7),
    #column(
    DT::dataTableOutput(ns("dt"),
                        height = "450px")
    #,width = 5))
  )

}



# ------------------------------------------------------------------------------


# self-contained geoseg point.hist app ------------------------------------------------

point.hist_app <- function() {

  # ui ---------------------------------------------------------------------------
  ui <- fluidPage(
    mod_geoseg_ui("gs", selectables),
    mod_point.histogram_ui("gs")
  )

  # server -----------------------------------------------------------------
  server <- function(input,output, session) {

    # parse core input using geoseg module
    c(gs.out, gs.palette) %<-%
      mod_geoseg("gs" )

    # show point-hist
    mod_point.histogram("gs", gs.out, gs.palette)
  }

  shinyApp(ui, server)
}


# launch -----------------------------------------------------------------------

# point.hist_app()
