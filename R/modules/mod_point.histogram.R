
# helpers ----------------------------------------------------------------------

#' add.hist.hilite
#'
#' Adds hilight circle to ggplot
add.hist.hilite <- function(hilite.pt, ...) {

  if(is.null(hilite.pt) || nrow(hilite.pt) == 0)
    return(NULL)

  geom_point(data = hilite.pt, shape = 1, stroke = 2.4,
             color = "#9EC3FF", size = 4, position = "identity",
             ...)
}


add.change_in.line <- function(change_in, ...) {

  if(!change_in)
    return(NULL)

  geom_vline(xintercept=0,
             linetype="dotted",
             color = "#235e66")
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

      hist.dat({
        # cat("transforming data for hist..\n")

        gs.dat() %>%
          appHelpers::prep_for_point_hist( bin_denom = 10 ) %>%
          mutate(color = palette()(binned_x)) %>%
          # arrange(desc(x)) %>% # do i need this?
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
        add.change_in.line(change_in)
    })

    # ------------------------------------------------------------------------------

    # interactivity -----------------------------------------------------

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

    # highlight circle on single-click on plot -------------------------------
    observeEvent( input$plot_click, {

      point <- shiny::nearPoints(hist.dat(), input$plot_click,
                                 threshold = 10, maxpoints = 1, addDist = T)
      hilite.point(point)

      #selectRows(dataTableProxy("dt"), NULL) # need to send in DT proxy if I want this.
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

    # highlight based on DT selected row -------------------------------
    ' This should be in a DT module
    observeEvent( input$dt_rows_selected, {
      selected_row <- hist.dat()[ input$dt_rows_selected, ]
      hilite_reactive( selected_row )
      make_histogram()
    })'

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
mod_point.histogram_ui <- function(id){
  ns <- NS(id)

  tagList(
    div(
      style = "position: relative; cursor: default;",
      plotOutput(ns("point.hist"),
                 height = "450px",
                 # The following defines tags to capture user interaction, which can
                 # be retrieved i.e. through input$plot_hover:
                 hover = hoverOpts(ns("plot_hover"), delay = 1, delayType = "throttle"),
                 dblclick = ns("plot_dbl.click"),
                 click = ns("plot_click")),
      uiOutput(ns("point.hist_tooltip")))

    #width = 7, height = "600px")
  )
}


# minimalist geoseg point.hist app ------------------------------------------------

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
