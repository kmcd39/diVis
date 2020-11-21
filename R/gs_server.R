

# server -----------------------------------------------------------------
gs_server <- function(input, output, session) {

  # global reactives -------------------------------------------------------------



  # render base leaflet & define proxy -------------------------------------------
  output$map <- renderLeaflet({
    create_leaflet_base()
  })
  # set proxy
  prox <- leafletProxy("map")
  # set initial zoom (lower 48 states)
  do.call('fitBounds',
          c(list(map = prox), l48bbox))

  # observer for map click + zoom
  # (seems to have to exist in main server call due to leaflet+shiny design )
  leaflet_interaction <- reactiveValues() #clicked_region = NULL, zoom_level = NULL)
  observeEvent( list(input$map_shape_click, input$map_zoom), {
    leaflet_interaction$click_info <- input$map_shape_click
    leaflet_interaction$zoom_level <- input$map_zoom
  })

  # -------------------------------------------------------------------------

  # call modules ------------------------------------------------------------

  # parse core input using geoseg module
  c(gs.out, gs.palette) %<-%
    mod_geoseg("gs")

  # send to map using leaflet module
  mod_geoseg_leaflet("gs", gs.out, show_CTs, gs.palette, prox)

  # get tract-level data to display, if relevant.
  show_CTs <-
    mod_parse_CT("gs", leaflet_interaction, gs.out, show_CTs, prox)

  # division overlay module
  mod_div_overlay_server("gs", show_CTs, prox)

  # filter population module
  pop.filtered.gs <-
    mod_population.filter("gs", gs.out)

  # point histogram module
  test <-
    mod_point.histogram("gs", pop.filtered.gs, gs.palette,
                        hilite.point = reactiveVal(NULL), change_in = FALSE)


  }


# launch -----------------------------------------------------------------------
#shinyApp(ui, server)
full.app <- function() {
  shinyApp(gs_ui, gs_server)
}

full.app()

