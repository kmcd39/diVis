# source("R/modules/mod_geoseg_leaflet.R")

# server -----------------------------------------------------------------
server <- function(input, output, session) {


  # global reactives -------------------------------------------------------------
  #show_CTs <- reactiveVal(NULL) # set to sf representing CTs to show--- NULL when showing larger areas


  # render base leaflet & define proxy -------------------------------------------
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Voyager,
                       options = providerTileOptions(noWrap = TRUE))
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
    leaflet_interaction$clicked_region <- click.hover_2region(input$map_shape_click, gs.out)
    leaflet_interaction$zoom_level <- input$map_zoom
  })

  # ---------------------------------------------------------------------------------

  # call modules ------------------------------------------------------------

  # parse core input using geoseg module
  c(gs.out, gs.palette) %<-%
    geoseg_server("gs")

  # send to map using leaflet module
  mod_geoseg_leaflet("gs", gs.out, show_CTs, gs.palette, prox)

  # get tract-level data to display, if relevant.
  #c(show_CTs, gs.palette) %<-%
  show_CTs <-
    mod_parse_CT_display("gs", leaflet_interaction, show_CTs, prox)

  observeEvent(show_CTs(), {
    print(head(show_CTs(), 1))
  })

}


# launch -----------------------------------------------------------------------
#shinyApp(ui, server)
full.app <- function() {
  shinyApp(ui, server)
}

full.app()
