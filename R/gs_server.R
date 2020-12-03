

# server -----------------------------------------------------------------
gs_server <- function(input, output, session) {

  # global reactives -------------------------------------------------------------

  # region can be zoomed into to show CT bkdwn. Option to zoom can happen various
  # places, so stored globally as reactive list
  selected_region <- reactiveVal(NULL)
  selected_CTs <- reactiveVal(NULL)

  # render base leaflet & define proxy & interaction list -------------------------
  output$map <- renderLeaflet({
    create_leaflet_base()
  })
  # set proxy
  prox <- leafletProxy("map")
  # set initial zoom (lower 48 states)
  do.call('fitBounds',
          c(list(map = prox), l48bbox))

  # interaction seems to have to exist in main server call due to leaflet+shiny
  # design so I save as reactive list. I'm not sure why this is and am curious
  leaflet_interaction <- reactiveValues()

  # observer for map click + zoom. Can pass this onto modules where necessary.
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
  mod_geoseg_leaflet("gs",
                     gs.out, show_CTs = selected_CTs,
                     gs.palette, prox)

  # parse leaflet interaction. This sets selected_region when user zooms out or
  # clicks a region.
  mod_parse_leaflet.interaction("gs",
                                gs.out,
                                show_CTs = selected_CTs,
                                leaflet_interaction = leaflet_interaction,
                                selection.reactive = selected_region,
                                prox)

  # updates selected_CTs when selected_region is updated
  mod_region2CTs("gs",
                 region.reactive = selected_region,
                 CT.reactive = selected_CTs)

  # division overlay module
  mod_div_overlay_server("gs", selected_CTs, prox)

  # filter population module
  pop.filtered.gs <-
    mod_population.filter("gs", gs.out)

  # point histogram module
  #observe({
  #  req(input$main_display == "distribution")
    mod_point.histogram("gs",
                        pop.filtered.gs,
                        gs.palette,
                        selection.reactive = selected_region,
                        hilite.point = reactiveVal(NULL),
                        change_in = FALSE)
  #})



  #observeEvent(region_picks$from_leaflet(), { #reactiveValuesToList(region_picks), {
    #browser()
  #  print("!!!")
   # CT_selection <-
    #  mod_region2CTs("gs", region_picks)
  #}, ignoreInit = T)
  #observeEvent( CTs_from_hist(), {
  #  if(!is.null(CTs_from_hist())) # (don't zoom out due to hist interaction)
  #    CT_selection( CTs_from_hist() )
  #})

  }


# launch -----------------------------------------------------------------------
#shinyApp(ui, server)
full.app <- function() {
  shinyApp(gs_ui, gs_server)
}

full.app()

