
# basic ui params --------------------------------------------------------------

sidebar.width = 3
output.height = 640

# ui ---------------------------------------------------------------------------

gs_ui <- shiny::fluidPage(

    shinyjs::useShinyjs(),
    #tags$head(
     #  includeCSS("R/www/stylesheet.css")
    #),

    sidebarLayout(

      # base UI
      sidebarPanel(
        mod_geoseg_ui("gs", selectables),
        width = sidebar.width
      ),

      mainPanel(
        shinydashboard::tabBox(id = "main_display",
                               selected =  "map", #"distribution",
                               width = 12,
                               height = "100%",

                               tabPanel("map",

                                        # leaflet output
                                        leafletOutput("map",
                                                      height = output.height),

                                        # textbox to show region name when zoomed in
                                        mod_region2CTs_ui("gs"),
                                        # mod_parse_CT_ui("gs"),

                                        # div overlay UI
                                        mod_div_overlay_ui("gs", div.opts)),


                               tabPanel("distribution",
                                        # point histogram
                                        mod_population.filter_ui("gs"),
                                        mod_point.histogram_ui("gs"))

        ),
        width = 12 - sidebar.width
      )

    )
  )
