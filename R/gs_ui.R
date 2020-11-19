
# basic ui params --------------------------------------------------------------

sidebar.width = 3
output.height = 640

# ui ---------------------------------------------------------------------------
ui <- fluidPage(

  shinyjs::useShinyjs(),

  sidebarLayout(

    # base UI
    sidebarPanel(
      geoseg_ui("gs", selectables),
      width = sidebar.width
    ),

    mainPanel(
      # leaflet output
      leafletOutput("map",
                    height = output.height),

      # output box w/ zoom-in region
      mod_parse_CT_ui("gs"),

      # div overlay UI
      mod_div_overlay_ui("gs", div.opts),

      width = 12 - sidebar.width
    )
  )
)
