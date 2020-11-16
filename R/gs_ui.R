
# basic ui params --------------------------------------------------------------

sidebar.width = 3
output.height = 640

# ui ---------------------------------------------------------------------------
ui <- fluidPage(

  sidebarLayout(
    sidebarPanel(
      geoseg_ui("gs", selectables),
      width = sidebar.width
    ),

    mainPanel(
      leafletOutput("map",
                    height = output.height),

      mod_div_overlay_ui("gs", div.opts),

      width = 12 - sidebar.width
    )
  )
)
