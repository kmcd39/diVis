library(shiny)
# basic module for setting arbitrary value -------------------------------------


mod_set.value <- function(id, x) {

  moduleServer(id, function(input, output, session) {

    #val <- reactiveVal(NULL)

    observeEvent(input$value_slider, {

      #val <- input$value_slider    # val( input$value_slider )
      #val( input$value_slider )
      x(input$value_slider )#val())


    })
    #return(val)
  })
}

mod_set.value_ui <- function(id) {
  ns <- NS(id)

  tagList(
    sliderInput(ns("value_slider"), label = strong("set value"),
                    min = 0, max = 1500,
                value = 500,
                step = 25)
  )
}




ui <- fluidPage(
  mod_set.value_ui("first"),
  mod_set.value_ui("second"),

  textOutput("either")

)

server <- function(input, output, session) {

  # global reactives -------------------------------------------------------------
  all.values <- reactiveValues()
  lastest.value <- reactiveVal(NULL)


  # modularized possibilities ----------------------------------------------------
  all.values$first <- mod_set.value("first", x = lastest.value)
  all.values$second <- mod_set.value("second", x = lastest.value)

  #lastest.value <- mod_set.value("first")
  #lastest.value <- mod_set.value("second")


  # retrieve latest --------------------------------------------------------------
  #observe()
  observeEvent(lastest.value(),{

    print( lastest.value() )

  })


  # send to output ---------------------------------------------------------------
  output$either <- renderText({
    req( lastest.value() )
    lastest.value()
  })

}


shinyApp(ui, server)






# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------


# basic leaflet wtf is happening -----------------------------------------------
library(sf)
library(shiny)
library(leaflet)
ui <- fluidPage(

  leafletOu

)


