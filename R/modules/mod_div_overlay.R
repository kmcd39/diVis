

# server module ----------------------------------------------------------------

#' mod_div_overlay_server Function
#'
#' @description Module to define server-side leaflet rendering
#'
#'
#'
#' @noRd
#'
#' @import dblinkr
mod_div_overlay_server <- function(id, show_CTs, proxy) {

  moduleServer(id, function(input, output, session) {

    # connect to overlay DB (or ensure divDat)  ----------------------------------

    # manage db connections
    con <- dblinkr::db.connect(db.usr, db.usr, pool = F)

    sessionId <- as.integer(runif(1, 1, 100000))
    output$sessionId <- renderText(paste0("Session id: ", sessionId))
    session$onSessionEnded(function() {
      DBI::dbDisconnect(con)
      cat(paste0("\nEnded: ", sessionId))
    })

    # update UI visibility & clear on zoom out --------------------------
    observeEvent(show_CTs(), ignoreNULL = F, {
      showing_CTs <- !is.null(show_CTs())

      # hide ui & clear div layer if zoomed out; show ui if zoomed in
      if( !showing_CTs ) {
        shinyjs::hide("all_div_ui")
        proxy %>% clearGroup("divs")
      } else if(showing_CTs)
        shinyjs::show("all_div_ui")
    })


    # lazy update div overlays -------------------------------------------
    div.overlays <- isolate(
      reactive({
        divs <- map(input$div.overlays,
                    ~dblinkr::persistent.query.division(con, show_CTs(),
                                                        paste0("divs.", .)))
        names(divs) <- input$div.overlays
        return(divs)
      }))

    # 'render' button events -- call update ------------------------
    observeEvent(input$render.divs, {

      # clear old layers
      proxy %>% clearGroup("divs")

      divs <- div.overlays()

      map2(divs, names(divs),
          ~add.div_fcn.index(proxy, .x, .y))
    })
  })
}



# ui module --------------------------------------------------------------------

#' mod_div_overlay_ui UI Function
#'
#' @description Module to define UI for manipulating the output dataset used
#'   throughout most of the app.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param div.opts Selectable options
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_div_overlay_ui <- function(id, div.opts){
  ns <- NS(id)
  shinyjs::useShinyjs()

  tagList(
    # divisions --
    shinyjs::hidden(div(
      id = ns("all_div_ui"),
      fluidRow(
        # select divisions
        selectizeInput(inputId = ns("div.overlays"),
                       label = strong("Select overlay features "),
                       choices = div.opts,
                       multiple = T),
        # render divisions button
        actionButton(ns("render.divs"), "Render Selections",
                     width = "50%")
      ),
      #actionBttn(inputId = ns("aggregate2div.polys"), )

      # RESET TO DEFAULT
      fluidRow(
        actionButton(ns("clear.divs"), "Clear Selection",
                     width = "80%"))
    ))
  )
}



# helper sets ------------------------------------------------------------------

# div options  (if using divDat)
# div.opts = data(package="divDat")$result[,"Item"]
# div.opts = div.opts[!grepl("(cz|count|cbsa|cts)", div.opts)]

# div options  (if using db)
# dblinkr::tbls.in.schema(con = pdb, "divs")
div.opts <- c("redlining",
              "rails_bts",
              "hwys",
              "places",
              "school_dists",
              "hwyPlan1947")


# helper fcns in processing-fcns/div-fcns/  ----------------------------------------------------------------------



#' get_sf_from.divDat
#'
#' Quick helper fcn that transforms string 'x' to 'divDat::x' and evaluates
get_sf_from.divDat <- function(div) {
  eval(parse(text = paste0("divDat::",div)))
}


#' add.div_fcn.index
#'
#' Wrapper fcn that just identifies what/if any column to color by for each division
#' layer, and passes that on to mapping fcn.
add.div_fcn.index <- function(proxy, div, div.str) {

  # escape if no divs
  if(nrow(div) == 0) return()

  ccol <- case_when(#div.str %in% c("places", "plc") ~ "NAME",
                    div.str == "sfr.polys" ~ "acre_sfr",
                    div.str == "redlining" ~ "holc_grade",
                    div.str == "hwys" ~ "SIGN1",
                    TRUE ~ as.character(NA) )


  proxy %>%
    leaflet.add_division_layer(div,
                               div.name = div.str,
                               color_col = ccol)
}
