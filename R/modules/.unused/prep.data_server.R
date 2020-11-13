#' prep.data_server UI Function
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
prep.data_server <- function(id) {

  moduleServer(id, function(input, output, session) {


    # cdc data compliance
    # keep_vars <- get.cdc.keep.vars(input$outcome)
    keep_vars <- reactive(input$oucome)

    # subset based on agg_level and outcome/var.name
    out <- reactive(metrics[metrics$region.type == input$region_type &
                     metrics$var.name %in% keep_vars, ] %>%
      mutate(region.id = region.id) %>%
      select(c(1:2, "x" = input$indicator,
               "var.name", "outcome", "year"))
      )
    # if CTs are selected, you're showing outcome
    if ( showing_cts &
         input$indicator != "outcome")
      out <- out %>% select(-"x") %>% rename("x" = "outcome")

    # suppress <10 counts if necessary & save for time-series before year-trim
    out <- suppress.low.CDC.counts(out)

    return(out)

    attr <- attr_subset( input = input, showing_cts = F )

    # bin & apply spatial subset (binning happens before subset if coloring by national distribution; otherwise happens after)
    # joining to spatial information happens here too
    # if(input$indicator == "outcome" | parsed_agg_level() == "ct") out <- out %>% apply_rounding(input$outcome)
    # out <- subset_and_bin(input, out, geo_subset(), bin_breaks = n_breaks, highlight_top_percent = input$highlight_top)
    #  validate(need( class(spatial_set()$id) == class(out$region.id), "..."))


    return(attr)
  })
}

