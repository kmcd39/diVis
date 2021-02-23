

#' bin_and_format
#'
#' Wraps steps common to any gs/ct data shown in app. Bins continuous variables and
#' gets a formatted column. Assumes column named 'x' to send output.
#' Flow-fcns/data-processing
#' @param df Dataframe to send to display
#' @param ... Add'l arguments passed to \code{bin.var_format}
#' @export bin_and_format
bin_and_format <- function(df, n_breaks = 7, ...) {

  df$binned_x <- appHelpers::bin.var_format(df$x, ...)

  # formatting & rounding -------------------------------------------------------
  df$formatted_x = df$x
  format_cols <- dplyr::intersect(c("formatted_x", "outcome"), colnames(df))
  df <- mutate(df,
               across(!!format_cols,
                      apply_rounding))
  return(df)
}





# CDC low-n suppression --------------------------------------------------------

#' CDC death data requests we suppress data that shows death data for areas where n <
#' 10. these are helper functions to do that.


#' get.cdc.keep.vars
#'
#' Returns list of columns to keep. Just one for requested output, unless a rate
#' calculated from a CDC variable is kept, and the count will also be retained to
#' ensure cdc data-use compliance. Compliance ensured by trimmed areas with counts
#' less than 10, with \code{......}
get.cdc.keep.vars <- function(input,
                              dod.vars = c("sdeaths_e", "srate_e", "odeaths_e", "orate_e"),
                              dod.counts = c("sdeaths_e", "odeaths_e")) {

  if (input$outcome %in% setdiff(dod.vars, dod.counts) &
      !input$change_in)
    keep_vars <- c(input$outcome, dod.counts)
  else
    keep_vars <- input$outcome

  return(keep_vars)
}

#' suppress.low.CDC.counts
#'
#' If CDC var selected, and we're not showing change-in, filter regions wheere n<10
suppress.low.CDC.counts <- function(df, input) {

  if ( !input$change_in ) {
    # suppress values were n<10, per CDC terms of use
    df <- df %>%
      mutate(x = ifelse(x<10, NA, x))
  }
  return(df)
}



# Handling time series ---------------------------------------------------------

#' get_change_in
#'
#' Get difference in var `x` at two years of time slider
get_change_in <- function(xdf, input) {

  starts <- xdf %>%
    filter(year == input$year_range[1])
  ends <- xdf %>%
    filter(year == input$year_range[2])

  change_df <- ends %>%
    select(c(contains("region."), "population"))
  change_df$x <- ends$x - starts$x

  return(change_df)
}
