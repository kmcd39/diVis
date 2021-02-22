#' get_available_indicators
#'
#' Based on outcome, get available indicators
get_available_indicators <- function(outcome) {

  avail <- allm %>%
    filter(var.name == !!outcome) %>%
    select_if(~any(!is.na(.))) %>%
    colnames()

  avail_indicators <- avail[avail %in% selectables$indicators]
  return(avail_indicators)
}


#' get_available_region.types
#'
#' get avaiable aggregation levels based on outcome and filter level -- cannot
#' aggregate to area larger than what you're filtering to
get_available_region.types <- function(outcome) {

  avail <- allm %>%
    filter(var.name == !!outcome) %>%
    pull(region.type) %>%
    unique()
  return(avail)
}

#' get_available_years
#'
#' Get available years, given outcome
get_available_years <- function(outcome) {

  avail <- allm %>%
    filter(var.name == !!outcome) %>%
    pull(year) %>%
    unique()

  #if (identical(avail, NA)) return(NA)
  avail <- avail %>% sort() %>% as.character()
  return(avail)
}
