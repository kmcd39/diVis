source("data-raw/selectables.R")
# define "selectability" rules -------------------------------------------------

# i.e., when x outcome is selected, are time series options enabled? Is zoom in? What
# indicators are available?

# this script will sometimes need to be re-run and maybe modified when input datasets
# are changed

# (...or is this the design?)

# selectability rules
seln.rules <- list()

# gs vars ----------------------------------------------------------------------

seln.rules$gs_vars <- unique(metrics$var.name)

seln.rules$dod_vars <- colnames(dod)[c(5:8)]
#  counts used to suppress low counts for cdc compliance
seln.rules$dod_counts <- seln.rules$dod_vars[grepl("deaths", seln.rules$dod_vars)]


# seln.rules$ts_vars is defined below

#   ----------------------------------------------------------------------------

# get available x fcns ----------------------------------------------------

# indicators, based on outcome --------------------------------------------
# all.indicators:
selectables$indicators
# get available indicators based on selected outcome
get_available_indicators <- function(outcome) {

  avail <- allm %>%
    filter(var.name == !!outcome) %>%
    select_if(~any(!is.na(.))) %>%
    colnames()

  avail_indicators <- avail[avail %in% selectables$indicators]
  return(avail_indicators)
}


# get avaiable aggregation levels based on outcome and filter level
#  -- cannot aggregate to area larger than what you're filtering to
get_available_region.types <- function(outcome) {

  avail <- allm %>%
    filter(var.name == !!outcome) %>%
    pull(region.type) %>%
    unique()

  return(avail)
}

get_available_years <- function(outcome) {

  avail <- allm %>%
    filter(var.name == !!outcome) %>%
    pull(year) %>%
    unique()

  #if (identical(avail, NA)) return(NA)
  avail <- avail %>% sort() %>% as.character()
  return(avail)
}

# map above to create selectability matrix --------------------------------------


seln.matrix <-
  tibble(
    outcome = all.vars
    ,avail_indicators = purrr::map(all.vars,
                                   get_available_indicators)
    ,avail_region.types = purrr::map(all.vars,
                                     get_available_region.types)
    ,avail_years = purrr::map(all.vars,
                              get_available_years)
  )


seln.rules$ts_vars <-
  seln.matrix %>%
  filter(lengths(avail_years) > 1) %>%
  pull(outcome)

# add default years, now that they're available
# think will delete b/c overcomplex; will just define as first and last
'
seln.matrix[seln.matrix$outcome=="srate_e",]$avail_years %>%
  unlist() %>% `[`(-1)
# an insane way of doing it or just so it updates automatically and little is hardcodes.
defaults$year <- seln.matrix[seln.matrix$outcome==defaults$outcome,]$avail_years %>%
  unlist() %>% tail(1)

defaults$year_range <-
  c((seln.matrix[seln.matrix$outcome==seln.rules$ts_vars[1],]$avail_years %>%
      unlist() %>% head(1)),
    (seln.matrix[seln.matrix$outcome==seln.rules$ts_vars[1],]$avail_years %>%
      unlist() %>% tail(1))
  )

'
