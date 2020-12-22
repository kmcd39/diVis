source("R/dev-global.R")
source("R/dev_build-test-sets.R")

test.cts

# get available indicators based on aggregation level and selected outcome


get_available_indicators <- function(agg_level, outcome) {

  avail <- dat %>%
    filter(region.type == !!agg_level &
             var.name == !!outcome) %>%
    select_if(~any(!is.na(.))) %>%
    colnames()

  avail_indicators <- avail[avail %in% value.cols]
  return(avail_indicators)
}

get_available_outcomes <- function(agg_level) {
  avail <- dat %>%
    filter(region.type == !!agg_level) %>%
    pull(var.name) %>% unique()

  avail <- avail[!grepl("_weighted", avail)]
  return(OUTCOME_OPTIONS[which(OUTCOME_OPTIONS %in% avail)])
}
