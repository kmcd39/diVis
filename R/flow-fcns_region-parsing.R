# this might not be necessary in refactor...


#' setup region hierarchy to control selectability flow
#' get_sub.regions("region")
#' get_sub.regions("county")
# old: region_types <- c("NULL", "region", "division", "state_name", "county_name")
'region_types <- c("NULL", "region", "division", "state", "cz", "county")
get_sub.regions <- function(region, hierarchy = region_types) {
  # otherwise, return all between selected and last, excluding selected
  subs <- hierarchy[which(hierarchy == region )+ 1:length(hierarchy)]
  return(subs[!is.na(subs)])
}

'
