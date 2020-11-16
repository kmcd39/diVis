
# checking ms_simplify params --------------------------------------------------

Tcts %>%
  tibble() %>%
  count(statefp, countyfp)

tmp = Tcts %>% filter(statefp=="01" & countyfp=="003")
library(mapview)
mapview(tmp, zcol = "geoid",
        legend = F)
tmp %>%
  rmapshaper::ms_simplify() %>%
  mapview(
    zcol = "geoid",
    legend = F)

tmp %>%
  rmapshaper::ms_simplify(keep = 0.05) %>%
  mapview(
    zcol = "geoid",
    legend = F)

#   ----------------------------------------------------------------------------


bin.var_format_T <- function (x, ...)  {
  # browser()
  breaks <- get_breaks(x, ...)
  bin_from_breaks(x, breaks, ...)
}

unlist(gui_test)
bin.var_format_T(x, (gui_test))
do.call('bin.var_format_T',
        c(list(x), gui_test))
