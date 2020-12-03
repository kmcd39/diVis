


# mapping assignments ----------------------------------------------------------
gen.list <- list(a = 1, b = 2, c = 3)
purrr::map(gen.list, ~`<-`(., NULL))

# divs from db -----------------------------------------------------------------

library(dblinkr)



# checking qual color schemes --------------------------------------------------

library(Polychrome)
# i think one of these two...
swatch(kelly.colors()[3:22])
swatch(dark.colors())

div.colors <- kelly.colors()[3:22]


swatch(viridis::cividis(8))
swatch(viridis::plasma(7))
swatch(viridis::magma(7))
swatch(viridis::inferno(7))

# working out div overlay process ----------------------------------------------

tmp.cts

tmp.redlining$holc_grade = factor(tmp.redlining$holc_grade)

leaflet() %>%
  addTiles() %>%
  choropleth_draw(tmp.cts, "cts",
                  tmp.tooltips, tmp.pal,
                  opacity_from_pop.dens = T,
                  stroke = F) %>%
  ?addPolylines(data = #tmp.redlining,
                st_boundary(long.lat_buffer(tmp.redlining, -25)),
              color = colorFactor(redlining.colors,
                                      tmp.redlining$holc_grade)(tmp.redlining$holc_grade),
              opacity = 1,
              stroke = T,
              label = tmp.redlining$holc_grade)



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

# do.call w/ params practice  -------------------------------------------------------


bin.var_format_T <- function (x, ...)  {
  # browser()
  breaks <- get_breaks(x, ...)
  bin_from_breaks(x, breaks, ...)
}

unlist(gui_test)
bin.var_format_T(x, (gui_test))
do.call('bin.var_format_T',
        c(list(x), gui_test))
