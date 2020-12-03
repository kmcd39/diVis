
# create test sets --------------------------------------------------------------
# for developing convenience
test.var = "e.0."
test.region = "cz"
test.indicator = "Gini"
simul.input <- list(outcome = test.var,
                    indicator = test.indicator,
                    region_type = test.region,
                    pop_weighted = FALSE)

tmp <- parse.geoseg.data(simul.input) %>% filter(region.name == "Philadelphia")

tmp.cts <- get_CTs_by_region(tmp, simul.input$outcome)

# divs
tmp.plc <- divDat::plc %>% st_transform(st_crs(tmp.cts)) %>% st_intersection(st_union(tmp.cts))
tmp.redlining <- divDat::redlining %>% st_transform(st_crs(tmp.cts)) %>% st_intersection(st_union(tmp.cts))

# quick mapping
tmp.tooltips <- make_tooltips(simul.input, tmp.cts, F)
tmp.pal <- colorFactor(viridis::plasma(7),
                       tmp.cts$binned_x)

# for visual tests -----------------------------------------------------------------

base.gs.choropleth <-
  create_leaflet_base() %>%
  choropleth_draw(tmp.cts, "cts",
                  tmp.tooltips, tmp.pal,
                  opacity_from_pop.dens = T,
                  stroke = F)


