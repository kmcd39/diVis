
# create test sets --------------------------------------------------------------
# for developing convenience
test.var = "e.0."
test.region = "cz"
test.indicator = "Gini"
simul.input <- list(outcome = test.var,
                    indicator = test.indicator,
                    region_type = test.region,
                    pop_weighted = FALSE)

test.gs.dat <- parse.geoseg.data(simul.input)

test.single.region <- test.gs.dat %>% filter(region.name == "Philadelphia")

test.cts <- get_CTs_by_region(test.single.region, simul.input$outcome)

# divs
#tmp.plc <- divDat::plc %>% st_transform(st_crs(test.cts)) %>% st_intersection(st_union(test.cts))
tmp.redlining <- divDat::redlining %>% st_transform(st_crs(test.cts)) %>% st_intersection(st_union(test.cts))

# quick mapping
tmp.tooltips <- make_tooltips(simul.input, test.cts, F)
tmp.pal <- colorFactor(viridis::plasma(7),
                       test.cts$binned_x)

# for visual tests -----------------------------------------------------------------

base.gs.choropleth <-
  create_leaflet_base() %>%
  choropleth_draw(test.cts, "cts",
                  tmp.tooltips, tmp.pal,
                  opacity_from_pop.dens = T,
                  stroke = F)


base.gs.choropleth
