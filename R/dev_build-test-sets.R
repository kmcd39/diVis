
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

tmp.cts <- get_CTs_by_region(tmp) %>% rename("x" = !!test.var)

tmp.cts <- bin_and_format(tmp.cts)
tmp.cts <- st_sf(tmp.cts)

# divs
tmp.plc <- divDat::plc %>% st_transform(st_crs(tmp.cts)) %>% st_intersection(st_union(tmp.cts))
tmp.redlining <- divDat::redlining %>% st_transform(st_crs(tmp.cts)) %>% st_intersection(st_union(tmp.cts))

# quick mapping
tmp.tooltips <- make_tooltips(simul.input, tmp.cts, F)
tmp.pal <- colorFactor(viridis::plasma(7),
                       tmp.cts$binned_x)


# visual tests -----------------------------------------------------------------
base.gs.choropleth <-
  leaflet() %>%
  addTiles() %>%
  choropleth_draw(tmp.cts, "cts",
                  tmp.tooltips, tmp.pal,
                  opacity_from_pop.dens = T,
                  stroke = F)

base.gs.choropleth

base.gs.choropleth %>%
  leaflet.add_division_layer(tmp.plc,
                             div.name = "plc",
                             color_col = "NAME"
                             )


base.gs.choropleth %>%
  leaflet.add_division_layer(tmp.redlining,
                             div.name = "redlining",
                             color_col = "holc_grade")

