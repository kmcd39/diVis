tmp
simul.input

tmp.to.map <- parse.geoseg.data(simul.input)

tmp.tts <- make_tooltips(simul.input, tmp.to.map)

tmp.pal <- colorFactor(viridis::viridis(8),
                       domain = tmp.to.map$binned_x)

create_leaflet_base() %>%
  choropleth_draw(tmp.to.map,
                  "gs-dat",
                  tooltips = tmp.tts,
                  pal = tmp.pal,
                  color = "white",
                  weight = .5) %>%
    fitBounds(l48bbox$lng1, l48bbox$lat1,
              l48bbox$lng2, l48bbox$lat2)


# base histogram --------------------------------------------


base.hist.tmp <- parse.geoseg.data(simul.input) %>%
  appHelpers::prep_for_point_hist( bin_denom = 10 ) %>%
  mutate(color = tmp.pal(binned_x)) %>%
  arrange(desc(x)) %>% # do i need this?
  rename(!!make_display_label(simul.input) := x) %>%
  draw_point_hist(var.name = make_display_label(simul.input))

