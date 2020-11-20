

# petty but i'm curious ------------------------------------------------------------------------

# df vs tibble conversion from sf
microbenchmark::microbenchmark(
  data.frame(geo.list$cz), # df is faster but units still microseconds for both
  tibble(geo.list$cz)
)


# dim vs nrow from checking emptdy dfs
microbenchmark::microbenchmark(
  nrow(metrics[0,]) == 0, # the same
  dim(metrics[0,])[1] == 0
)


# bboxin -----------------------------------------------------------------------

microbenchmark::microbenchmark( # the same:)
  st_bbox(tmp),
  st_bbox(tmp.cts)
)

microbenchmark::microbenchmark( # comparable
  query.division(pdb, tmp.cts, "divs.places"),
  query.division(pdb, tmp, "divs.places")
)


# rendering outlines -----------------------------------------------------------
microbenchmark::microbenchmark(
  (leaflet() %>% addTiles() %>% addPolylines(data = tmp.plc)), # faster
  (leaflet() %>% addTiles() %>% addPolylines(data = st_union(tmp.plc))) # much slower
)

microbenchmark::microbenchmark(
  (leaflet() %>% addTiles() %>% addPolylines(data = st_boundary(tmp.plc))),
  (leaflet() %>% addTiles() %>% addPolylines(data = tmp.plc)) # still faster
)

# finding centroids ------------------------------------------------------------


fast.approx.centroid <- function(region) {
  bbox = as.list(st_bbox(region))
  lon = mean(bbox$xmax, bbox$xmin)
  lat = mean(bbox$ymax, bbox$ymin)
  return(list(lon = lon,
              lat = lat))
}


(st_centroid(tmp)$geometry %>% st_coordinates()) # [,"X"]
geosphere::centroid(as(tmp, 'Spatial'))
fast.approx.centroid(tmp)


microbenchmark::microbenchmark( # decently accurate; 2.5-3MS
  (st_centroid(tmp)$geometry %>% st_coordinates())
)
microbenchmark::microbenchmark(  # most accurate; 30 MS
  geosphere::centroid(as(tmp, 'Spatial'))
)
microbenchmark::microbenchmark(  # least accurate; 65 microseconds
  fast.approx.centroid(tmp)
)
