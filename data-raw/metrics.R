library(dplyr)
rm(list= ls())
# take geoseg::metrics and add population totals


# get larger-region info from metrics ------------------------------------------

metrics <- geoseg::metrics
metrics %>% count(region.type)

# abv/standardize some varnames/region names
metrics$var.name <- gsub("_pooled_pooled_", "_", metrics$var.name)

metrics <- metrics %>% filter(!is.na(region.id)) # (where no cbsa defined)


# add year column --------------------------------------------------------------

# placeholder for when TS vars are added..
metrics$year = 2010

# checks -----------------------------------------------------------------------

metrics %>% purrr::map(~sum(is.na(.)))
metrics %>% filter(is.na(outcome)) %>% summary()
nrow(geoseg::metrics)
metrics %>% count(region.type)

# write ------------------------------------------------------------------------

# rds for dev
saveRDS(metrics, file = "R/data/metrics.RDS")

# rda for publishing
#usethis::use_data(metrics,
#                  overwrite = T)
