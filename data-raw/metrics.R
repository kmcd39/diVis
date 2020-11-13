library(dplyr)
rm(list= ls())
# take geoseg::metrics and add population totals
cts <- geoseg::cts

pops <- cts %>%
  select(1:8)


pops <- pops %>%
  left_join(xwalks::ctx[,c(4:8)]
            #,by = c("geoid")
            )

#library(purrr)
#map(pops, ~sum(is.na(.)))

pops$county = paste0(pops$statefp, pops$countyfp)
pops <- pops %>% select(geoid, county, cz, cbsa, population, hh)

cz.pops <- pops %>% group_by(cz) %>% summarise(across(c(population, hh), sum))
cbsa.pops <- pops %>% group_by(cbsa) %>% summarise(across(c(population, hh), sum))
county.pops <- pops %>% group_by(county) %>% summarise(across(c(population, hh), sum))

popL <- purrr::map2(list(cz.pops, cbsa.pops, county.pops),
                    list("cz", "cbsa", "county"),
                    ~geoseg::region.reorg(.x, .y))

popL

pops <- do.call('rbind', popL)
geo
metrics <- geoseg::metrics
metrics$var.name <- gsub("_pooled_pooled_", "_", metrics$var.name)
metrics$region.type <- if_else(metrics$region.type == "countyfp",
                               "county",
                               metrics$region.type)

metrics <- left_join(metrics,
                     pops)

metrics <- rename(metrics, outcome = x)
metrics <- metrics %>% filter(!is.na(region.id)) # (where no cbsa defined)


# checks -----------------------------------------------------------------------

metrics %>% purrr::map(~sum(is.na(.)))
metrics %>% filter(is.na(outcome)) %>% summary()
nrow(geoseg::metrics)

# write ------------------------------------------------------------------------

# rds for dev
saveRDS(metrics, file = "data/metrics.RDS")

# rda for publishing
#usethis::use_data(metrics,
#                  overwrite = T)
