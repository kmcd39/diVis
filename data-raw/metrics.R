library(dplyr)
rm(list= ls())
# take geoseg::metrics and add population totals


# get population totals by region ----------------------------------------------
cts <- geoseg::cts

pops <- cts %>%
  select(1:8)

pops <- pops %>%
  left_join(xwalks::ctx[,c(4:8)]
            )

#library(purrr)
#map(pops, ~sum(is.na(.)))
pops$county = paste0(pops$statefp, pops$countyfp)
pops <- pops %>% select(geoid, county, cz, cbsa, population, hh)

cz.pops <- pops %>% group_by(cz) %>% summarise(across(c(population, hh), sum))
cbsa.pops <- pops %>% group_by(cbsa) %>% summarise(across(c(population, hh), sum))
county.pops <- pops %>% group_by(county) %>% summarise(across(c(population, hh), sum))

# make wide by region.type/region.id
(popL <- purrr::map2(list(cz.pops, cbsa.pops, county.pops),
                    list("cz", "cbsa", "county"),
                    ~geoseg::region.reorg(.x, .y)))

pops <- do.call('rbind', popL)


# get larger-region info from metrics ------------------------------------------
metrics %>% tibble() %>% select(x)
metrics <- geoseg::metrics
# abv/standardize some varnames/region names
metrics$var.name <- gsub("_pooled_pooled_", "_", metrics$var.name)
metrics$region.type <- if_else(metrics$region.type == "countyfp",
                               "county",
                               metrics$region.type)
# add population totals
metrics <- left_join(metrics,
                     pops)

metrics <- rename(metrics, outcome = x)
metrics <- metrics %>% filter(!is.na(region.id)) # (where no cbsa defined)


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
