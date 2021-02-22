
# add Deaths of Despair data ---------------------------------------------------


# add soc-ineq data -------------------------------------------------------
# rm(dod)
dod <- read.csv("data-raw/dod_ts.csv") %>% tibble()
dod$county =
  stringr::str_pad(dod$county, 5, "left", "0")
dod <- dod %>%
  select(-county_name) %>%
  geoseg::region.reorg("county")

dod.vars <- colnames(dod)[grepl("_e$", colnames(dod))]
# are there rows with NA regions?
dod[is.na(dod$region.id), ]

# xwalks to add more region ids (aggregate up by counties)
xwalks::co2cz
xwalks::state2div

# add czs
dod_agg <- left_join(dod,
                     xwalks::co2cz,
                     by = c("region.id" = "countyfp"))

dod_agg <- left_join(dod_agg,
                     xwalks::state2div,
                     by = c("state" = "abv"))
dod_agg$abv = dod_agg$state
dod_agg$state = dod_agg$statefp
(dod_agg <- dod_agg %>% select(-statefp))
dod_agg$national <- 1

# pre-process / aggregate outcomes to each new region level
dod_aggL <- list()
for (region.str in c("cz", "state", "division", "region", "national")) {
  # dod for region as list of df's; one for each var (each df in list containing both rates and counts)
  to.add <- purrr::map(c("population",
                         dod.vars[grepl("rate", dod.vars)]), # i only do rates because flexi aggr always keeps the counts
                       ~appHelpers::flexible_ts_aggregate(dod_agg, ., group_col = region.str))

  # reduce to single df with 1 colm per var
  to.add <- purrr::reduce(to.add, left_join, by = c("year", region.str)) %>%
    geoseg::region.reorg(region.str) %>%
    select(1:3, population,
           sdeaths_e, srate_e,
           odeaths_e, orate_e) %>% list()
  names(to.add) <- region.str
  # add to list w/ 1 df per region.type
  dod_aggL <- append(dod_aggL, to.add)
}

dod_aggL
# add source values for counties
dod_aggL$county <- dod

# collapse
dod <- do.call("rbind", dod_aggL)

# check
dod %>%
  group_by(region.type) %>%
  summarise(  n()
            , sum(sdeaths_e, na.rm = T)
            , sum(odeaths_e, na.rm = T))

# check
dod %>% count(region.type)


# drop non-included region types -----------------------------------------------

# seems to over-complexify to add divisions and regions
dod <- dod %>%
  filter(region.type != "region")

# write ------------------------------------------------------------------------

# rds for dev
saveRDS(dod, file = "R/data/dod.RDS")


# usethis::
