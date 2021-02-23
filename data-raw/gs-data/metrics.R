library(tidyverse)
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


# add safegraphSeg stuff -------------------------------------------------------
'
metrics
ddir <- paste0(
  Sys.getenv("drop_dir"),
  "seg-measures/"
  )
fn <- list.files(ddir,
                 pattern = "CZ_seg\\.") # _with_intratract
czseg <-
  vroom::vroom(paste0(ddir, fn))
fn <- list.files(ddir,
                 pattern =
                   "CBSA_seg\\.") # _with_intratract

cbsaseg <-
  vroom::vroom(paste0(ddir, fn))

czseg <-
  geoseg::region.reorg(
  czseg,
  "cz"
  ) %>% select(-cz_name)

cbsaseg <-
  geoseg::region.reorg(
    cbsaseg,
    "cbsa"
  ) %>% select(-cbsa_name)

seg <- rbind(czseg, cbsaseg)

metrics <- metrics %>%
  left_join(seg,
            by = c("region.type", "region.id"))
'
# write ------------------------------------------------------------------------

# rds for dev
'saveRDS(metrics,
        file = "R/data/metrics.RDS")
'
# rda
usethis::use_data(metrics,
                  overwrite = T)
