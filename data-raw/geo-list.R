## code to prepare `geo.list` dataset goes here

# geo.list stores different geography layers as a list of sf objects.
# Lists can easily be parsed by indexing from shiny input or other strings.
library(dplyr)
library(sf)
library(lwgeom)
rm(list = ls())

requireNamespace("divDat")
requireNamespace("xwalks")
# selectables$region_type

# czs --------------------------------------------------------------------------
(cz <- divDat::czs_simplified)

# counties ---------------------------------------------------------------------
(county <- divDat::counties_simplified)

# make minimal
county <- county %>%
  select(county = geoid,
         county_name = name)

# cbsas ------------------------------------------------------------------------
(cbsa <- divDat::cbsas)

# make minimal
cbsa <- cbsa %>% select(1,2,geometry)

# build list -------------------------------------------------------------------
geo.list <- list("cz" = cz,
                 "county" = county,
                 "cbsa" = cbsa)

# rmapsimplify -----------------------------------------------------------------
geo.list <-
  purrr::imap(geo.list,
              ~rmapshaper::ms_simplify(
                st_buffer(st_make_valid(.), 0)) )

# re-project -------------------------------------------------------------------
geo.list <-
  purrr::imap( geo.list,
               ~st_transform(., 4326 ) )


# remove part of alaska that stretches into eastern hemisphere -----------------
# also run st_make_valid
# i.e., see:
geo.list[['cz']]['cz'] %>% plot()
geo.list[['cz']]['cz'] %>%
  st_crop(c(xmin = -180, xmax = 40,
            ymin = 20, ymax = 80)) %>% plot()


geo.list <-
  purrr::map(geo.list,
             ~tibble(
               st_make_valid(
                 st_crop(st_sf(.),
                 c(xmin = -180, xmax = 40,
                   ymin = 20, ymax = 80))))
             )

# peek & write ------------------------------------------------------------------------
geo.list

# as RDS (for early development)
saveRDS(geo.list, file = "data/geo.list.RDS")


# geo.list <- readRDS("data/geo.list.RDS)
#usethis::use_data(geo.list
#                  ,overwrite = TRUE)
