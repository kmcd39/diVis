# census-tract  data
rm(list=ls())
library(sf)
library(lwgeom)
library(dplyr)
requireNamespace("geoseg")

cattr <- geoseg::cts
tibble(cattr)

# get trees too ----------------------------------------------------------------
treects = "~/R/trees2tracts/by-tract saves/treects_finished.csv"
treects = data.table::fread(treects) %>% tibble()

# ideally i re-generate the tree data from divDat::cts
treects$geoid = stringr::str_pad(treects$geoid, 11, "left", "0")

cattr = left_join(cattr
                  ,treects[ , c("geoid", "t.perc")])

cattr = tibble(cattr)

# add cbsa & concat county joiners ---------------------------------------------
ctx <- xwalks::ctx %>% select(-contains("cz"))

cattr <- left_join(cattr, ctx)

cattr$county = paste0(cattr$statefp, cattr$countyfp)


# get geos ---------------------------------------------------------------------

# state list
stateL = xwalks::state2div$statefp

# get simplified CTs
# note 2015!
Tcts = purrr::map_dfr(
  stateL,
  ~tigris::tracts(. ,
                  cb = T,
                  year = 2015 )
)
Tcts

#ctgeos <- divDat::prep_cts(Tcts)
colnames(Tcts) <- tolower(colnames(Tcts))
Tcts$name %>% as.numeric() %>% sort(decreasing = T) %>% head()
ctgeos <- Tcts %>%
  filter(as.numeric(name) < 9900) %>%
  select(c(1:3,geoid, aland, geometry))

# transform
ctgeos <- st_transform(st_sf(ctgeos), 4326)


# simplify FURTHER
# capital S for simplified
ctgeoS <-
  rmapshaper::ms_simplify(
    st_make_valid(ctgeos),
    keep = 0.05,
    keep_shapes = T)


# combine attr and geos ------------------------------------------------
sum(duplicated( cattr$geoid ))

# cattr <- distinct(cattr)

sum(duplicated( ctgeos$geoid ))
cattr[duplicated(cattr$geoid), ]


cts <- left_join(cattr,
                 ctgeoS[,c("geoid", "aland", "geometry")])



# simplify colnames / drop unused ------------------------------------------------------------

colnames(cts) <- gsub("_pooled_pooled_", "_", colnames(cts))
colnames(cts)[grepl(".+_n$", colnames(cts))]

cts <- cts %>% select(-matches(".+_n$|.+_se$"))
cts <- cts %>% select(-tractce)



# generate population density/tract --------------------------------------------

# aland 2 km
cts$aland = cts$aland / 1e6
cts$pop.dens = cts$population / cts$aland
cts$pop.dens = round(cts$pop.dens, 3)


# checks -----------------------------------------------------------------------
cts %>% summary()
nrow(ctgeoS) == nrow(ctgeos)

cts %>% purrr::map( ~sum(is.na(.)))
# sum(!st_is_valid(cts$geometry))
sum(st_is_empty(cts$geometry))

unmatched <- cts %>% filter(st_is_empty(.$geometry)) %>% select(geoid)
cts %>%
  filter(st_is_empty(.$geometry)) %>%
  select(1:10) %>% distinct()

# write ------------------------------------------------------------------------

# cts <- readRDS("data/cts.RDS")

saveRDS(cts,
         "data/cts.RDS")

usethis::use_data(cts
                  ,overwrite = TRUE)
