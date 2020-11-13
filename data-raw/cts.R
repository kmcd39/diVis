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
  select(c(1:3,geoid, geometry))


# simplify FURTHER
# capital S for simplified
ctgeoS <-
  rmapshaper::ms_simplify(
    st_make_valid(ctgeos),
    keep = 0.04,
    keep_shapes = T)


# combine attr and geos ------------------------------------------------
sum(duplicated( cattr$geoid ))

# cattr <- distinct(cattr)

sum(duplicated( ctgeos$geoid ))
cattr[duplicated(cattr$geoid), ]


cts <- left_join(cattr,
                 ctgeoS[,c("geoid", "geometry")])

# checks -----------------------------------------------------------------------
cts %>% summary()

cts %>% purrr::map( ~sum(is.na(.)))
# sum(!st_is_valid(cts$geometry))
sum(st_is_empty(cts$geometry))

unmatched <- cts %>% filter(st_is_empty(.$geometry)) %>% select(geoid)
cts %>%
  filter(st_is_empty(.$geometry)) %>%
  select(1:10) %>% distinct()

# write ------------------------------------------------------------------------

saveRDS(cts,
         "data/cts.RDS")

usethis::use_data(cts
                  ,overwrite = TRUE)
