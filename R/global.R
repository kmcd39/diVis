
# (shiny) global -------------------------------------------------------------------

# this uses different relative file paths and might have some other changes compared
# to "dev-global.R." This one is designed for shiny deployment rather than sourcing
# while developing app.

library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinydashboard)
library(leaflet)
library(DT)
library(ggplot2)
#library(ggiraph)

library(sf)
library(purrr)
library(dplyr)

library(zeallot)


# add'l shinyapps.io setup -----------------------------------------------------
# cairo graphical library
# following discussion: https://stackoverflow.com/questions/26285786/ggplot2-graph-quality-in-shiny-on-shinyapps-io
#install.packages("Cairo")
library(Cairo)
options(shiny.usecairo=T)


# rm(list=ls())
# get datasets -----------------------------------------------------------------


# for deployment (shiny path relative to "./R/")
# can maybe eventually bundle with pkg or host on DB or smthing
# dif sourcing for modules below too as well
geo.list <- readRDS(file = "data/geo.list.RDS")
metrics <- readRDS(file = "data/metrics.RDS") #geoseg::metrics %>% rename(outcome = x)
cts <- readRDS(file = "data/cts.RDS")


geo.list[1:3] <- imap(geo.list[1:3], ~rename(., region.id = 1, region.name = 2))



# explicitly set CRS for shiny deployment --------------------------------------

# sometimes PROJ bundled with spatial data isn't transferred to shinapps.io server so
# must be set explicitly in script
for(r in geo.list){
  st_crs(r) <- 4326
}
cts <- cts %>% st_sf()
st_crs(cts) <- 4326
cts <- tibble(cts)
# minor helper sets ------------------------------------------------------------

# (now in appHelpers)
bbox_to_lnglat <- function(bbox, padding = 1) {

  bbox <- as.list(bbox)

  list(lng1 = bbox$xmax + padding,
       lng2 = bbox$xmin - padding,
       lat1 = bbox$ymax + padding,
       lat2 = bbox$ymin - padding)
}

# appHelpers::lower_48
states <- tigris::states(cb = T)
l48 = states[states$STATEFP %in% appHelpers::lower_48, ]
l48bbox <- st_bbox(l48) %>% bbox_to_lnglat(padding = 0)


# get helper fcns ------------------------------------------------------------------

# gets all R scripts in these subfolders and sources them
src.dirs <- c(
   "params/"
  ,"processing-fcns/"
  ,"modules/"
)

source.in.dir <- function(srcd) {
  # get all .R scripts in specified dirs/subdirectories
  env.src <- dir(path = srcd, pattern="\\.[rR]$", recursive=F,
                 all.files = TRUE)
  # source 'em
  map(env.src, ~source(paste0(srcd, .)))
}

map(src.dirs, source.in.dir)

# to get development sets ----------------------------------------------------------------

#  source("R/dev_build-test-sets.R")

