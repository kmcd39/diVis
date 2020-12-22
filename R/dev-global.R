
# DEV global -------------------------------------------------------------------

# this uses different relative file paths and might have some other changes compared
# to "global.R." This one is designed for sourcing while developing app; the other is
# for shiny deployment.

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

# rm(list=ls())
# get datasets -----------------------------------------------------------------

# for development, map through R/data sets. Eventually I'll clean up so can devtools
# load as a package

geo.list <- readRDS(file = "R/data/geo.list.RDS")
metrics <- readRDS(file = "R/data/metrics.RDS")
cts <- readRDS(file = "R/data/cts.RDS")    #geoseg::cts
dod <- readRDS("R/data/dod.RDS")
source("data-raw/selectabilities.R")

geo.list[1:3] <- imap(geo.list[1:3], ~rename(., region.id = 1, region.name = 2))

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
   "R/params/"
  ,"R/processing-fcns/"
  ,"R/modules/"
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

