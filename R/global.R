library(shiny)
library(shinyWidgets)
library(shinyjs)
#library(shinydashboard)
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


#
#geo.list <- readRDS(file = "../data/geo.list.rds")
#metrics <- readRDS(file = "../data/metrics.RDS") #geoseg::metrics %>% rename(outcome = x)

# geo.list[[4]]

geo.list <- readRDS(file = "data/geo.list.rds")
metrics <- readRDS(file = "data/metrics.RDS")
cts <- readRDS(file = "data/cts.RDS")    #geoseg::cts

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
  env.src <- dir(path = srcd, pattern="\\.[rR]$", recursive=F)
  # source 'em
  map(env.src, ~source(paste0(srcd, .)))
}

map(src.dirs, source.in.dir)

db.pw
# get dev sets ----------------------------------------------------------------

source("R/dev_build-test-sets.R")

