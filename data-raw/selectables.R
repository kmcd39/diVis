
# selectables ------------------------------------------------------------------

# script defines baseline selection options, as oppossed to selectabilities/selection
# rules, that define which options are available when, based on prior selections

library(dplyr)

# devtools::load_all(export_all = F)


# build combined df for parsing out all possible options -----------------------
seln.dod <- dod %>%
  tidyr::pivot_longer(cols = c(contains("_e")),
                      names_to = "var.name",
                      values_to = "outcome")

# create super-df containing all outcomes
allm <- plyr::rbind.fill(metrics,
                         seln.dod) %>% tibble()

all.vars <- allm$var.name %>% unique()

# define options ---------------------------------------------------------------
selectables <- list()
#OUTCOMES
selectables$outcomes <- list("Opp Insights" = list("kfr 29 pooled" = "kfr_29_mean",
                                       "kfr 26 pooled" = "kfr_26_mean",
                                       "kfr pooled p25" ="kfr_p25",
                                       "kfr pooled p75" = "kfr_p75",
                                       "jail pooled mean" = "jail_mean"),
                 "Economic Indicators" = list("Household median income" = "hh.median.income",
                                              "Jobless rate" = "jobless_rate",
                                              "Unemployment rate" = "unemployment_rate",
                                              "Poverty rate" = "poverty_rate",
                                              "Labor force participation rate" = "lfpr"),
                 "Deaths of Despair" = list("Suicide rate per 100,000 people" = "srate_e"
                                            ,"Overdose deaths per 100,000 people" = "orate_e"
                                            ,"Suicide deaths" = "sdeaths_e"
                                            ,"Overdose deaths" = "odeaths_e"),
                 "Life Expectancy" = list("Life expectancy" = "e.0."),
                 "Demographics" = list("Percent non-white" = "perc.nonwhite"))

# INDICATORS
# geoseg::metrics %>% colnames()

selectables$indicators <- # indicators are calculated descriptive statistics; moran_i, gini, etc.
  c("outcome", colnames(metrics)[4:14])

# region types (CTs not selectable from dropdown menu)
selectables$region_types = allm %>% pull(region.type) %>% unique()
# shiny ui needs a list
names(selectables$region_types) = as.list(
  case_when(
  regions == "cbsa" ~ "Metro area",
  regions == "cz" ~ "Commuting Zone",
  regions == "us" ~ "National",
  TRUE ~ stringr::str_to_title(regions))
  )


# Division overlay options
# defined in div-fcns/ script

# Years
selectables$years <- allm$year %>% unique()

# gui selectables ---------------------------------------------------------
# color palette options
col_palettes <- list("v7" = colorspace::darken(viridis::viridis(7), .1),
                     "p7" = colorspace::darken(viridis::plasma(7)),
                     "hcl5" = colorspace::diverging_hcl(5, h = c(180, 50),
                                                        c = 80, l = c(20, 95),
                                                        power = c(0.7, 1.3)))
palette.options <- c("Heat by value" = "p7",
                     "Greens by value" = "v7",
                     #"Gradient greens" = "v11",
                     "BrGn by divergence" = "hcl5")

map.gui.opts <- list(col_palettes,
                     palette.options)

# ts layout options
ts.layout.options <- c("tiled" = "facet"
                       ,"ridgeline"
                       ,"aggregated" = "single")




# DEFAULTS + INITIAL VALUES -----------------------------------------------
# initial settings for options, and what it gets replaced to when "reset to
# defaults" button is pressed

defaults <- list(  outcome = selectables$outcomes$`Opp Insights`$`kfr 29 pooled`
                   ,indicator = "Gini"
                   ,region_type = "cz"
                   ,ts_layout = "ridgeline"
                   )


# write ------------------------------------------------------------------------


# usethis::use_data(defaults, internal = TRUE, overwrite = TRUE)

