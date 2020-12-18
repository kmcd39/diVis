library(dplyr)

# devtools::load_all(export_all = F)
# load("data/geo.list.rda")
#OUTCOMES

outcomes <- list("Opp Insights" = list("kfr 29 pooled" = "kfr_29_mean",
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

indicators <- # indicators are calculated descriptive statistics; moran_i, gini, etc.
  colnames(metrics)[4:15]

# larger region types
region_type <- list("Commuting Zone" = "cz",
                     "County" = "county",
                     "Metro Area" = "cbsa")

# Division overlay options
# defined in div-fcns/ script

# to check state of database:
selectables <- list(outcomes = outcomes,
                    indicators = indicators,
                    region_type = region_type)


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

defaults <- list(  outcome = outcomes$`Opp Insights`$`kfr 29 pooled`
                  ,indicator = "Gini"
                  #area_filter = FILTER_OPTIONS$countrywide[1], # FILTER_OPTIONS["countrywide"],
                  ,region_type = "cz"
                  #local_zoom = no_zoom[1],
                  #ts_layout = "ridgeline",
                  #div_overlay = "None"
                  )


# write ------------------------------------------------------------------------


# usethis::use_data(defaults, internal = TRUE)

# usethis::use_data(x, mtcars, internal = TRUE)
