
library(testthat)


# make_display_label ---------------------------------------------------
test_that("make_display_label", {
  expect_equal(make_display_label(list(outcome = selectables$outcomes$`Opp Insights`$`kfr 29 pooled`,
                                               indicator = "Gini"),
                                          outcome.opts = selectables$outcomes),
               "Gini - kfr 29 pooled")
  expect_equal(make_display_label(list(outcome = selectables$outcomes$`Deaths of Despair`$`Suicide deaths`,
                                               indicator = "outcome"),
                                          outcome.opts = selectables$outcomes,
                                          change_in = T),
               "Change in Suicide deaths")
})


# formatting numbers for readability ---------------------------------------------------

big.numerics = c(1.37e5, 1.11e5, 6.7e4, 4.5e4, NA, 1.6e3)
small.numerics = c(.99, .75, .95432*4, NA, 288/7)

test_that("apply_rounding", {
  expect_equal(
    apply_rounding(big.numerics),
    appHelpers::q.format(round(big.numerics, -3), 0)
  )
  expect_equal(
    apply_rounding(small.numerics),
    appHelpers::q.format(small.numerics, 2)
  )
})



# map tooltips and zoom option -------------------------------------------------

make_tooltips
metrics$Gini
(metrics$var.name %>% unique())[1:5]
test.var = "e.0."
test.region = "cz"
test.indicator = "Gini"
simul.input <- list(outcome = test.var,
                    indicator = test.indicator,
                    region_type = test.region,
                    pop_weighted = FALSE)

test.gs.set <- parse.geoseg.data(simul.input, F, NULL) %>% head(2)

test_that("gs_tooltips", {
  expect_equal(
    make_tooltips(input = simul.input,
                  test.gs.set),
    c("<b>Gini - Life expectancy:</b> 0.02<br><b>population:</b> 609,299<br><b>(click to zoom)</b>"
      ,"<b>Gini - Life expectancy:</b> 0.02<br><b>population:</b> 262,159<br><b>(click to zoom)</b>"
    ))
})
