library(testthat)

library(diVis)
# devtools::load_all(export_all = F)


# getting smaller regions ------------------------------------------------------
get_sub.regions

test_that("get sub regions", {
  expect_equal(
    get_sub.regions("region", region_types),
    c("division", "state", "cz", "county"))

  expect_equal(
    get_sub.regions("cz", region_types),
    c("county"))

  expect_equal(
    get_sub.regions("county", region_types),
    as.character())
})




