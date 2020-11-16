library(testthat)


simul.input <- list(outcome = "e.0.",
                    indicator = "Gini",
                    region_type = "cz")

# attr_subset.. ---------------------------------------------------
test_that("gs.attr_subset", {
  expect_type(
    attr_subset(simul.input)
    , "list" )
})




# bin & format -----------------------------------------------------------------

tmp <- attr_subset(simul.input)

test_that("gs.attr_subset", {
  expect_type(
    bin_and_format(tmp)
    , "list" )
})
