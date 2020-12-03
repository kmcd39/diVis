library(testthat)


# attr_subset.. ---------------------------------------------------

simul.input

# filter based on input to selected indicator
test_that("gs.attr_subset", {
  expect_s3_class(
    attr_subset(simul.input)
    , "data.frame" )
})




# bin & format -----------------------------------------------------------------

# additionally bin & filter columns
test_that("gs.bin_format", {
  expect_s3_class(
    bin_and_format(
      attr_subset(simul.input)
    )
    , "data.frame" )
})



# umbrella fcn for larger regions ----------------------------------------------

# also applies region.type input and adds spatial info
test_that("gs.umbrella_parse", {
  expect_s3_class(
    (parse.geoseg.data(simul.input)),
    "sf" )
  })


# CTs from region --------------------------------------------------------------
# given single regions, extract matching cts, and prep input$outcome column to display

tmp


tmp.cts <-
  get_CTs_by_region(tmp) %>% rename("x" = !!test.var)
tmp.cts <- bin_and_format(tmp.cts)
tmp.cts <- st_sf(tmp.cts)

parse.geoseg.data(simul.input)

