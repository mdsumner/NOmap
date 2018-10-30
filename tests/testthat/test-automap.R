context("test-automap")

disp_auto_map <- function() SOauto_map(c(100:110), c(-70:-60))

test_that("auto map returns data", {
  expect_s3_class(x <- disp_auto_map(), "SOmap")
  vdiffr::expect_doppelganger("Soauto_map basic", disp_auto_map)
  expect_that(sort(names(x)), equals(c("bathy", "coastline", "crs", "data", "graticule", "target")))
})
