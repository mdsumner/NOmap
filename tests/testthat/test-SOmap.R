context("test SOmap")
p <- SOmap(Trim = -45, Grats = TRUE, fronts = TRUE)

test_that("SOmap returns a SOmap object", {
    expect_s3_class(p, "SOmap")
  expect_silent(print(p))
})

test_that("adding data to a plot is fine", {
  longitudes <- c(-180, -90, 0, 90)
  latitudes <- c(-50, -60, -50,-60)
  SOplot(longitudes, latitudes, pch = 1:4)
})
