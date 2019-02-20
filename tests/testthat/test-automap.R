context("test-automap")

disp_auto_map <- function() plot(SOauto_map(c(100:110), c(-70:-60)))

test_that("auto map returns data", {
    x <- SOauto_map(c(100:110), c(-70:-60))
    expect_s3_class(x, "SOmap")
    expect_identical(sort(names(x)), sort(c("bathy", "bathyleg", "bathy_palette", "coastline", "target", "lines_data", "points_data", "ppch", "pcol", "pcex", "contours", "levels", "contour_colour", "graticule", "crs")))
    vdiffr::expect_doppelganger("Soauto_map basic", disp_auto_map)
})

disp_resblocks <- function() plot(SOauto_map(SOmap_data$CCAMLR_research_blocks[c(1, 4, 5), ]))
test_that("auto map plots polygons", {
  vdiffr::expect_doppelganger("Soauto_map research blocks", disp_resblocks)
})

#SOmap
disp_somap <- function() SOmap()
test_that("SOmap plots", {
  vdiffr::expect_doppelganger("Somap basemap",
                              disp_somap)
})

#SOmap2
disp_somap2 <- function() SOmap2(CCAMLR=TRUE)
test_that("SOmap2 plots", {
  vdiffr::expect_doppelganger("Somap2 basemap",
                              disp_somap2)
})

#SOleg
disp_soleg <- function() {SOmap()
  SOleg(ticks=6, tlabs = seq(1:6))}
  test_that("SOmap legends", {
  vdiffr::expect_doppelganger("SOmap legends",
                              disp_soleg)
})


#SOmanagement
disp_soman <- function() {SOmap()
    SOmanagement(CCAMLR = TRUE)}
test_that("SOmap management", {
    vdiffr::expect_doppelganger("SOmap management",
                                disp_soman)
  })
