#' \pkg{SOmap}
#'
#' Create publication-quality Southern Ocean maps in a simple manner with multiple management layer options.
#'
#' @name SOmap-package
#' @docType package
#' @importFrom assertthat assert_that is.flag
#' @importFrom graphics contour lines par plot plot.new plot.window points text
#' @importFrom methods as
#' @importFrom raster aggregate contour crop extend extent ncell plot projectExtent projectRaster xmin xmax ymin ymax
#' @importFrom rgdal project
#' @importFrom sf st_graticule st_as_sf
#' @importFrom sp plot
#' @importFrom stats na.omit runif
#' @importFrom utils data head
NULL

