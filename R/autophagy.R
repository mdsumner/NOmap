#' Map will eat itself (WIP)
#'
#' TODO:
#'  - deconstruct SOauto_map to separate the plotting and aesthetics handling from the data prep
#'  - etc.
#'
#'  @param x object to plot
#'
#' @param ...
#'
#' @examples
#' x <- SOauto_map()
#' autophagy(a)
autophagy <- function(x, ...) {
  UseMethod("autophagy")
}
#' @name autophagy
autophagy.SOmap <- function(x, ...) {
  plot_background(x)
  plot_polygons(x)
  plot_contours(x)
  plot_lonlat_lines(x)
  plot_lines(x)
  plot_points(x)

}

plot_background <- function(x, ...) {
  ramp2 <- grDevices::colorRampPalette(c("#54A3D1","#60B3EB","#78C8F0","#98D1F5","#B5DCFF","#BDE1F0","#CDEBFA","#D6EFFF","#EBFAFF","grey92","grey94","grey96", "white"))
  bluepal <- ramp2(45)
  raster::image(x$bathy, add = TRUE, col = bluepal, axes = FALSE)
}

plot_polygons <- function(x, ...) {
  plot(x$coastline, add = TRUE)
}
plot_contours <- function(x, ...) {
  levels <- c(-500, -1000, -2000)
  contour(x$bathy, nlevels=1, levels = levels, col="black", add= TRUE)
}
plot_lines <- function(x, ...) {
  lines(x$data)
}

plot_points <- function(x, ...) {
  points(x$data, col = "red", pch = 19)
}

plot_lonlat_lines <- function(x, ...) {
  target <- x$target

  grat <- sf::st_graticule(c(raster::xmin(target), raster::ymin(target), raster::xmax(target), raster::ymax(target)), crs = projection(target))
  op <- par(xpd = NA)
  plot_graticule(grat)
  par(op)

}
