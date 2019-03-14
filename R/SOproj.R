
#' Southern projection
#'
#' @description
#' Function for reprojecting data.
#'
#' @param lon
#' longitude object.
#'
#' @param lat
#' lattitude object.
#'
#' @param prj
#' starting projection (default = longlat)
#'
#' @param crs
#' target projection (default = stereo)
#'
#' @param data
#' optional data to be included
#'
#' @return
#' Produces at the very base a round bathymetry map of the southern hemisphere.
#'
#' @examples
#' \dontrun{
#'  x <- c(-70, -60,-50, -90)
#'  y <- c(-50, -75, -45, -60)
#'  pnts <- SOproj(lon = y, lat = x)
#'  SOmap2(CCAMLR = TRUE)
#'  plot(pnts, pch = 19, col = 3, add = TRUE)
#' }
#' @export
#' @importFrom reproj reproj
#' @importFrom raster projection<-
#' @importFrom sp coordinates<-
SOproj <- function(lon, lat, prj, crs, data){
  if (missing(lon) || missing(lat)) {
      stop("lon and lat must be provided")
  }
  if (missing(prj) || is.null(prj) || !nzchar(prj)) {
    message("No projection provided, assuming longlat")
    prj <- "+proj=longlat +datum=WGS84"
  }

  if (missing(crs)) {
    message("No CRS provided, assuming SOmap default")
    crs <- "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  }
  if (missing(data)) data <- 1
  xy0 <- reproj::reproj(cbind(lon, lat), target = crs, source = prj)
  df <- data.frame(x = xy0[,1], y = xy0[,2], data = data)
  sp::coordinates(df) <- c("x", "y")
  raster::projection(df) <- crs
  df
}

#' @importFrom raster projection
projection.SOmap <- function(x, asText = TRUE) {
 raster::projection(x$projection, asText = asText)
}
#' Reproject SOmap
#'
#' Reproject a SOmap object by specifying a 'target' projection string (PROJ4)
#'
#' See [reproj::reproj()] for details.
#'
#' @section Warning:
#' So many ...
#' @seealso reproj::reproj
#' @inheritParams reproj::reproj
#' @export
#' @export reproj
#' @aliases reproj reproj.SOauto_map
#' @importFrom reproj reproj
#' @importFrom raster projectRaster raster
#' @examples
#' set.seed(27)
#' amap <- SOauto_map()
#' reproj(amap, "+proj=moll")
#' reproj(amap, "+proj=laea +lat_0=-55 +lon_0=154 +datum=WGS84")
#'
#' bmap <- SOmap(Trim = -35)
#'
#' reproj(bmap, "+proj=stere +lat_0=-90 +lon_0=147 +lat_ts=-71 +datum=WGS84")
#' ## these aren't exactly ideal
#' reproj(bmap, "+proj=ortho +lat_0=-70")
#' reproj(bmap, "+proj=laea +lat_0=-55 +lon_0=154 +datum=WGS84")
reproj.SOmap <- function(x, target, ..., source = NULL) {
  if (missing(target)) stop("'target' projection string required")
  if (!is.null(source)) warning("source ignored, should be NULL for SOmap objects")
  rast <- try(raster::projectRaster(x$bathy$plotargs$x, crs = target), silent = TRUE)
  if (inherits(rast, "try-error")) {
    stop("unable to reproject raster sensibly")
  }
  x$bathy$plotargs$x <- rast
  x$target <- raster::raster(x$bathy$plotargs$x)
  ## box?
  x$coastline$plotargs$x <- sf::st_transform(x$coastline$plotargs$x, target)
  x$projection <- target
  x
}
#' @export
#' @name reproj.SOmap
reproj.SOauto_map <- function(x, target, ..., source = NULL) {
  if (missing(target)) stop("'target' projection string required")
  if (!is.null(source)) warning("source ignored, should be NULL for SOmap objects")
  x$bathy <- raster::projectRaster(x$bathy, crs = target)
  x$target <- raster::raster(x$bathy)
  ## box?
  if (!is.null(x$coastline$data)) x$coastline$data <- sp::spTransform(x$coastline$data, target)
  if (!is.null(x$graticule)) x$graticule <- sf::st_transform(x$graticule, target)

  if (!is.null(x$lines_data)) x$lines_data <- reproj(x$lines_data, target, source = x$projection)
  if (!is.null(x$points_data)) x$points_data <- reproj(x$points_data, target, source = x$projection)

  x$projection <- target
  x
}
