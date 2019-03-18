# record crs in use, or if NULL return it
SOcrs <- function(crs = NULL) {
  if (!is.null(crs)) {
    options(SOmap.crs.inuse = crs)
    return(crs)
  }
  crs <- getOption("SOmap.crs.inuse")
  if (is.null(crs)) warning("No SOmap.crs.inuse")
  crs
}
