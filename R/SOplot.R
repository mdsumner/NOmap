
#' Southern Ocean plot
#'
#' @description
#' Reproject and add point layers to either SOmap or SOauto_map layers.
#'
#' @param x
#' longitude vector, or object with coordinates
#'
#' @param y
#' lattitude vector, or missing if x is an object
#'
#' @param source
#' starting projection (default = longlat)
#'
#' @param target
#' target projection (default = stereo)
#'
#' @param add
#' add layer to plot (default = TRUE)
#'
#' @param ...
#' other plot options
#'
#' @return
#' Produces at the very base a round bathymetry map of the southern hemisphere.
#'
#' @examples
#' \dontrun{
#'  x<-c(-70, -60,-50, -90)
#'  y<-c(-50, -75, -45, -60)
#'  map<-SOauto_map(x,y, input_lines = FALSE)
#'  map
#'  SOplot(x = x, y = y, target = map$projection,pch=19,col=6)
#' }
#' @export
#'


SOplot<-function(x, y = NULL, target = NULL, ..., source = NULL, add=TRUE){
  SObj <- SOproj(x = x, y= y, target = target, source = source, ...)
  plot(SObj, add=add, ...)
}
