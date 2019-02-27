
#' Southern Ocean plot
#'
#' @description
#' Reproject and add point layers to either SOmap or SOauto_map layers.
#'
#' @param lon
#' longitude object
#'
#' @param lat
#' lattitude object
#'
#' @param prj
#' starting projection (default = longlat)
#'
#' @param crs
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
#'  SOplot(lon=y, lat=x,crs = map$crs,pch=19,col=6)
#' }
#' @export
#'


SOplot<-function(lon, lat,prj, crs,add=TRUE,...){
  SObj<-SOproj(lon,lat,prj, crs)
  plot(SObj,add=add,...)
}
