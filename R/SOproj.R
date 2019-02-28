
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
#'  x<-c(-70, -60,-50, -90)
#'  y<-c(-50, -75, -45, -60)
#'  pnts<-SOproj(lon = y, lat = x)
#'  SOmap2(CCAMLR=T)
#'  plot(pnts, pch=19, col=3, add=TRUE)
#' }
#' @export
#'

SOproj<-function(lon, lat,prj, crs, data){
  if(missing(lon) || missing(lat)){
    stop("lon and lat must be provided")}
  if(missing(prj) || is.null(prj) || !nzchar(prj)){
    message("No projection provided, assuming longlat")
    prj<-"+proj=longlat +datum=WGS84"}

  if(missing(crs)){message("No CRS provided assuming SOmap default")}
  if(missing(data)){data<-1}
  df<-data.frame(lon=lon,lat=lat, data=data)
  coordinates(df)<-c("lon","lat")

  projection(df)<-prj
  if(missing(crs)){crs<-"+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"}
  dft<-spTransform(df,CRS(crs))
}
