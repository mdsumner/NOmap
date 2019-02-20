
#' SOleg creating rounded legends for SOmap
#'
#' @param x
#' Object to obtain min and max values from for type='continuous' default=NULL
#' @param position
#' Where you want the legend ("topleft","topright", "bottomright")
#' @param col
#' Color pattern to use.
#' @param ticks
#' Number of ticks to include on the legend. Only used with type='continuous'
#' @param tlabs
#' Tick labels. Needed for type='discrete' optonal for type='continuous' if x is given
#' @param breaks
#' Numeric vector to create legend ticks for type='continuous' if x is given eg breaks<-c(1,2,3)
#' @param Trim
#' Trim that was used to create the SOmap.
#' @param label
#' Legend label.
#' @param type
#' Type of legend to be plotted 'discrete' or 'continuous' default='discrete'
#' @param ladj
#' Distance to adjust the tick labels from the ticks. default = 0.5
#' @param lcex
#' Size of the tick labels. default = 0.75
#' @param lsrt
#' Angle of the tick labels. default = 0
#' @param tadj
#' Distance to adjust the title from the ticks. default = 0.5
#' @param tcex
#' Size of the title text. default=1
#' @param rnd
#' optional rounding factor for continuous legends using the round() function. default = NULL.
#'
#' @return
#' Creates rounded legends
#' @export
#' @importFrom grDevices heat.colors
#' @examples
#' \dontrun{
#' #Discrete Legend
#'
#' SOleg(position="topleft",
#'       col=viridis::viridis(5),
#'       tlabs = c("a","b","c","d", "e"),
#'       Trim=-45,
#'       label="Species")
#'
#' #Continuous Legend
#'
#' nums<-runif(100)
#'
#' brks<-c(0.1,0.2,0.5,0.9)
#'
#' SOleg(x=nums,
#'       position="topright",
#'       col=viridis::viridis(80),
#'       breaks=brks
#'       Trim=-45,
#'       label="Species"
#'       rnd=1,
#'       type="continuous")

#' }

SOleg <-function(x=NULL,
                 position="topright",
                 col= NULL,
                 ticks=NULL,
                 tlabs=NULL,
                 breaks=NULL,
                 Trim=-45,
                 type="discrete",
                 label="",
                 ladj=0.5,
                 lsrt=0,
                 lcex=0.75,
                 tadj=0.5,
                 tcex=1,
                 rnd=NULL){

  if(is.null(col)){col<-c("#440154FF",  "#3E4A89FF",  "#26828EFF", "#35B779FF",  "#B4DE2CFF" )}

  ## data
  Bathy <- NULL
  data("Bathy", package = "SOmap", envir = environment())
  if(type=="continuous" && !is.null(ticks) && !is.null(breaks)&& length(breaks)!= ticks ){
    stop("Number of ticks and breaks do not match. You do not need to use ticks if you have breaks")
  }

  if(type=="continuous" & !is.null(breaks)){ if (!inherits(breaks, "numeric")){
    stop("Breaks must be numeric or integer")
  }}


  if(type=="continuous" && is.null(ticks) && !is.null(breaks)){
    ticks<-length(breaks)
  }

  if(type=="continuous" && is.null(ticks) && is.null(breaks)){
    stop("Ticks number needs to be set for continuous legends")
  }

  if(type=="continuous" && !is.null(ticks) && !is.null(tlabs) && length(tlabs)!= ticks){
    stop("Number of ticks and labels do not match")
  }

  if(type=="continuous" && is.factor(x)==TRUE || is.character(x)==TRUE){
    stop("Discrete variable given to continuous legend. Try type='discrete'")
  }

  if(type=="discrete" && is.discrete(col)==FALSE){
    stop("Continuous colors given for discrete variable")
  }

  if(type=="discrete"){
    qbins   <-length(tlabs)
    qtadjust<-(80/length(tlabs))/2} #how far in to move the tick marks each end.
  qticks  <-length(tlabs)#(80-qtadjust)/(length(tlabs)-1)} #how far between ticks. Currenty deprecated.
  cols<-col


  if(type=="continuous"){
    qbins<-80
    qticks<-ticks
    qtadjust<-0

    if(is.discrete(cols)) {
      ramp<-grDevices::colorRampPalette(col)
      cols<-ramp(80)} else(cols<-col(80))
    if(!is.null(x) & is.null(tlabs) & !inherits(x, "BasicRaster")){
      lmins<-min(x)
      lmax<-max(x)
      lbs<-seq(from=lmins,to= lmax, length.out = ticks)
      if(is.null(rnd)==FALSE){lbs<-base::round(lbs, digits = rnd)}
      tlabs<-as.character(lbs)
    }
    if(!is.null(x) & is.null(tlabs) & inherits(x, "BasicRaster")){
      lmins<-raster::cellStats(x,stat='min', na.rm=T)
      lmax<-raster::cellStats(x, stat='max', na.rm=T)
      lbs<-seq(from=lmins,to= lmax, length.out = ticks)
      if(is.null(rnd)==FALSE){lbs<-base::round(lbs, digits = rnd)}
      tlabs<-as.character(lbs)
    }
    if(is.null(x) & is.null(tlabs) & !is.null(breaks)){
      lmins<-min(breaks)
      lmax<-max(breaks)
      lbs<-breaks
      #if(is.null(rnd)==FALSE){lbs<-base::round(lbs, digits = rnd)}
      tlabs<-as.character(lbs)
    }
  }


  switch(position,
         "topright"   ={jklons<-seq(  4,  86, by=1);
         bllons<-seq(  5,  85, length.out=qbins+1);
         btlons<-seq(  5+qtadjust,  85-qtadjust, length.out=qticks);
         lablon<- 45;
         SRT   <--45;
         strt  <-  5},
         "topleft"    ={jklons<-seq(274, 356, by=1);
         bllons<-seq(275, 355, length.out =qbins+1);
         btlons<-seq(275+qtadjust, 355-qtadjust, length.out=qticks);
         lablon<-315;
         SRT   <- 45;
         strt  <-275},
         "bottomright"={jklons<-seq( 94, 176, by=1);
         bllons<-seq( 95, 175, length.out =qbins+1);
         btlons<-seq( 95+qtadjust, 175-qtadjust, length.out=qticks);
         lablon<-135;
         SRT   <- 45;
         strt  <- 95})

  if(type=="continuous" & !is.null(breaks)){
    nms<- (breaks-lmins)/(lmax-lmins)
    btlons<-round(nms*80, 2) + strt
    tlabs<-as.character(breaks)

  }

  #Graticule for colors
  bleg  <- graticule::graticule(lons = bllons,lats = c(Trim+3,Trim+5), tiles = TRUE, proj = raster::projection(Bathy))
  #Graticule for ticks
  btick <- graticule::graticule(lons = btlons ,lats=c(Trim+4,Trim+7),  proj=raster::projection(Bathy), tiles=F)
  #Graticule for masks
  k     <- graticule::graticule(lons = jklons,lats = c(Trim+10,Trim+6.75), tiles = TRUE, proj = raster::projection(Bathy))
  j     <- graticule::graticule(lons = jklons,lats = c(Trim+15,Trim+2), tiles = TRUE, proj = raster::projection(Bathy))

  #Tick labels
  df2 <- data.frame(a = tlabs,lon = btlons, lat=rep(Trim+9, length(tlabs))) #Create dataframe with labels and locations.
  sp::coordinates(df2) <- c("lon", "lat") #Assign the current coordinate type
  raster::projection(df2) <- "+init=epsg:4326" #Assign the current projection type
  lab_pos2 <- sp::spTransform(df2, raster::crs(raster::projection(Bathy))) #Reproject to the polar map coordinates.

  #Legend label
  df3 <- data.frame(a = label,lon = lablon, lat=rep(Trim+12.5))
  sp::coordinates(df3) <- c("lon", "lat")
  raster::projection(df3) <- "+init=epsg:4326"
  lab_pos3 <- sp::spTransform(df3, raster::crs(raster::projection(Bathy)))

  raster::plot(j, border=F,col="white", add=T) #White mask
  raster::plot(btick, add=T, col=1)
  raster::plot(bleg, lwd=2, add=T)
  raster::plot(bleg, border=F,  col=cols, add=T)
  raster::plot(k, border=F,col="white", add=T)
  text(lab_pos2, labels=lab_pos2$a, cex= lcex, adj=ladj, srt=lsrt)
  text(lab_pos3, labels=lab_pos3$a, cex= tcex, adj=tadj, srt=SRT) } ## Need to set SRT during the position if statements.




