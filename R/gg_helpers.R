## currently just internal functions to help with ggplotting
## might be exported for users later

## bathymetry as a raster grob
## this is much faster than naively plotting bathy using geom_tile or similar
## example usage along the lines of:
## cst <- fortify(SOmap_data$ADD_coastline_med)
## ggplot() + bathy_raster() + geom_path(data = cst, aes(long, lat, group = group), colour = "black")

bathy_raster <- function() {
    bathy_grob <- NULL
    data("bathy_grob", package = "SOmap", envir = environment())
    ext <- attr(bathy_grob, "extent")
    annotation_custom(bathy_grob, xmin = ext[1], xmax = ext[2], ymin = ext[3], ymax = ext[4])
}

