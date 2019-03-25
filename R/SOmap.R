#' Southern Ocean Map
#'
#' @description
#' Function for creating round Southern Ocean maps.
#'
#' @param Bathleg logical: if \code{TRUE}, insert the bathymetry legend
#' @param Border logical: if \code{TRUE}, insert longitude border
#' @param Trim numeric: latitude to trim map to, set to -10 for effectively no trim
#' @param Grats logical: if \code{TRUE}, insert graticule grid
#' @param bordercol character: colours for longitude border
#' @param borderwidth numeric: thickness (in degrees of latitude) of the border
#' @param gratcol string: colour for graticule grid
#' @param straight logical: if \code{TRUE}, leave a blank space on the side for a straight legend
#' @param land logical: if \code{TRUE}, plot coastline
#' @param fronts logical: if \code{TRUE}, plot ocean fronts (Subantarctic Front, Polar Front, Southern Antarctic Circumpolar Current Front)
#' @param frontcols character: colours for fronts
#'
#' @return Produces at the very least a round bathymetry map of the southern hemisphere.
#'
#' @examples
#' \dontrun{
#' tfile <- tempfile("SOmap", fileext = ".png")
#' png(tfile, width=22, height=20, units='cm', res=600)
#' SOmap(Trim=-45, Grats=TRUE)
#' dev.off()
#' unlink(tfile)
#'  SOmap(Trim = -45, Grats = TRUE)
#' }
#' @export
#'

SOmap <- function(Bathleg = TRUE, Border = TRUE, Trim = -45, Grats = FALSE, straight = FALSE, land = TRUE, fronts = FALSE, frontcols = c("hotpink", "orchid", "plum"), bordercol = c("white", "black"), borderwidth = 2, gratcol = "grey70") {
    ## data
    SOmap_data <- NULL
    Bathy <- NULL
    data("SOmap_data", package = "SOmap", envir = environment())
    data("Bathy", package = "SOmap", envir = environment())

    ## Set up color palette for bathy
    ramp2 <- grDevices::colorRampPalette(c("#54A3D1", "#60B3EB", "#78C8F0", "#98D1F5", "#B5DCFF", "#BDE1F0", "#CDEBFA", "#D6EFFF", "#EBFAFF","grey99", "grey90", "grey92", "grey94", "grey96", "white"))
    bluepal <- ramp2(100)
    bluepal2 <- ramp2(80)
    ## fix trim without legend
    if (!Border) borderwidth <- 0

    #bathy legend
    if (Bathleg) {
        ## White Mask
        mask_graticule <- graticule::graticule(lons = seq(-180, 180, by = 1),lats = c(Trim+borderwidth+11.5, Trim+borderwidth), tiles = TRUE, proj = raster::projection(Bathy))

        ## Legend
        ## Colored legend
        bleg <- graticule::graticule(lons = seq(185, 265, by = 1),lats = c(Trim+borderwidth+1, Trim+borderwidth+3), tiles = TRUE, proj = raster::projection(Bathy))

        btick <- graticule::graticule(lats = c(Trim+borderwidth+2, Trim+borderwidth+5), lons = seq(190, 260, by = 11.666), proj = raster::projection(Bathy), tiles = FALSE)

        spud <- graticule::graticule(lons = seq(-180, 180, by = 1), lats = c(Trim+borderwidth+8, Trim+borderwidth+4.75), tiles = TRUE, proj = raster::projection(Bathy))
        df2 <- data.frame(a = c("-8000", "-6000", "-4000", "-2000", "0", "2000", "4000"),
                          lon = seq(190, 260, by = 11.666),
                          lat=rep(Trim+borderwidth+7, 7))
        sp::coordinates(df2) <- c("lon", "lat")
        raster::projection(df2) <- "+init=epsg:4326"
        lab_pos2 <- sp::spTransform(df2, raster::crs(raster::projection(Bathy)))
    }
    ## Graticule dots #
    xx <- c(0, 45, 90, 135, 180, 225, 270, 315, 360)
    yy <- c(seq(from = -90, to = Trim-1, by = 15), Trim)
    grat <- graticule::graticule(xx, yy, proj = raster::projection(Bathy))
    gratlab <- graticule::graticule_labels(lons = 180,lats = c(-45, -30, -60, -75), xline = 180, yline = -15, proj = raster::projection(Bathy))

    ## crop bathy raster depending on legend yes or no
    q <- ifelse(Bathleg, Trim+borderwidth+11, Trim+borderwidth)
    Bathy <- raster::trim(SOmap::latmask(Bathy, latitude = q))
    out <- list(projection = raster::projection(Bathy), target = raster::raster(Bathy), straight = straight, trim = Trim)
    out$bathy <- as_plotter(plotfun = if (straight) "plot" else "image", plotargs = list(x = Bathy, col = bluepal, yaxt = "n", xaxt = "n", asp = 1))
    if (straight) out$bathy$plotargs$legend <- FALSE

    out$box <- as_plotter(plotfun = "graphics::box", plotargs = list(col = "white"))
    out$plot_sequence <- c("bathy", "box")
    buf <- make_buf(Trim+borderwidth, proj = out$projection)
    if (land) {
      xland <-sf::st_as_sf(SOmap::SOmap_data$continent)
      xland <- sf::st_buffer(xland, 0)
      out$coastline <- as_plotter(plotfun = "plot", plotargs = list(x = suppressWarnings(sf::st_intersection(buf, xland)$geometry), col = NA, border = "black", add = TRUE))
      out$plot_sequence <- c(out$plot_sequence, "coastline")
    }

    ## fronts
    if (fronts) {
      xfront <-sf::st_as_sf(SOmap::SOmap_data$fronts_orsi)
      out$fronts <- as_plotter(plotfun = "plot", plotargs = list(x = suppressWarnings(sf::st_intersection(buf, xfront)$geometry), col = frontcols, add = TRUE))
      out$plot_sequence <- c(out$plot_sequence, "fronts")
    }

    ## Graticule grid
    if (Grats) {
        out$graticule <- as_plotter(plotfun = "plot", plotargs = list(x = grat, col = gratcol, lty = 3, add = TRUE))
        out$graticule$labels <- list(plotfun = "text", plotargs = list(x = gratlab, labels = parse(text = gratlab$lab), col = gratcol, cex = 0.5))
        out$plot_sequence <- c(out$plot_sequence, "graticule")
    }

    ## Legend
    if (Bathleg) {
        ## TODO split this into multiple plot functions?
        out$bathy_legend <- as_plotter(
            plotfun = function(mask, ticks, legend, graticules, labels) {
                plot(mask$graticule, border = mask$border, col = mask$col, add = TRUE) ## white mask
                plot(ticks$ticks, add = TRUE, col = ticks$col)
                plot(legend$legend, lwd = legend$lwd, add = TRUE)
                plot(legend$legend, border = legend$border, col = legend$col, add = TRUE)
                plot(graticules$graticules, border = graticules$border, col = graticules$col, add = TRUE)
                text(labels$data, labels = labels$labels, cex = labels$cex, adj = labels$adj)
            },
            plotargs = list(mask = list(graticule = mask_graticule, col = "white", border = FALSE),
                            ticks = list(ticks = btick, col = "black"),
                            legend = list(legend = bleg, lwd = 2, col = bluepal2, border = FALSE),
                            graticules = list(graticules = spud, border = FALSE, col = "white"),
                            labels = list(data = lab_pos2, labels = lab_pos2$a, cex = 0.75, adj = 0.5)
                            ))
        out$plot_sequence <- c(out$plot_sequence, "bathy_legend")
    }
    if (Border) {
        bord <- graticule::graticule(lons = seq(-180, 180, by = 15), lats = c(Trim+borderwidth, Trim), tiles = TRUE, proj = raster::projection(Bathy))
        out$border <- as_plotter(plotfun = "plot", plotargs = list(x = bord, col = bordercol, border = "black", add = TRUE))
        out$plot_sequence <- c(out$plot_sequence, "border")
    }
    structure(out, class = "SOmap")
}

#' @method plot SOmap
#' @export
plot.SOmap <- function (x, y, ...) {
    print(x)
    invisible()
}

#' @method print SOmap
#' @export
print.SOmap <- function(x, ...) {
    op <- par(mar = rep(0.01, 4),  mai= rep(0.0, 4))#oma= rep(0.0, 4),
    #on.exit(par(op))
    ## record current CRS
    SOcrs(x$projection)
    ## iterate through plot_sequence
    plot_all(x)
    invisible(x)
}
