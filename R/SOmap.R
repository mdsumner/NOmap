#' Southern Ocean Map
#'
#' @description
#' Function for creating round Southern Ocean maps.
#'
#' @param Bathleg logical: if \code{TRUE}, insert the bathymetry legend
#' @param Border logical: if \code{TRUE}, insert longitude border
#' @param Trim numeric: longitude to trim map to
#' @param Grats logical: if \code{TRUE}, insert graticule grid
#' @param bordercol character: colours for longitude border
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
#'   SOmap(Trim = -45, Grats = TRUE)
#' }
#' @export
#'

SOmap <- function(Bathleg = TRUE, Border = TRUE, Trim = -45, Grats = FALSE, straight = FALSE, land = TRUE, fronts = FALSE, frontcols = c("hotpink", "orchid", "plum"), bordercol = c("white", "black"), gratcol = "grey70") {
    ## data
    SOmap_data <- NULL
    Bathy <- NULL
    data("SOmap_data", package = "SOmap", envir = environment())
    data("Bathy", package = "SOmap", envir = environment())

    ## Set up color palette for bathy
    ramp2 <- grDevices::colorRampPalette(c("#54A3D1", "#60B3EB", "#78C8F0", "#98D1F5", "#B5DCFF", "#BDE1F0", "#CDEBFA", "#D6EFFF", "#EBFAFF","grey99", "grey90", "grey92", "grey94", "grey96", "white"))
    bluepal <- ramp2(100)
    bluepal2 <- ramp2(80)
    ## Setup color border
    bord <- graticule::graticule(lons = seq(-180, 180, by = 15), lats = c(Trim+2, Trim), tiles = TRUE, proj = raster::projection(Bathy))
    ## fix trim without legend.
    if (!Border) Trim <- Trim-2

    #bathy legend
    if (Bathleg) {
        ## White Mask
        mask_graticule <- graticule::graticule(lons = seq(-180, 180, by = 1),lats = c(Trim+13.5, Trim+2), tiles = TRUE, proj = raster::projection(Bathy))

        ## Legend
        ## Colored legend
        bleg <- graticule::graticule(lons = seq(185, 265, by = 1),lats = c(Trim+3, Trim+5), tiles = TRUE, proj = raster::projection(Bathy))

        btick <- graticule::graticule(lats = c(Trim+4, Trim+7), lons = seq(190, 260, by = 11.666), proj = raster::projection(Bathy), tiles = FALSE)

        spud <- graticule::graticule(lons = seq(-180, 180, by = 1), lats = c(Trim+10, Trim+6.75), tiles = TRUE, proj = raster::projection(Bathy))
        df2 <- data.frame(a = c("-8000", "-6000", "-4000", "-2000", "0", "2000", "4000"),
                          lon = seq(190, 260, by = 11.666),
                          lat=rep(Trim+9, 7))
        sp::coordinates(df2) <- c("lon", "lat")
        raster::projection(df2) <- "+init=epsg:4326"
        lab_pos2 <- sp::spTransform(df2, raster::crs(raster::projection(Bathy)))
    }
    ## Graticule dots #
    xx <- c(0, 45, 90, 135, 180, 225, 270, 315, 360)
    yy <- c(-90, -75, -60, -45, Trim)
    grat <- graticule::graticule(xx, yy, proj = raster::projection(Bathy))
    gratlab <- graticule::graticule_labels(lons = 180,lats = c(-45, -30, -60, -75), xline = 180, yline = -15, proj = raster::projection(Bathy))

    ## Set the Trim value depending on legend yes or no
    q <- ifelse(Bathleg, Trim+13, Trim+2)
    Bathy <- raster::trim(SOmap::latmask(Bathy, latitude = q))
    out <- list(projection = raster::projection(Bathy), target = raster::raster(Bathy), straight = straight, trim = Trim)
    ##out$bathy <- list(plotfun = if (straight) "raster::plot(x, col = col, legend = legend, yaxt = yaxt, xaxt = xaxt, asp = asp)" else "raster::image(x, col = col, yaxt = yaxt, xaxt = xaxt, asp = asp)", plotenv = list(x = Bathy, col = bluepal, yaxt = "n", xaxt = "n", asp = 1))
    out$bathy <- list(plotfun = if (straight) "raster::plot" else "raster::image",
                      plotargs = list(x = Bathy, col = bluepal, yaxt = "n", xaxt = "n", asp = 1))
    if (straight) out$bathy$plotargs$legend <- FALSE

    ##out$box <- list(plotfun = "graphics::box(col = col)", plotenv = list(col = "white"))
    out$box <- list(plotfun = "graphics::box", plotargs = list(col = "white"))
    out$plot_sequence <- c("bathy", "box")

    if (land) {
      xland <-sf::st_as_sf(SOmap::SOmap_data$continent)
      xland <- sf::st_buffer(xland, 0)
      buf <- sf::st_sf(a = 1, geometry = sf::st_sfc(sf::st_buffer(sf::st_point(cbind(0, 0)), 111111 * (90-abs(Trim+2)))), crs = raster::projection(SOmap_data$continent))
      ##out$coastline <- list(plotfun = "plot(x, col = col, border = border, add = add)",
      ##                      plotenv = list(x = suppressWarnings(sf::st_intersection(buf, xland)$geometry), col = NA, border = "black", add = TRUE))
      out$coastline <- list(plotfun = "plot",
                            plotargs = list(x = suppressWarnings(sf::st_intersection(buf, xland)$geometry), col = NA, border = "black", add = TRUE))
      out$plot_sequence <- c(out$plot_sequence, "coastline")
    }

    ## fronts
    if (fronts) {
      xfront <-sf::st_as_sf(SOmap::SOmap_data$fronts_orsi)
      buf <- sf::st_sf(a = 1, geometry = sf::st_sfc(sf::st_buffer(sf::st_point(cbind(0, 0)), 111111 * (90-abs(Trim+2)))), crs = raster::projection(SOmap_data$continent))
      ##out$fronts <- list(plotfun = "plot", plotargs = list(x = suppressWarnings(sf::st_intersection(buf, xfront)$geometry), col = frontcols, add = TRUE))
      ##out$fronts <- list(plotfun = "plot(x, col = col, add = add)", plotenv = list(x = suppressWarnings(sf::st_intersection(buf, xfront)$geometry), col = frontcols, add = TRUE))
      ####out$fronts <- list(plotfun = "do.call(plot, c(list(x = x), plotargs))", plotenv = list(x = suppressWarnings(sf::st_intersection(buf, xfront)$geometry), plotargs = list(col = frontcols, add = TRUE)))
      out$fronts <- list(plotfun = "plot", plotargs = list(x = suppressWarnings(sf::st_intersection(buf, xfront)$geometry), col = frontcols, add = TRUE))
      out$plot_sequence <- c(out$plot_sequence, "fronts")
    }
    
    ## Graticule grid
    if (Grats) {
        ##out$graticule <- list(data = grat, col = gratcol, lty = 3, labels = list(data = gratlab, labels = gratlab$lab, col = gratcol, cex = 0.5))
        out$graticule <- list(plotfun = "raster::plot", plotargs = list(x = grat, col = gratcol, lty = 3, add = TRUE))
        out$graticule$labels <- list(plotfun = "text", plotargs = list(x = gratlab, labels = parse(text = gratlab$lab), col = gratcol, cex = 0.5))
        out$plot_sequence <- c(out$plot_sequence, "graticule")
    }

    ## Legend
    if (Bathleg) {
##        out$bathy_legend <- list(
##            mask = list(graticule = mask_graticule, col = "white", border = FALSE),
##            ticks = list(ticks = btick, col = "black"),
##            legend = list(legend = bleg, lwd = 2, col = bluepal2, border = FALSE),
##            graticules = list(graticules = spud, border = FALSE, col = "white"),
##            labels = list(data = lab_pos2, labels = lab_pos2$a, cex = 0.75, adj = 0.5))

##        out$bathy_legend <- list(
##            plotfun = "raster::plot(mask$graticule, border = mask$border, col = mask$col, add = TRUE) ## white mask
##        raster::plot(ticks$ticks, add = TRUE, col = ticks$col)
##        raster::plot(legend$legend, lwd = legend$lwd, add = TRUE)
##        raster::plot(legend$legend, border = legend$border, col = legend$col, add = TRUE)
##        raster::plot(graticules$graticules, border = graticules$border, col = graticules$col, add = TRUE)
##        text(labels$data, labels = labels$labels, cex = labels$cex, adj = labels$adj)",
##            plotenv = list(mask = list(graticule = mask_graticule, col = "white", border = FALSE),
##                           ticks = list(ticks = btick, col = "black"),
##                           legend = list(legend = bleg, lwd = 2, col = bluepal2, border = FALSE),
##                           graticules = list(graticules = spud, border = FALSE, col = "white"),
##                           labels = list(data = lab_pos2, labels = lab_pos2$a, cex = 0.75, adj = 0.5)
##                           ))
        out$bathy_legend <- list(
            plotfun = function(mask, ticks, legend, graticules, labels) {
                raster::plot(mask$graticule, border = mask$border, col = mask$col, add = TRUE) ## white mask
                raster::plot(ticks$ticks, add = TRUE, col = ticks$col)
                raster::plot(legend$legend, lwd = legend$lwd, add = TRUE)
                raster::plot(legend$legend, border = legend$border, col = legend$col, add = TRUE)
                raster::plot(graticules$graticules, border = graticules$border, col = graticules$col, add = TRUE)
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
        ##out$border <- list(data = bord, col = bordercol)
        ##out$border <- list(plotfun = "raster::plot(x, col = col, add = add)", plotenv = list(x = bord, col = bordercol, add = TRUE))
        out$border <- list(plotfun = "raster::plot", plotargs = list(x = bord, col = bordercol, add = TRUE))
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
    op <- par(mar = rep(0.01, 4), oma= rep(0.0, 4), mai= rep(0.0, 4))
    on.exit(par(op))
    ## iterate through plot_sequence
    plot_all(x)
    ## Plot bathymetry
#    eval(parse(text = x$bathy$plotfun), envir = x$bathy$plotenv)
    #if (x$straight) {
    #    raster::plot(x$bathy$data, col = x$bathy$col, legend = FALSE, yaxt = "n", xaxt = "n", asp = 1)
    #} else {
    #    raster::image(x$bathy$data, col = x$bathy$col, yaxt = "n", xaxt = "n", asp = 1)
    #}
#    eval(parse(text = x$box$plotfun), envir = x$box$plotenv)

#    if (!is.null(x$coastline)) eval(parse(text = x$coastline$plotfun), envir = x$coastline$plotenv)

#    plot_iwc(x$iwc)

#    ## fronts
#    if (!is.null(x$fronts)) {
#        eval(parse(text = x$fronts$plotfun), envir = x$fronts$plotenv)
#        ##plot(x$fronts$data$geometry, add = TRUE, col = x$fronts$linecol)
#    }

#    ## Graticule grid
#    if (!is.null(x$graticule)) {
#        raster::plot(x$graticule$data, add = TRUE, col = x$graticule$col, lty = x$graticule$lty)
#        text(x$graticule$labels$data, lab = parse(text = x$graticule$labels$labels), col = x$graticule$labels$col, cex = x$graticule$labels$cex)
#    }

    ## the plot_* function for each management layer is defined in SOmanagement.R
#    plot_research_blocks(x$research_blocks)
#    plot_sprfmo(x$sprfmo_research_blocks)
#    plot_ssru(x$ccamlr_ssru)
#    plot_ssmu(x$ccamlr_ssmu)
#    plot_ccamlr_areas(x$ccamlr_statistical_areas)
#    plot_eez(x$eez)
#    plot_mpa(x$mpa)
#    plot_domains(x$ccamlr_planning_domains)

    ## Legend
#    if (!is.null(x$bathy_legend)) {
#        raster::plot(x$bathy_legend$mask$graticule, border = x$bathy_legend$mask$border, col = x$bathy_legend$mask$col, add = TRUE) ## white mask
#        raster::plot(x$bathy_legend$ticks$ticks, add = TRUE, col = x$bathy_legend$ticks$col)
#        raster::plot(x$bathy_legend$legend$legend, lwd = x$bathy_legend$legend$lwd, add = TRUE)
#        raster::plot(x$bathy_legend$legend$legend, border = x$bathy_legend$legend$border, col = x$bathy_legend$legend$col, add = TRUE)
#        raster::plot(x$bathy_legend$graticules$graticules, border = x$bathy_legend$graticules$border, col = x$bathy_legend$graticules$col, add = TRUE)
#        text(x$bathy_legend$labels$data, labels = x$bathy_legend$labels$labels, cex = x$bathy_legend$labels$cex, adj = x$bathy_legend$labels$adj)
#    }

#    if (!is.null(x$border)) {
#        raster::plot(x$border$data, col = x$border$col, add = TRUE)
#    }
    invisible(x)
}
