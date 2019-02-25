#' Southern Ocean Map 2
#'
#' @description
#' Function for creating round Southern Ocean maps with inbuild base layers.
#'
#' @param Bathleg
#' Insert the bathymetry legend.
#' @param CCAMLR
#' Insert the CCAMLR boundaries.
#' @param CCAMLRlab
#' Insert the CCAMLR labels.
#' @param SSRU
#' Insert the CCAMLR small scale research unit boundaries.
#' @param SSRUlab
#' Insert the CCAMLR small scale research unit labels.
#' @param SSMU
#' Insert the CCAMLR small scale management unit boundaries.
#' @param SSMUlab
#' Insert the CCAMLR small scale management unit labels.
#' @param RB
#' Insert the CCAMLR research block boundaries.
#' @param RBlab
#' Insert the CCAMLR research block labels.
#' @param SPRFMORB
#' Insert the SPRFMO toothfish research block boundaries.
#' @param Border
#' Insert longitude border.
#' @param Trim
#' Longitude to trim map to.
#' @param Grats
#' Insert graticule grid.
#' @param EEZ
#' Insert Exclusive Economic Zones.
#' @param EEZlab
#' Insert Exclusive Economic Zone labels.
#' @param MPA
#' Insert CCAMLE Marine Protected Areas.
#' @param MPAlab
#' Insert CCAMLE Marine Protected Area labels.
#' @param Domains
#' Insert CCAMLR Marine Protected Areas planning Domains.
#' @param Domainslab
#' Insert CCAMLR Marine Protected Area planning Domains labels.
#' @param IWC
#' Insert International Whaling Commission boundaries.
#' @param IWClab
#' Insert International Whaling Commission labels.
#' @param rbcol
#' Color for CCAMLR research blocks.
#' @param sprfmocol
#' Color for SPRFMO toothfish research blocks
#' @param ccamlrcol
#' Color for CCAMLR boundaries
#' @param ssrucol
#' Color for CCAMLR small scale research units.
#' @param ssmucol
#' Color for CCAMLR small scale management units.
#' @param eezcol
#' Color for Exclusive Economic Zone boundaries; Default is maroon.
#' @param mpacol
#' Color for CCAMLR Marine Protected Areas; Default is yellow.
#' @param bordercol
#' Colors for longitude border; Default is c("black","white").
#' @param gratcol
#' Color for graticule grid; Default is grey.
#' @param iwccol
#' Color for IWC boundaries; Default is blue.
#' @param domcol
#' Color for the Domain boundaries. Default is magenta.
#' @param straight
#' Do you need a blank space on the side for a straight legend.
#' @param land
#' Plot land boundary
#' @param frontcols
#' colors for fronts
#' @param fronts
#' Plot ocean fronts: Subantarctic Front, Polar Front, Southern Antarctic Circumpolar Current Front
#'
#' @return
#' Produces at the very base a round bathymetry map of the southern hemisphere.
#'
#' @examples
#' \dontrun{
#'   SOmap2(CCAMLR = TRUE, MPA = TRUE, Domains = TRUE, Trim = -45)
#' }
#' @export
#'

SOmap2<-function(Bathleg=TRUE,
                 land=TRUE,
                 CCAMLR= FALSE,
                 CCAMLRlab= FALSE,
                 SSRU= FALSE,
                 SSRUlab = FALSE,
                 SSMU= FALSE,
                 SSMUlab= FALSE,
                 RB= FALSE,
                 RBlab= FALSE,
                 SPRFMORB= FALSE,
                 Border= TRUE,
                 Trim= -45,
                 Grats= FALSE,
                 EEZ=FALSE,
                 EEZlab=FALSE,
                 MPA=FALSE,
                 MPAlab=FALSE,
                 Domains=FALSE,
                 Domainslab=FALSE,
                 IWC=FALSE,
                 IWClab=FALSE,
                 straight=FALSE,
                 fronts=FALSE,
                 frontcols=c("hotpink","orchid","plum"),
                 rbcol=3,
                 sprfmocol='grey50',
                 ccamlrcol=2,
                 ssrucol="grey50",
                 ssmucol="grey70",
                 eezcol="maroon",
                 mpacol= "yellow",
                 bordercol=c("white","black"),
                 gratcol="grey70",
                 iwccol="blue",
                 domcol="magenta") {

    out <- SOmap(Bathleg = Bathleg, Border = Border, Trim = Trim, Grats = Grats, straight = straight, land = land, fronts = fronts, frontcols = frontcols, bordercol = bordercol, gratcol = gratcol)

    ## data
    Bathy <- NULL
    SOmap_data <- NULL
    data("SOmap_data", package = "SOmap", envir = environment())
    data("Bathy", package = "SOmap", envir = environment())

    ## CCAMLR Labels
    cclabs<-c("88.3", "48.4", "88.2", "48.2", "48.3", "58.4.3a", "58.4.3b", "58.5.2", "48.5", "48.6", "58.4.1", "88.1", "58.4.4a", "58.7", "58.6", "58.5.1", "58.4.4b")

    ## Set the Trim value depending on legend yes or no
    q <- ifelse(Bathleg, Trim+13, Trim+2)
    if (land) {
        if (CCAMLR) {
            ## change coastline data
            notANT <- sf::st_as_sf(SOmap_data$continent[SOmap_data$continent$continent != "Antarctica",])
            notANT <- sf::st_buffer(notANT, 0)
            buf <- sf::st_sf(a = 1, geometry = sf::st_sfc(sf::st_buffer(sf::st_point(cbind(0, 0)), 111111 * (90-abs(q+3)))), crs = raster::projection(SOmap_data$continent))
            out$coastline$data <- suppressWarnings(sf::st_intersection(buf, notANT))
        }
    }

  if (IWC) {
      ## iwc<-graticule::graticule(c(-170,-120,-60,0,70,130,-230), c(-90,Trim+0.5), proj=raster::projection(Bathy))
      ## plot(iwc,col=iwccol, add = TRUE)

      out$IWC <- list(data = list(
                          rgdal::project(rbind(c(-170, Trim), c(-170, -78.40)), out$projection),
                          rgdal::project(rbind(c(-120, Trim), c(-120, -73.844137)), out$projection),
                          rgdal::project(rbind(c(-60, -65.168), c(-60, -75.146206)), out$projection),
                          rgdal::project(rbind(c(-60, Trim), c(-60, -62.4505)), out$projection),
                          rgdal::project(rbind(c(0, Trim), c(0, -69.596701)), out$projection),
                          rgdal::project(rbind(c(70, Trim), c(70, -68.366691)), out$projection),
                          rgdal::project(rbind(c(130, Trim), c(130, -66.295027)), out$projection)),
                      col = iwccol)
      if (IWClab) {
          df3 <- data.frame(a = c("Area VI", "Area I", "Area II", "Area III", "Area IV", "Area V"),
                            lon = c(-145, -90, -30, 35, 100, 160),
                            lat=rep(-60, 6))
          sp::coordinates(df3) <- c("lon", "lat")
          raster::projection(df3) <- "+init=epsg:4326"
          lab_pos3 <- sp::spTransform(df3, raster::crs(out$projection))
          out$IWC$labels <- list(data = lab_pos3, labels = lab_pos3$a, col = iwccol, cex = 0.4, pos = 1, offset = -0.05)
      }
  }

    if (RB) {
        out$research_blocks <- list(data = SOmap_data$CCAMLR_research_blocks, border = rbcol)
        if (RBlab) {
            out$research_blocks$labels <- list(data = SOmap_data$CCAMLR_research_blocks, labels = SOmap_data$CCAMLR_research_blocks$GAR_Short_, col = rbcol, cex = 0.4, pos = 4, offset = 0.3)
        }
    }

    if (SPRFMORB) {
        sprfmoa <- graticule::graticule(lats = c(-59.9, -57.9), lons = c(-155.3333, -150), proj = out$projection)
        sprfmob <- graticule::graticule(lats = c(-59.0, -60.0),lons = c(-142.1666667, -145.833333), proj = out$projection)
        out$sprfmo_research_blocks <- list(data = list(sprfmoa, sprfmob), col = sprfmocol)
    }

    if (SSRU) {
        out$ccamlr_ssru <- list(data = SOmap_data$CCAMLR_SSRU, border = ssrucol)
        if (SSRUlab) {
            out$ccamlr_ssru$labels <- list(data = SOmap_data$CCAMLR_SSRU, labels = SOmap_data$CCAMLR_SSRU@data$ShortLabel, col = ssrucol, cex = 0.4, pos = 1, offset = -0.05)
        }
    }

    if (SSMU) {
        out$ccamlr_ssmu <- list(data = SOmap_data$CCAMLR_SSMU, border = ssmucol)
        if (SSMUlab) {
            out$ccamlr_ssmu$labels <- list(data = SOmap_data$CCAMLR_SSMU, labels = SOmap_data$CCAMLR_SSMU$ShortLabel, col = ssmucol, cex = 0.5, pos = 1, offset = 0.6)
        }
    }

    if (CCAMLR) {
        out$ccamlr_statistical_areas <- list(data = SOmap_data$CCAMLR_statistical_areas, border = ccamlrcol)
        if (CCAMLRlab) {
            out$ccamlr_statistical_areas$labels <- list(
                list(data = SOmap_data$CCAMLR_statistical_areas[!SOmap_data$CCAMLR_statistical_areas$LongLabel %in% c("48.1", "58.4.2"), ], labels = cclabs, col = ccamlrcol, cex = 0.5, pos = 1, offset = -0.3),
                list(data = SOmap_data$CCAMLR_statistical_areas[SOmap_data$CCAMLR_statistical_areas$LongLabel == "58.4.2", ], labels = "58.4.2", col = ccamlrcol,cex = 0.5, pos = 3, offset = 0.5),
                list(data = SOmap_data$CCAMLR_statistical_areas[SOmap_data$CCAMLR_statistical_areas$LongLabel == "48.1", ], labels = "48.1", col = ccamlrcol, cex = 0.5, pos = 2, offset = -0.1))
        }
    }

    if (EEZ) {
        out$eez <- list(data = SOmap_data$EEZ, border = eezcol)
        if (EEZlab) {
            out$eez$labels <- list(data = SOmap_data$EEZ, labels = SOmap_data$EEZ$Name, col = eezcol, cex = 0.35, pos = 4, offset = 0.8)
        }
    }

    if (MPA) {
        out$mpa <- list(data = SOmap_data$CCAMLR_MPA, border = mpacol)
        if (MPAlab) {
            out$mpa$labels <- list(data = SOmap_data$CCAMLR_MPA, labels = SOmap_data$CCAMLR_MPA@data$ShortLabel, col = mpacol, cex = 0.35, pos = 1, offset =0.2)
        }
    }

    if (Domains) {
        out$ccamlr_planning_domains <- list(data = SOmap_data$CCAMLR_planning_domains, border = domcol)
        if (Domainslab) {
            labs <- c("Domain  8", "Domain  9", "", "", "Domain  3", "", "Domain  4", "Domain  5", "Domain  6")
            labs1 <- c("", "", "Domain  1", "", "", "", "", "", "")
            labs2 <- c("", "", "", "", "", "Domain  2", "", "", "")
            labs7 <- c("", "", "", "Domain  7", "", "", "", "", "")
            out$ccamlr_planning_domains$labels <- list(
                list(data = SOmap_data$CCAMLR_planning_domains, labels = labs, col = domcol, cex = 0.7, pos = 3, offset = 0.05),
                list(data = SOmap_data$CCAMLR_planning_domains, labels = labs1, col = domcol, cex = 0.7, pos = 1, offset = 3.0),
                list(data = SOmap_data$CCAMLR_planning_domains, labels = labs2, col = domcol, cex = 0.7, pos = 3, offset = 0.5),
                list(data = SOmap_data$CCAMLR_planning_domains, labels = labs7, col = domcol, cex = 0.7, pos = 4, offset = 0.9)
            )
        }
    }
    out
}












