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
#'   SOmap2(CCAMLR = TRUE, MPA = TRUE, Trim = -45)
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
    SOmap_data <- NULL
    data("SOmap_data", package = "SOmap", envir = environment())

    ## get the management layer details from SOmanagement
    mx <- SOmanagement(CCAMLR = CCAMLR, CCAMLRlab = CCAMLRlab, ccamlrcol = ccamlrcol,
                       SSRU = SSRU, SSRUlab = SSRUlab, ssrucol = ssrucol,
                       SSMU = SSMU, SSMUlab = SSMUlab, ssmucol = ssmucol,
                       RB = RB, RBlab = RBlab, rbcol = rbcol,
                       SPRFMORB = SPRFMORB, sprfmocol = sprfmocol,
                       Trim = Trim,
                       EEZ = EEZ, EEZlab = EEZlab, eezcol = eezcol,
                       MPA = MPA, MPAlab = MPAlab, mpacol= mpacol,
                       Domains = Domains, Domainslab = Domainslab, domcol = domcol,
                       IWC = IWC, IWClab = IWClab, iwccol = iwccol)

    if (land) {
        if (CCAMLR) {
            ## change coastline data
            notANT <- sf::st_as_sf(SOmap_data$continent[SOmap_data$continent$continent != "Antarctica",])
            notANT <- sf::st_buffer(notANT, 0)
            buf <- make_buf(Trim, proj = out$projection)
            out$coastline$plotargs$x <- suppressWarnings(sf::st_intersection(buf, notANT)$geometry)
        }
    }

    ## copy management layers into out
    out[mx$plot_sequence] <- mx[mx$plot_sequence]
    out$plot_sequence <- insert_into_sequence(out$plot_sequence, ins = mx$plot_sequence, after = c("bathy", "box", "coastline", "fronts", "graticule"))
    out
}

