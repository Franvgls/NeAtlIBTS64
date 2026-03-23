#' Function IBTSNeAtl_map plots the map with all the surveys **STILL ON DEVELOPMENT**
#'
#' #' @description
#  Produces a map from the shapefiles that define the IBTSNeAtl Surveys from Scotland
#' to Cadiz, Still needs to include the shapefiles within the package, or right references.
#'
#' @details
#' The default limits for the IBTS including the North Sea or not, but the script could
#' be used to get maps all arround the world playing with nl,sl and xlims
#'
#' @param nl = 61.5 northernmost limit of the map
#' @param sl = 35 Southernmost limit of the map
#' @param leg = TRUE if TRUE includes the legend with the colors of the surveys
#' @param legpos defines where the legend is placed
#' @param cex.leg = .7 Size of the legend
#' @param dens = 30 density of the shading lines for all the surveys
#' @param ICESdiv = TRUE if TRUE plots the IBTS divisions behind the shapefiles
#' @param ICESrect = FALSE if TRUE plots the lines of the ICES statistic rectangles
#' @param ICESlab = FALSE if TRUE plots labs por ICES rectangles
#' @param ICESlabcex tamano del ICESlab en cex, .5 por defecto subirlo si se quiere mas grande
#' @param axlab =1 size of axis labs
#' @param NS = FALSE if TRUE includes the ICES rectangles only for the North Sea area
#' @param bathy = TRUE if TRUE plots the isobaths under the behind the shapefiles
#' @param bw = True plots the map with land in grey, if F in light brown (burlywood3)
#' @param bords = TRUE plots the borders o the countries, FALSE leaves dashed lines
#' @param axlab= .8 decides the size of the axis numbers
#' @param shpdir = path to the folder with the shapefiles
#' @param load = T or F to load all the shapes files.
#' @param places = if T adds towns in the maps
#' @param minpop = adds the limit of population of cities (places) to plot.
#' @param graf si F el grafico sale en la pantalla, si nombre fichero va a fichero en el directorio de trabajo del Rstudio ver getwd()
#' @param xpng width archivo png si graf es el nombre del fichero
#' @param ypng height archivo png si graf es el nombre del fichero
#' @param ppng points png archivo si graf es el nombre del fichero
#' @examples IBTSNeAtl_map(dens=0,nl=45,leg=F,load=TRUE,ICESrect = T,graf="MapIBTS");text(stat_y~stat_x,Area,labels=ICESNAME,cex=.8,font=4);text(stat_y~stat_x,Area,labels=Area,cex=.6,pos=1,col=2)
#' @export
IBTSNeAtl_map <- function(nl=60.5, sl=36.0, xlims=c(-18,3), leg=TRUE, legpos=c("bottomright"), cex.leg=.9, dens=300,
                          load=TRUE, ICESdiv=TRUE, ICESrect=FALSE, ICESlab=FALSE, ICESlabcex=.8, NS=FALSE, bathy=TRUE,
                          bw=FALSE, axlab=.8, bords=TRUE, lwdl=.1, shpdir="c:/GitHubRs/shapes/", places=FALSE, minpop=200000,
                          graf=FALSE, xpng=1200, ypng=800, ppng=15) {
  library(mapdata)
  library(maps)
  library(sp)
  library(sf)
  colores<-c("tomato1","yellow","yellow4","cyan4","green","red","navy","violet","blue","orange","lightgreen","sienna")
  if (all(c(sl,nl)<0) | all(c(sl,nl)>0)) {
    largo <- rev(abs(nl-sl))*1
  } else largo <- (nl-sl)
  if (xlims[2] < 0) {
    ancho <- diff(rev(abs(xlims)))*1
  } else ancho <- diff(xlims)*1

  # --- Lectura de shapefiles base ---
  ices.div <- sf::st_read(paste0(shpdir, "ices_div.shp"))
  if (is.na(sf::st_crs(ices.div))) sf::st_crs(ices.div) <- 4326
  ices.div <- ices.div[!sf::st_is_empty(ices.div), ]
  ices.div_sp <- as(ices.div, "Spatial")

  bath100 <- sf::st_read(paste0(shpdir, "100m.shp"))
  if (is.na(sf::st_crs(bath100))) sf::st_crs(bath100) <- 4326
  bath100 <- bath100[!sf::st_is_empty(bath100), ]
  bath100_sp <- as(bath100, "Spatial")

  bathy.geb <- sf::st_read(paste0(shpdir, "bathy_geb.shp"))
  if (is.na(sf::st_crs(bathy.geb))) sf::st_crs(bathy.geb) <- 4326
  bathy.geb <- bathy.geb[!sf::st_is_empty(bathy.geb), ]
  bathy.geb_sp <- as(bathy.geb, "Spatial")

  ices.areas <- sf::st_read(paste0(shpdir, "ICES_Areas_20160601_cut_dense_3857.shp"))
  if (is.na(sf::st_crs(ices.areas))) {
    message("ICES Areas CRS no definido; se asigna EPSG:4326")
    sf::st_crs(ices.areas) <- 4326
  } else {
    ices.areas <- tryCatch(sf::st_transform(ices.areas, 4326), error=function(e) ices.areas)
  }
  ices.areas <- ices.areas[!sf::st_is_empty(ices.areas), ]
  ices.areas_sp <- as(ices.areas, "Spatial")

  # --- Carga de campa\u00f1as si load = TRUE ---
  if (load) {
    SWC_Q1_sf <- sf::st_read(paste0(shpdir, "SWC_Q1.shp"))
    if (is.na(sf::st_crs(SWC_Q1_sf))) sf::st_crs(SWC_Q1_sf) <- 4326
    SWC_Q1_sf <- SWC_Q1_sf[!sf::st_is_empty(SWC_Q1_sf), ]
    SWC_Q1_sp <- as(SWC_Q1_sf, "Spatial")

    SCOROC <- rgdal::readOGR(paste0(shpdir,"SCOROC.dbf"),"SCOROC",verbose=FALSE)
    SCOROC <- sp::spTransform(SCOROC, CRS("+proj=longlat +datum=WGS84"))

    IGFS <- rgdal::readOGR(paste0(shpdir,"IGFS.dbf"),"IGFS",verbose=FALSE)
    IGFS_w84 <- sp::spTransform(IGFS, CRS("+proj=longlat +datum=WGS84"))

    NIGFS <- rgdal::readOGR(paste0(shpdir,"NI_IBTS.dbf"),"NI_IBTS",verbose=FALSE)
    NIGFS_w84 <- sp::spTransform(NIGFS, CRS("+proj=longlat +datum=WGS84"))

    CGFS <- rgdal::readOGR(paste0(shpdir,"CGFS_stratum.dbf"),"CGFS_stratum",verbose=FALSE)
    WCGFS <- rgdal::readOGR(paste0(shpdir,"CGFS_Western_Channel_stratification-2023.dbf"),
                            "CGFS_Western_Channel_stratification-2023",verbose=FALSE)
    Porc <- rgdal::readOGR(paste0(shpdir,"Porcupine.dbf"),"Porcupine",verbose=FALSE)
    Porc_w84 <- sp::spTransform(Porc, CRS("+proj=longlat +datum=WGS84"))

    EVHOE <- rgdal::readOGR(paste0(shpdir,"EVHOE.dbf"),"EVHOE",verbose=FALSE)
    EVHOE_w84 <- sp::spTransform(EVHOE, CRS("+proj=longlat +datum=WGS84"))

    Sp_North_w84 <- rgdal::readOGR(paste0(shpdir,"Sp_North.WGS84.dbf"),verbose=FALSE)
    PT_IBTS <- rgdal::readOGR(paste0(shpdir,"PT_IBTS_2015.dbf"),"PT_IBTS_2015",verbose=FALSE)
    Sp_Cadiz <- rgdal::readOGR(paste0(shpdir,"Sp_Cadiz.dbf"),verbose=FALSE)
    Sp_Cadiz_w84 <- sp::spTransform(Sp_Cadiz, CRS("+proj=longlat +datum=WGS84"))
  }

  # --- Preparacion del lienzo ---
  if (!is.logical(graf)) png(filename=paste0(graf,".png"),width=xpng,height=ypng,pointsize=ppng)
  par(mar=c(3.5,2,2,2)+0.1)
  maps::map(database="worldHires",xlim=xlims,ylim=c(sl,nl),type="n")
  if (!bw) rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],par("usr")[1],col="lightblue1")

  # --- Dibujos base ---
  if (bathy) {
    sp::plot(bath100_sp[1], add=TRUE, col=ifelse(bw,gray(.85),gray(.70)), lwd=.1)
    sp::plot(bathy.geb_sp[bathy.geb_sp$DEPTH!=100,1], add=TRUE, col=ifelse(bw,gray(.85),gray(.65)), lwd=.1)
  }
  if (ICESdiv) sp::plot(ices.div_sp[1], add=TRUE, col=NA, border="burlywood")

  # --- Coordenadas, ejes ---
  # (mantengo tus rutinas originales de ejes y etiquetas)
  if (all(xlims < 0)) {
    degs = seq(round(xlims[1],0), round(xlims[2],0), ifelse(ancho>10,4,1))
    alg = sapply(degs, function(x) bquote(.(abs(x))*degree ~ W))
    axis(1, at=degs, lab=do.call(expression, alg), font.axis=2, cex.axis=axlab)
    axis(3, at=degs, lab=do.call(expression, alg), font.axis=2, cex.axis=axlab)
  }

  if (all(c(sl,nl) > 0)) {
    degs = seq(round(sl,0), round(nl,0), ifelse(largo>10,4,1))
    alg = sapply(degs, function(x) bquote(.(abs(x))*degree ~ N))
    axis(2, at=degs, lab=do.call(expression, alg), las=2, cex.axis=axlab)
    axis(4, at=degs, lab=do.call(expression, alg), las=2, cex.axis=axlab)
  }

  # --- Dibujar campa\u00f1as si load=TRUE ---
  if (load) {
    sp::plot(SWC_Q1_sp, add=TRUE, col="yellow", lwd=.1, density=dens, angle=0)
    sp::plot(SCOROC, add=TRUE, col="yellow4", lwd=.1, density=dens, angle=32)
    sp::plot(NIGFS_w84, add=TRUE, col="cyan4", lwd=.1, density=dens, angle=64)
    sp::plot(IGFS_w84, add=TRUE, col="green", lwd=.1, density=dens, angle=96)
    sp::plot(Porc_w84, add=TRUE, col="red", lwd=.1, density=dens, angle=128)
    sp::plot(CGFS, add=TRUE, col="navy", lwd=.1, density=dens, angle=160)
    sp::plot(WCGFS, add=TRUE, col="violet", lwd=.1, density=dens, angle=192)
    sp::plot(EVHOE_w84, add=TRUE, col="blue", lwd=.1, density=dens, angle=224)
    sp::plot(Sp_North_w84, add=TRUE, col="orange", lwd=.1, density=dens, angle=256)
    sp::plot(PT_IBTS, add=TRUE, col="lightgreen", lwd=.1, density=dens, angle=288)
    sp::plot(Sp_Cadiz_w84, add=TRUE, col="sienna", lwd=.1, density=dens, angle=320)
  }

  # --- Costas ---
  maps::map(database="worldHires", xlim=xlims, ylim=c(sl,nl),
            fill=TRUE, col=ifelse(bw,"gray","burlywood3"), add=TRUE,
            fg="blue", interior=TRUE, boundary=TRUE, lty=1, lwd=.0005)

  # --- Ciudades ---
  if (places) {
    points(-6.26,53.35,pch=18); text(-6.26,53.35,"Dublin",cex=.5*cex.leg,font=2,pos=2)
    points(-9.14,38.72,pch=18); text(-9.14,38.72,"Lisbon",cex=.5*cex.leg,font=2,pos=4)
    points(2.35,48.85,pch=18); text(2.35,48.85,"Paris",cex=.5*cex.leg,font=2,pos=3)
    points(-3.71,40.21,pch=18); text(-3.71,40.21,"Madrid",cex=.5*cex.leg,font=2,pos=3)
    points(4.35,50.85,pch=18); text(4.35,50.85,"Brussels",cex=.5*cex.leg,font=2,pos=3)
    points(-0.13,51.51,pch=18); text(-0.13,51.51,"London",cex=.5*cex.leg,font=2,pos=3)
    points(12.57,55.68,pch=18); text(12.57,55.68,"Copenhagen",cex=.5*cex.leg,pos=3)
  }

  box()

  # --- Leyenda ---
  if (leg) {
    if (NS) {
      survs <- c("NS-IBTS","SCOWCGFS","SCOROC","NIGFS","IE-IGFS","SP-PORC","FR-CGFS","FR-WCGFS","EVHOE","SP-NORTH","PT-IBTS","SP-ARSA");cols=c("tomato1","yellow","")
    }
    else {
      survs<- c("SCOWCGFS","SCOROC","NIGFS","IE-IGFS","SP-PORC","FR-CGFS","FR-WCGFS","EVHOE","SP-NORTH","PT-IBTS","SP-ARSA")
      colores=colores[2:length(colores)]
    }
    legend(legpos,legend = survs,fill = colores,density = dens,angle = seq(0, 350, by = 32),cex = cex.leg,
           inset = c(.03, .03),title = "SURVEYS",bg = "white",text.col = "black")
    # legend(legpos, legend=survs, pch=15,col=colores, cex=cex.leg,
    #        inset=c(.03,.03),pt.cex = cex.leg, title="SURVEYS", bg="white", text.col="black",
    #        dens=dens, angle=seq(0,350,by=32))
  }

  if (!is.logical(graf)) {
    dev.off()
    message(paste0("figura: ", getwd(), "/", graf, ".png"))
  }
}
