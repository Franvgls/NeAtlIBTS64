#' Function qcHaulsPosit plots warp length vs. Depth behaviour
#'
#' Produces a warplength vs. DoorSpread plot and a model with lm unction. Data are taken directly from DATRAS using function getHHdata from library(icesDatras)
#' It only produces plots for surveys with HH files uploaded in DATRAS
#' Since sweeps length does not affect the warp that is decided by the chief scientist, there are no differences depending on sweeps
#' Maps try to use ShootLat and ShootLong cause old data from NS have a lot of -9 (NA) values in the NS-IBTS
#' @param Survey: either the Survey to be downloaded from DATRAS (see details), or a data frame with the HH information with  the DATRAS HH format  and the years and quarter selected in years and quarter
#' @param years: years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in steelblue2, it depends on the order of years, not the actual chronological year.
#' @param quarter: the quarter of the survey to be ploted
#' @param country: The country chosen to be plotted (checks if it's available in the HH file)
#' @param col1: the color of the lines
#' @param nhauls: if T shows the number of the hauls on the plot
#' @param ti= if T includes the title of the graph with country, survey, year and quarter
#' @param getICES: Should the data be downloaded from DATRAS? If T, default, the data are taken from DATRAS through the icesDatras package.
#' @param graf if FALSE the graph goes to screen, if its a file name (i.e. "graf") a .png file with that name is created and a message with location (wd) is shown in screen.
#' @param xpng width file png if graf is the name of the file
#' @param ypng height file png if graf is the name of the file
#' @param ppng points png parameter if graf is the name of the file
#' @details Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @return Produces a map with segments of the hauls performed in that survey.
#, it also includes information on the ship, the time series used the model used and parameters estimated.
#' @examples
#' \dontrun{
#' gearPlotdumb.wrpdp("NS-IBTS",c(2014:2017),1,"SWE")
#' }
#' @export
qcHaulsPosit<-function(Survey="NS-IBTS",years,quarter,col1="red",ti=TRUE,Hpoints=FALSE,Nhauls=FALSE,getICES=TRUE,esc.mult=1,graf=FALSE,xpng=800,ypng=800,ppng=15) {
  if (getICES) {
    dumb<-icesDatras::getDATRAS("HH",Survey,years,quarter)
  }
  if (!getICES) {
    dumb<-Survey
    Survey<-dumb$Survey
    if (!all(unique(years) %in% unique(dumb$Year))) stop(paste0("Not all years selected in years are present in the data.frame, check: ",unique(years)[which(!(unique(years) %in% unique(dumb$Year)))]))
    if (unique(dumb$Quarter)!=quarter) stop(paste0("Quarter selected ",quarter," is not available in the data.frame, check please"))
  }
  quarter<-unique(dumb$Quarter)
  dumb<-dplyr::filter(dumb,HaulVal=="V")
  for(i in 1:nrow(dumb)) {
    if(dumb$HaulLat[i]==c(-9)) {dumb$HaulLat[i]<-NA}
    if(dumb$HaulLong[i]==c(-9)) {dumb$HaulLong[i]<-NA}
    }
  countries<-unique(dumb$Country)
  #dumb<-dplyr::filter(dumb,Country==country)
  if (length(unique(dumb$Year))>1) stop("Only one year can be shown in this function")
  # print(tapply(dumb$Country,dumb[,c("Country","Year")],"length"))
  if (!is.logical(graf)) png(filename=paste0(graf,".png"),width = xpng,height = ypng, pointsize = ppng)
  if (is.logical(graf)) par(mar=c(2,2.5,2, 2.5) + 0.3,xaxs="i",yaxs="i")
  ## Creates *replong* variable correct the longitude in PORCUPINE and ROCKALL surveys that
  ##  are faraway from land and are not displayed otherwise
  if (any(Survey=="SP-PORC")) replong<-2
  if (any(Survey=="SCOROC")) replong<-9
  if (any(! Survey %in% c("SP-PORC","SCOROC"))) replong<-.5
  ##
  IBTSNeAtl_map64(load=F,NS=F,leg=F,newdev=FALSE,xlims = c(min(dumb$ShootLong)-.5,replong+max(dumb$ShootLong))
                  ,ylims=c(min(dumb$ShootLat)-.5,.5+max(dumb$ShootLat)),places=T)
  segments(dumb$ShootLong,dumb$ShootLat,dumb$HaulLong,dumb$HaulLat,col="red",lwd=2)
  if (Nhauls) text(dumb$ShootLong,dumb$ShootLat,labels = dumb$HaulNo,cex=.8)
  if (Hpoints) points(ShootLat~ShootLong,dumb,pch=21,cex=2,bg="blue")
  if (is.logical(ti)) {
    if (ti) {tit<-list(paste0(Survey," ",years," ",paste0("Q",quarter,collapse = "-")),font=2,cex=1.2*esc.mult)}
    else {tit<-NULL}
  }
  else {
    if(is.list(ti)) tit<-ti
    else tit<-list(ti)
  }
  if (ti) title(tit,line = 2)
  if (!is.logical(graf)) {
    dev.off()
    message(paste0("figura: ",getwd(),"/",graf,".png"))
  }
}




