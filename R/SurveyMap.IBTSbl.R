#' Map of hauls (stations) from an IBTS survey taken directly from DATRAS
#'
#' Uses the DATRAS HH data of a survey, year and quarter to plot stations in a map
#' @param Survey Code to plot. Has to be one of those available from DATRAS survey list, and from a year and quarter avalable
#' @param Year year data to plot
#' @param Quarter quarter data to plot
#' @param ti TRUE includes the Survey Year Quarter title by default, a text includes the txt as title
#' @param colhaul choose the color for the bg of the haul if country and sweeplngth are False
#' @param country if TRUE hauls have different colors by country, useful only for NS where there are different countries
#' @param sweeplngt if TRUE haul symbols change with different sweeptlengths. Useful to check if sweep lengths and depths math
#' @param Depth if TRUE includes the depth of each haul, useful to check if it matches sweep length used
#' @param leg TRUE includes a legend with the country-sweeplength-colors codes
#' @param legpos Position of the legend, by default bottomright, (bottomleft, upperleft...) 
#' @param graf if FALSE the graph goes to screen, if its a file name (i.e. "graf") a file with that name is created and a message with location (wd) is shown in screen
#' @param xpng width file png if graf is the name of the file
#' @param ypng height file png if graf is the name of the file
#' @param ppng points png parameter if graf is the name of the file
#' @return Presents the map with the stations and a legend with the countries that have participated in the survey
#' @examples
#' SurveyMap.IBTS("NS-IBTS",2021,3,sweeplngt=F,country=T,graf="NS_2021_Q3")
#' SurveyMap.IBTS("FR-WCGFS",2023,3,ICESrect = T,ICESlab = T,ICESlabcex = .6)
#' @family maps
#' @export
SurveyMap.IBTSbl<-function(Survey,Year,Quarter,ti=TRUE,leg=TRUE,legpos="bottomright",colhaul="yellow",
                         ICESrect=FALSE,ICESlab=FALSE,ICESlabcex=.7,graf=FALSE,xpng=800,ypng=800,ppng=15){
  if (length(Year)>1) stop("Only one year can be shown in this function")
  hauls<-icesDatras::getDATRAS("HH",Survey,Year,Quarter)
  print(tapply(hauls$Country,hauls[,c("Country","SweepLngt","Year")],"length"))
  if (!is.logical(graf)) png(filename=paste0(graf,".png"),width = xpng,height = ypng, pointsize = ppng)
  if (is.logical(graf)) par(mar=c(2,2.5,2, 2.5) + 0.3,xaxs="i",yaxs="i")
## Creates *replong* variable correct the longitude in PORCUPINE and ROCKALL surveys that
##  are faraway from land and are not displayed otherwise
  if (Survey=="SP-PORC") replong<-2
  if (Survey=="SCOROC") replong<-5
  if (! Survey %in% c("SP-PORC","SCOROC")) replong<-.5
##  
  IBTSNeAtl_map(load=F,NS=F,leg = F,xlims = c(min(hauls$HaulLong)-.5,replong+max(hauls$HaulLong)),sl=min(hauls$HaulLat)-.5,nl=.5+max(hauls$HaulLat),ICESrect = ICESrect,ICESlab = ICESlab,ICESlabcex = ICESlabcex)
  if (is.logical(ti)) {
    if (ti) {tit<-list(paste0(Survey," ",Year," ",paste0("Q",Quarter,collapse = "-")),font=2,cex=1.2)}
    else {tit<-NULL}
  }
  else {
    if(is.list(ti)) tit<-ti
    else tit<-list(ti)
  }
  if (ti) title(tit,line = 1.5)
  if (!is.logical(graf)) {
    dev.off()
    message(paste0("figura: ",getwd(),"/",graf,".png"))
    }
}

