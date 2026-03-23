#' Function InfoHH gives the information on Sweep lengths for the years selected
#' Data are taken directly from DATRAS getting all the data from DATRAS using function getHHdata from library(icesDatras) 
#' @param Survey: either the Survey to be downloaded from DATRAS (see details), or a data frame with the HH information with  the DATRAS HH format  and the years and quarter selected in years and quarter 
#' @param years: years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in yellow, it depends on the order of years, not the actual chronological year.
#' @param  quarter: the quarter of the survey to be ploted
#' @param country: The country chosen to be plotted (checks if it's available in the HH file)
#' @param getICES: Should the data be downloaded from DATRAS? If T, default, the data are taken from DATRAS through the icesDATRAS package. If F it analyzes the HH dataframe just with the available years independently of the years and quarter asked for.
#' @details Surveys available in DATRAS: i.e. NS-IBTS, SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @return Produces a graph with WingSpread vs. Depth. it also includes information on the ship, the time series used, the models and parameters estimated.
#' @examples InfoHH(Survey="SWC-IBTS",years=c(2011:2016),quarter=4)
#' @examples InfoHH(damb,c(2014:2016),4,getICES=F)
#' @export
InfoHH<-function(Survey,years,quarter,country=NA,getICES=T) {
  if (getICES) {
    dumb<-icesDatras::getDATRAS("HH",Survey,years,quarter)
    country<-unique(dumb$Country)
    if(!is.na(country)) {
      if (any(!dumb$Country %in% country)) {stop(paste(country,"is not present in this survey/quarter"))}
    dumb<-dplyr::filter(dumb,Country==country)
    }
    print(tapply(dumb$HaulNo,dumb[,c("SweepLngt","Year")],"length"))
  }
  if (!getICES) {
    dumb<-Survey
    dumb<-dplyr::filter(dumb,RecordType=="HH")
    print(paste("Number of hauls per sweeplength, year, ship and quarter in",unique(dumb$Survey)))
    print(tapply(dumb$HaulNo,dumb[,c("SweepLngt","Year","Quarter","Ship")],"length",na.rm=T))
    #print(tapply(dumb$HaulNo,dumb[,c("Ship","Year")],"length"))
    #if (exists("quarter")) {if (unique(dumb$Quarter)!=quarter) stop(paste0("Quarter selected ",quarter," is not available in the data.frame, check please"))}
    #if (exists("years")) {if (!all(unique(years) %in% unique(dumb$Year))) stop(paste0("Not all years selected in years are present in the data.frame, check: ",unique(years)[which(!(unique(years) %in% unique(damb$Year)))]))}
  } 
}
