#' Function gearPlotsHH to plot gear parameters and behaviour 
#' 
#' Data are taken directly from DATRAS getting all the data from DATRAS using function getHHdata from library(icesDatras)
#' it only produces plots for surveys with HH files uploaded in DATRAS
#' If there are DoorSpread and WingSpread values in HH records produces four graphs, if only DoorSpread values are available produces only two graphs
#' @param Survey: either the Survey to be downloaded from DATRAS (see details), or a data frame with the HH information with  the DATRAS HH format  and the years and quarter selected in years and quarter 
#' @param years: years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in yellow, it depends on the order of years, not the actual chronological year.
#' @param quarter: the quarter of the survey to be ploted
#' @param c.inta: the confidence interval to be used in the confint function for long sweeps and for sweeps if there is only one length
#' @param c.intb: the confidence interval to be used in the confint function for short sweeps if there are two
#' @param col1: color for the symbols and lines for the whole set if only one set of sweeps are used, and for the data from the long set of sweeps.
#' @param col2: color for the symbols and lines for the data from the short sweeps in case there are two.
#' @param getICES: Should the data be downloaded from DATRAS? If T, default, the data are taken from DATRAS through the icesDatras package.
#' @param pF: takes out the points and leaves only the lines in the graphs
#' @details Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @return Produces a set of 4 or 2 graphs: Warp shot vs. depth, DoorSpread vs. WingSpread, WingSpread vs. Depth, DoorSpread vs. Depth, Vertical Opening vs Depth  it also includes information on the ship, the time series used (bottom fourth graph), the models and parameters estimated.
#' @examples gearPlotsHH("SWC-IBTS",c(2014:2016),1)
#' @examples gearPlotsHH("SWC-IBTS",c(2013:2016),4)
#' @examples gearPlotsHH("ROCKALL",c(2013:2016),3)
#' @examples gearPlotsHH("IE-IGFS",c(2011:2016),4)
#' @examples gearPlotsHH("SP-PORC",c(2003:2016),3)
#' @examples gearPlotsHH("FR-CGFS",c(2015:2016),4)
#' @examples gearPlotsHH("EVHOE",c(1997:2015),4)
#' @examples gearPlotsHH("SP-NORTH",c(2014:2016),4)
#' @examples gearPlotsHH("SP-ARSA",c(2014:2016),1)
#' @examples gearPlotsHH("SP-ARSA",c(2014:2016),4)
#' @examples gearPlotsHH(damb,c(2014:2016),4,getICES=F)
#' @export
gearPlotsHH<-function(Survey,years,quarter=T,c.inta=.5,c.intb=.5,col1="darkblue",col2="steelblue2",getICES=T,pF=T) {
  if (getICES) {
    dumb<-icesDatras::getDATRAS("HH",Survey,years,quarter)
  }
  if (!getICES) {
    dumb<-Survey
    if (!all(unique(years) %in% unique(dumb$Year))) stop(paste0("Not all years selected in years are present in the data.frame, check: ",unique(years)[which(!(unique(years) %in% unique(damb$Year)))]))
    if (unique(dumb$Quarter)!=quarter) stop(paste0("Quarter selected ",quarter," is not available in the data.frame, check please"))
  }
  opar<-par(no.readonly=T)
  par(mfrow=c(2,2))
   if (length(subset(dumb$WingSpread,dumb$WingSpread> c(-9)))>0){
      gearPlotHH.wrpdp(Survey,years,quarter,col1=col1,getICES=getICES,pF=pF)
      mtext("a)",line=2.5,font=2,adj=0)
      gearPlotHH.wgdp(Survey,years,quarter,c.inta,c.intb,col1,col2,getICES=getICES,pF=pF)
      mtext("b)",line=2.5,font=2,adj=0)
   } else par(mfrow=c(1,2))
    if (length(subset(dumb$DoorSpread,dumb$DoorSpread> c(-9)))>0){
      gearPlotHH.dodp(Survey=Survey,years=years,quarter=quarter,c.inta=c.inta,c.intb=c.intb,col1=col1,col2=col2,getICES=getICES,pF=pF)
      mtext("c)",line=2.5,font=2,adj=0)
      gearPlotHH.nodp(Survey,years,quarter,c.inta,c.intb,col1,col2,getICES=getICES,pF=pF)
      mtext("d)",line=2.5,font=2,adj=0)
    }
  par(opar)
  }

            