#' Function gearPlotHHNS.wrpdp plots warp length vs. Depth behaviour 
#' 
#' Produces a warplength vs. DoorSpread plot and a model with lm unction. Data are taken directly from DATRAS using function getHHdata from library(icesDatras)
#' It only produces plots for surveys with HH files uploaded in DATRAS
#' Since sweeps length does not affect the warp that is decided by the chief scientist, there are no differences depending on sweeps
#' @param Survey: either the Survey to be downloaded from DATRAS (see details), or a data frame with the HH information with  the DATRAS HH format  and the years and quarter selected in years and quarter 
#' @param years: years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in steelblue2, it depends on the order of years, not the actual chronological year.
#' @param quarter: the quarter of the survey to be ploted
#' @param country: The country chosen to be plotted (checks if it's available in the HH file)
#' @param line: includes a regression line between Warp and depth and the formula of the linear regression. If F the line is omited
#' @param c.inta: the confidence interval to be used in the predict.lm function 
#' @param col1: the color of the points, last year fill and previous years empty symbol
#' @param getICES: Should the data be downloaded from DATRAS? If T, default, the data are taken from DATRAS through the icesDatras package.
#' @param pF: takes out the points and leaves only the lines in the graphs
#' @details Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @return Produces a graph Warp length vs. Depth for the years selected.
#, it also includes information on the ship, the time series used the model used and parameters estimated.
#' @examples gearPlotHHNS.wrpdp("NS-IBTS",c(2014:2017),1,"SWE")
#' @export
gearPlotHHNS.wrpdp<-function(Survey="NS-IBTS",years,quarter,country,line=T,c.inta=.85,col1="darkblue",getICES=T,pF=T) {
  if (getICES) {
    dumb<-icesDatras::getDATRAS("HH",Survey,years,quarter)
  }
  if (!getICES) {
    dumb<-Survey
    if (!all(unique(years) %in% unique(dumb$Year))) stop(paste0("Not all years selected in years are present in the data.frame, check: ",unique(years)[which(!(unique(years) %in% unique(dumb$Year)))]))
    if (unique(dumb$Quarter)!=quarter) stop(paste0("Quarter selected ",quarter," is not available in the data.frame, check please"))
  }
  dumb<-dplyr::filter(dumb,HaulVal=="V")
  countries<-unique(dumb$Country)
  if (!country %in% countries) stop(paste(country,"is not present in this survey/quarter"))
  dumb<-dplyr::filter(dumb,Country==country)
  if (any(!is.na(dumb$Warplngt))) warps=T else warps=F       # Present graphs
   if (warps) {
     wrp<-range(subset(dumb$Warplngt,dumb$Warplngt> c(0)),na.rm=T)
     dpthA<-range(dumb$Depth[dumb$Depth>0],na.rm=T)
     plot(Warplngt~Depth,dumb,type="n",subset=c(HaulVal!="I" & Year!=years[length(years)]),cex=1,pch=21,col=col1,ylab="Warp length (m)",xlab="Depth (m)",xlim=c(0,max(dumb$Depth,na.rm=T)),ylim=c(0,max(dumb$Warplngt,na.rm=T)*1.1))
     title(main=paste0("Warp shot vs. Depth in ",country," ",dumb$Survey[1],".Q",quarter," survey"),line=2.5)
     if (pF) {points(Warplngt~Depth,dumb,subset=c(HaulVal!="I" & Year %in% years),pch=21,cex=1,col=col1)
        points(Warplngt~Depth,dumb,subset=c(HaulVal!="I" & Year==years[length(years)]),pch=21,bg=col1,col=grey(.0))
     }
     mtext(paste(c("Ship: ", unique(dumb$Ship)), collapse=" "),line=.4,cex=.8,adj=0)
     if (line) {
       lm.WarpVsDepth<-lm(Warplngt~Depth,dumb,subset=HaulVal=="V" & Warplngt > c(0) & Depth> c(0))
       dpt<-data.frame(Depth=seq(dpthA[1],dpthA[2],length.out = 100))
       pred.plim<-predict(lm.WarpVsDepth,newdata=dpt,interval="prediction",level=c.inta)
       pred.clim<-predict(lm.WarpVsDepth,newdata=dpt,interval="confidence",level=c.inta)
       matlines(dpt$Depth,cbind(pred.clim,pred.plim[,-1]),lty=c(1,2,2,2,2),lwd=c(2,1,1,1,1),col=col1)
       # lines(dpt$Depth,predCI$fit[,"upr"],col="red",lwd=2,lty=2)
       # lines(dpt$Depth,predCI.2[,"upr"],col="green",lwd=2,lty=2)
       # lines(dpt$Depth,predCI.3[,"upr"],col="yellow",lwd=2,lty=2)
       # lines(pred~dpt$Depth,col=col1,lty=1,lwd=2)
       # lines(dpt$Depth,predCI[,"upr"],col=col1,lwd=1,lty=2)
       # lines(dpt$Depth,predCI[,"lwr"],col=col1,lwd=1,lty=2)
       legend("topleft",legend=substitute(paste(Wrp == a + b %*% Dpth),list(a=round(coef(lm.WarpVsDepth)[1],2),b=(round(coef(lm.WarpVsDepth)[2],2)))),bty="n",text.font=2,inset=.05,xjust=0)
       legend("topleft",legend=substitute(paste(r^2 ==resq),list(resq=round(summary(lm.WarpVsDepth)$adj.r.squared,2))),inset=c(.2,.1),xjust=0.5,cex=.9,bty="n")
       dumbo<-bquote("Warp"== a + b %*% Depth)
       mtext(dumbo,line=.4,side=3,cex=.8,font=2,adj=1)
     }
     if (pF) {
        if (length(years)>1) {legend("bottomright",legend=c(paste("Hauls",years[length(years)]),"Previous hauls"),pch=c(21),col=c(1,col1),pt.bg=c(col1,NA),inset=.05,bty="n")}
        else legend("bottomright",legend=paste("Hauls",years),pch=21,col=1,pt.bg=col1,inset=.04,bty="n")
      }
     if (length(years)>1) {
     txt<-paste0("Years: ",paste0(c(years[1],"-",years[length(years)]),collapse=""),collapse="")
     mtext(txt,1,line=-1.1,adj=0.01, font=1, cex=.8)
     }
   }
}
   
   

