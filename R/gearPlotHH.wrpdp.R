#' Function gearPlotHH.wrpdp plots warp length vs. Depth behaviour 
#' 
#' Produces a warplength vs. DoorSpread plot and a model with lm unction. Data are taken directly from DATRAS using function getDATRAS from library(icesDatras)
#' It only produces plots for surveys with HH files uploaded in DATRAS
#' Since sweeps length does not affect the warp that is decided by the chief scientist, there are no differences depending on sweeps
#' @param Survey: either the Survey to be downloaded from DATRAS (see details), or a data frame with the HH information with  the DATRAS HH format  and the years and quarter selected in years and quarter 
#' @param years: years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in steelblue2, it depends on the order of years, not the actual chronological year.
#' @param quarter: the quarter of the survey to be ploted
#' @param line: includes a regression line between Warp and depth and the formula of the linear regression. If F the line is omited
#' @param c.inta: the confidence interval to be used in the predict.lm function 
#' @param es: if TRUE all labels axes and legends are in Spanish, if FALSE in English
#' @param col1: the color of the points, last year fill and previous years empty symbol
#' @param getICES: Should the data be downloaded from DATRAS? If T, default, the data are taken from DATRAS through the icesDatras package.
#' @param pF: takes out the points and leaves only the lines in the graphs
#' @param ti: if T includes autmoatically the title, F leaves it blank an can be added later.
#' @details Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @return Produces a graph Warp length vs. Depth for the years selected.
#, it also includes information on the ship, the time series used the model used and parameters estimated.
#' @examples gearPlotHH.wrpdp("SWC-IBTS",c(2011:2016),1,c.inta=.95,col1="darkblue",pF=F)
#' @examples gearPlotHH.wrpdp("SWC-IBTS",c(2013:2016),4)
#' @examples gearPlotHH.wrpdp("ROCKALL",c(2013:2016),3)
#' @examples gearPlotHH.wrpdp("NIGFS",c(2005:2016),1)
#' @examples gearPlotHH.wrpdp("NIGFS",c(2006:2007,2009:2016),4)
#' @examples gearPlotHH.wrpdp("IE-IGFS",c(2005:2016),4)
#' @examples gearPlotHH.wrpdp("SP-PORC",c(2003:2016),3)
#' @examples gearPlotHH.wrpdp("FR-CGFS",c(1998:2016),4)
#' @examples gearPlotHH.wrpdp("EVHOE",c(1997:2016),4)
#' @examples gearPlotHH.wrpdp("SP-NORTH",c(2014:2016),4)
#' @examples gearPlotHH.wrpdp("SP-ARSA",c(2014:2016),1)
#' @examples gearPlotHH.wrpdp("SP-ARSA",c(2014:2016),4)
#' @examples gearPlotHH.wrpdp(damb,c(2014:2016),4,getICES=F,pF=F)
#' @export
gearPlotHH.wrpdp<-function(Survey,years,quarter,incl2=TRUE,line=TRUE,c.inta=.95,es=FALSE,col1="darkblue",getICES=TRUE,pF=TRUE,ti=TRUE) {
  if (getICES) {
    dumb<-icesDatras::getDATRAS("HH",Survey,years,quarter)
  }
  if (!getICES) {
    dumb<-Survey
    if (!all(unique(years) %in% unique(dumb$Year))) stop(paste0("Not all years selected in years are present in the data.frame, check: ",unique(years)[which(!(unique(years) %in% unique(dumb$Year)))]))
    if (!all(unique(dumb$Quarter) %in% quarter)) warning(paste0("Quarter selected ",quarter," is not available in the data.frame, check please"))
  }
  dumb<-if(incl2) dplyr::filter(dumb,HaulVal!="I") else dplyr::filter(dumb,HaulVal=="V")
  if (any(!is.na(dumb$Warplngt))) warps=T else warps=F       # Present graphs
   if (warps) {
     wrp<-range(subset(dumb$Warplngt,dumb$Warplngt> c(0)),na.rm=T)
     dpthA<-range(dumb$Depth[dumb$Depth>0],na.rm=T)
     plot(Warplngt~Depth,dumb,type="n",subset=c(Year!=years[length(years)]),cex=1,pch=21,col=col1,ylab=ifelse(es,"Cable largado (m)","Warp length (m)"),xlab=ifelse(es,"Profundidad (m)","Depth (m)"),xlim=c(0,max(dumb$Depth,na.rm=T)),ylim=c(0,max(dumb$Warplngt,na.rm=T)*1.1))
     if (ti) title(main=paste0(ifelse(es,"Cable largado vs. profundidad en ","Warp shot vs. Depth in "),dumb$Survey[1],".Q",quarter),line=2.5)
     if (pF) {
       points(Warplngt~Depth,dumb,subset=c(Year %in% years),pch=21,cex=1,col=col1)
       points(Warplngt~Depth,dumb,subset=c(Year==years[length(years)]),pch=21,bg=col1,col=grey(.0))
     }
     mtext(dumb$Ship[1],line=.4,cex=.8,adj=0)
     if (line) {
       if (length(years)>1) {lm.WarpVsDepth<-lm(Warplngt~Depth,dumb,subset=HaulVal=="V" & Warplngt > c(0) & Depth> c(0) & Year!=years[length(years)])}
       else lm.WarpVsDepth<-lm(Warplngt~Depth,dumb,subset=Warplngt > c(0) & Depth> c(0)) 
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
       if (es) dumbo<-bquote("Cable"== a + b %*% Prof)
       else dumbo<-bquote("Warp"== a + b %*% Depth)
       mtext(dumbo,line=.4,side=3,cex=.8,font=2,adj=1)
     }
     if (pF) {
      if (length(years)>1) {legend("bottomright",legend=c(paste(years[length(years)]),paste0(years[1],"-",years[length(years)-1])),pch=c(21),col=c(1,col1),pt.bg=c(col1,NA),inset=.05,bty="n")}
        else legend("bottomright",legend=paste("Hauls",years),pch=21,col=1,pt.bg=col1,inset=.04,bty="n")
      }
     if (length(years)>1) {
     txt<-paste0(ifelse(es,"A\u00f1os: ","Years: "),paste0(c(years[1],"-",years[length(years)]),collapse=""),collapse="")
     mtext(txt,1,line=-1.1,adj=0.01, font=1, cex=.9)
     }
   }
  else {
    plot(HaulNo~Depth,dumb,type="n",subset=c(HaulVal!="I" & Year!=years[length(years)]),cex=1,pch=21,col=col1,ylab=ifelse(es,"Cable largado (m)","Warp length (m)"),xlab=ifelse(es,"Profundidad (m)","Depth (m)"),xlim=c(0,max(dumb$Depth,na.rm=T)),ylim=c(0,max(dumb$Depth,na.rm=T)*1.1))
    if (ti) title(main=paste0("Warp shot vs. Depth in ",dumb$Survey[1],".Q",quarter," survey"),line=2.5)
    mtext(ifelse(es,"Sin datos de cable","No Data for Warp Length"),font=2,cex=.8,line=.2)
  }
}
   
   

