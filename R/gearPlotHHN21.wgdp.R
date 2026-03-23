#' Function gearPlotHHN21.wgdp plots Wing Spread vs. Depth for N21
#' 
#' Produces a WingSpread vs. DoorSpread plot and a model with nls R function. Data are taken directly from DATRAS using function getDATRAS from library(icesDatras)
#' It only produces plots for surveys with HH files uploaded in DATRAS
#' If there are two different sweeps in the data, produces a model for each sweep length.
#' @param Survey: either the Survey to be downloaded from DATRAS (see details), or a data frame with the HH information with  the DATRAS HH format  and the years and quarter selected in years and quarter 
#' @param years: years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in steelblue2, it depends on the order of years, not the actual chronological year.
#' @param quarter: the quarter of the survey to be ploted
#' @param c.inta: the confidence interval to be used in the confint function for long sweeps and for sweeps if there is only one length
#' @param c.intb: the confidence interval to be used in the confint function for short sweeps if there are two
#' @param es: If TRUE all labels and legends are in Spanish, if FALSE in English
#' @param col1: color for the symbols and lines for the whole set if only one set of sweeps are used, and for the data from the long set of sweeps.
#' @param col2: color for the symbols and lines for the data from the short sweeps in case there are two.
#' @param getICES: Should the data be downloaded from DATRAS? If T, default, the data are taken from DATRAS through the icesDatras package.
#' @param pF: takes out the points and leaves only the lines in the graphs
#' @param ti: if F title will not be included automatically and can be addedd later
#' @details Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @return Produces a graph with WingSpread vs. Depth. it also includes information on the ship, the time series used, the models and parameters estimated.
#' @examples gearPlotHH.wgdp("SWC-IBTS",c(2014:2016),1,.6,.2,col1="darkblue",col2="steelblue")
#' @examples gearPlotHH.wgdp("SWC-IBTS",c(2013:2016),4,.6,.2)
#' @examples gearPlotHH.wgdp("ROCKALL",c(2013:2016),3,.8)
#' @examples gearPlotHH.wgdp("NIGFS",c(2005:2016),1,.2)
#' @examples gearPlotHH.wgdp("IE-IGFS",c(2005:2016),4,.9,.8,pF=F)
#' @examples gearPlotHH.wgdp("SP-PORC",c(2015:2016),3,.5)
#' @examples gearPlotHH.wgdp("FR-CGFS",c(1998:2016),4,.8)
#' @examples gearPlotHH.wgdp("EVHOE",c(1997:2016),4,.9)
#' @examples gearPlotHH.wgdp("SP-NORTH",c(2014:2016),4,.3,col1="darkblue")
#' @examples gearPlotHH.wgdp("SP-ARSA",c(2014:2016),1,.2)
#' @examples gearPlotHH.wgdp("SP-ARSA",c(2014:2016),4,.5)
#' @examples gearPlotHH.wgdp(damb,c(2014:2016),4,pF=F,getICES=F)
#' @export
gearPlotHHN21.wgdp<-function(Survey="SP-NORTH",years=2021,quarter=4,incl2=T,c.inta=.8,c.intb=.3,es=FALSE,col1="darkblue",col2="red",getICES=TRUE,pF=TRUE,ti=TRUE) {
  if (getICES) {
    dumb<-icesDatras::getDATRAS("HH",Survey,years,quarter)
  }
  if (!getICES) {
    dumb<-Survey
    if (!all(unique(years) %in% unique(dumb$Year))) stop(paste0("Not all years selected in years are present in the data.frame, check: ",unique(years)[which(!(unique(years) %in% unique(dumb$Year)))]))
    if (unique(dumb$Quarter)!=quarter) stop(paste0("Quarter selected ",quarter," is not available in the data.frame, check please"))
  }
  if (incl2) dumb<-dplyr::filter(dumb,HaulVal!="I") else dumb<-dplyr::filter(dumb,HaulVal=="V")
  #if (all(is.na(dumb$SweepLngt))) {stop("No valid Sweep Length data, this graph can not be produced")}
  dumb$Ship<-factor(dumb$Ship)
  dumb$SweepLngt[is.na(dumb$SweepLngt)]<-0
  dumb$Sweeplngt[dumb$SweepLngt>0]<-factor(dumb$SweepLngt[dumb$SweepLngt>0])
  if (length(subset(dumb$WingSpread,dumb$WingSpread> c(-9)))>0){
    wspr<-range(subset(dumb$WingSpread,dumb$WingSpread> c(0)))
    dspr<-range(subset(dumb$DoorSpread,dumb$DoorSpread>c(0)))
    dpthA<-range(dumb$Depth[dumb$Depth>0],na.rm=T)
    if (length(years)>1) {plot(WingSpread~Depth,dumb,xlim=c(0,dpthA[2]+20),ylim=c(0,wspr[2]+10),type="n",subset=WingSpread!=c(-9) & Year!=years[length(years)],pch=21,col=grey(.5),ylab=ifelse(es,"Abertura de calones (m)","Wing spread (m)"),xlab=ifelse(es,"Profundidad (m)","Depth (m)"))
      if(pF) {
        points(WingSpread~Depth,dumb,subset=WingSpread!=c(-9) & Year!=years[length(years)],pch=21,col=col1)
      }
    }
    if (length(years)==1) {plot(WingSpread~Depth,dumb,xlim=c(0,dpthA[2]+20),ylim=c(0,wspr[2]+10),type="n",subset=WingSpread!=c(-9) & WingSpread>0,pch=21,col=grey(.5),ylab=ifelse(es,"Abertura calones (m)","Wing spread (m)"),xlab=ifelse(es,"Profundidad (m)","Depth (m)"))
      if (pF) {
        points(WingSpread~Depth,dumb,xlim=c(0,dpthA[2]+20),subset=WingSpread!=c(-9) & WingSpread>0,pch=21,col=col1)
        #legend("bottomright",legend=as.character(c(years)),pch=21,col=col1,pt.bg=col1,bty="n",inset=.04)
      } 
    }
    if (ti) title(main=paste0(ifelse(es,"Abertura calones vs. profundidad en ","Wing Spread vs. Depth in "),dumb$Survey[1],".Q",quarter),line=2.5)
    # if (length(levels(factor(dumb$sweeplngt)))<2) {
    #   dpthA<-range(dumb$Depth[dumb$Depth>0],na.rm=T)
    #   dp<-seq(dpthA[1],dpthA[2]+20,length=650)
    #   if (length(years)>1) WingSpread.log<-nls(WingSpread~a1+b1*log(Depth),dumb,start=c(a1=.1,b1=1),subset=WingSpread!=c(-9) & WingSpread>0 & Year!=years[length(years)])
    #   else WingSpread.log<-nls(WingSpread~a1+b1*log(Depth),dumb,start=c(a1=.1,b1=1),subset=WingSpread!=c(-9) & WingSpread>0)
    #   if (pF) {
    #     points(WingSpread~Depth,dumb,subset=Year==years[length(years)],pch=21,bg=col1)
    #     if (length(years)>1) legend("bottomright",legend=c(paste0(years[1],"-",years[length(years)-1]),as.character(years[length(years)])),pch=21,col=col1,pt.bg=c(NA,col1),bty="n",inset=.02)
    #     else legend("bottomright",as.character(years),pch=21,col=col1,pt.bg=col1,bty="n",inset=.04)
    #   }
    #   if (ti) title(paste0("Wing Spread vs. Depth in ",dumb$Survey[1],".Q",quarter," survey"),line=2.5)
    #   mtext(dumb$Ship[1],line=.4,cex=.8,adj=0)
    #   a1<-round(coef(WingSpread.log)[1],2)
    #   b1<-round(coef(WingSpread.log)[2],2)
    #   lines(dp,a1+b1*log(dp),col=col1,lwd=2)
    #   a1low<-confint(WingSpread.log,level=c.inta)[1,1]
    #   b1low<-confint(WingSpread.log,level=c.inta)[2,1]
    #   lines(dp,a1low+b1low*log(dp),col=col1,lty=2,lwd=1)
    #   a1Upr<-confint(WingSpread.log,level=c.inta)[1,2]
    #   b1Upr<-confint(WingSpread.log,level=c.inta)[2,2]
    #   lines(dp,a1Upr+b1Upr*log(dp),col=col1,lty=2,lwd=1)
    #   legend("bottomright",legend=substitute(WS == a1 + b1 %*% log(depth),list(a1=round(coef(WingSpread.log)[1],2),b1=(round(coef(WingSpread.log)[2],2)))),bty="n",text.font=2,inset=.2)
    #   dumbo<-bquote("Wing Spread"== a + b %*% log("Depth"))
    #   mtext(dumbo,line=.4,side=3,cex=.8,font=2,adj=1)
    #   print(summary(WingSpread.log))
    # }
    if (length(levels(factor(dumb$Ship)))==2) {
      dumbmol<-subset(dumb,Ship=="29MO")
      dumbvde<-subset(dumb,Ship=="29VE")
      dpthAmol<-range(dumbmol$Depth,na.rm=T)
      dpthAvde<-range(dumbvde$Depth,na.rm=T)
      dpmol<-seq(dpthAmol[1],dpthAmol[2]+20,length=650)
      dpvde<-seq(dpthAvde[1],dpthAvde[2]+20,length=650)
      if (length(years)>1) {
        WingSpreadmol.log<-nls(WingSpread~a1+b1*log(Depth),dumbmol,start=c(a1=.1,b1=1),subset=WingSpread!=c(-9) & Year!=years[length(years)])
        WingSpreadvde.log<-nls(WingSpread~a1+b1*log(Depth),dumbvde,start=c(a1=.1,b1=1),subset=WingSpread!=c(-9) & Year!=years[length(years)])
      }
      else {
        WingSpreadmol.log<-nls(WingSpread~a1+b1*log(Depth),dumbmol,start=c(a1=.1,b1=1),subset=WingSpread!=c(-9))
        WingSpreadvde.log<-nls(WingSpread~a1+b1*log(Depth),dumbvde,start=c(a1=.1,b1=1),subset=WingSpread!=c(-9))
      }
      if (pF) {
        points(WingSpread~Depth,dumbmol,subset=HaulVal=="V",pch=21,col=col2)
        points(WingSpread~Depth,dumbvde,subset=HaulVal=="V",pch=21,col=col1)            
        if (length(years)>1) legend("bottomright",c(paste(c(paste(years[1],years[length(years)-1],sep="-"),years[length(years)]),c("mol sweeps"),sep=" "),paste(c(paste(years[1],years[length(years)-1],sep="-"),years[length(years)]),c("29VE"),sep=" ")),pch=21,col=c(col2,col1,col1,col1),pt.bg=c(NA,col2,NA,col1),bty="n",inset=c(.02),ncol=2)           
        else legend("bottomright",c("28MO","29VE"),pch=21,col=c(col1,col1),pt.bg = c(col2,col1),bty="n",inset=.04)
      }
      if (ti) title(main=paste0(ifelse(es,"Abertura calones vs. profundidad en ","Wing Spread vs. Depth in "),dumb$Survey[1],".Q",quarter),line=2.5)
      mtext(dumb$Ship[1],line=.4,cex=.8,adj=0)
      a1mol<-round(coef(WingSpreadmol.log)[1],2)
      b1mol<-round(coef(WingSpreadmol.log)[2],2)
      lines(dpmol,a1mol+b1mol*log(dpmol),col=col2,lwd=2)
      a1lowmol<-confint(WingSpreadmol.log,level=c.intb)[1,1]
      b1lowmol<-confint(WingSpreadmol.log,level=c.intb)[2,1]
      lines(dpmol,a1lowmol+b1lowmol*log(dpmol),col=col2,lty=2,lwd=1)
      a1Uprmol<-confint(WingSpreadmol.log,level=c.intb)[1,2]
      b1Uprmol<-confint(WingSpreadmol.log,level=c.intb)[2,2]
      lines(dpmol,a1Uprmol+b1Uprmol*log(dpmol),col=col2,lty=2,lwd=1)
      if (pF) {
        points(WingSpread~Depth,dumbmol,subset=Year==years[length(years)],pch=21,bg=col2)
        points(WingSpread~Depth,dumbvde,subset=Year==years[length(years)],pch=21,bg=col1)
        #legend("bottomright",c("Short sweeps","Long sweeps"),pch=21,col=c(col1,col1),pt.bg=c(col2,col1),bty="n",inset=.04)
      }
      legend("bottomleft",legend=substitute(WS29MO == a1mol + b1mol %*% log(depth),list(a1mol=round(coef(WingSpreadmol.log)[1],2),b1mol=(round(coef(WingSpreadmol.log)[2],2)))),bty="n",text.font=2,inset=c(.05,.1))
      a1vde<-round(coef(WingSpreadvde.log)[1],2)
      b1vde<-round(coef(WingSpreadvde.log)[2],2)
      lines(dpvde,a1vde+b1vde*log(dpvde),col=col1,lwd=2)
      a1lowvde<-confint(WingSpreadvde.log,level=c.inta)[1,1]
      b1lowvde<-confint(WingSpreadvde.log,level=c.inta)[2,1]
      lines(dpvde,a1lowvde+b1lowvde*log(dpvde),col=col1,lty=2,lwd=1)
      a1Uprvde<-confint(WingSpreadvde.log,level=c.inta)[1,2]
      b1Uprvde<-confint(WingSpreadvde.log,level=c.inta)[2,2]
      lines(dpvde,a1Uprvde+b1Uprvde*log(dpvde),col=col1,lty=2,lwd=1)
      legend("topright",legend=substitute(WSvde == a1vde + b1vde %*% log(depth),list(a1vde=round(coef(WingSpreadvde.log)[1],2),b1vde=(round(coef(WingSpreadvde.log)[2],2)))),bty="n",text.font=2,inset=.1)
      if (es) dumbo<-bquote("Abertura calones"== a + b %*% log("Prof"))
      else dumbo<-bquote("Wing Spread"== a + b %*% log("Depth"))
      mtext(dumbo,line=.4,side=3,cex=.8,font=2,adj=1)
      print(summary(WingSpreadmol.log))
      print(summary(WingSpreadvde.log))
    }
  }
  if (length(years)>1) txt<-paste0(ifelse(es,"A\u00f1os: ","Years: "),paste0(c(years[1],"-",years[length(years)]),collapse=" "))
  else txt<-paste0(ifelse(es,"A\u00f1o: ","Year: "),as.character(years))
  mtext(txt,1,line=-1.1,adj=0.01, font=1, cex=.9)
}
