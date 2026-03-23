#' Function gearPlotHHN21.dodp Door Spread versus Depth N21 Spanish North with two vessels
#' 
#' Produces a DoorSpread vs. Depth plot and model with nls R function. Data are taken directly from DATRAS using function getDATRAS from library(icesDatras)
#' it only produces plots for surveys with HH files uploaded in DATRAS
#' If there are two different sweeps in the data, produces a model for each sweep length.
#' @param getICES: Should the data be downloaded from DATRAS? If T, default, the data are taken from DATRAS through the icesDatras package.				   
#' @param Survey: either the Survey to be downloaded from DATRAS (see details), or a data frame with the HH information with  the DATRAS HH format  and the years and quarter selected in years and quarter 
#' @param years: years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in steelblue2, it depends on the order of years, not the actual chronological year.
#' @param quarter: the quarter of the survey to be plotted
#' @param c.int: the confidence interval to be used in the confint function
#' @param c.inta: the confidence interval to be used in the confint function for all data if only one sweep length, and for the short sweeps in case there are two
#' @param c.intb: the confidence interval to be used in the confint function for the long set of sweeps.
#' @param col1: color for the symbols and lines for the whole set if only one set of sweeps are used, and for the data from the long set of sweeps.
#' @param col2: color for the symbols and lines for the data from the short sweeps in case there are two.
#' @param pF: takes out the points and leaves only the lines in the graphs
#' @param ti: if F title will not be included automatically and can be addedd later
#' @details Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @return Produces a graph DoorSpread vs. Depth, it also includes information on the ship, the time series used, the models and parameters estimated.
#' @examples gearPlotHHN21.dodp("SP-NORTH",c(2021),3,.8,.3,col1="darkblue",col2="red")
#' @export
gearPlotHHN21.dodp<-function(Survey="SP-NORTH",years=2021,quarter=4,c.int=.95,c.inta=.95,c.intb=.95,es=FALSE,col1="darkblue",col2="red",getICES=TRUE,pF=TRUE,ti=TRUE) {
  if (getICES) {
    dumb<-icesDatras::getDATRAS("HH",Survey,years,quarter)
  }
  if (!getICES) {
    dumb<-Survey
    if (!all(unique(years) %in% unique(dumb$Year))) stop(paste0("Not all years selected in years are present in the data.frame, check: ",unique(years)[which(!(unique(years) %in% unique(dumb$Year)))]))
    if (unique(dumb$Quarter)!=quarter) stop(paste0("Quarter selected ",quarter," is not available in the data.frame, check please"))
  }
  dumb<-dplyr::filter(dumb,HaulVal!="I")
  dumb$ship<-factor(dumb$Ship)
  if (length(subset(dumb$DoorSpread,dumb$DoorSpread> c(-9)))>0){
    dspr<-range(subset(dumb$DoorSpread,dumb$DoorSpread>c(-9)))
    dpthA<-range(dumb$Depth,na.rm=T)
    dp<-seq(dpthA[1],dpthA[2]+20,length=650)
    plot(DoorSpread~Depth,dumb,type="n",xlim=c(0,dpthA[2]+20),ylim=c(0,dspr[2]+40),pch=21,col=col1,ylab=ifelse(es,"Abertura puertas (m)","Door spread (m)"),xlab=ifelse(es,"Profundidad (m)","Depth (m)"),subset=DoorSpread!=c(-9)& Year!=years[length(years)])
    if (pF & length(levels(dumb$sweeplngt))==1) {
      points(DoorSpread~Depth,dumb,pch=21,bg=col1,subset=c(Ship=="29MO"))         
      points(DoorSpread~Depth,dumb,subset=c(Ship=="29VE"),pch=21,bg=col2)
      if (length(years)==1) legend("bottomright",legend=as.character(c(years)),pch=21,col=col1,pt.bg=col1,bty="n",inset=.04)
      else legend("bottomright",c(paste0(years[1],"-",years[length(years)-1]),years[length(years)]),pch=c(1,21),col=c(col1),pt.bg=c(NA,col1),bty="n",inset=.02)
    }
    #if (ti) title(main=paste0(ifelse(es,"Abertura puertas vs. profundidad en","Door Spread vs. Depth in "),dumb$Survey[1],".Q",quarter),line=2.5)
    #       if (length(levels(dumb$sweeplngt))<2) {
    #          if(length(years)>1) {
    #            DoorSpread.log<-nls(DoorSpread~a1+b1*log(Depth),dumb,start=c(a1=.1,b1=1),subset=HaulVal=="V" & DoorSpread> c(-9) & Year!=years[length(years)])
    #            }
    #          else {
    #            DoorSpread.log<-nls(DoorSpread~a1+b1*log(Depth),dumb,start=c(a1=.1,b1=1),subset=HaulVal=="V" & DoorSpread> c(-9))
    #            }
    #          dspr<-range(subset(dumb,DoorSpread>c(-9))$DoorSpread,na.rm=T)
    #          mtext(paste(dumb$Ship[1]),line=.4,cex=.8,adj=0)
    #          a1<-round(coef(DoorSpread.log)[1],2)
    #          b1<-round(coef(DoorSpread.log)[2],2)
    #          lines(dp,a1+b1*log(dp),col=col1,lwd=2)
    #          a1low<-confint(DoorSpread.log,level=c.int)[1,1]
    #          b1low<-confint(DoorSpread.log,level=c.int)[2,1]
    #          lines(dp,a1low+b1low*log(dp),col=col1,lty=2,lwd=1)
    #          a1Upr<-confint(DoorSpread.log,level=c.int)[1,2]
    #          b1Upr<-confint(DoorSpread.log,level=c.int)[2,2]
    #          lines(dp,a1Upr+b1Upr*log(dp),col=col1,lty=2,lwd=1)
    #          legend("bottomright",legend=substitute(DS == a1 + b1 %*% log(depth),list(a1=round(coef(DoorSpread.log)[1],2),b1=(round(coef(DoorSpread.log)[2],2)))),bty="n",text.font=2,inset=.2)
    # #         text("bottomleft",paste0(c(years[1],"-",years[length(years)])),inset=c(0,.1))
    #          dumbo<-bquote("Door Spread"== a + b %*% log("Depth"))
    #          mtext(dumbo,line=.4,side=3,cex=.8,font=2,adj=1)
    #          summary(DoorSpread.log)
    #          }
    #          if (length(levels(dumb$sweeplngt))==2) {
    dumbmol<-subset(dumb,Ship=="29MO")
    dumbvde<-subset(dumb,Ship=="29VE")
    dpth<-range(dumb$Depth)
    dp<-seq(dpth[1],dpth[2]+20,length=650)
    #else legend("topright",c(paste0(years[1],"-",years[length(years)-1]),years[length(years)]),pch=c(1,21),col=c(col1),pt.bg=c(NA,col1))
    DoorSpreadmol.log<-nls(DoorSpread~a1+b1*log(Depth),dumbmol,start=c(a1=.1,b1=1),subset=HaulVal=="V" & DoorSpread> c(-9))
    DoorSpreadvde.log<-nls(DoorSpread~a1+b1*log(Depth),dumbvde,start=c(a1=.1,b1=1),subset=HaulVal=="V" & DoorSpread> c(-9))
    if (pF) {
      legend("bottomright",c("29MO","29VE"),pch=21,col=c(col1,col1),pt.bg=c(col1,col2),bty="n",inset=.04)
      points(DoorSpread~Depth,dumbmol,subset=Year==years[length(years)],pch=21,bg=col1)
      points(DoorSpread~Depth,dumbvde,subset=Year==years[length(years)],pch=21,bg=col2)
    }
  }
  
  dspr<-range(subset(dumbmol$DoorSpread,dumbmol$DoorSpread>c(-9)))
  if (pF) {
  }
  if (ti) title(main=paste0(ifelse(es,"Abertura de puertas vs. profundidad en ","Door Spread vs. Depth in "),dumb$Survey[1],".Q",quarter),line=2.5)
  mtext(dumb$Ship[1],line=.4,cex=.8,adj=0)
  a1mol<-round(coef(DoorSpreadmol.log)[1],2)
  b1mol<-round(coef(DoorSpreadmol.log)[2],2)
  lines(dp,a1mol+b1mol*log(dp),col=col1,lwd=2)
  a1lowmol<-confint(DoorSpreadmol.log,level=c.inta)[1,1]
  b1lowmol<-confint(DoorSpreadmol.log,level=c.inta)[2,1]
  lines(dp,a1lowmol+b1lowmol*log(dp),col=col1,lty=2,lwd=1)
  a1Uprmol<-confint(DoorSpreadmol.log,level=c.inta)[1,2]
  b1Uprmol<-confint(DoorSpreadmol.log,level=c.inta)[2,2]
  lines(dp,a1Uprmol+b1Uprmol*log(dp),col=col1,lty=2,lwd=1)
  legend("bottomleft",legend=substitute(DS29MO == a1mol + b1mol %*% log(depth),list(a1mol=round(coef(DoorSpreadmol.log)[1],2),b1mol=(round(coef(DoorSpreadmol.log)[2],2)))),bty="n",text.font=2,cex=.9,inset=c(.05,.2))
  a1vde<-round(coef(DoorSpreadvde.log)[1],2)
  b1vde<-round(coef(DoorSpreadvde.log)[2],2)
  lines(dp,a1vde+b1vde*log(dp),col=col2,lwd=2)
  a1lowvde<-confint(DoorSpreadvde.log,level=c.intb)[1,1]
  b1lowvde<-confint(DoorSpreadvde.log,level=c.intb)[2,1]
  lines(dp,a1lowvde+b1lowvde*log(dp),col=col2,lty=2,lwd=1)
  a1Uprvde<-confint(DoorSpreadvde.log,level=c.intb)[1,2]
  b1Uprvde<-confint(DoorSpreadvde.log,level=c.intb)[2,2]
  lines(dp,a1Uprvde+b1Uprvde*log(dp),col=col2,lty=2,lwd=1)
  legend("bottomright",legend=substitute(DS29VE == a1vde + b1vde %*% log(depth),list(a1vde=round(coef(DoorSpreadvde.log)[1],2),b1vde=(round(coef(DoorSpreadvde.log)[2],2)))),bty="n",text.font=2,cex=.9,inset=c(.01,.4))
  #         text("bottomleft",paste0(c(years[1],"-",years[length(years)])),inset=c(0,.1))
  dumbo<-bquote("Door Spread"== a + b %*% log("Depth"))
  mtext(dumbo,line=.4,side=3,cex=.8,font=2,adj=1)
  summary(DoorSpreadmol.log)
  summary(DoorSpreadvde.log)
}
#     if (length(years)>1) txt<-paste0("Years: ",paste0(c(years[1],"-",years[length(years)]),collapse=" "))
  #     else txt<-paste0("Year: ",as.character(years))
  #     mtext(txt,1,line=-1.1,adj=0.01, font=1, cex=.9)
  #  }
  # }

# windows()
# dumb<-icesDatras::getDATRAS("HH","SP-NORTH",2021,4)
# plot(DoorSpread~Depth,dumb,type="n",subset=DoorSpread>0,xlim=c(0,900),ylim=c(0,140),main="N21 Survey")
# points(DoorSpread~Depth,dumb,subset = Ship=="29MO",pch=21,bg="blue")
# points(DoorSpread~Depth,dumb,subset = Ship=="29VE",pch=21,bg="red")
# legend("bottomright",c("Miguel Oliver","Vizconde de Eza"),pch=21,pt.bg=c("blue","red"),inset=(.05))
# text(DoorSpread~Depth,dumb,label=paste(Day,Month),cex=.7,font=2,pos=1)
# text(DoorSpread~Depth,dumb,label=HaulNo,cex=.7,font=2,pos=3)
