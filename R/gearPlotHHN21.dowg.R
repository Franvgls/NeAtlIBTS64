#' gearPlotHH.dowg Wing Spread vs. DoorSpread in N21 Spanish North with two vessels
#'
#' Plots Door Spread vs. Wing Spread behaviour and produces a model using lm. If there are DoorSpread and WingSpread values.
#' If there are two different sweeps in the data, produces a model for each sweep length.
#' @param getICES: Should the data be downloaded from DATRAS? If T, default, the data are taken from DATRAS through the icesDatras package.
#' @param Survey: either the Survey to be downloaded from DATRAS (see details), or a data frame with the HH information with  the DATRAS HH format  and the years and quarter selected in years and quarter
#' @param years: years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in steelblue2, it depends on the order of years, not the actual chronological year.
#' @param quarter: the quarter of the survey to be ploted
#' @param datHH: an HH data object with Survey, Year and Quarter columns, overrides Survey, Years, Quarters
#' @param c.int: the confidenc interval to be used in the confint function
#' @param c.inta: the confidence interval to be used in the confint function for all data if only one sweep length, and for the short sweeps in case there are two
#' @param c.intb: the confidence interval to be used in the confint function for the long set of sweeps.
#' @param es: if TRUE all axes, labels and titles are in Spanish, if FALSE in English
#' @param col1: color for the symbols and lines for the whole set if only one set of sweeps are used, and for the data from the long set of sweeps.
#' @param col2: color for the symbols and lines for the data from the short sweeps in case there are two.
#' @param pF: takes out the points and leaves only the lines in the graphs
#' @param ti: if F title will not be included automatically and can be addedd later
#' @details Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @return Produces a graph with WingSpread vs. DoorSpread, it also includes information on the ship, the time series used, the models and parameters estimated.
#' @examples
#' \dontrun{
#' gearPlotHH.dowg("SCOWCGFS",c(2014:2016),1,col1="darkblue",col2="steelblue3")
#' gearPlotHH.dowg("SCOWCGFS",c(2013:2016),4)
#' gearPlotHH.dowg("SCOROC",c(2013:2016),3)
#' gearPlotHH.dowg("NIGFS",c(2015:2016),1)
#' gearPlotHH.dowg(damb,c(2014:2016),4,getICES=F,pF=F)
#' }
#' @export
gearPlotHHN21.dowg<-function(Survey="SP-NORTH",years="2021",quarter=4,c.int=.9,c.inta=.8,c.intb=.8,es=TRUE,col1="darkblue",col2="red",getICES=T,pF=T,ti=T) {
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
    if (length(subset(dumb$WingSpread,dumb$WingSpread> c(-9)))==0) {stop("No records with valid WingSpread > 0")}
    #if (length(subset(dumb$DoorSpread,dumb$DoorSpread==0))>0) {stop("Records with DoorSpread = 0, please check and remove")}
    if (length(subset(dumb,WingSpread>c(-9)))>0){
      dumb<-dumb[c(dumb$WingSpread>c(-9) & dumb$WingSpread>c(-9)),]
      wspr<-range(subset(dumb$WingSpread,dumb$WingSpread> c(0)))
      dspr<-range(subset(dumb$DoorSpread,dumb$DoorSpread>c(0)))
      # if (length(levels(factor(dumb$SweepLngt)))<2) {
      #   if (length(years)>1) lm.DoorVsWing<-lm(DoorSpread~WingSpread,dumb,subset=c(HaulVal=="V" & WingSpread>0 & DoorSpread>0 & Year!=years[length(years)]))
      #   else lm.DoorVsWing<-lm(DoorSpread~WingSpread,dumb,subset=c(HaulVal=="V" & WingSpread>0 & DoorSpread>0))
      #   #outlierTest(lm.DoorVsWing,data=dumb)
      #   ws<-data.frame(WingSpread=seq(wspr[1],wspr[2],length.out = 10))
      #   plot(DoorSpread~WingSpread,dumb,type="n",subset=HaulVal=="V" & Year!=years[length(years)],xlim=c(wspr[1]-10,wspr[2]+10),ylim=c(dspr[1]-20,dspr[2]+20),xlab="Wing Spread (m)",ylab="Door Spread (m)",pch=21,col="grey")
      #   if (pF) {
      #     points(DoorSpread~WingSpread,dumb,subset=HaulVal=="V" & Year!=years[length(years)])
      #     if (length(years)>1) legend("bottomright",legend=c(paste0(years[1],"-",years[length(years)-1]),as.character(years[length(years)])),pch=21,col=col1,pt.bg=c(NA,col1),bty="n",inset=.02)
      #     else legend("bottomright",as.character(years),pch=21,col=col1,pt.bg=col1,bty="n",inset=.04)
      #   }
      #   if (ti) title(main=paste0("Door spread vs. wing spread in ",dumb$Survey[1],".Q",quarter," survey"),line=2.5)
      #   mtext(dumb$Ship[1],line=.4,cex=.8,adj=0)
      #   if (pF) {points(DoorSpread~WingSpread,dumb,subset=c(HaulVal=="V" & Year==years[length(years)]),pch=21,bg=col1)}
      #   ws<-data.frame(WingSpread=seq(wspr[1],wspr[2],length.out = 10))
      #   pred <- predict(lm.DoorVsWing, newdata = ws)
      #   lines(pred~ws$WingSpread,col=col1,lty=1,lwd=2)
      #   a1low<-confint(lm.DoorVsWing,level=c.int)[1,1]
      #   b1low<-confint(lm.DoorVsWing,level=c.int)[2,1]
      #   lines(ws$WingSpread,a1low+b1low*ws$WingSpread,col= col1, lty=2,lwd=1)
      #   a1Upr<-confint(lm.DoorVsWing,level=c.int)[1,2]
      #   b1Upr<-confint(lm.DoorVsWing,level=c.int)[2,2]
      #   lines(ws$WingSpread,a1Upr+b1Upr*ws$WingSpread,col=col1,lty=2,lwd=1)
      #   #abline(lm.DoorVsWing,col=2,lty=2)
      #   legend("bottomright",legend=substitute(paste(DS == a + b %*% WS),list(a=round(coef(lm.DoorVsWing)[1],2),b=(round(coef(lm.DoorVsWing)[2],2)))),bty="n",text.font=2,inset=.2)
      #   legend("bottomright",legend=substitute(paste(r^2 ==resq),list(resq=round(summary(lm.DoorVsWing)$adj.r.squared,2))),inset=c(.25,.15),cex=.9,bty="n")
      #   dumbo<-bquote("DS"== a + b %*% WS)
      #   mtext(dumbo,line=.4,side=3,cex=.8,font=2,adj=1)
      # }
      if (length(levels(factor(dumb$Ship)))==2) {
        dumbmol<-subset(dumb,Ship=="29MO")
        dumbvde<-subset(dumb,ship=="29VE")
        if (length(years)>1) {
          lm.DoorVsWing.mol<-lm(DoorSpread~WingSpread,dumbmol,subset=WingSpread > c(-9) & DoorSpread> c(-9) & SweepLngt==levels(factor(SweepLngt))[1] & c(StNo!="FG1"&Year!=2015) & Year!= years[length(years)])
          lm.DoorVsWing.vde<-lm(DoorSpread~WingSpread,dumbvde,subset=WingSpread > c(-9) & DoorSpread> c(-9) & SweepLngt==levels(factor(SweepLngt))[2] & Year!= years[length(years)])
        }
        else {
          lm.DoorVsWing.mol<-lm(DoorSpread~WingSpread,dumbmol,subset=WingSpread > c(-9) & DoorSpread> c(-9))
          lm.DoorVsWing.vde<-lm(DoorSpread~WingSpread,dumbvde,subset=WingSpread > c(-9) & DoorSpread> c(-9))
        }
        plot(DoorSpread~WingSpread,dumb,type="n",subset=HaulVal!="I" & Year!=years[length(years)],xlim=c(wspr[1]-10,wspr[2]+10),ylim=c(dspr[1]-20,dspr[2]+20),xlab=ifelse(es,"Abertura calones (m)","Wing Spread (m)"),ylab=ifelse(es,"Abertura puertas (m)","Door Spread (m)"),pch=21,col="grey")
        if (ti) title(main=paste0("Door spread vs. wing spread in ",dumb$Survey[1],".Q",quarter," survey"),line=2.5)
        #mtext(dumb$Ship[1],line=.4,cex=.8,adj=0)
        if (pF){
          points(DoorSpread~WingSpread,dumbmol,subset=c(HaulVal!="I"),pch=21,col=col1)
          points(DoorSpread~WingSpread,dumbvde,subset=c(HaulVal!="I"),pch=21,col=col2)
          points(DoorSpread~WingSpread,dumbmol,
                 subset=c(HaulVal!="I" & Year==years[length(years)]),pch=21,bg=col1)
          points(DoorSpread~WingSpread,dumbvde,
                 subset=c(HaulVal!="I" & Year==years[length(years)]),pch=21,bg=col2)
          if (length(years)>1) legend("bottomright",c(paste(c(paste(years[1],years[length(years)-1],sep="-"),years[length(years)]),c("Short sweeps"),sep=" "),paste(c(paste(years[1],years[length(years)-1],sep="-"),years[length(years)]),c("Long sweeps"),sep=" ")),pch=21,col=c(col2,col1,col1,col1),pt.bg=c(NA,col2,NA,col1),bty="n",inset=c(.02),ncol=2)
          else legend("bottomright",c("29MO","29VE"),pch=21,col=c(col1,col1),pt.bg = c(col1,col2),bty="n",inset=.04)
        }
        wsprsrt<-range(dumb$WingSpread)
        wsshort<-data.frame(WingSpread=seq(wsprsrt[1],wsprsrt[2],length.out = 10))
        predmol <- predict(lm.DoorVsWing.mol, newdata = wsshort)
        predvde <- predict(lm.DoorVsWing.vde, newdata = wsshort)
        lines(predmol~wsshort$WingSpread,col=col1,lty=1,lwd=2)
        lines(predvde~wsshort$WingSpread,col=col2,lty=1,lwd=2)
        a1low.m<-confint(lm.DoorVsWing.mol,level=c.inta)[1,1]
        b1low.m<-confint(lm.DoorVsWing.mol,level=c.inta)[2,1]
        lines(wsshort$WingSpread,a1low.m+b1low.m*wsshort$WingSpread,col= col1, lty=2,lwd=1)
        a1Upr.m<-confint(lm.DoorVsWing.mol,level=c.inta)[1,2]
        b1Upr.m<-confint(lm.DoorVsWing.mol,level=c.inta)[2,2]
        lines(wsshort$WingSpread,a1Upr.m+b1Upr.m*wsshort$WingSpread,col=col1,lty=2,lwd=1)
        a1low.v<-confint(lm.DoorVsWing.vde,level=c.intb)[1,1]
        b1low.v<-confint(lm.DoorVsWing.vde,level=c.intb)[2,1]
        lines(wsshort$WingSpread,a1low.v+b1low.v*wsshort$WingSpread,col= col2, lty=2,lwd=1)
        a1Upr.v<-confint(lm.DoorVsWing.vde,level=c.intb)[1,2]
        b1Upr.v<-confint(lm.DoorVsWing.vde,level=c.intb)[2,2]
        lines(wsshort$WingSpread,a1Upr.v+b1Upr.v*wsshort$WingSpread,col=col2,lty=2,lwd=1)               #lm.DoorVsWing.long
        legend("bottomleft",legend=substitute(paste(DS.29MO == a + b %*% WS.29MO),list(a=round(coef(lm.DoorVsWing.mol)[1],2),b=(round(coef(lm.DoorVsWing.mol)[2],2)))),inset=c(.09,.1),bty="n",text.font=2,text.col=col1)
        legend("bottomleft",legend=substitute(paste(r^2 ==resq),list(resq=round(summary(lm.DoorVsWing.mol)$adj.r.squared,2))),inset=c(.17,.04),cex=.9,bty="n",text.col=col1)
        legend("topright",legend=substitute(paste(DS.29VE == a + b %*% WS.VDE),list(a=round(coef(lm.DoorVsWing.vde)[1],2),b=(round(coef(lm.DoorVsWing.vde)[2],2)))),bty="n",text.font=2,inset=.05,text.col=col1)
        legend("topright",legend=substitute(paste(r^2 ==resq),list(resq=round(summary(lm.DoorVsWing.vde)$adj.r.squared,2))),inset=c(.15,.12),cex=.9,bty="n",text.col=col1)
        dumbo<-bquote("WS"== a + b %*% DS)
        mtext(dumbo,line=.4,side=3,cex=.8,font=2,adj=1)
      }
    } else {stop("No records with DoorSpread>0")}
    txt<-paste0(ifelse(es,"A\u00f1os: ","Years: "),paste0(c(years[1],"-",years[length(years)]),collapse=" "))
    if (length(years)>1) mtext(txt,1,line=-1.1,adj=0.01, font=1, cex=.9)
  }
}
