#' Function gearPlotHHNS.wgdp plots Wing Spread vs. Depth behaviour including the NS
#' 
#' Produces a WingSpread vs. DoorSpread plot and a model with nls R function. Data are taken directly from DATRAS using function getHHdata from library(icesDatras)
#' It only produces plots for surveys with HH files uploaded in DATRAS
#' If there are two different sweeps in the data, produces a model for each sweep length.
#' @param Survey: either the Survey to be downloaded from DATRAS (see details), or a data frame with the HH information with  the DATRAS HH format  and the years and quarter selected in years and quarter 
#' @param years: years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in steelblue2, it depends on the order of years, not the actual chronological year.
#' @param quarter: the quarter of the survey to be ploted
#' @param country: The country chosen to be plotted (checks if it's available in the HH file)
#' @param c.inta: the confidence interval to be used in the confint function for long sweeps and for sweeps if there is only one length
#' @param c.intb: the confidence interval to be used in the confint function for short sweeps if there are two
#' @param col1: color for the symbols and lines for the whole set if only one set of sweeps are used, and for the data from the long set of sweeps.
#' @param col2: color for the symbols and lines for the data from the short sweeps in case there are two.
#' @param getICES: Should the data be downloaded from DATRAS? If T, default, the data are taken from DATRAS through the icesDatras package.
#' @param pF: if set to F takes out the points and leaves only the lines in the graphs
#' @details Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @return Produces a graph with WingSpread vs. Depth. it also includes information on the ship, the time series used, the models and parameters estimated.
#' @examples gearPlotHHNS.wgdp("NS-IBTS",c(2015:2017),1,"SWE",c.inta=.5,c.intb=.8,pF=T)
#' @examples gearPlotHHNS.wgdp("NS-IBTS",c(2015:2017),1,"SWE",c.inta=.2,c.intb=.3,pF=F)
#' @export
gearPlotHHNS.wgdp<-function(Survey="NS-IBTS",years,quarter,country,c.inta=.8,c.intb=.3,col1="darkblue",col2="steelblue2",getICES=T,pF=T) {
  if (getICES) {
    dumb<-icesDatras::getDATRAS("HH",Survey,years,quarter)
  }
  if (!getICES) {
    dumb<-Survey
    if (!all(unique(years) %in% unique(dumb$Year))) stop(paste0("Not all years selected in years are present in the data.frame, check: ",unique(years)[which(!(unique(years) %in% unique(dumb$Year)))]))
    if (unique(dumb$Quarter)!=quarter) stop(paste0("Quarter selected ",quarter," is not available in the data.frame, check please"))
  }
  countries<-unique(dumb$Country)
  if (!country %in% countries) stop(paste(country,"is not present in this survey/quarter"))
  dumb<-dplyr::filter(dumb,Country==country)
  dumb<-dplyr::filter(dumb,HaulVal=="V")
  if (all(is.na(dumb$SweepLngt))) {stop("No valid Sweep Length data, this graph can not be produced")}
    dumb$sweeplngt[dumb$SweepLngt>0]<-factor(dumb$SweepLngt[dumb$SweepLngt>0])
      if (length(levels(factor(dumb$sweeplngt)))>2) {
        print(tapply(dumb$sweeplngt,dumb[,c("sweeplngt","Year")],"length"))
        stop("This function only works with data sets with two different sweep lengths, check you data")}
      if (length(subset(dumb$WingSpread,dumb$WingSpread> c(-9)))>0){
         dumb<-dumb[c(dumb$WingSpread>c(-9) & dumb$DoorSpread>c(-9)),]
         wspr<-range(subset(dumb$WingSpread,dumb$WingSpread> c(0)))
         dspr<-range(subset(dumb$DoorSpread,dumb$DoorSpread>c(0)))
         dpthA<-range(dumb$Depth[dumb$Depth>0],na.rm=T)
         if (length(years)>1) {plot(WingSpread~Depth,dumb,xlim=c(0,dpthA[2]+20),ylim=c(0,wspr[2]+10),type="n",subset=WingSpread!=c(-9) & Year!=years[length(years)],pch=21,col=grey(.5),ylab="Wing spread (m)",xlab="Depth (m)")
           if(pF) {points(WingSpread~Depth,dumb,subset=WingSpread!=c(-9) & Year!=years[length(years)],pch=21,col=grey(.5))}
           }
         if (length(years)==1) {plot(WingSpread~Depth,dumb,xlim=c(0,dpthA[2]+20),ylim=c(0,wspr[2]+10),type="n",subset=WingSpread!=c(-9) & WingSpread>0,pch=21,col=grey(.5),ylab="Wing spread (m)",xlab="Depth (m)")
           if (pF) {points(WingSpread~Depth,dumb,xlim=c(0,dpthA[2]+20),,subset=WingSpread!=c(-9) & WingSpread>0,pch=21,col=grey(.5))} 
           }
         title(paste0("Wing Spread vs. Depth in ",country," ",dumb$Survey[1],".Q",quarter," survey"),line=2.5)
         mtext(paste("Ship:",paste0(unique(dumb$Ship),collapse=" ")),line=.4,cex=.8,adj=0)
         if (length(levels(factor(dumb$sweeplngt)))<2) {
            dpthA<-range(dumb$Depth[dumb$Depth>0],na.rm=T)
            dp<-seq(dpthA[1],dpthA[2]+20,length=650)
            WingSpread.log<-nls(WingSpread~a1+b1*log(Depth),dumb,start=c(a1=.1,b1=1),subset=WingSpread!=c(-9) & WingSpread>0)
            if (pF) {points(WingSpread~Depth,dumb,subset=Year==years[length(years)],pch=21,bg=col1)}
            a1<-round(coef(WingSpread.log)[1],2)
            b1<-round(coef(WingSpread.log)[2],2)
            lines(dp,a1+b1*log(dp),col=col1,lwd=2)
            a1low<-confint(WingSpread.log,level=c.inta)[1,1]
            b1low<-confint(WingSpread.log,level=c.inta)[2,1]
            lines(dp,a1low+b1low*log(dp),col=col1,lty=2,lwd=1)
            a1Upr<-confint(WingSpread.log,level=c.inta)[1,2]
            b1Upr<-confint(WingSpread.log,level=c.inta)[2,2]
            lines(dp,a1Upr+b1Upr*log(dp),col=col1,lty=2,lwd=1)
            legend("bottomright",legend=substitute(WS == a1 + b1 %*% log(depth),list(a1=round(coef(WingSpread.log)[1],2),b1=(round(coef(WingSpread.log)[2],2)))),bty="n",text.font=2,inset=.2)
            dumbo<-bquote("Wing Spread"== a + b %*% log("Depth"))
            mtext(dumbo,line=.4,side=3,cex=.8,font=2,adj=1)
            print(summary(WingSpread.log))
         }
         if (length(levels(factor(dumb$sweeplngt)))==2) {
            dumbshort<-subset(dumb,SweepLngt==levels(factor(dumb$SweepLngt))[1])
            dumblong<-subset(dumb,SweepLngt==levels(factor(dumb$SweepLngt))[2])
            dpthAst<-range(dumbshort$Depth,na.rm=T)
            dpthAlg<-range(dumblong$Depth,na.rm=T)
            dpst<-seq(dpthAst[1],dpthAst[2]+20,length=650)
            dplg<-seq(dpthAlg[1],dpthAlg[2]+20,length=650)
            WingSpreadst.log<-nls(WingSpread~a1+b1*log(Depth),dumbshort,start=c(a1=.1,b1=1),subset=WingSpread!=c(-9))
            WingSpreadlg.log<-nls(WingSpread~a1+b1*log(Depth),dumblong,start=c(a1=.1,b1=1),subset=WingSpread!=c(-9))
            if (pF) {points(WingSpread~Depth,dumbshort,subset=HaulVal=="V",pch=21,col=col2)
            points(WingSpread~Depth,dumblong,subset=HaulVal=="V",pch=21,col=col1)}
            a1st<-round(coef(WingSpreadst.log)[1],2)
            b1st<-round(coef(WingSpreadst.log)[2],2)
            lines(dpst,a1st+b1st*log(dpst),col=col2,lwd=2)
            a1lowst<-confint(WingSpreadst.log,level=c.intb)[1,1]
            b1lowst<-confint(WingSpreadst.log,level=c.intb)[2,1]
            lines(dpst,a1lowst+b1lowst*log(dpst),col=col2,lty=2,lwd=1)
            a1Uprst<-confint(WingSpreadst.log,level=c.intb)[1,2]
            b1Uprst<-confint(WingSpreadst.log,level=c.intb)[2,2]
            lines(dpst,a1Uprst+b1Uprst*log(dpst),col=col2,lty=2,lwd=1)
            if (pF) {points(WingSpread~Depth,dumbshort,subset=Year==years[length(years)],pch=21,bg=col2)
            points(WingSpread~Depth,dumblong,subset=Year==years[length(years)],pch=21,bg=col1)}
            legend("bottomleft",legend=substitute(WSshort == a1st + b1st %*% log(depth),list(a1st=round(coef(WingSpreadst.log)[1],2),b1st=(round(coef(WingSpreadst.log)[2],2)))),bty="n",text.font=2,inset=c(.05,.1))
            a1lg<-round(coef(WingSpreadlg.log)[1],2)
            b1lg<-round(coef(WingSpreadlg.log)[2],2)
            lines(dplg,a1lg+b1lg*log(dplg),col=col1,lwd=2)
            a1lowlg<-confint(WingSpreadlg.log,level=c.inta)[1,1]
            b1lowlg<-confint(WingSpreadlg.log,level=c.inta)[2,1]
            lines(dplg,a1lowlg+b1lowlg*log(dplg),col=col1,lty=2,lwd=1)
            a1Uprlg<-confint(WingSpreadlg.log,level=c.inta)[1,2]
            b1Uprlg<-confint(WingSpreadlg.log,level=c.inta)[2,2]
            lines(dplg,a1Uprlg+b1Uprlg*log(dplg),col=col1,lty=2,lwd=1)
            legend("topright",legend=substitute(WSlong == a1lg + b1lg %*% log(depth),list(a1lg=round(coef(WingSpreadlg.log)[1],2),b1lg=(round(coef(WingSpreadlg.log)[2],2)))),bty="n",text.font=2,inset=.1)
            dumbo<-bquote("Wing Spread"== a + b %*% log("Depth"))
            mtext(dumbo,line=.4,side=3,cex=.8,font=2,adj=1)
            print(summary(WingSpreadst.log))
            print(summary(WingSpreadlg.log))
         }
   }
   txt<-paste0("Years: ",paste0(c(years[1],"-",years[length(years)]),collapse=" "))
   mtext(txt,1,line=-1.1,adj=0.01, font=1, cex=.8)
}

