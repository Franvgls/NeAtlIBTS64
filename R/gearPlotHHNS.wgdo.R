#' gearPlotHH.wgdo Door Spread vs. WingSpread
#' 
#' Plots Door Spread vs. Wing Spread behaviour and produces a model using lm. If there are DoorSpread and WingSpread values.  
#' If there are two different sweeps in the data, produces a model for each sweep length.
#' @param getICES: Should the data be downloaded from DATRAS? If T, default, the data are taken from DATRAS through the icesDatras package.
#' @param Survey: either the Survey to be downloaded from DATRAS (see details), or a data frame with the HH information with  the DATRAS HH format  and the years and quarter selected in years and quarter 
#' @param years: years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in steelblue2, it depends on the order of years, not the actual chronological year.
#' @param quarter: the quarter of the survey to be ploted
#' @param country: The country chosen to be plotted (checks if it's available in the HH file)
#' @param datHH: an HH data object with Survey, Year and Quarter columns, overrides Survey, Years, Quarters
#' @param col1: color for the symbols and lines for the whole set if only one set of sweeps are used, and for the data from the long set of sweeps.
#' @param col2: color for the symbols and lines for the data from the short sweeps in case there are two.
#' @param pF: takes out the points and leaves only the lines in the graphs
#' @param ti: if T includes autmoatically the title, F leaves it blank an can be added later.
#' @details Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @return Produces a graph with DoorSpread vs. WingSpread, it also includes information on the ship, the time series used, the models and parameters estimated.
#' @examples gearPlotHHNS.wgdo("NS-IBTS",c(2016:2017),1,"SWE")
#' @examples gearPlotHHNS.wgdo("NS-IBTS",c(2015:2017),1,"SWE")
#' @export
gearPlotHHNS.wgdo<-function(Survey="NS-IBTS",years,quarter,country,col1="darkblue",col2="steelblue2",getICES=T,pF=T) {
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
  if (length(unique(dumb$SweepLngt))>2) {
    print(tapply(dumb$SweepLngt,dumb[,c("SweepLngt","Year")],"length"))
    stop("This function only works with data sets with two different sweep lengths, check your data")
  }
  if (all(is.na(dumb$SweepLngt))) {stop("All information on sweeplength is NA. No wings used in this survey?")}
    dumb$sweeplngt[dumb$SweepLngt>0]<-factor(dumb$SweepLngt[dumb$SweepLngt>0])
   if (length(levels(dumb$SweepLngt))>2) {
     print(tapply(dumb$SweepLngt,dumb[,c("SweepLngt","Year")],"length"))
     stop("This function only works with data sets with two different sweep lengths, check your data")}
   if (length(subset(dumb$WingSpread,dumb$WingSpread> c(-9)))==0) {stop("No records with WingSpread>0")}
   if (length(subset(dumb,DoorSpread>c(-9)))>0){
         wspr<-range(subset(dumb$WingSpread,dumb$WingSpread> c(0)))
         dspr<-range(subset(dumb$DoorSpread,dumb$DoorSpread>c(0)))
         if (length(levels(factor(dumb$sweeplngt)))<2) {
            lm.WingVsDoor<-lm(WingSpread~DoorSpread,dumb,subset=HaulVal=="V" & WingSpread > c(0) & DoorSpread> c(0))
            #outlierTest(lm.WingVsDoor,data=dumb)
            ds<-data.frame(DoorSpread=seq(dspr[1],dspr[2],length.out = 10))
            plot(WingSpread~DoorSpread,dumb,type="n",subset=HaulVal=="V" & Year!=years[length(years)],xlim=c(dspr[1]-20,dspr[2]+20),ylim=c(wspr[1]-10,wspr[2]+10),xlab="Door Spread (m)",ylab="Wing Spread (m)",pch=21,col="grey")
            if (pF) {points(WingSpread~DoorSpread,dumb,subset=HaulVal=="V" & Year!=years[length(years)])}
            if (ti) {title(main=paste0("Wing Spread vs. door spread in ",country," ",dumb$Survey[1],".Q",quarter," survey"),line=2.5)}
            mtext(paste(c("Ship: ", unique(dumb$Ship)), collapse=" "),line=.4,cex=.8,adj=0)
            if (pF) {points(WingSpread~DoorSpread,dumb,subset=c(HaulVal=="V" & Year==years[length(years)]),pch=21,bg=col1)}
            ds<-data.frame(DoorSpread=seq(dspr[1],dspr[2],length.out = 10))
            pred <- predict(lm.WingVsDoor, newdata = ds)
            lines(pred~ds$DoorSpread,col=col1,lty=1,lwd=2)
            #abline(lm.WingVsDoor,col=2,lty=2)
            legend("bottomright",legend=substitute(paste(WS == a + b %*% DS),list(a=round(coef(lm.WingVsDoor)[1],2),b=(round(coef(lm.WingVsDoor)[2],2)))),bty="n",text.font=2,inset=.2)
            legend("bottomright",legend=substitute(paste(r^2 ==resq),list(resq=round(summary(lm.WingVsDoor)$adj.r.squared,2))),inset=c(.25,.15),cex=.9,bty="n")
            dumbo<-bquote("WS"== a + b %*% DS)
            mtext(dumbo,line=.4,side=3,cex=.8,font=2,adj=1)
            }
         if (length(levels(factor(dumb$sweeplngt)))==2) {
            lm.WingVsDoor.short<-lm(WingSpread~DoorSpread,dumb,subset=HaulVal=="V" & WingSpread > c(-9) & DoorSpread> c(-9) & sweeplngt==levels(factor(sweeplngt))[1])
            lm.WingVsDoor.long<-lm(WingSpread~DoorSpread,dumb,subset=HaulVal=="V" & WingSpread > c(-9) & DoorSpread> c(-9) & sweeplngt==levels(factor(sweeplngt))[2])
            plot(WingSpread~DoorSpread,dumb,type="n",subset=HaulVal=="V" & Year!=years[length(years)],xlim=c(dspr[1]-20,dspr[2]+20),ylim=c(wspr[1]-10,wspr[2]+10),xlab="Door Spread (m)",ylab="Wing Spread (m)",pch=21,col="grey")
            title(main=paste0("Wing Spread vs. door spread in ",country," ",dumb$Survey[1],".Q",quarter," survey"),line=2.5)
            mtext(paste(c("Ship: ", unique(dumb$Ship)), collapse=" "),line=.4,cex=.8,adj=0)
            if (pF){
              points(WingSpread~DoorSpread,dumb,subset=c(HaulVal=="V" & sweeplngt==levels(factor(sweeplngt))[1]),pch=21,col=col2)
              points(WingSpread~DoorSpread,dumb,subset=c(HaulVal=="V" & sweeplngt==levels(factor(sweeplngt))[2]),pch=21,col=col1)
              points(WingSpread~DoorSpread,dumb,
                subset=c(HaulVal=="V" & Year==years[length(years)]& sweeplngt==levels(factor(sweeplngt))[1]),pch=21,bg=col2)
              points(WingSpread~DoorSpread,dumb,
                subset=c(HaulVal=="V" & Year==years[length(years)] & sweeplngt==levels(factor(sweeplngt))[2]),pch=21,bg=col1)
            }
            dsprsrt<-range(subset(dumb,sweeplngt==levels(factor(sweeplngt))[1] & DoorSpread> c(-9))$DoorSpread)
            dsshort<-data.frame(DoorSpread=seq(dsprsrt[1],dsprsrt[2],length.out = 10))
            predshort <- predict(lm.WingVsDoor.short, newdata = dsshort)
            lines(predshort~dsshort$DoorSpread,col=col2,lty=1,lwd=2)
            dsprlng<-range(subset(dumb,sweeplngt==levels(factor(sweeplngt))[2] & DoorSpread> c(-9))$DoorSpread)
            dslong<-data.frame(DoorSpread=seq(dsprlng[1],dsprlng[2],length.out = 10))
            predlong <- predict(lm.WingVsDoor.long, newdata = dslong)
            lines(predlong~dslong$DoorSpread,col=col1,lty=1,lwd=2)
            legend("bottomleft",legend=substitute(paste(WSshort == a + b %*% DSshort),list(a=round(coef(lm.WingVsDoor.short)[1],2),b=(round(coef(lm.WingVsDoor.short)[2],2)))),inset=c(.09,.1),bty="n",text.font=2,text.col=col1) 
            legend("bottomleft",legend=substitute(paste(r^2 ==resq),list(resq=round(summary(lm.WingVsDoor.short)$adj.r.squared,2))),inset=c(.17,.04),cex=.9,bty="n",text.col=col1)
            legend("topright",legend=substitute(paste(WSlong == a + b %*% DSlong),list(a=round(coef(lm.WingVsDoor.long)[1],2),b=(round(coef(lm.WingVsDoor.long)[2],2)))),bty="n",text.font=2,inset=.05,text.col=col1)
            legend("topright",legend=substitute(paste(r^2 ==resq),list(resq=round(summary(lm.WingVsDoor.long)$adj.r.squared,2))),inset=c(.15,.12),cex=.9,bty="n",text.col=col1)
            dumbo<-bquote("WS"== a + b %*% DS)
            mtext(dumbo,line=.4,side=3,cex=.8,font=2,adj=1)
         }
   } else {stop("No records with DoorSpread>0")}
   txt<-paste0("Years: ",paste0(c(years[1],"-",years[length(years)]),collapse=" "))
   mtext(txt,1,line=-1.1,adj=0.01, font=1, cex=.9)
}