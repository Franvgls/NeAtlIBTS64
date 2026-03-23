#' gearPlotHH.wgdo Wing Spread vs. DoorSpread
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
#' @param es: If TRUE all labels and titles are in Spanish, if FALSE in English
#' @param col1: color for the symbols and lines for the whole set if only one set of sweeps are used, and for the data from the long set of sweeps.
#' @param col2: color for the symbols and lines for the data from the short sweeps in case there are two.
#' @param pF: takes out the points and leaves only the lines in the graphs
#' @param ti: if F title will not be included automatically and can be addedd later
#' @details Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @return Produces a graph with DoorSpread vs. WingSpread, it also includes information on the ship, the time series used, the models and parameters estimated.
#' @examples gearPlotHH.wgdo("SCOWCGFS",c(2014:2016),1,col1="darkblue",col2="steelblue3")
#' @examples gearPlotHH.wgdo("SCOWCGFS",c(2013:2016),4)
#' @examples gearPlotHH.wgdo("SCOROC",c(2013:2016),3)
#' @examples gearPlotHH.wgdo("NIGFS",c(2015:2016),1)
#' @examples gearPlotHH.wgdo("NIGFS",c(2006:2007,2009:2016),4)
#' @examples gearPlotHH.wgdo("IE-IGFS",c(2011:2016),4)
#' @examples gearPlotHH.wgdo("SP-PORC",c(2003:2015),3,c.int=.2)
#' @examples gearPlotHH.wgdo("FR-CGFS",c(2016:2018),4)
#' @examples gearPlotHH.wgdo("EVHOE",c(1997:2016),4)
#' @examples gearPlotHH.wgdo("SP-NORTH",c(2014:2016),4,col1="darkblue")
#' @examples gearPlotHH.wgdo("SP-ARSA",c(2014:2016),1,col1="darkblue",col2="steelblue2")
#' @examples gearPlotHH.wgdo("SP-ARSA",c(2014:2016),4)
#' @examples gearPlotHH.wgdo(damb,c(2014:2016),4,getICES=F,pF=F)
#' @export
gearPlotHH.wgdo<-function(Survey,years,quarter,c.int=.9,c.inta=.8,c.intb=.8,es=FALSE,col1="darkblue",col2="steelblue2",getICES=T,pF=T,ti=T) {
  if (getICES) {
    dumb<-icesDatras::getDATRAS(record = "HH",survey = Survey, year= years,quarter = quarter)
  }
  if (!getICES) {
    dumb<-Survey
    if (!all(unique(years) %in% unique(dumb$Year))) stop(paste0("Not all years selected in years are present in the data.frame, check: ",unique(years)[which(!(unique(years) %in% unique(dumb$Year)))]))
    if (!all(unique(quarter) %in% unique(dumb$Quarter))) stop(paste0("Quarter selected ",quarter," is not available in the data.frame, check please"))
  }
  # if (all(is.na(dumb$SweepLngt))) {stop("All information on sweeplength is NA. No wings used in this survey?")}
  dumb$SweepLngt[is.na(dumb$SweepLngt)]<-0
  dumb$SweepLngt[dumb$SweepLngt>0]<-factor(dumb$SweepLngt[dumb$SweepLngt>0],levels=sort(unique(dumb$SweepLngt[dumb$SweepLngt>0])),ordered = T)
   if (length(unique(dumb$SweepLngt))>2) {
     print(tapply(dumb$SweepLngt,dumb[,c("SweepLngt","Year")],"length"))
     stop("This function only works with data sets with two different sweep lengths, check you data")}
  if (length(subset(dumb$WingSpread,dumb$WingSpread> c(-9)))==0) {stop("No records with valid WingSpread > 0")}
  #if (length(subset(dumb$DoorSpread,dumb$DoorSpread==0))>0) {stop("Records with DoorSpread = 0, please check and remove")}
  if (length(subset(dumb,DoorSpread>c(-9)))>0){
     dumb<-dplyr::filter(dumb,HaulVal=="V")
     dumb<-dumb[c(dumb$WingSpread>c(-9) & dumb$DoorSpread>c(-9)),]
     wspr<-range(subset(dumb$WingSpread,dumb$WingSpread> c(0)))
     dspr<-range(subset(dumb$DoorSpread,dumb$DoorSpread>c(0)))
     if (length(unique(dumb$SweepLngt))<2) {
            if (length(years)>1) lm.WingVsDoor<-lm(WingSpread~DoorSpread,dumb,subset=c(HaulVal=="V" & WingSpread>0 & DoorSpread>0 & Year!=years[length(years)]))
            else lm.WingVsDoor<-lm(WingSpread~DoorSpread,dumb,subset=c(HaulVal=="V" & WingSpread>0 & DoorSpread>0))
                        #outlierTest(lm.WingVsDoor,data=dumb)
            ds<-data.frame(DoorSpread=seq(dspr[1],dspr[2],length.out = 10))
            plot(WingSpread~DoorSpread,dumb,type="n",subset=HaulVal=="V" & Year!=years[length(years)],xlim=c(dspr[1]-20,dspr[2]+20),ylim=c(wspr[1]-10,wspr[2]+10),xlab=ifelse(es,"Abertura puertas (m)","Door Spread (m)"),ylab=ifelse(es,"Abertura calones (m)","Wing Spread (m)"),pch=21,col="grey")
            if (pF) {
              points(WingSpread~DoorSpread,dumb,subset=HaulVal=="V" & Year!=years[length(years)])
              if (length(years)>1) legend("bottomright",legend=c(paste0(years[1],"-",years[length(years)-1]),as.character(years[length(years)])),pch=21,col=col1,pt.bg=c(NA,col1),bty="n",inset=.02)
              else legend("bottomright",as.character(years),pch=21,col=col1,pt.bg=col1,bty="n",inset=.04)
              }
            if (ti) title(main=paste0(ifelse(es,"Abertura de calones vs. Abertura de puertas","Wing Spread vs. door spread in "),dumb$Survey[1],".Q",quarter),line=2.5)
            mtext(dumb$Ship[1],line=.4,cex=.8,adj=0)
            if (pF) {points(WingSpread~DoorSpread,dumb,subset=c(HaulVal=="V" & Year==years[length(years)]),pch=21,bg=col1)}
            ds<-data.frame(DoorSpread=seq(dspr[1],dspr[2],length.out = 10))
            pred <- predict(lm.WingVsDoor, newdata = ds)
            lines(pred~ds$DoorSpread,col=col1,lty=1,lwd=2)
            a1low<-confint(lm.WingVsDoor,level=c.int)[1,1]
            b1low<-confint(lm.WingVsDoor,level=c.int)[2,1]
            lines(ds$DoorSpread,a1low+b1low*ds$DoorSpread,col= col1, lty=2,lwd=1)
            a1Upr<-confint(lm.WingVsDoor,level=c.int)[1,2]
            b1Upr<-confint(lm.WingVsDoor,level=c.int)[2,2]
            lines(ds$DoorSpread,a1Upr+b1Upr*ds$DoorSpread,col=col1,lty=2,lwd=1)
            #abline(lm.WingVsDoor,col=2,lty=2)
            legend("bottomright",legend=substitute(paste(WS == a + b %*% DS),list(a=round(coef(lm.WingVsDoor)[1],2),b=(round(coef(lm.WingVsDoor)[2],2)))),bty="n",text.font=2,inset=.2)
            legend("bottomright",legend=substitute(paste(r^2 ==resq),list(resq=round(summary(lm.WingVsDoor)$adj.r.squared,2))),inset=c(.25,.15),cex=.9,bty="n")
            dumbo<-bquote("WS"== a + b %*% DS)
            mtext(dumbo,line=.4,side=3,cex=.8,font=2,adj=1)
            }
         if (length(levels(factor(dumb$SweepLngt)))==2) {
           if (length(years)>1) {
            lm.WingVsDoor.short<-lm(WingSpread~DoorSpread,dumb,subset=HaulVal=="V" & WingSpread > c(-9) & DoorSpread> c(-9) & SweepLngt==levels(factor(SweepLngt))[1] & c(StNo!="FG1"&Year!=2015) & Year!= years[length(years)])
            lm.WingVsDoor.long<-lm(WingSpread~DoorSpread,dumb,subset=HaulVal=="V" & WingSpread > c(-9) & DoorSpread> c(-9) & SweepLngt==levels(factor(SweepLngt))[2] & Year!= years[length(years)])
           }
           else {
             lm.WingVsDoor.short<-lm(WingSpread~DoorSpread,dumb,subset=HaulVal=="V" & WingSpread > c(-9) & DoorSpread> c(-9) & SweepLngt==levels(factor(SweepLngt))[1] & c(StNo!="FG1"& Year!=2015) )
             lm.WingVsDoor.long<-lm(WingSpread~DoorSpread,dumb,subset=HaulVal=="V" & WingSpread > c(-9) & DoorSpread> c(-9) & SweepLngt==levels(factor(SweepLngt))[2])
           }
            plot(WingSpread~DoorSpread,dumb,type="n",subset=HaulVal=="V" & Year!=years[length(years)],xlim=c(dspr[1]-20,dspr[2]+20),ylim=c(wspr[1]-10,wspr[2]+10),xlab=ifelse(es,"Abertura puertas (m)","Door Spread (m)"),ylab=ifelse(es,"Abertura calones (m)","Wing Spread (m)"),pch=21,col="grey")
            #title(main=paste0(ifelse(es,"Abertura de calones vs. Abertura de puertas","Wing Spread vs. door spread in "),dumb$Survey[1],".Q",quarter),line=2.5)
            if (ti) title(main=paste0(ifelse(es,"Abertura de calones vs. Abertura de puertas","Wing Spread vs. door spread in "),dumb$Survey[1],".Q",quarter),line=2.5)
            mtext(dumb$Ship[1],line=.4,cex=.8,adj=0)
            if (pF){
              points(WingSpread~DoorSpread,dumb,subset=c(HaulVal=="V" & SweepLngt==levels(factor(SweepLngt))[1] & c(StNo!="FG1" & Year!=2015)),pch=21,col=col2)
              points(WingSpread~DoorSpread,dumb,subset=c(HaulVal=="V" & SweepLngt==levels(factor(SweepLngt))[2]),pch=21,col=col1)
              points(WingSpread~DoorSpread,dumb,
                subset=c(HaulVal=="V" & Year==years[length(years)] & SweepLngt==levels(factor(SweepLngt))[1]),pch=21,bg=col2)
              points(WingSpread~DoorSpread,dumb,
                subset=c(HaulVal=="V" & Year==years[length(years)] & SweepLngt==levels(factor(SweepLngt))[2]),pch=21,bg=col1)
              if (length(years)>1) legend("bottomright",c(paste(c(paste(years[1],years[length(years)-1],sep="-"),years[length(years)]),c(ifelse(es,"Malletas cortas","Short sweeps")),sep=" "),paste(c(paste(years[1],years[length(years)-1],sep="-"),years[length(years)]),c(ifelse(es,"Malletas largas","Long sweeps")),sep=" ")),pch=21,col=c(col2,col1,col1,col1),pt.bg=c(NA,col2,NA,col1),bty="n",inset=c(.02),ncol=2)
              else { if(es) legend("bottomright",c("Malletas cortas","Malletas largas"),pch=21,col=c(col1,col1),pt.bg = c(col2,col1),bty="n",inset=.04)
                else legend("bottomright",c("Short sweeps","Long sweeps"),pch=21,col=c(col1,col1),pt.bg = c(col2,col1),bty="n",inset=.04)}
            }
            dsprsrt<-range(subset(dumb,SweepLngt==levels(factor(SweepLngt))[1] & DoorSpread> c(-9))$DoorSpread)
            dsshort<-data.frame(DoorSpread=seq(dsprsrt[1],dsprsrt[2],length.out = 10))
            predshort <- predict(lm.WingVsDoor.short, newdata = dsshort)
            lines(predshort~dsshort$DoorSpread,col=col2,lty=1,lwd=2)
            dsprlng<-range(subset(dumb,SweepLngt==levels(factor(SweepLngt))[2] & DoorSpread> c(-9))$DoorSpread)
            dslong<-data.frame(DoorSpread=seq(dsprlng[1],dsprlng[2],length.out = 10))
            predlong <- predict(lm.WingVsDoor.long, newdata = dslong)
            lines(predlong~dslong$DoorSpread,col=col1,lty=1,lwd=2)
            a1low.s<-confint(lm.WingVsDoor.short,level=c.inta)[1,1]
            b1low.s<-confint(lm.WingVsDoor.short,level=c.inta)[2,1]
            lines(dsshort$DoorSpread,a1low.s+b1low.s*dsshort$DoorSpread,col= col2, lty=2,lwd=1)
            a1Upr.s<-confint(lm.WingVsDoor.short,level=c.inta)[1,2]
            b1Upr.s<-confint(lm.WingVsDoor.short,level=c.inta)[2,2]
            lines(dsshort$DoorSpread,a1Upr.s+b1Upr.s*dsshort$DoorSpread,col=col2,lty=2,lwd=1)
            a1low.l<-confint(lm.WingVsDoor.long,level=c.intb)[1,1]
            b1low.l<-confint(lm.WingVsDoor.long,level=c.intb)[2,1]
            lines(dslong$DoorSpread,a1low.l+b1low.l*dslong$DoorSpread,col= col1, lty=2,lwd=1)
            a1Upr.l<-confint(lm.WingVsDoor.long,level=c.intb)[1,2]
            b1Upr.l<-confint(lm.WingVsDoor.long,level=c.intb)[2,2]
            lines(dslong$DoorSpread,a1Upr.l+b1Upr.l*dslong$DoorSpread,col=col1,lty=2,lwd=1)
            legend("bottomleft",legend=substitute(paste(WSshort == a + b %*% DSshort),list(a=round(coef(lm.WingVsDoor.short)[1],2),b=(round(coef(lm.WingVsDoor.short)[2],2)))),inset=c(.09,.1),bty="n",text.font=2,text.col=col1) 
            legend("bottomleft",legend=substitute(paste(r^2 ==resq),list(resq=round(summary(lm.WingVsDoor.short)$adj.r.squared,2))),inset=c(.17,.04),cex=.9,bty="n",text.col=col1)
            legend("topright",legend=substitute(paste(WSlong == a + b %*% DSlong),list(a=round(coef(lm.WingVsDoor.long)[1],2),b=(round(coef(lm.WingVsDoor.long)[2],2)))),bty="n",text.font=2,inset=.05,text.col=col1)
            legend("topright",legend=substitute(paste(r^2 ==resq),list(resq=round(summary(lm.WingVsDoor.long)$adj.r.squared,2))),inset=c(.15,.12),cex=.9,bty="n",text.col=col1)
            dumbo<-bquote("WS"== a + b %*% DS)
            mtext(dumbo,line=.4,side=3,cex=.8,font=2,adj=1)
         }
   } else {stop("No records with DoorSpread>0")}
  yearsb<-unique(dplyr::filter(dumb,!is.na(WingSpread) & WingSpread>0)$Year)
  if (length(years)>1 & !all(years %in% yearsb)) txt<-paste("Years:",paste0(c(yearsb[yearsb %in% years]),collapse=" "))
  if (length(years)>1 & all(years %in% yearsb)) txt<-paste0("Years: ",paste0(c(years[1],"-",years[length(years)]),collapse=" "))
  if (length(years)==1) txt<-paste0("Year: ",as.character(years))
  mtext(txt,1,line=-1.1,adj=0.01, font=1, cex=.8)
  # txt<-paste0("Years: ",paste0(c(years[1],"-",years[length(years)]),collapse=" "))
   # if(length(years)>1) mtext(txt,1,line=-1.1,adj=0.01, font=1, cex=.9)
}