#' gearPlotHH.dowg Wing Spread vs. DoorSpread
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
#' @param es if TRUE all legends and axis labels are in Spanish, if F in English
#' @param col1: color for the symbols and lines for the whole set if only one set of sweeps are used, and for the data from the long set of sweeps.
#' @param col2: color for the symbols and lines for the data from the short sweeps in case there are two.
#' @param pF: takes out the points and leaves only the lines in the graphs
#' @param ti: if F title will not be included automatically and can be addedd later
#' @details Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @return Produces a graph with WingSpread vs. DoorSpread, it also includes information on the ship, the time series used, the models and parameters estimated.
#' @examples
#' \dontrun{
#' gearPlotHH.dowg("SCOWCGFS",c(2014:2016),1,col1="darkblue",col2="steelblue3")
#' gearPlotHH.dowg("SP-ARSA",c(2014:2016),4)
#' gearPlotHH.dowg(damb,c(2014:2016),4,getICES=F,pF=F)
#' }
#' @export
gearPlotHH.dowgSH<-function(Survey,years,quarter,c.int=.9,c.inta=.8,c.intb=.8,es=FALSE,col1="darkblue",col2="steelblue2",getICES=T,pF=T,ti=T,esc.mult=1) {
  if (getICES) {
    dumb<-icesDatras::getDATRAS("HH",Survey,years,quarter)
  }
  if (!getICES) {
    dumb<-Survey
    if (!all(unique(years) %in% unique(dumb$Year))) stop(paste0("Not all years selected in years are present in the data.frame, check: ",unique(years)[which(!(unique(years) %in% unique(dumb$Year)))]))
    if (!all(unique(quarter) %in% unique(dumb$Quarter))) stop(paste0("Quarter selected ",quarter," is not available in the data.frame, check please"))
  }
  # if (all(is.na(dumb$SweepLngt))) {stop("All information on sweeplength is NA. No wings used in this survey?")}
  dumb$SweepLngt[is.na(dumb$SweepLngt)]<-0
  dumb$SweepLngt<-factor(dumb$SweepLngt,levels=sort(unique(dumb$SweepLngt)),ordered = T)
   if (length(levels(dumb$SweepLngt))>2) {
     print(tapply(dumb$SweepLngt,dumb[,c("SweepLngt","Year")],"length"))
     stop("This function only works with data sets with two different sweep lengths, check you data")}
  if (length(subset(dumb$WingSpread,dumb$WingSpread> c(-9)))==0) {stop("No records with valid WingSpread > 0")}
  #if (length(subset(dumb$DoorSpread,dumb$DoorSpread==0))>0) {stop("Records with DoorSpread = 0, please check and remove")}
  if (length(subset(dumb,WingSpread>c(-9)))>0){
    dumb<-dumb[dumb$HaulVal=="V",] #dumb<-dplyr::filter(dumb,HaulVal=="V")
     dumb<-dumb[c(dumb$WingSpread>c(-9) & dumb$WingSpread>c(-9)),]
     wspr<-range(subset(dumb$WingSpread,dumb$WingSpread> c(0)))
     dspr<-range(subset(dumb$DoorSpread,dumb$DoorSpread>c(0)))
     if (length(levels(factor(dumb$SweepLngt)))<2) {
            if (length(years)>1) lm.DoorVsWing<-lm(DoorSpread~WingSpread,dumb,subset=c(HaulVal=="V" & WingSpread>0 & DoorSpread>0))
            else lm.DoorVsWing<-lm(DoorSpread~WingSpread,dumb,subset=c(HaulVal=="V" & WingSpread>0 & DoorSpread>0))
                        #outlierTest(lm.DoorVsWing,data=dumb)
            ws<-data.frame(WingSpread=seq(wspr[1],wspr[2],length.out = 10))
            plot(DoorSpread~WingSpread,dumb,type="n",subset=HaulVal=="V",xlim=c(wspr[1]-10,wspr[2]+10),ylim=c(dspr[1]-20,dspr[2]+20),
                 xlab=ifelse(es,"Abertura Calones (m)","Wing Spread (m)"),ylab=ifelse(es,"Abertura puertas (m)","Door Spread (m)"),pch=21,col="grey",cex.lab=1*esc.mult,cex.axis=1*esc.mult)
            if (pF) {
              points(DoorSpread~WingSpread,dumb,subset=HaulVal=="V")
              #if (length(years)>1) legend("bottomright",legend=c(paste0(years[1],"-",years[length(years)-1]),as.character(years[length(years)])),pch=21,col=col1,pt.bg=c(NA,col1),bty="n",inset=.02)
              #else
              legend("bottomright",as.character(paste0(years[1],"-",years[length(years)])),pch=21,col=col1,pt.bg=col1,bty="n",inset=.04)
              }
            if (ti) title(main=paste0(ifelse(es,"Abertura puertas vs. Abertura calones en ","Door spread vs. wing spread in "),dumb$Survey[1],".Q",quarter),line=2.5,cex.main=1.1*esc.mult)
            mtext(dumb$Ship[1],line=.4,cex=.8*esc.mult,adj=0)
            if (pF) {points(DoorSpread~WingSpread,dumb,subset=c(HaulVal=="V"),pch=21,bg=col1)}
            ws<-data.frame(WingSpread=seq(wspr[1],wspr[2],length.out = 10))
            pred <- predict(lm.DoorVsWing, newdata = ws)
            lines(pred~ws$WingSpread,col=col1,lty=1,lwd=2)
            a1low<-confint(lm.DoorVsWing,level=c.int)[1,1]
            b1low<-confint(lm.DoorVsWing,level=c.int)[2,1]
            lines(ws$WingSpread,a1low+b1low*ws$WingSpread,col= col1, lty=2,lwd=1)
            a1Upr<-confint(lm.DoorVsWing,level=c.int)[1,2]
            b1Upr<-confint(lm.DoorVsWing,level=c.int)[2,2]
            lines(ws$WingSpread,a1Upr+b1Upr*ws$WingSpread,col=col1,lty=2,lwd=1)
            #abline(lm.DoorVsWing,col=2,lty=2)
            legend("bottomright",legend=substitute(paste(DS == a + b %*% WS),list(a=round(coef(lm.DoorVsWing)[1],2),b=(round(coef(lm.DoorVsWing)[2],2)))),bty="n",cex=1*esc.mult,text.font=2,inset=.2)
            legend("bottomright",legend=substitute(paste(r^2 ==resq),list(resq=round(summary(lm.DoorVsWing)$adj.r.squared,2))),inset=c(.25,.15),cex=.9*esc.mult,bty="n")
            dumbo<-bquote("DS"== a + b %*% WS)
            mtext(dumbo,line=.4,side=3,cex=.8*esc.mult,font=2,adj=1)
            }
         if (length(levels(factor(dumb$SweepLngt)))==2) {
           if (length(years)>1) {
            lm.DoorVsWing.short<-lm(DoorSpread~WingSpread,dumb,subset=HaulVal=="V" & WingSpread > c(-9) & DoorSpread> c(-9) & SweepLngt==levels(factor(SweepLngt))[1])
            lm.DoorVsWing.long<-lm(DoorSpread~WingSpread,dumb,subset=HaulVal=="V" & WingSpread > c(-9) & DoorSpread> c(-9) & SweepLngt==levels(factor(SweepLngt))[2])
           }
           else {
             lm.DoorVsWing.short<-lm(DoorSpread~WingSpread,dumb,subset=HaulVal=="V" & WingSpread > c(-9) & DoorSpread> c(-9) & SweepLngt==levels(factor(SweepLngt))[1])
             lm.DoorVsWing.long<-lm(DoorSpread~WingSpread,dumb,subset=HaulVal=="V" & WingSpread > c(-9) & DoorSpread> c(-9) & SweepLngt==levels(factor(SweepLngt))[2])
           }
            plot(DoorSpread~WingSpread,dumb,type="n",subset=HaulVal=="V",xlim=c(wspr[1]-10,wspr[2]+10),ylim=c(dspr[1]-20,dspr[2]+20),
                 xlab=ifelse(es,"Abertura Calones (m)","Wing Spread (m)"),ylab=ifelse(es,"Abertura puertas (m)","Door Spread (m)"),
                 pch=21,col="grey",cex.lab=1*esc.mult,cex.axis=1*esc.mult)
            if (ti) title(main=paste0(ifelse(es,"Abertura puertas vs. Abertura calones en ","Door spread vs. wing spread in "),dumb$Survey[1],".Q",quarter),line=2.5,cex.main=1*esc.mult)
            mtext(dumb$Ship[1],line=.4,cex=.8*esc.mult,adj=0)
            if (pF){
              # points(DoorSpread~WingSpread,dumb,subset=c(HaulVal=="V" & SweepLngt==levels(factor(SweepLngt))[1]),pch=21,col=col2)
              # points(DoorSpread~WingSpread,dumb,subset=c(HaulVal=="V" & SweepLngt==levels(factor(SweepLngt))[2]),pch=21,col=col1)
              points(DoorSpread~WingSpread,dumb,
                subset=c(HaulVal=="V" & SweepLngt==levels(factor(SweepLngt))[1]),pch=21,bg=col2)
              points(DoorSpread~WingSpread,dumb,
                subset=c(HaulVal=="V" & SweepLngt==levels(factor(SweepLngt))[2]),pch=21,bg=col1)
              #if (length(years)>1)
              legend("bottomright",c((ifelse(es,"Malletas cortas","Short sweeps")),c(ifelse(es,"Malletas largas","Long sweeps"))),
              pch=21,col=c(col2,col1),pt.bg=c(col2,col1),bty="n",inset=c(.02),cex=1*esc.mult)
              #else legend("bottomright",c("Short sweeps","Long sweeps"),pch=21,col=c(col1,col1),pt.bg = c(col2,col1),bty="n",inset=.04)
            }
            wsprsrt<-range(subset(dumb,SweepLngt==levels(factor(SweepLngt))[1] & DoorSpread> c(-9))$WingSpread)
            wsshort<-data.frame(WingSpread=seq(wsprsrt[1],wsprsrt[2],length.out = 10))
            predshort <- predict(lm.DoorVsWing.short, newdata = wsshort)
            lines(predshort~wsshort$WingSpread,col=col2,lty=1,lwd=2)
            wsprlng<-range(subset(dumb,SweepLngt==levels(factor(SweepLngt))[2] & DoorSpread> c(-9))$WingSpread)
            wslong<-data.frame(WingSpread=seq(wsprlng[1],wsprlng[2],length.out = 10))
            predlong <- predict(lm.DoorVsWing.long, newdata = wslong)
            lines(predlong~wslong$WingSpread,col=col1,lty=1,lwd=2)
            a1low.s<-confint(lm.DoorVsWing.short,level=c.inta)[1,1]
            b1low.s<-confint(lm.DoorVsWing.short,level=c.inta)[2,1]
            lines(wsshort$WingSpread,a1low.s+b1low.s*wsshort$WingSpread,col= col2, lty=2,lwd=1)
            a1Upr.s<-confint(lm.DoorVsWing.short,level=c.inta)[1,2]
            b1Upr.s<-confint(lm.DoorVsWing.short,level=c.inta)[2,2]
            lines(wsshort$WingSpread,a1Upr.s+b1Upr.s*wsshort$WingSpread,col=col2,lty=2,lwd=1)
            a1low.l<-confint(lm.DoorVsWing.long,level=c.intb)[1,1]
            b1low.l<-confint(lm.DoorVsWing.long,level=c.intb)[2,1]
            lines(wslong$WingSpread,a1low.l+b1low.l*wslong$WingSpread,col= col1, lty=2,lwd=1)
            a1Upr.l<-confint(lm.DoorVsWing.long,level=c.intb)[1,2]
            b1Upr.l<-confint(lm.DoorVsWing.long,level=c.intb)[2,2]
            lines(wslong$WingSpread,a1Upr.l+b1Upr.l*wslong$WingSpread,col=col1,lty=2,lwd=1)
            legend("bottomleft",legend=substitute(paste(DSshort == a + b %*% WSshort),list(a=round(coef(lm.DoorVsWing.short)[1],2),b=(round(coef(lm.DoorVsWing.short)[2],2)))),inset=c(.09,.1),bty="n",text.font=2,text.col=col1,cex=1*esc.mult)
            legend("bottomleft",legend=substitute(paste(r^2 ==resq),list(resq=round(summary(lm.DoorVsWing.short)$adj.r.squared,2))),inset=c(.17,.04),cex=.9*esc.mult,bty="n",text.col=col1)
            legend("topright",legend=substitute(paste(DSlong == a + b %*% WSlong),list(a=round(coef(lm.DoorVsWing.long)[1],2),b=(round(coef(lm.DoorVsWing.long)[2],2)))),bty="n",text.font=2,inset=.05,text.col=col1,cex=1*esc.mult)
            legend("topright",legend=substitute(paste(r^2 ==resq),list(resq=round(summary(lm.DoorVsWing.long)$adj.r.squared,2))),inset=c(.15,.12),cex=.9*esc.mult,bty="n",text.col=col1)
            dumbo<-bquote("WS"== a + b %*% DS)
            mtext(dumbo,line=.4,side=3,cex=.8*esc.mult,font=2,adj=1)
         }
   } else {stop("No records with DoorSpread>0")}
  yearsb<-unique(dplyr::filter(dumb,!is.na(dumb$WingSpread) & dumb$WingSpread>0)$Year)
  if (length(years)>1 & !all(years %in% yearsb)) txt<-paste(ifelse(es,"A\u00f1os: ","Years: "),paste0(c(yearsb[yearsb %in% years]),collapse=" "))
  if (length(years)>1 & all(years %in% yearsb)) txt<-paste0(ifelse(es,"A\u00f1os: ","Years: "),paste0(c(years[1],"-",years[length(years)]),collapse=" "))
  if (length(years)==1) txt<-paste0("Year: ",as.character(years))
  mtext(txt,1,line=-1.1,adj=0.01, font=1, cex=.8*esc.mult)
}
