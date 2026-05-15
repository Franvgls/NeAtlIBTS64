#' Function gearPlotHH.wgdp plots Wing Spread vs. Depth
#'
#' Produces a WingSpread vs. DoorSpread plot and a model with nls R function. Data are taken directly from DATRAS using function getDATRAS from library(icesDatras)
#' It only produces plots for surveys with HH files uploaded in DATRAS
#' If there are two different sweeps in the data, produces a model for each sweep length.
#' @param Survey: either the Survey to be downloaded from DATRAS (see details), or a data frame with the HH information with  the DATRAS HH format  and the years and quarter selected in years and quarter
#' @param years: years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in steelblue2, it depends on the order of years, not the actual chronological year.
#' @param quarter: the quarter of the survey to be ploted
#' @param c.inta: the confidence interval to be used in the confint function for long sweeps and for sweeps if there is only one length
#' @param c.intb: the confidence interval to be used in the confint function for short sweeps if there are two
#' @param es: if TRUE all titles legends... are in Spanish, if FALSE in English
#' @param col1: color for the symbols and lines for the whole set if only one set of sweeps are used, and for the data from the long set of sweeps.
#' @param col2: color for the symbols and lines for the data from the short sweeps in case there are two.
#' @param getICES: Should the data be downloaded from DATRAS? If T, default, the data are taken from DATRAS through the icesDatras package.
#' @param pF: takes out the points and leaves only the lines in the graphs
#' @param ti: if F title will not be included automatically and can be addedd later
#' @details Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @return Produces a graph with WingSpread vs. Depth. it also includes information on the ship, the time series used, the models and parameters estimated.
#' @examples
#' \dontrun{
#' gearPlotHH.wgdp("SP-NORTH",c(2014:2016),4,.3,col1="darkblue")
#' gearPlotHH.wgdp("SP-ARSA",c(2014:2016),1,.2)
#' gearPlotHH.wgdp("SP-ARSA",c(2014:2016),4,.5)
#' gearPlotHH.wgdp(damb,c(2014:2016),4,pF=F,getICES=F)
#' }
#' @export
gearPlotHH.wgdp<-function(Survey,years,quarter,c.inta=.8,c.intb=.3,es=FALSE,col1="darkblue",col2="steelblue2",getICES=TRUE,pF=TRUE,ti=TRUE) {
  if (getICES) {
    dumb<-icesDatras::getDATRAS("HH",Survey,years,quarter)
  }
  if (!getICES) {
    dumb<-Survey
    if (!all(unique(years) %in% unique(dumb$Year))) stop(paste0("Not all years selected in years are present in the data.frame, check: ",unique(years)[which(!(unique(years) %in% unique(dumb$Year)))]))
    if (!all(unique(dumb$Quarter) %in% quarter)) stop(paste0("Quarter selected ",quarter," is not available in the data.frame, check please"))
  }
  dumb<-dplyr::filter(dumb,HaulVal!="I")
  #if (all(is.na(dumb$SweepLngt))) {stop("No valid Sweep Length data, this graph can not be produced")}
  dumb$SweepLngt[is.na(dumb$SweepLngt)]<-0
  dumb$sweeplngt[dumb$SweepLngt>0]<-factor(dumb$SweepLngt[dumb$SweepLngt>0])
      if (length(levels(factor(dumb$sweeplngt)))>2) {
        print(tapply(dumb$sweeplngt,dumb[,c("sweeplngt","Year")],"length"))
        stop("This function only works with data sets with two different sweep lengths, check you data")}
      if (length(subset(dumb$WingSpread,dumb$WingSpread> c(-9)))>0){
         wspr<-range(subset(dumb$WingSpread,dumb$WingSpread> c(0)))
         dspr<-range(subset(dumb$DoorSpread,dumb$DoorSpread>c(0)))
         dpthA<-range(dumb$Depth[dumb$Depth>0],na.rm=T)
         if (length(years)>1) {plot(WingSpread~Depth,dumb,xlim=c(0,dpthA[2]+20),ylim=c(0,wspr[2]+10),type="n",subset=WingSpread!=c(-9) & Year!=years[length(years)],pch=21,col=grey(.5),ylab=ifelse(es,"Abertura calones (m)","Wing spread (m)"),xlab=ifelse(es,"Profundidad (m)","Depth (m)"))
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
         #if (ti) title(main=paste0(ifelse(es,"Abertura calones vs. profundidad en ","Wing Spread vs. Depth in "),dumb$Survey[1],".Q",quarter),line=2.5)
         if (length(levels(factor(dumb$sweeplngt)))<2) {
            dpthA<-range(dumb$Depth[dumb$Depth>0],na.rm=T)
            dp<-seq(dpthA[1],dpthA[2]+20,length=650)
            if (length(years)>1) WingSpread.log<-nls(WingSpread~a1+b1*log(Depth),dumb,start=c(a1=.1,b1=1),subset=WingSpread!=c(-9) & WingSpread>0 & Year!=years[length(years)])
            else WingSpread.log<-nls(WingSpread~a1+b1*log(Depth),dumb,start=c(a1=.1,b1=1),subset=WingSpread!=c(-9) & WingSpread>0)
            if (pF) {
              points(WingSpread~Depth,dumb,subset=Year==years[length(years)],pch=21,bg=col1)
              if (length(years)>1) legend("bottomright",legend=c(paste0(years[1],"-",years[length(years)-1]),as.character(years[length(years)])),pch=21,col=col1,pt.bg=c(NA,col1),bty="n",inset=.02)
              else legend("bottomright",as.character(years),pch=21,col=col1,pt.bg=col1,bty="n",inset=.04)
              }
            if (ti) title(paste0(ifelse(es,"Abertura vertical vs. profundidad en","Wing Spread vs. Depth in "),dumb$Survey[1],".Q",quarter),line=2.5)
            mtext(dumb$Ship[1],line=.4,cex=.8,adj=0)
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
            if (length(years)>1) {
            WingSpreadst.log<-nls(WingSpread~a1+b1*log(Depth),dumbshort,start=c(a1=.1,b1=1),subset=WingSpread!=c(-9) & Year!=years[length(years)])
            WingSpreadlg.log<-nls(WingSpread~a1+b1*log(Depth),dumblong,start=c(a1=.1,b1=1),subset=WingSpread!=c(-9) & Year!=years[length(years)])
            }
            else {
              WingSpreadst.log<-nls(WingSpread~a1+b1*log(Depth),dumbshort,start=c(a1=.1,b1=1),subset=WingSpread!=c(-9))
              WingSpreadlg.log<-nls(WingSpread~a1+b1*log(Depth),dumblong,start=c(a1=.1,b1=1),subset=WingSpread!=c(-9))
            }
            if (pF) {
              points(WingSpread~Depth,dumbshort,subset=HaulVal=="V",pch=21,col=col2)
              points(WingSpread~Depth,dumblong,subset=HaulVal=="V",pch=21,col=col1)
              if (length(years)>1) legend("bottomright",c(paste(c(paste(years[1],years[length(years)-1],sep="-"),years[length(years)]),c(ifelse(es,"Malletas cortas","Short sweeps")),sep=" "),paste(c(paste(years[1],years[length(years)-1],sep="-"),years[length(years)]),c(ifelse(es,"Malletas largas","Long sweeps")),sep=" ")),pch=21,col=c(col2,col1,col1,col1),pt.bg=c(NA,col2,NA,col1),bty="n",inset=c(.02),ncol=2)
              else {if(es) legend("bottomright",c("Malletas cortas","Malletas largas"),pch=21,col=c(col1,col1),pt.bg = c(col2,col1),bty="n",inset=.04)
              else legend("bottomright",c("Short sweeps","Long sweeps"),pch=21,col=c(col1,col1),pt.bg = c(col2,col1),bty="n",inset=.04)}
              }
            if (ti) title(paste0(ifelse(es,"Abertura calones vs. profundidad en ","Wing Spread vs. Depth in "),dumb$Survey[1],".Q",quarter),line=2.5)
            mtext(dumb$Ship[1],line=.4,cex=.8,adj=0)
            a1st<-round(coef(WingSpreadst.log)[1],2)
            b1st<-round(coef(WingSpreadst.log)[2],2)
            lines(dpst,a1st+b1st*log(dpst),col=col2,lwd=2)
            a1lowst<-confint(WingSpreadst.log,level=c.intb)[1,1]
            b1lowst<-confint(WingSpreadst.log,level=c.intb)[2,1]
            lines(dpst,a1lowst+b1lowst*log(dpst),col=col2,lty=2,lwd=1)
            a1Uprst<-confint(WingSpreadst.log,level=c.intb)[1,2]
            b1Uprst<-confint(WingSpreadst.log,level=c.intb)[2,2]
            lines(dpst,a1Uprst+b1Uprst*log(dpst),col=col2,lty=2,lwd=1)
            if (pF) {
              points(WingSpread~Depth,dumbshort,subset=Year==years[length(years)],pch=21,bg=col2)
              points(WingSpread~Depth,dumblong,subset=Year==years[length(years)],pch=21,bg=col1)
              #legend("bottomright",c("Short sweeps","Long sweeps"),pch=21,col=c(col1,col1),pt.bg=c(col2,col1),bty="n",inset=.04)
              }
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
            if(es) dumbo<-bquote("Abertura calones"== a + b %*% log("Prof"))
            else dumbo<-bquote("Wing Spread"== a + b %*% log("Depth"))
            mtext(dumbo,line=.4,side=3,cex=.8,font=2,adj=1)
            print(summary(WingSpreadst.log))
            print(summary(WingSpreadlg.log))
         }
      }
   yearsb<-unique(dplyr::filter(dumb,!is.na(WingSpread) & WingSpread>0)$Year)
   if (length(years)>1 & !all(years %in% yearsb)) txt<-paste(ifelse(es,"A\u00f1os:","Years:"),paste0(c(yearsb[yearsb %in% years]),collapse=" "))
   if (length(years)>1 & all(years %in% yearsb)) txt<-paste0(ifelse(es,"A\u00f1os: ","Years: "),paste0(c(years[1],"-",years[length(years)]),collapse=" "))
   if (length(years)==1) txt<-paste0(ifelse(es,"A\u00f1o: ","Year: "),as.character(years))
   mtext(txt,1,line=-1.1,adj=0.01, font=1, cex=.8)
}
