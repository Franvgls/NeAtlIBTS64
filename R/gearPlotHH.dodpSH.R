#' Function gearPlotHH.dodp Door Spread versus Depth
#'
#' Produces a DoorSpread vs. Depth plot and model with nls R function. Data are taken directly from DATRAS using function getDATRAS from library(icesDatras)
#' it only produces plots for surveys with HH files uploaded in DATRAS
#' If there are two different sweeps in the data, produces a model for each sweep length.
#' @param Survey: either the Survey to be downloaded from DATRAS (see details), or a data frame with the HH information with  the DATRAS HH format  and the years and quarter selected in years and quarter
#' @param years: years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in steelblue2, it depends on the order of years, not the actual chronological year.
#' @param quarter: the quarter of the survey to be plotted
#' @param c.inta: the level (e.g. .95) of the band plotted for all data if only one sweep length, and for the long sweeps in case there are two
#' @param c.intb: the level of the band plotted for the short set of sweeps in case there are two
#' @param int.type: "prediction" (default) plots the range where an individual haul is expected to fall, does not shrink with more data and is the appropriate choice for flagging hauls with an abnormal gear geometry; "confidence" plots the range for the mean curve, which shrinks as data accumulates; "both" plots both bands
#' @param es: if T titles and legend are in Spanish, if F in English
#' @param col1: color for the symbols and lines for the whole set if only one set of sweeps are used, and for the data from the long set of sweeps.
#' @param col2: color for the symbols and lines for the data from the short sweeps in case there are two.
#' @param esc.mult Size of the legends and text in graphs,
#' @param getICES: Should the data be downloaded from DATRAS? If T, default, the data are taken from DATRAS through the icesDatras package.
#' @param ti: if F title will not be included automatically and can be addedd later
#' @param pF: takes out the points and leaves only the lines in the graphs
#' @details Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @return Produces a graph DoorSpread vs. Depth, it also includes information on the ship, the time series used, the models and parameters estimated.
#' @examples
#' \dontrun{
#' gearPlotHHNS.dodp("NS-IBTS",c(2014:2017),3,"SCO",.8,.3,col1="darkblue",col2="darkgreen")
#' }
#' @export
gearPlotHH.dodpSH<-function(Survey,years,quarter,c.inta=.8,c.intb=.3,int.type=c("prediction","confidence","both"),es=F,col1="darkblue",col2="steelblue2",esc.mult=1,getICES=T,ti=T,pF=T) {
  int.type<-match.arg(int.type)
  if (getICES) {
    dumb<-icesDatras::getDATRAS("HH",Survey,years,quarter)
  }
  if (!getICES) {
    dumb<-Survey
    if (!all(unique(years) %in% unique(dumb$Year))) stop(paste0("Not all years selected in years are present in the data.frame, check: ",unique(years)[which(!(unique(years) %in% unique(dumb$Year)))]))
    if (!all(unique(quarter) %in% unique(dumb$Quarter))) stop(paste0("Quarter selected ",quarter," is not available in the data.frame, check please"))
  }
  dumb<-dplyr::filter(dumb,HaulVal=="V")
  dumb$sweeplngt<-factor(dumb$SweepLngt)
   if (length(subset(dumb$DoorSpread,dumb$DoorSpread> c(-9)))>0){
      dspr<-range(subset(dumb$DoorSpread,dumb$DoorSpread>c(-9)))
      dpthA<-range(dumb$Depth,na.rm=T)
      dp<-seq(dpthA[1],dpthA[2]+20,length=650)
      plot(DoorSpread~Depth,dumb,type="n",xlim=c(0,dpthA[2]+20),ylim=c(0,dspr[2]+20),pch=21,col=col1,ylab=ifelse(es,"Abertura de puertas (m)","Door spread (m)"),xlab=ifelse(es,"Profundidad (m)","Depth (m)"),subset=DoorSpread!=c(-9)& Year!=years[length(years)],cex.lab=1*esc.mult,cex.axis=1*esc.mult)
      if (pF) {points(DoorSpread~Depth,dumb,pch=21,col=col1,subset=c(DoorSpread!=c(-9)))}
      if (ti) title(main=paste0(ifelse(es,"Abertura de puertas vs. profundidad en ","Door Spread vs. Depth in "),dumb$Survey[1],".Q",quarter),line=2.5,cex.main=1.1*esc.mult)
      if (length(levels(dumb$sweeplngt))<2) {
         DoorSpread.log<-nls(DoorSpread~a1+b1*log(Depth),dumb,start=c(a1=.1,b1=1),subset=HaulVal=="V" & DoorSpread> c(-9))
         dspr<-range(subset(dumb,DoorSpread>c(-9))$DoorSpread,na.rm=T)
         if (pF) {
           points(DoorSpread~Depth,dumb,pch=21,bg=col1)
           # if (length(years)>1) legend("bottomright",c(paste0(years[1],"-",years[length(years)-1]),years[length(years)]),pch=c(1,21),col=c(col1),pt.bg=c(NA,col1),bty="n",inset=.02)
           # else
           legend("bottomright",as.character(paste(years[1],"-",years[length(years)])),pch=21,col=col1,pt.bg=col1,bty="n",inset=.02,cex=1*esc.mult)
          }
         mtext(paste(dumb$Ship[1]),line=.4,cex=.8*esc.mult,adj=0)
         a1<-round(coef(DoorSpread.log)[1],2)
         b1<-round(coef(DoorSpread.log)[2],2)
         lines(dp,a1+b1*log(dp),col=col1,lwd=2)
         band<-gearBand(DoorSpread.log,"Depth",dp,level=c.inta,type=int.type)
         lines(dp,band$lwr,col=col1,lty=2,lwd=1)
         lines(dp,band$upr,col=col1,lty=2,lwd=1)
         if (int.type=="both") {
           lines(dp,band$lwr.conf,col=col1,lty=3,lwd=1)
           lines(dp,band$upr.conf,col=col1,lty=3,lwd=1)
         }
         legend("bottomright",legend=substitute(DS == a1 + b1 %*% log(depth),list(a1=round(coef(DoorSpread.log)[1],2),b1=(round(coef(DoorSpread.log)[2],2)))),bty="n",text.font=2,inset=.2,cex=1*esc.mult)
         #         text("bottomleft",paste0(c(years[1],"-",years[length(years)])),inset=c(0,.1))
         if (es){
           dumbo<-bquote("Abertura puertas"== a + b %*% log("prof"))
         }
         else dumbo<-bquote("Door Spread"== a + b %*% log("Depth"))
         mtext(dumbo,line=.4,side=3,cex=.8*esc.mult,font=2,adj=1)
         summary(DoorSpread.log)
         }
         if (length(levels(dumb$sweeplngt))==2) {
            dumbshort<-subset(dumb,SweepLngt==levels(factor(dumb$SweepLngt))[1])
            dumblong<-subset(dumb,SweepLngt==levels(factor(dumb$SweepLngt))[2])
            dpthAst<-range(dumbshort$Depth,na.rm=T)
            dpthAlg<-range(dumblong$Depth,na.rm=T)
            dpst<-seq(dpthAst[1],dpthAst[2]+20,length=650)
            dplg<-seq(dpthAlg[1],dpthAlg[2]+20,length=650)
            DoorSpreadst.log<-nls(DoorSpread~a1+b1*log(Depth),dumbshort,start=c(a1=.1,b1=1),subset=HaulVal=="V" & DoorSpread> c(-9))
            DoorSpreadlg.log<-nls(DoorSpread~a1+b1*log(Depth),dumblong,start=c(a1=.1,b1=1),subset=HaulVal=="V" & DoorSpread> c(-9))
            dspr<-range(subset(dumbshort$DoorSpread,dumbshort$DoorSpread>c(-9)))
            if (pF) {
              points(DoorSpread~Depth,dumbshort,pch=21,bg=col2)
            }
  #          if (length(years)>1) legend("bottomright",c(paste(c(paste(years[1],years[length(years)-1],sep="-"),years[length(years)]),c(ifelse(es,"Malletas cortas","Short sweeps")),sep=" "),paste(c(paste(years[1],years[length(years)-1],sep="-"),years[length(years)]),c(ifelse(es,"Malletas largas","Long sweeps")),sep=" ")),pch=21,col=c(col2,col2,col1,col1),pt.bg=c(NA,col2,NA,col1),bty="n",inset=c(.02),ncol=2)
#            else {
              if (es) legend("bottomright",legend=c("Malletas cortas","Malletas largas"),pch=21,col=c(col1,col1),pt.bg=c(col2,col1),inset=.04,bty="n",cex=1*esc.mult)
              else legend("bottomright",legend=c("Short sweeps","Long sweeps"),pch=21,col=c(col1,col1),pt.bg=c(col2,col1),inset=.04,bty="n",cex=1*esc.mult)
              text(0,0, as.character(years),adj=0.01,font=1, cex=.8*esc.mult,pos=4)
#            }
            if (ti) title(main=paste0(ifelse(es,"Abertura de puertas vs. profunfidad en ","Door Spread vs. Depth in "),dumb$Survey[1],".Q",quarter),line=2.5,cex.main=1.1*esc.mult)
            mtext(dumb$Ship[1],line=.4,cex=.8*esc.mult,adj=0)
            a1st<-round(coef(DoorSpreadst.log)[1],2)
            b1st<-round(coef(DoorSpreadst.log)[2],2)
            lines(dpst,a1st+b1st*log(dpst),col=col2,lwd=2)
            bandst<-gearBand(DoorSpreadst.log,"Depth",dpst,level=c.intb,type=int.type)
            lines(dpst,bandst$lwr,col=col2,lty=2,lwd=1)
            lines(dpst,bandst$upr,col=col2,lty=2,lwd=1)
            if (int.type=="both") {
              lines(dpst,bandst$lwr.conf,col=col2,lty=3,lwd=1)
              lines(dpst,bandst$upr.conf,col=col2,lty=3,lwd=1)
            }
            legend("bottomleft",legend=substitute(DSshort == a1st + b1st %*% log(depth),list(a1st=round(coef(DoorSpreadst.log)[1],2),b1st=(round(coef(DoorSpreadst.log)[2],2)))),bty="n",text.font=2,inset=c(.05,.2),cex=1*esc.mult)
            if (pF) {
              #points(DoorSpread~Depth,dumblong,subset=HaulVal=="V",pch=21,col=col1)
              points(DoorSpread~Depth,dumblong,subset=HaulVal=="V",pch=21,bg=col1)
            }
            a1lg<-round(coef(DoorSpreadlg.log)[1],2)
            b1lg<-round(coef(DoorSpreadlg.log)[2],2)
            lines(dplg,a1lg+b1lg*log(dplg),col=col1,lwd=2)
            bandlg<-gearBand(DoorSpreadlg.log,"Depth",dplg,level=c.inta,type=int.type)
            lines(dplg,bandlg$lwr,col=col1,lty=2,lwd=1)
            lines(dplg,bandlg$upr,col=col1,lty=2,lwd=1)
            if (int.type=="both") {
              lines(dplg,bandlg$lwr.conf,col=col1,lty=3,lwd=1)
              lines(dplg,bandlg$upr.conf,col=col1,lty=3,lwd=1)
            }
            legend("topright",legend=substitute(DSlong == a1lg + b1lg %*% log(depth),list(a1lg=round(coef(DoorSpreadlg.log)[1],2),b1lg=(round(coef(DoorSpreadlg.log)[2],2)))),bty="n",text.font=2,inset=c(.01,.4),cex=1*esc.mult)
#         text("bottomleft",paste0(c(years[1],"-",years[length(years)])),inset=c(0,.1))
         if (!es) {dumbo<-bquote("Door Spread"== a + b %*% log("Depth"))}
         else dumbo<-bquote("Abertura puertas"== a + b %*% log("prof"))
         mtext(dumbo,line=.4,side=3,cex=.8*esc.mult,font=2,adj=1)
         summary(DoorSpreadst.log)
         summary(DoorSpreadlg.log)
         }
      yearsb<-unique(dplyr::filter(dumb,!is.na(DoorSpread) & DoorSpread>0)$Year)
      if (length(years)>1 & !all(years %in% yearsb)) txt<-paste(ifelse(es,"A\u00f1os:","Years:"),paste0(c(yearsb[yearsb %in% years]),collapse=" "))
      if (length(years)>1 & all(years %in% yearsb)) txt<-paste0(ifelse(es,"A\u00f1os:","Years:"),paste0(c(years[1],"-",years[length(years)]),collapse=" "))
      # if (length(years)==1) txt<-paste0(ifelse(es,"A\u00f1o: ","Year: "),as.character(years))
      # mtext(txt,1,line=-1.1,adj=0.01, font=1, cex=.8)
   }
  }
