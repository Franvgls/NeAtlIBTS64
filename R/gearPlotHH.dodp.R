#' Function gearPlotHH.dodp Door Spread versus Depth
#'
#' Produces a DoorSpread vs. Depth plot and model with nls R function. Data are taken directly from DATRAS using function getDATRAS from library(icesDatras)
#' it only produces plots for surveys with HH files uploaded in DATRAS
#' If there are two different sweeps in the data, produces a model for each sweep length.
#' @param Survey: either the Survey to be downloaded from DATRAS (see details), or a data frame with the HH information with  the DATRAS HH format  and the years and quarter selected in years and quarter
#' @param years: years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in steelblue2, it depends on the order of years, not the actual chronological year.
#' @param quarter: the quarter of the survey to be plotted
#' @param c.int: the confidenc interval to be used in the confint function
#' @param c.inta: the confidence interval to be used in the confint function for all data if only one sweep length, and for the short sweeps in case there are two
#' @param c.intb: the confidence interval to be used in the confint function for the long set of sweeps.
#' @param es: si T titulos y leyendas salen en Spanish, si no en ingles.
#' @param col1: color for the symbols and lines for the whole set if only one set of sweeps are used, and for the data from the long set of sweeps.
#' @param col2: color for the symbols and lines for the data from the short sweeps in case there are two.
#' @param getICES: Should the data be downloaded from DATRAS? If T, default, the data are taken from DATRAS through the icesDatras package.
#' @param ti: if F title will not be included automatically and can be addedd later
#' @param pF: takes out the points and leaves only the lines in the graphs
#' @details Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @return Produces a graph DoorSpread vs. Depth, it also includes information on the ship, the time series used, the models and parameters estimated.
#' @examples gearPlotHHNS.dodp("NS-IBTS",c(2014:2017),3,"SCO",.8,.3,col1="darkblue",col2="darkgreen")
#' @export
gearPlotHH.dodp<-function(Survey,years,quarter,c.inta=.8,c.intb=.3,es=F,col1="darkblue",col2="steelblue2",getICES=T,ti=T,pF=T) {
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
      plot(DoorSpread~Depth,dumb,type="n",xlim=c(0,dpthA[2]+20),ylim=c(0,dspr[2]+20),pch=21,col=col1,ylab=ifelse(es,"Abertura de puertas (m)","Door spread (m)"),xlab=ifelse(es,"Profundidad (m)","Depth (m)"),subset=DoorSpread!=c(-9)& Year!=years[length(years)])
      if (pF) {points(DoorSpread~Depth,dumb,pch=21,col=col1,subset=c(DoorSpread!=c(-9) & Year!=years[length(years)]))}
      if (ti) title(main=paste0(ifelse(es,"Abertura de puertas vs. profundidad en ","Door Spread vs. Depth in "),dumb$Survey[1],".Q",quarter),line=2.5)
      if (length(levels(dumb$sweeplngt))<2) {
         DoorSpread.log<-nls(DoorSpread~a1+b1*log(Depth),dumb,start=c(a1=.1,b1=1),subset=HaulVal=="V" & DoorSpread> c(-9))
         dspr<-range(subset(dumb,DoorSpread>c(-9))$DoorSpread,na.rm=T)
         if (pF) {
           points(DoorSpread~Depth,dumb,subset=Year==years[length(years)],pch=21,bg=col1)
           if (length(years)>1) legend("bottomright",c(paste0(years[1],"-",years[length(years)-1]),years[length(years)]),pch=c(1,21),col=c(col1),pt.bg=c(NA,col1),bty="n",inset=.02)
           else legend("bottomright",as.character(years),pch=21,col=col1,pt.bg=col1,bty="n",inset=.02)
          }
         mtext(paste(dumb$Ship[1]),line=.4,cex=.8,adj=0)
         a1<-round(coef(DoorSpread.log)[1],2)
         b1<-round(coef(DoorSpread.log)[2],2)
         lines(dp,a1+b1*log(dp),col=col1,lwd=2)
         a1low<-confint(DoorSpread.log,level=c.inta)[1,1]
         b1low<-confint(DoorSpread.log,level=c.inta)[2,1]
         lines(dp,a1low+b1low*log(dp),col=col1,lty=2,lwd=1)
         a1Upr<-confint(DoorSpread.log,level=c.inta)[1,2]
         b1Upr<-confint(DoorSpread.log,level=c.inta)[2,2]
         lines(dp,a1Upr+b1Upr*log(dp),col=col1,lty=2,lwd=1)
         legend("bottomright",legend=substitute(DS == a1 + b1 %*% log(depth),list(a1=round(coef(DoorSpread.log)[1],2),b1=(round(coef(DoorSpread.log)[2],2)))),bty="n",text.font=2,inset=.2)
         #         text("bottomleft",paste0(c(years[1],"-",years[length(years)])),inset=c(0,.1))
         if (es){
           dumbo<-bquote("Abertura puertas"== a + b %*% log("prof"))
         }
         else dumbo<-bquote("Door Spread"== a + b %*% log("Depth"))
         mtext(dumbo,line=.4,side=3,cex=.8,font=2,adj=1)
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
              points(DoorSpread~Depth,dumbshort,subset=HaulVal=="V",pch=21,col=col2)
              points(DoorSpread~Depth,dumbshort,subset=Year==years[length(years)],pch=21,bg=col2)
            }
            if (length(years)>1) legend("bottomright",c(paste(c(paste(years[1],years[length(years)-1],sep="-"),years[length(years)]),c(ifelse(es,"Malletas cortas","Short sweeps")),sep=" "),paste(c(paste(years[1],years[length(years)-1],sep="-"),years[length(years)]),c(ifelse(es,"Malletas largas","Long sweeps")),sep=" ")),pch=21,col=c(col2,col2,col1,col1),pt.bg=c(NA,col2,NA,col1),bty="n",inset=c(.02),ncol=2)
            else {
              if (es) legend("bottomright",legend=c("Malletas cortas","Malletas largas"),pch=21,col=c(col1,col1),pt.bg=c(col2,col1),inset=.04,bty="n")
              else legend("bottomright",legend=c("Short sweeps","Long sweeps"),pch=21,col=c(col1,col1),pt.bg=c(col2,col1),inset=.04,bty="n")
              text(0,0, as.character(years),adj=0.01,font=1, cex=.9,pos=4)
            }
            if (ti) title(main=paste0(ifelse(es,"Abertura de puertas vs. profunfidad en ","Door Spread vs. Depth in "),dumb$Survey[1],".Q",quarter),line=2.5)
            mtext(dumb$Ship[1],line=.4,cex=.8,adj=0)
            a1st<-round(coef(DoorSpreadst.log)[1],2)
            b1st<-round(coef(DoorSpreadst.log)[2],2)
            lines(dpst,a1st+b1st*log(dpst),col=col2,lwd=2)
            a1lowst<-confint(DoorSpreadst.log,level=c.intb)[1,1]
            b1lowst<-confint(DoorSpreadst.log,level=c.intb)[2,1]
            lines(dpst,a1lowst+b1lowst*log(dpst),col=col2,lty=2,lwd=1)
            a1Uprst<-confint(DoorSpreadst.log,level=c.intb)[1,2]
            b1Uprst<-confint(DoorSpreadst.log,level=c.intb)[2,2]
            lines(dpst,a1Uprst+b1Uprst*log(dpst),col=col2,lty=2,lwd=1)
            legend("bottomleft",legend=substitute(DSshort == a1st + b1st %*% log(depth),list(a1st=round(coef(DoorSpreadst.log)[1],2),b1st=(round(coef(DoorSpreadst.log)[2],2)))),bty="n",text.font=2,cex=.9,inset=c(.05,.2))
            if (pF) {
              points(DoorSpread~Depth,dumblong,subset=HaulVal=="V",pch=21,col=col1)
              points(DoorSpread~Depth,dumblong,subset=Year==years[length(years)],pch=21,bg=col1)
            }
            a1lg<-round(coef(DoorSpreadlg.log)[1],2)
            b1lg<-round(coef(DoorSpreadlg.log)[2],2)
            lines(dplg,a1lg+b1lg*log(dplg),col=col1,lwd=2)
            a1lowlg<-confint(DoorSpreadlg.log,level=c.inta)[1,1]
            b1lowlg<-confint(DoorSpreadlg.log,level=c.inta)[2,1]
            lines(dplg,a1lowlg+b1lowlg*log(dplg),col=col1,lty=2,lwd=1)
            a1Uprlg<-confint(DoorSpreadlg.log,level=c.inta)[1,2]
            b1Uprlg<-confint(DoorSpreadlg.log,level=c.inta)[2,2]
            lines(dplg,a1Uprlg+b1Uprlg*log(dplg),col=col1,lty=2,lwd=1)
            legend("topright",legend=substitute(DSlong == a1lg + b1lg %*% log(depth),list(a1lg=round(coef(DoorSpreadlg.log)[1],2),b1lg=(round(coef(DoorSpreadlg.log)[2],2)))),bty="n",text.font=2,cex=.9,inset=c(.01,.4))
#         text("bottomleft",paste0(c(years[1],"-",years[length(years)])),inset=c(0,.1))
         if (!es) {dumbo<-bquote("Door Spread"== a + b %*% log("Depth"))
         }
         else dumbo<-bquote("Abertura puertas"== a + b %*% log("prof"))
         mtext(dumbo,line=.4,side=3,cex=.8,font=2,adj=1)
         summary(DoorSpreadst.log)
         summary(DoorSpreadlg.log)
         }
      yearsb<-unique(dplyr::filter(dumb,!is.na(DoorSpread) & DoorSpread>0)$Year)
      if (length(years)>1 & !all(years %in% yearsb)) txt<-paste(ifelse(es,"A\u00f1os:","Years:"),paste0(c(yearsb[yearsb %in% years]),collapse=" "))
      if (length(years)>1 & all(years %in% yearsb)) txt<-paste0(ifelse(es,"A\u00f1os:","Years:"),paste0(c(years[1],"-",years[length(years)]),collapse=" "))
      if (length(years)==1) txt<-paste0(ifelse(es,"A\u00f1o: ","Year: "),as.character(years))
      mtext(txt,1,line=-1.1,adj=0.01, font=1, cex=.8)
   }
  }
