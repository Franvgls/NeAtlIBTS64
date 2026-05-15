#' Function gearPlotHH.nodp to plot net opening vs. depth for the Spanish N21 Survey
#'
#'
#' Produces Net Vertical opening vs. Depth plot and a model with nls R function. Data are taken directly from DATRAS getting all the data from DATRAS using function getDATRAS from library(icesDatras)
#' it only produces plots for surveys with HH files uploaded in DATRAS
#' If there are two different sweeps in the data, produces a model for each sweep length.
#' @param Survey: either the Survey to be downloaded from DATRAS (see details), or a data frame with the HH information with  the DATRAS HH format  and the years and quarter selected in years and quarter
#' @param years: years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in steelblue2, it depends on the order of years, not the actual chronological year.
#' @param quarter: the quarter of the survey to be ploted
#' @param c.inta: the confidence interval to be used in the confint function for all data if only one sweep length, and for the short sweeps in case there are two
#' @param c.intb: the confidence interval to be used in the confint function for the long set of sweeps.
#' @param es: If TRUE all labels and legends are in Spanish, if FALSE in English
#' @param col1: color for the symbols and lines for the whole set if only one set of sweeps are used, and for the data from the long set of sweeps.
#' @param col2: color for the symbols and lines for the data from the short sweeps in case there are two.
#' @param getICES: Should the data be downloaded from DATRAS? If T, default, the data are taken from DATRAS through the icesDatras package.
#' @param pF: takes out the points and leaves only the lines in the graphs
#' @param ti: if F title will not be included automatically and can be addedd later
#' @details Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @return Produces Net Vertical opening vs. Depth plot it also includes information on the ship, the time series used (bottom fourth graph), the models and parameters estimated.
#' @examples
#' \dontrun{
#' gearPlotHH.nodp("SWC-IBTS",c(2014:2016),1,.07,.5,col1="darkblue",col2="steelblue2")
#' gearPlotHH.nodp("SWC-IBTS",c(2014:2016),1,.07,.5,col1="darkblue",col2="steelblue2",pF=F)
#' gearPlotHH.nodp(getICES=F,Survey=damb,years=c(2014:2016),quarter=4,pF=F)
#' }
#' @export
gearPlotHHN21.nodp<-function(Survey="SP-NORTH",years=2021,quarter=4,c.inta=.8,c.intb=.3,es=FALSE,col1="darkblue",col2="red",incl2=TRUE,getICES=TRUE,pF=TRUE,ti=TRUE) {
  if (getICES) {
   dumb<-icesDatras::getDATRAS("HH",Survey,years,quarter)
   }
  if (!getICES) {
    dumb<-Survey
    if (!all(unique(years) %in% unique(dumb$Year))) stop(paste0("Not all years selected in years are present in the data.frame, check: ",unique(years)[which(!(unique(years) %in% unique(dumb$Year)))]))
    if (unique(dumb$Quarter)!=quarter) warning(paste0("Quarter selected ",quarter," is not available in the data.frame, check please"))
    }
  if(incl2) dumb<-subset(dumb,dumb$HaulVal!="I")
  if(!incl2) dumb<-subset(dumb,dumb$HaulVal=="V")
  dumb$ship<-factor(dumb$Ship)
  if (length(subset(dumb$Netopening,dumb$Netopening> c(-9)))>0){
      dpthA<-range(dumb$Depth[dumb$Depth>0],na.rm=T)
      vrt<-range(subset(dumb$Netopening,dumb$Netopening> c(0)))
      plot(Netopening~Depth,dumb,xlim=c(0,dpthA[2]+20),ylim=c(0,vrt[2]+2),type="n",pch=21,col=col1,
         ylab=ifelse(es,"Abertura vertical (m)","Vertical opening (m)"),xlab=ifelse(es,"Profundidad (m)","Depth (m)"),subset=Year!=years[length(years)] & Netopening> c(-9))
          if (pF) points(Netopening~Depth,dumb,pch=21,col=col1,subset=Year!=years[length(years)] & Netopening> c(-9))
          # if (length(levels(dumb$sweeplngt))<2) {
          #   dp<-seq(dpthA[1],dpthA[2]+20,length=650)
          #   if (length(years)>1) {Netopening.log<-nls(Netopening~a1+b1*log(Depth),dumb,start=c(a1=.1,b1=1),subset=HaulVal=="V" & Netopening> c(0) & Year!= years[length(years)])}
          #   else {Netopening.log<-nls(Netopening~a1+b1*log(Depth),dumb,start=c(a1=.1,b1=1),subset=HaulVal=="V" & Netopening> c(0))}
          #   if (ti) title(main=paste0("Vertical opening vs. Depth in ",dumb$Survey[1],".Q",quarter," survey"),line=2.5)
          #   mtext(dumb$Ship[length(dumb$Ship)],line=.4,cex=.9,adj=0)
          #   a1<-round(coef(Netopening.log)[1],2)
          #   b1<-round(coef(Netopening.log)[2],2)
          #   lines(dp,a1+b1*log(dp),col=col1,lwd=2)
          #   a1low<-confint(Netopening.log,level=c.inta)[1,1]
          #   b1low<-confint(Netopening.log,level=c.inta)[2,1]
          #   lines(dp,a1low+b1low*log(dp),col=col1,lty=2,lwd=1)
          #   a1Upr<-confint(Netopening.log,level=c.inta)[1,2]
          #   b1Upr<-confint(Netopening.log,level=c.inta)[2,2]
          #   lines(dp,a1Upr+b1Upr*log(dp),col=col1,lty=2,lwd=1)
          #   if (pF) {
          #     points(Netopening~Depth,dumb,subset=Year==years[length(years)],pch=21,bg=col1,lwd=1)
          #     if (length(years)>1) legend("bottomright",c(paste0(years[1],"-",years[length(years)-1]),years[length(years)]),pch=c(1,21),col=c(col1),pt.bg=c(NA,col1),bty="n",inset=.02)
          #     else legend("bottomright",as.character(years),pch=21,col=col1,pt.bg=col1,bty="n",inset=.02)
          #     }
          #   legend("topright",legend=substitute(NetOpening == a1 + b1 %*% log(depth),list(a1=round(coef(Netopening.log)[1],2),b1=(round(coef(Netopening.log)[2],2)))),bty="n",text.font=2,inset=.05)
          #   dumbo<-bquote("Net vert. opening"== a + b %*% log("Depth"))
          #   summary(Netopening.log)
          # }
          if (length(unique(dumb$Ship))==2) {
            dumbmol<-subset(dumb,Ship=="29MO")
            dumbvde<-subset(dumb,ship=="29VE")
            dpthAmol<-range(dumbmol$Depth,na.rm=T)
            dpthAvde<-range(dumbvde$Depth,na.rm=T)
            dpmol<-seq(dpthAmol[1],dpthAmol[2]+20,length=650)
            dpvde<-seq(dpthAvde[1],dpthAvde[2]+20,length=650)
            if (length(years)>1) {
              Netopeningmol.log<-nls(Netopening~a1+b1*log(Depth),dumbmol,start=c(a1=.1,b1=1),subset=HaulVal=="V" & Netopening> c(-9) & Year!=years[length(years)])  # se puede utilizar avde="plinear" para cuando no hay muestra suficiente para calcular los valores iniciales
              Netopeningvde.log<-nls(Netopening~a1+b1*log(Depth),dumbvde,start=c(a1=.1,b1=1),subset=HaulVal=="V" & Netopening> c(-9) & Year!=years[length(years)])
            }
           else {
              Netopeningmol.log<-nls(Netopening~a1+b1*log(Depth),dumbmol,start=c(a1=.1,b1=1),subset=HaulVal=="V" & Netopening> c(-9))  # se puede utilizar avde="plinear" para cuando no hay muestra suficiente para calcular los valores iniciales
              Netopeningvde.log<-nls(Netopening~a1+b1*log(Depth),dumbvde,start=c(a1=.1,b1=1),subset=HaulVal=="V" & Netopening> c(-9))
              }
           vrtmol<-range(subset(dumbmol$Netopening,dumbmol$Netopening> c(-9)))
           vrtvde<-range(subset(dumbvde$Netopening,dumbvde$Netopening> c(-9)))
           if (pF) {
              points(Netopening~Depth,dumbmol,subset=HaulVal=="V",pch=21,col=col1)
              points(Netopening~Depth,dumbmol,subset=Year==years[length(years)],pch=21,bg=col1,lwd=1)
              points(Netopening~Depth,dumbvde,subset=HaulVal=="V",pch=21,col=col2)
              points(Netopening~Depth,dumbvde,subset=Year==years[length(years)],pch=21,bg=col2,lwd=1)
              if (length(years)>1) legend("bottomright",c(paste(c(paste(years[1],years[length(years)-1],sep="-"),years[length(years)]),c("29MO"),sep=" "),paste(c(paste(years[1],years[length(years)-1],sep="-"),years[length(years)]),c("29VE"),sep=" ")),pch=21,col=c(col2,col2,col1,col1),pt.bg=c(NA,col2,NA,col1),bty="n",inset=c(.02),ncol=2)
              else {
                legend("bottomright",legend=c("28MO","29VE"),pch=21,col=c(col1,col1),pt.bg=c(col1,col2),inset=.04,bty="n")
                }
           }
           if (ti) title(main=paste0(ifelse(es,"Abertura vertical vs. profundidad en ","Vertical opening vs. Depth in "),dumb$Survey[1],".Q",quarter),line=2.5)
           mtext(dumb$Ship[length(dumb$Ship)],line=.4,cex=.9,adj=0)
           a1mol<-round(coef(Netopeningmol.log)[1],2)
           b1mol<-round(coef(Netopeningmol.log)[2],2)
           lines(dpmol,a1mol+b1mol*log(dpmol),col=col1,lwd=2)
           a1lowmol<-confint(Netopeningmol.log,level=c.inta)[1,1]
           b1lowmol<-confint(Netopeningmol.log,level=c.inta)[2,1]
           lines(dpmol,a1lowmol+b1lowmol*log(dpmol),col=col1,lty=2,lwd=1)
           a1Uprmol<-confint(Netopeningmol.log,level=c.inta)[1,2]
           b1Uprmol<-confint(Netopeningmol.log,level=c.inta)[2,2]
           lines(dpmol,a1Uprmol+b1Uprmol*log(dpmol),col=col1,lty=2,lwd=1)
           legend("topleft",legend=substitute(MoliverVop == a1mol + b1mol %*% log(depth),list(a1mol=round(coef(Netopeningmol.log)[1],2),b1mol=(round(coef(Netopeningmol.log)[2],2)))),bty="n",text.font=2,inset=.05)
           a1vde<-round(coef(Netopeningvde.log)[1],2)
           b1vde<-round(coef(Netopeningvde.log)[2],2)
           lines(dpvde,a1vde+b1vde*log(dpvde),col=col2,lwd=2)
           a1lowvde<-confint(Netopeningvde.log,level=c.intb)[1,1]
           b1lowvde<-confint(Netopeningvde.log,level=c.intb)[2,1]
           lines(dpvde,a1lowvde+b1lowvde*log(dpvde),col=col2,lty=2,lwd=1)
           a1Uprvde<-confint(Netopeningvde.log,level=c.intb)[1,2]
           b1Uprvde<-confint(Netopeningvde.log,level=c.intb)[2,2]
           lines(dpvde,a1Uprvde+b1Uprvde*log(dpvde),col=col2,lty=2,lwd=1)
           legend("topright",legend=substitute(VdeEzaVop == a1vde + b1vde %*% log(depth),list(a1vde=round(coef(Netopeningvde.log)[1],2),b1vde=(round(coef(Netopeningvde.log)[2],2)))),bty="n",text.font=2,inset=.2)
           summary(Netopeningmol.log)
           summary(Netopeningvde.log)
           }
           if (es) dumbo<-bquote("Abertura vertical red"== a + b %*% log("Prof"))
           else bquote("Net vert. opening"== a + b %*% log("Depth"))
           mtext(dumbo,line=.4,side=3,cex=.8,font=2,adj=1)
         # if (length(years)>1) mtext(txt,1,line=-1.1,adj=0.01, font=1, cex=.9)
      }
  if (length(years)>1) txt<-paste0(ifelse(es,"A\u00f1os: ","Years: "),paste0(c(years[1],"-",years[length(years)]),collapse=" "))
  else txt<-paste0(ifelse(es,"A\u00f1o: ","Year: "),as.character(years))
  mtext(txt,1,line=-1.1,adj=0.01, font=1, cex=.9)
  }

