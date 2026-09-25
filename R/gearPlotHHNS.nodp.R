#' Function gearPlotHH.nodp to plot net opening vs. depth including the NS-IBTS and producing plots by country
#'
#'
#' Produces Net Vertical opening vs. Depth plot and a model with nls R function. Data are taken directly from DATRAS getting all the data from DATRAS using function getHHdata from library(icesDatras)
#' it only produces plots for surveys with HH files uploaded in DATRAS
#' If there are two different sweeps in the data, produces a model for each sweep length.
#' @param Survey: either the Survey to be downloaded from DATRAS (see details), or a data frame with the HH information with  the DATRAS HH format  and the years and quarter selected in years and quarter
#' @param years: years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in steelblue2, it depends on the order of years, not the actual chronological year.
#' @param quarter: the quarter of the survey to be ploted
#' @param country: The country chosen to be plotted (checks if it's available in the HH file)
#' @param c.inta: the level (e.g. .95) of the band plotted for all data if only one sweep length, and for the short sweeps in case there are two
#' @param c.intb: the level of the band plotted for the long set of sweeps
#' @param int.type: "prediction" (default) plots the range where an individual haul is expected to fall, does not shrink with more data and is the appropriate choice for flagging hauls with an abnormal gear geometry; "confidence" plots the range for the mean curve, which shrinks as data accumulates; "both" plots both bands
#' @param col1: color for the symbols and lines for the whole set if only one set of sweeps are used, and for the data from the long set of sweeps.
#' @param col2: color for the symbols and lines for the data from the short sweeps in case there are two.
#' @param getICES: Should the data be downloaded from DATRAS? If T, default, the data are taken from DATRAS through the icesDatras package.
#' @param pF: takes out the points and leaves only the lines in the graphs
#' @param ti: if T includes autmoatically the title, F leaves it blank an can be added later.
#' @details Surveys available in DATRAS: i.e. NS-IBTS,SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @return Produces Net Vertical opening vs. Depth plot it also includes information on the ship, the time series used (bottom fourth graph), the models and parameters estimated.
#' @examples
#' \dontrun{
#' gearPlotHHNS.nodp(Survey="NS-IBTS",years=c(2014:2016),quarter=3,country="ENG")
#' }
#' @export
gearPlotHHNS.nodp<-function(Survey="NS-IBTS",years,quarter,country,c.inta=.8,c.intb=.3,int.type=c("prediction","confidence","both"),col1="darkblue",col2="steelblue2",getICES=TRUE,pF=TRUE,ti=TRUE) {
  int.type<-match.arg(int.type)
  if (getICES) {
   dumb<-icesDatras::getDATRAS("HH",Survey,years,quarter)
   }
  if (!getICES) {
    dumb<-Survey
    if (!all(unique(years) %in% unique(dumb$Year))) stop(paste0("Not all years selected in years are present in the data.frame, check: ",unique(years)[which(!(unique(years) %in% unique(dumb$Year)))]))
    if (unique(dumb$Quarter)!=quarter) stop(paste0("Quarter selected ",country," ",quarter," is not available in the data.frame, check please"))
    }
    dumb<-dplyr::filter(dumb,HaulVal=="V")
    countries<-unique(dumb$Country)
    if (!country %in% countries) stop(paste(country,"is not present in this survey/quarter"))
    dumb<-dplyr::filter(dumb,Country==country)
    dumb$sweeplngt<-factor(dumb$SweepLngt)
    if (length(subset(dumb$Netopening,dumb$Netopening> c(-9)))>0){
      dpthA<-range(dumb$Depth[dumb$Depth>0],na.rm=T)
      vrt<-range(subset(dumb$Netopening,dumb$Netopening> c(0)))
      plot(Netopening~Depth,dumb,xlim=c(0,dpthA[2]+20),ylim=c(0,vrt[2]+2),type="n",pch=21,col=col1,
         ylab="Vertical opening (m)",xlab="Depth (m)",subset=Year!=years[length(years)] & Netopening> c(-9))
         if (ti) title(main=paste0("Vertical opening vs. Depth in ",country," ",dumb$Survey[1],".Q",quarter," survey"),line=2.5)
         mtext(paste("Ship: ",paste0(unique(dumb$Ship),collapse=" ")),line=.4,cex=.8,adj=0)
      if (pF) points(Netopening~Depth,dumb,pch=21,col=col1,subset=Year!=years[length(years)] & Netopening> c(-9))
             if (length(levels(dumb$sweeplngt))<2) {
           dp<-seq(dpthA[1],dpthA[2]+20,length=650)
           Netopening.log<-nls(Netopening~a1+b1*log(Depth),dumb,start=c(a1=.1,b1=1),subset=HaulVal=="V" & Netopening> c(0))
           a1<-round(coef(Netopening.log)[1],2)
           b1<-round(coef(Netopening.log)[2],2)
           lines(dp,a1+b1*log(dp),col=col1,lwd=2)
           band<-gearBand(Netopening.log,"Depth",dp,level=c.inta,type=int.type)
           lines(dp,band$lwr,col=col1,lty=2,lwd=1)
           lines(dp,band$upr,col=col1,lty=2,lwd=1)
           if (int.type=="both") {
             lines(dp,band$lwr.conf,col=col1,lty=3,lwd=1)
             lines(dp,band$upr.conf,col=col1,lty=3,lwd=1)
           }
           if (pF) {points(Netopening~Depth,dumb,subset=Year==years[length(years)],pch=21,bg=col1,lwd=1)}
           legend("topright",legend=substitute(NetOpening == a1 + b1 %*% log(depth),list(a1=round(coef(Netopening.log)[1],2),b1=(round(coef(Netopening.log)[2],2)))),bty="n",text.font=2,inset=.05)
           mtext(gearIntLabel(c.inta,int.type),side=1,line=-1.1,adj=.99,cex=1,font=2)
           dumbo<-bquote("Net vert. opening"== a + b %*% log("Depth"))
           summary(Netopening.log)
           }
       if (length(levels(dumb$sweeplngt))==2) {
           dumbshort<-subset(dumb,SweepLngt==levels(factor(dumb$SweepLngt))[1])
           dumblong<-subset(dumb,SweepLngt==levels(factor(dumb$SweepLngt))[2])
           dpthAst<-range(dumbshort$Depth,na.rm=T)
           dpthAlg<-range(dumblong$Depth,na.rm=T)
           dpst<-seq(dpthAst[1],dpthAst[2]+20,length=650)
           dplg<-seq(dpthAlg[1],dpthAlg[2]+20,length=650)
           Netopeningst.log<-nls(Netopening~a1+b1*log(Depth),dumbshort,start=c(a1=.1,b1=1),subset=HaulVal=="V" & Netopening> c(-9))  # se puede utilizar alg="plinear" para cuando no hay muestra suficiente para calcular los valores iniciales
           Netopeninglg.log<-nls(Netopening~a1+b1*log(Depth),dumblong,start=c(a1=.1,b1=1),subset=HaulVal=="V" & Netopening> c(-9))
           vrtst<-range(subset(dumbshort$Netopening,dumbshort$Netopening> c(-9)))
           vrtlg<-range(subset(dumblong$Netopening,dumblong$Netopening> c(-9)))
           if (pF) {points(Netopening~Depth,dumbshort,subset=HaulVal=="V",pch=21,col=col2)
              points(Netopening~Depth,dumbshort,subset=Year==years[length(years)],pch=21,bg=col2,lwd=1)
              points(Netopening~Depth,dumblong,subset=HaulVal=="V",pch=21,col=col1)
              points(Netopening~Depth,dumblong,subset=Year==years[length(years)],pch=21,bg=col1,lwd=1)
           }
           a1st<-round(coef(Netopeningst.log)[1],2)
           b1st<-round(coef(Netopeningst.log)[2],2)
           lines(dpst,a1st+b1st*log(dpst),col=col2,lwd=2)
           bandst<-gearBand(Netopeningst.log,"Depth",dpst,level=c.inta,type=int.type)
           lines(dpst,bandst$lwr,col=col2,lty=2,lwd=1)
           lines(dpst,bandst$upr,col=col2,lty=2,lwd=1)
           if (int.type=="both") {
             lines(dpst,bandst$lwr.conf,col=col2,lty=3,lwd=1)
             lines(dpst,bandst$upr.conf,col=col2,lty=3,lwd=1)
           }
           legend("topleft",legend=substitute(SortVop == a1st + b1st %*% log(depth),list(a1st=round(coef(Netopeningst.log)[1],2),b1st=(round(coef(Netopeningst.log)[2],2)))),bty="n",text.font=2,inset=.05)
           a1lg<-round(coef(Netopeninglg.log)[1],2)
           b1lg<-round(coef(Netopeninglg.log)[2],2)
           lines(dplg,a1lg+b1lg*log(dplg),col=col1,lwd=2)
           bandlg<-gearBand(Netopeninglg.log,"Depth",dplg,level=c.intb,type=int.type)
           lines(dplg,bandlg$lwr,col=col1,lty=2,lwd=1)
           lines(dplg,bandlg$upr,col=col1,lty=2,lwd=1)
           if (int.type=="both") {
             lines(dplg,bandlg$lwr.conf,col=col1,lty=3,lwd=1)
             lines(dplg,bandlg$upr.conf,col=col1,lty=3,lwd=1)
           }
           legend("bottomright",legend=substitute(LongVop == a1lg + b1lg %*% log(depth),list(a1lg=round(coef(Netopeninglg.log)[1],2),b1lg=(round(coef(Netopeninglg.log)[2],2)))),bty="n",text.font=2,inset=.1)
           if (isTRUE(all.equal(c.inta,c.intb))) {
             mtext(gearIntLabel(c.inta,int.type),side=1,line=-1.1,adj=.99,cex=1,font=2)
           } else {
             legend("topleft",legend=gearIntLabel(c.inta,int.type),text.col=col2,bty="n",text.font=2,cex=1,inset=c(.05,.15))
             legend("bottomright",legend=gearIntLabel(c.intb,int.type),text.col=col1,bty="n",text.font=2,cex=1,inset=c(.05,.2))
           }
           summary(Netopeningst.log)
           summary(Netopeninglg.log)
           }
           dumbo<-bquote("Net Vert. opening"== a + b %*% log("Depth"))
           mtext(dumbo,line=.4,side=3,cex=.8,font=2,adj=1)
         txt<-paste0("Years: ",paste0(c(years[1],"-",years[length(years)]),collapse=" "))
         mtext(txt,1,line=-1.1,adj=0.01, font=1, cex=.8)
#         text(0,0, txt,adj=0.01,font=1, cex=.9,pos=4)
      }
      }
