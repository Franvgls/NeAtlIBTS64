#' Function gearPlotHH.nodp to plot net opening vs. depth 
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
#' @param es: If T titles and labels are in Spanish, if FALSE in English
#' @param col1: color for the symbols and lines for the whole set if only one set of sweeps are used, and for the data from the long set of sweeps.
#' @param col2: color for the symbols and lines for the data from the short sweeps in case there are two.
#' @param getICES: Should the data be downloaded from DATRAS? If T, default, the data are taken from DATRAS through the icesDatras package.
#' @param pF: takes out the points and leaves only the lines in the graphs
#' @param ti: if F title will not be included automatically and can be addedd later
#' @details Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @return Produces Net Vertical opening vs. Depth plot it also includes information on the ship, the time series used (bottom fourth graph), the models and parameters estimated.
#' @examples gearPlotHH.nodp("SWC-IBTS",c(2014:2016),1,.07,.5,col1="darkblue",col2="steelblue2")
#' @examples gearPlotHH.nodp("SWC-IBTS",c(2014:2016),1,.07,.5,col1="darkblue",col2="steelblue2",pF=F)
#' @examples gearPlotHH.nodp("SWC-IBTS",c(2013:2016),4)
#' @examples gearPlotHH.nodp("ROCKALL",c(2013:2016),3)
#' @examples gearPlotHH.nodp("NIGFS",c(2005:2016),1)
#' @examples gearPlotHH.nodp("NIGFS",c(2006:2007,2009:2016),4)
#' @examples gearPlotHH.nodp("IE-IGFS",c(2011:2016),4,.8)
#' @examples gearPlotHH.nodp("SP-PORC",c(2010:2016),3)
#' @examples gearPlotHH.nodp("FR-CGFS",c(2014:2016),4)
#' @examples gearPlotHH.nodp("EVHOE",c(1997:2015),4)
#' @examples gearPlotHH.nodp("SP-NORTH",c(2014:2016),4,col1="darkblue",col2="yellow")
#' @examples gearPlotHH.nodp("SP-ARSA",c(2014:2016),1)
#' @examples gearPlotHH.nodp("SP-ARSA",c(2014:2016),4)
#' @examples gearPlotHH.nodp(getICES=F,damb,c(2014:2016),4,pF=F)
#' @examples gearPlotHH.nodp(getICES=F,Survey=damb,years=c(2014:2016),quarter=4,pF=F)
#' @export
gearPlotHH.nodp<-function(Survey,years,quarter,c.inta=.8,c.intb=.3,es=FALSE,col1="darkblue",col2="steelblue2",getICES=TRUE,pF=TRUE,ti=TRUE) {
## get the data
    if (getICES) {
   dumb<-icesDatras::getDATRAS("HH",Survey,years,quarter)
   }
  if (!getICES) {
    dumb<-Survey
    if (!all(unique(years) %in% unique(dumb$Year))) stop(paste0("Not all years selected in years are present in the data.frame, check: ",unique(years)[which(!(unique(years) %in% unique(dumb$Year)))]))
    if (!all(unique(dumb$Quarter) %in% unique(quarter))) warning(paste0("Quarter selected ",quarter," is not available in the data.frame, check please"))
    }
  dumb<-dumb[dumb$HaulVal!="I",]
  dumb$sweeplngt<-factor(dumb$SweepLngt)
   if (length(subset(dumb$Netopening,dumb$Netopening> c(-9)))>0){
      dpthA<-range(dumb$Depth[dumb$Depth>0],na.rm=T)
      vrt<-range(subset(dumb$Netopening,dumb$Netopening> c(0)))
      plot(Netopening~Depth,dumb,xlim=c(0,dpthA[2]+20),ylim=c(0,vrt[2]+2),type="n",pch=21,col=col1,
         ylab=ifelse(es,"Abertura vertical","Vertical opening (m)"),xlab=ifelse(es,"Profundidad (m)","Depth (m)"),subset=Year!=years[length(years)] & Netopening> c(-9))
          if (pF) points(Netopening~Depth,dumb,pch=21,col=col1,subset=Year!=years[length(years)] & Netopening> c(-9))    
          if (length(levels(dumb$sweeplngt))<2) {
            dp<-seq(dpthA[1],dpthA[2]+20,length=650)
            if (length(years)>1) {Netopening.log<-nls(Netopening~a1+b1*log(Depth),dumb,start=c(a1=.1,b1=1),subset=HaulVal=="V" & Netopening> c(0) & Year!= years[length(years)])}
            else {Netopening.log<-nls(Netopening~a1+b1*log(Depth),dumb,start=c(a1=.1,b1=1),subset=HaulVal=="V" & Netopening> c(0))}
            if (ti) title(main=paste0(ifelse(es,"Abertura vertical vs. profundidad en ","Vertical opening vs. Depth in "),dumb$Survey[1],".Q",quarter," survey"),line=2.5)
            mtext(dumb$Ship[length(dumb$Ship)],line=.4,cex=.9,adj=0)
            a1<-round(coef(Netopening.log)[1],2)
            b1<-round(coef(Netopening.log)[2],2)
            lines(dp,a1+b1*log(dp),col=col1,lwd=2)
            a1low<-confint(Netopening.log,level=c.inta)[1,1]
            b1low<-confint(Netopening.log,level=c.inta)[2,1]
            lines(dp,a1low+b1low*log(dp),col=col1,lty=2,lwd=1)
            a1Upr<-confint(Netopening.log,level=c.inta)[1,2]
            b1Upr<-confint(Netopening.log,level=c.inta)[2,2]
            lines(dp,a1Upr+b1Upr*log(dp),col=col1,lty=2,lwd=1)
            if (pF) {
              points(Netopening~Depth,dumb,subset=Year==years[length(years)],pch=21,bg=col1,lwd=1)
              if (length(years)>1) legend("bottomright",c(paste0(years[1],"-",years[length(years)-1]),years[length(years)]),pch=c(1,21),col=c(col1),pt.bg=c(NA,col1),bty="n",inset=.02)              
              else legend("bottomright",as.character(years),pch=21,col=col1,pt.bg=col1,bty="n",inset=.02)
              }
            legend("topright",legend=substitute(NetOpening == a1 + b1 %*% log(depth),list(a1=round(coef(Netopening.log)[1],2),b1=(round(coef(Netopening.log)[2],2)))),bty="n",text.font=2,inset=.05)
            if (es) dumbo<-bquote("Abertura vertial red"== a + b %*% log("Prof"))
            else dumbo<-bquote("Net vert. opening"== a + b %*% log())
            summary(Netopening.log)
          }
          if (length(levels(dumb$sweeplngt))==2) {
           dumbshort<-subset(dumb,SweepLngt==levels(factor(dumb$SweepLngt))[1])
           dumblong<-subset(dumb,SweepLngt==levels(factor(dumb$SweepLngt))[2])
           dpthAst<-range(dumbshort$Depth,na.rm=T)
           dpthAlg<-range(dumblong$Depth,na.rm=T)
           dpst<-seq(dpthAst[1],dpthAst[2]+20,length=650)
           dplg<-seq(dpthAlg[1],dpthAlg[2]+20,length=650)
           if (length(years)>1) {
              Netopeningst.log<-nls(Netopening~a1+b1*log(Depth),dumbshort,start=c(a1=.1,b1=1),subset=HaulVal=="V" & Netopening> c(-9) & Year!=years[length(years)])  # se puede utilizar alg="plinear" para cuando no hay muestra suficiente para calcular los valores iniciales
              Netopeninglg.log<-nls(Netopening~a1+b1*log(Depth),dumblong,start=c(a1=.1,b1=1),subset=HaulVal=="V" & Netopening> c(-9) & Year!=years[length(years)])
           }
           else {
              Netopeningst.log<-nls(Netopening~a1+b1*log(Depth),dumbshort,start=c(a1=.1,b1=1),subset=HaulVal=="V" & Netopening> c(-9))  # se puede utilizar alg="plinear" para cuando no hay muestra suficiente para calcular los valores iniciales
              Netopeninglg.log<-nls(Netopening~a1+b1*log(Depth),dumblong,start=c(a1=.1,b1=1),subset=HaulVal=="V" & Netopening> c(-9))
              }
           vrtst<-range(subset(dumbshort$Netopening,dumbshort$Netopening> c(-9)))
           vrtlg<-range(subset(dumblong$Netopening,dumblong$Netopening> c(-9)))
           if (pF) {
              points(Netopening~Depth,dumbshort,subset=HaulVal=="V",pch=21,col=col2)   
              points(Netopening~Depth,dumbshort,subset=Year==years[length(years)],pch=21,bg=col2,lwd=1)   
              points(Netopening~Depth,dumblong,subset=HaulVal=="V",pch=21,col=col1)   
              points(Netopening~Depth,dumblong,subset=Year==years[length(years)],pch=21,bg=col1,lwd=1)   
              if (length(years)>1) legend("bottomright",c(paste(c(paste(years[1],years[length(years)-1],sep="-"),years[length(years)]),c(ifelse(es,"Malletas cortas","Short sweeps")),sep=" "),paste(c(paste(years[1],years[length(years)-1],sep="-"),years[length(years)]),c(ifelse(es,"Malletas largas","Long sweeps")),sep=" ")),pch=21,col=c(col2,col2,col1,col1),pt.bg=c(NA,col2,NA,col1),bty="n",inset=c(.02),ncol=2)           
              else {
                if (es) legend("bottomright",legend=c("Malletas cortas","Malletas largas"),pch=21,col=c(col1,col1),pt.bg=c(col2,col1),inset=.04,bty="n")
                else legend("bottomright",legend=c("Short sweeps","Long sweeps"),pch=21,col=c(col1,col1),pt.bg=c(col2,col1),inset=.04,bty="n")
                text(0,0, as.character(years),adj=0.01,font=1, cex=.9,pos=4)
                }
           }
           if (ti) title(main=paste0(ifelse(es,"Abertura vertical vs. profujndidad en ","Vertical opening vs. Depth in "),dumb$Survey[1],".Q",quarter),line=2.5)
           mtext(dumb$Ship[1],line=.4,cex=.9,adj=0)
           a1st<-round(coef(Netopeningst.log)[1],2)
           b1st<-round(coef(Netopeningst.log)[2],2)
           lines(dpst,a1st+b1st*log(dpst),col=col2,lwd=2)
           a1lowst<-confint(Netopeningst.log,level=c.inta)[1,1]
           b1lowst<-confint(Netopeningst.log,level=c.inta)[2,1]
           lines(dpst,a1lowst+b1lowst*log(dpst),col=col2,lty=2,lwd=1)
           a1Uprst<-confint(Netopeningst.log,level=c.inta)[1,2]
           b1Uprst<-confint(Netopeningst.log,level=c.inta)[2,2]
           lines(dpst,a1Uprst+b1Uprst*log(dpst),col=col2,lty=2,lwd=1)
           legend("topleft",legend=substitute(SortVop == a1st + b1st %*% log(depth),list(a1st=round(coef(Netopeningst.log)[1],2),b1st=(round(coef(Netopeningst.log)[2],2)))),bty="n",text.font=2,inset=.05)
           a1lg<-round(coef(Netopeninglg.log)[1],2)
           b1lg<-round(coef(Netopeninglg.log)[2],2)
           lines(dplg,a1lg+b1lg*log(dplg),col=col1,lwd=2)
           a1lowlg<-confint(Netopeninglg.log,level=c.intb)[1,1]
           b1lowlg<-confint(Netopeninglg.log,level=c.intb)[2,1]
           lines(dplg,a1lowlg+b1lowlg*log(dplg),col=col1,lty=2,lwd=1)
           a1Uprlg<-confint(Netopeninglg.log,level=c.intb)[1,2]
           b1Uprlg<-confint(Netopeninglg.log,level=c.intb)[2,2]
           lines(dplg,a1Uprlg+b1Uprlg*log(dplg),col=col1,lty=2,lwd=1)
           legend("topright",legend=substitute(LongVop == a1lg + b1lg %*% log(depth),list(a1lg=round(coef(Netopeninglg.log)[1],2),b1lg=(round(coef(Netopeninglg.log)[2],2)))),bty="n",text.font=2,inset=.2)
           summary(Netopeningst.log)
           summary(Netopeninglg.log)
           }
           if (es) dumbo<-bquote("Net Vert. opening"== a + b %*% log("Depth"))
           else dumbo<-bquote("Abertura vertical red"== a + b %*% log("Prof"))
           mtext(dumbo,line=.4,side=3,cex=.8,font=2,adj=1)
      }
  yearsb<-unique(dumb[c(!is.na(dumb$DoorSpread) & dumb$DoorSpread>0),]$Year)
  if (length(years)>1 & !all(years %in% yearsb)) txt<-paste(ifelse(es,"A\u00f1os:","Years:"),paste0(c(yearsb[yearsb %in% years]),collapse=" "))
  if (length(years)>1 & all(years %in% yearsb)) txt<-paste0(ifelse(es,"A\u00f1os:","Years:"),paste0(c(years[1],"-",years[length(years)]),collapse=" "))
  if (length(years)==1) txt<-paste0(ifelse(es,"A\u00f1o:","Year:"),as.character(years))
  mtext(txt,1,line=-1.1,adj=0.01, font=1, cex=.8)
}
            
