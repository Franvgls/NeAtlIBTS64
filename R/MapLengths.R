#' Function SplitLengths extracts from DATRAS the data to produce the IBTS maps with two length ranges for a fix set of species see file SpeciesCodes.csv
#' @param datSurvey: The Survey to be downloaded from DATRAS (see details), or a data frame with the HH information with  the DATRAS HH format  and the years and quarter selected in years and quarter
#' @param dtyear: year to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in yellow, it depends on the order of years, not the actual chronological year.
#' @param dtq: the quarter of the survey to be downloaded
#' @param esp: species to be included in the resulting map if plot=True
#' @param adj: weight abundance within species to the maximum abundance of the specie in the survey
#' @param escmult: regulates point size
#' @param cexleg: size of the legends and the texts in the graph
#' @param plot: if TRUE a map with the data selected in the species esp is presented
#' @param leg: if TRUE includes a legend to show relative size of the points
#' @param colo: bg of the color of the points
#' @param ti: includes a title with the name of the species in the plot
#' @param bw: if T map in black and white
#' @param save.dat: by default set to FALSE, if TRUE saves the data in a file: IBTSdataSURVEYyrQX.csv
#' @param out.dat: by default set to FALSE, if TRUE gives the data as output of the function
#' @param zeros: if TRUE the map includes small points with the hauls with out catch of the species
#' @details Surveys available in DATRAS recent years and not discontinued: EVHOE, FR-CGFS,
#' FR-WCGFS, IE-IAMS, IE-IGFS, NIGFS, NS-IBTS, PT-IBTS, SCOROC, SCOWCGFS, SP-ARSA, SP-NORTH, SP-PORC
#' @return during the calculations shows the types of data in the file "C", "P", "R", And the species of the set present in the Survey/year
#' @examples MapLengths("SP-NORTH",2024,4,esp="HKE",tmin=0,tmax=20,zeros=T,add=F)
#' @examples MapLengths("NS-IBTS",2023,3,esp="MEG",tmin=20,tmax=50,zeros=T,add=F)
#' @examples NeAtlIBTS::IBTSNeAtl_map(load=F,leg=F,xlims=c(-16,13),bw=T);MapLengths("NS-IBTS",2023,3,esp="HKE",tmin=0,tmax=21,zeros=T,add=T)
#' @export
#setwd("D:/FVG/Campanas/IBTS/IBTS_2024/mapping/DATOS")
MapLengths<-function(esp,dtSurv,dtyear,dtq,tmin,tmax,add=FALSE,ti=TRUE,subti=TRUE,leg=TRUE,colo="red",bw=FALSE,save.dat=FALSE,out.dat=FALSE,zeros=FALSE,onlyVal=F,cexleg=1,escmult=1,escCPUE=NA) {
  library(dplyr)
  worms<-SpeciesCodes[SpeciesCodes$Code==esp,"WoRMSCode"]
  dat.HH<-icesDatras::getHHdata(dtSurv,dtyear,dtq)
  dat.HL<-dplyr::filter(icesDatras::getHLdata(dtSurv,dtyear,dtq),Valid_Aphia==worms)
  dat.HL<-rbind(dat.HL%>%dplyr::filter(LngtCode%in%c(".", "5"))%>%dplyr::mutate(LngtClass=LngtClass/10),
                dat.HL%>%dplyr::filter(!LngtCode%in%c(".", "5")))
  #print(unique(dat.HH$DataType)) # if only "C" it would already be CPUE, with "R" or "P" has to be weighted to hour: subfactor*60/hauldur already in the code.
  dat.HL$HaulVal<-dat.HH$HaulVal[match(dat.HL$StNo,dat.HH$StNo)]
  dat.HL <-dat.HL[dat.HL$HaulVal!="I",]
  if (onlyVal) {dat.HL<-dat.HL[dat.HL$HaulVal=="V",]}
  dat.HL<-dplyr::filter(dat.HL, LngtClass>=tmin & LngtClass<=tmax)
  #print(range(dat.HL$LgntClass))
  if (nrow(dat.HL)==0) {
    warning(paste("No catches of ",esp,"in",dtSurv,dtyear))
    return()
    }
  dat.HL$HaulDur<-dat.HH$HaulDur[match(dat.HL$StNo,dat.HH$StNo)]
  dat.HL$DataType<-dat.HH$DataType[match(dat.HL$StNo,dat.HH$StNo)]
# str(dat.HH)
# str(dat.HL)
  if (sum(is.na(dat.HL$sufactor))>0) print(dplyr::filter(dat.HL,is.na(SubFactor)))
  dat.HL$CPUE<-NA
  for (i in 1:nrow(dat.HL)) {
    if (dat.HL$DataType[i]=="C") dat.HL$CPUE[i]<-dat.HL$HLNoAtLngt[i]*dat.HL$SubFactor[i]
    else dat.HL$CPUE[i]<-dat.HL$HLNoAtLngt[i]*dat.HL$SubFactor[i]*60/dat.HL$HaulDur[i]
  }
  #dat.HL$Species <- SpeciesCodes[match(dat.HL[,"Valid_Aphia"],SpeciesCodes$WoRMScode),"Code"]
  #dat.HL$Species<-as.factor(as.character(dat.HL$Species))
  #dat.HL$SizeRange <- SpeciesCodes[match(dat.HL[,"Valid_Aphia"],SpeciesCodes$WoRMScode),"LengthSplitMM"]
  #str(dat.HL)
  #if (nrow(dat.HL[dat.HL$Species==i,])>0) i<-levels(as.factor(as.character(SpeciesCodes$Code)))[1]
  #else i<-levels(as.factor(as.character(SpeciesCodes$Code)))[2]
  if (sum(dat.HL$CPUE)==0) {stop(paste("No catches of ",esp,"in",dtSurv))}
  datmap<-aggregate(CPUE~Year+Survey+Ship+HaulNo,dat.HL,sum)
  toplot<-merge(datmap,dat.HH[,c("HaulNo","ShootLat","ShootLong")],by="HaulNo")
      if (!add) {
    NeAtlIBTS64::IBTSNeAtl_map64(load=F,leg=F,dens=0,nl=max(dat.HH$ShootLat)+.5,sl=min(dat.HH$ShootLat)-.5,bw=ifelse(bw,T,F),
                             xlims=c(min(dat.HH$ShootLong)-1,1+ifelse(max(dat.HH$ShootLong)>-8,max(dat.HH$ShootLong),-8)))
      }
        if (ti) title(main=SpeciesCodes[match(esp,SpeciesCodes$Code),"Scientific"],font.main=4,line=2.5,cex.main=1*cexleg)
        if (subti) {
          if (tmin==0) sub<-bquote(" "<=.(format(paste0(tmax," cm"))))
          if (tmax==999) sub<-bquote(" ">.(format(paste0(tmin," cm"))))
          if (tmin!=0 & tmax!=999) sub<-paste(tmin,"-",tmax,"cm")
          if (tmin==0 & tmax==999) sub<-NA
          mtext(sub,side=1,line=2,font=2,cex=.9*cexleg)
        }
    if (is.na(escCPUE)) {points(ShootLat~ShootLong,toplot,cex=sqrt(toplot$CPUE/(.1*hablar::max_(toplot$CPUE)))*escmult,pch=21,col="black",bg=colo,lwd=1)}
    else {points(ShootLat~ShootLong,toplot,cex=sqrt(toplot$CPUE/(.1*escCPUE))*escmult,pch=21,col="black",bg=colo,lwd=1)}
  if (zeros)  points(ShootLat~ShootLong,filter(dat.HH,!HaulNo %in%toplot$HaulNo),pch=4,cex=1,col="black",lwd=2)
  if (leg) legend("bottomright",legend=round(fivenum(toplot$CPUE)[c(3:5)],0),pt.cex=sqrt(fivenum(toplot$CPUE)[c(3:5)]/(.1*hablar::max_(toplot$CPUE)))*escmult,inset=.02,pch=21,pt.bg="red",bg="white")
  if (out.dat) list(data=toplot,maxCPUE=hablar::max_(toplot$CPUE))
}
# #    if (out.dat)  write.csv(dataIBTS.dat,paste0("IBTSdata",datSurvey,substr(dtyear,3,4),"Q",dtq,".csv"),row.names=F)
# }
