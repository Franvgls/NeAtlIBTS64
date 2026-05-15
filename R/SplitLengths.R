#' Function SplitLengths extracts from DATRAS the data to produce the IBTS maps with two length ranges for a fix set of species see file SpeciesCodes.csv
#' @param datSurvey: The Survey to be downloaded from DATRAS (see details), or a data frame with the HH information with  the DATRAS HH format  and the years and quarter selected in years and quarter
#' @param dtyear: year to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in yellow, it depends on the order of years, not the actual chronological year.
#' @param dtq: the quarter of the survey to be downloaded
#' @param esp: species to be included in the resulting map if plot=True
#' @param adj: weight abundance within species to the maximum abundance of the specie in the survey
#' @param plot: if TRUE a map with the data selected in the species esp is presented
#' @param ti: includes a title with the name of the species in the plot
#' @param save.dat: by default set to FALSE, if TRUE saves the data in a file: IBTSdataSURVEYyrQX.csv
#' @param out.dat: by default set to FALSE, if TRUE gives the data as output of the function
#' @param zeros: if TRUE the map includes small points with the hauls with out catch of the species
#' @details Surveys available in DATRAS recent years and not discontinued: EVHOE, FR-CGFS,
#' FR-WCGFS, IE-IAMS, IE-IGFS, NIGFS, NS-IBTS, PT-IBTS, SCOROC, SCOWCGFS, SP-ARSA, SP-NORTH, SP-PORC
#' @return during the calculations shows the types of data in the file "C", "P", "R", And the species of the set present in the Survey/year
#' @examples
#' \dontrun{
#' SplitLengths("NS-IBTS",2023,3,esp="HKE",zeros=T)
#' SplitLengths("NS-IBTS",2023,3,esp="MEG",zeros=T)
#' }
#' @export
#setwd("D:/FVG/Campanas/IBTS/IBTS_2024/mapping/DATOS")
SplitLengths<-function(datSurvey,dtyear,dtq,adj=FALSE,esp="HKE",plot=TRUE,ti=TRUE,save.dat=FALSE,out.dat=FALSE,zeros=FALSE) {
  dat.HH<-icesDatras::getHHdata(datSurvey,dtyear,dtq)
  dat.HL<-icesDatras::getHLdata(datSurvey,dtyear,dtq)
  print(unique(dat.HH$DataType)) # if only "C" it would already be CPUE, with "R" or "P" has to be weighted to hour: subfactor*60/hauldur already in the code.
  SpeciesCodes$LengthSplitMM<-SpeciesCodes$LengthSplit*10
  dat.HL<-subset(dat.HL,Valid_Aphia %in% SpeciesCodes$WoRMScode)
  dat.HL$HaulVal<-dat.HH$HaulVal[match(dat.HL$StNo,dat.HH$StNo)]
  dat.HL$HaulDur<-dat.HH$HaulDur[match(dat.HL$StNo,dat.HH$StNo)]
  dat.HL$DataType<-dat.HH$DataType[match(dat.HL$StNo,dat.HH$StNo)]
  if (nrow(dplyr::filter(dat.HL,LngtCode=="1"))>0) {dat.HL[dat.HL$LngtCode=="1","LngtClass"]<-dat.HL[dat.HL$LngtCode=="1","LngtClass"]*10}
# str(dat.HH)
# str(dat.HL)
  datexch23<-dat.HL[dat.HL$Valid_Aphia %in% SpeciesCodes$WoRMScode & dat.HL$HaulVal=="V",]
  print(dplyr::filter(datexch23,is.na(SubFactor)))
  datexch23$CPUE<-NA
  for (i in 1:nrow(datexch23)) {
    if (datexch23$DataType[i]=="C") datexch23$CPUE[i]<-datexch23$HLNoAtLngt[i]*datexch23$SubFactor[i]
    else datexch23$CPUE[i]<-datexch23$HLNoAtLngt[i]*datexch23$SubFactor[i]*60/datexch23$HaulDur[i]
  }
  datexch23$Species <- SpeciesCodes[match(datexch23[,"Valid_Aphia"],SpeciesCodes$WoRMScode),"Code"]
  # datexch23$Species<-as.factor(as.character(datexch23$Species))
  datexch23$SizeRange <- SpeciesCodes[match(datexch23[,"Valid_Aphia"],SpeciesCodes$WoRMScode),"LengthSplitMM"]
  str(datexch23)
  datexch23$Size<-NULL
  print(sort(unique(as.character(datexch23$Species))))
  print(tapply(datexch23$Species,datexch23[,c("Species")],length))
  i<-sort(unique(as.character(datexch23$Species)))[1]
  #if (nrow(datexch23[datexch23$Species==i,])>0) i<-levels(as.factor(as.character(SpeciesCodes$Code)))[1]
  #else i<-levels(as.factor(as.character(SpeciesCodes$Code)))[2]
  dumb<-datexch23[datexch23$Species==i,]
  dumbtot<-aggregate(CPUE~Year+Survey+Ship+HaulNo,dumb,sum)
  colnames(dumbtot)[match("CPUE",colnames(dumbtot))]<-c("Total")
  if (is.na(SpeciesCodes[match(i,SpeciesCodes$Code),"LengthSplitMM"])) dattot<-data.frame(dumbtot[,1:4],SpeciesCode=i,Small=NA,Large=NA,Total=dumbtot[,5]/mean(dumbtot[,5]))
  if (!is.na(SpeciesCodes[match(i,SpeciesCodes$Code),"LengthSplitMM"])) {
  dumbsm<-dumb[dumb$LngtClass<SpeciesCodes[match(i,SpeciesCodes$Code),"LengthSplitMM"],]
  if (nrow(dumbsm)>0) {
     dumbsm<-aggregate(CPUE~Year+Survey+Ship+HaulNo,dumbsm,sum)
     colnames(dumbsm)[match("CPUE",colnames(dumbsm))]<-c("Small")
     if (adj) {dumbsm$Small<-dumbsm$Small/mean(dumbsm$Small)}
     }
  else {
    dumbsm<-data.frame(Year=dtyear,Survey=datSurvey,Ship=dumb$Ship,HaulNo=levels(as.factor(dumb$HaulNo)),Small=0)
    }
  dumblg<-dumb[dumb$LngtClas>=SpeciesCodes[match(i,SpeciesCodes$Code),"LengthSplitMM"],]
  if (nrow(dumblg)>0) {
     dumblg<-aggregate(CPUE~Year+Survey+Ship+HaulNo,dumblg,sum,na.rm=T)
     colnames(dumblg)[match("CPUE",colnames(dumblg))]<-c("Large")
     if (adj) dumblg$Large<-dumblg$Large/mean(dumblg$Large)
    }
  else  {
    dumblg<-data.frame(Year=dtyear,Survey=datSurvey,Ship=dumb$Ship,HaulNo=levels(as.factor(dumb$HaulNo)),Large=0)
    }
  datsize<-merge(dumbsm,dumblg,all.x=T,all.y=T)
  dattot<-merge(datsize,dumbtot,all.x=T,all.y=T)
  dattot<-data.frame(dattot[,1:4],SpeciesCode=i,dattot[5:7])
  }
#ifelse (is.na(SpeciesCodes[match(i,SpeciesCodes$Code),"LengthSplitMM"])) dattot<-data.frame(dumbtot[,1:6],Small=0,Large=0,dumbtot[,7])
  dataIBTS.dat<-dattot
  nbsps<-length(sort(unique(as.character(datexch23$Species))))
  for (i in levels(as.factor(as.character(datexch23$Species)))[2:nbsps]) {
    dumb<-datexch23[datexch23$Species==i,]
    if (nrow(dumb)==0)  next
    dumbtot<-aggregate(CPUE~Year+Survey+Ship+HaulNo,dumb,sum)
    colnames(dumbtot)[match("CPUE",colnames(dumbtot))]<-c("Total")
    if (is.na(SpeciesCodes[match(i,SpeciesCodes$Code),"LengthSplitMM"])) {
      dattot<-data.frame(dumbtot[,1:4],SpeciesCode=i,Small=NA,Large=NA,Total=dumbtot[,5]/mean(dumbtot[,5]))
    }
    else {
      dumbsm<-dumb[dumb$LngtClass<SpeciesCodes[match(i,SpeciesCodes$Code),"LengthSplitMM"],]
      if (nrow(dumbsm)>0) {
        dumbsm<-aggregate(CPUE~Year+Survey+Ship+HaulNo,dumbsm,sum,na.rm=T)
        colnames(dumbsm)[match("CPUE",colnames(dumbsm))]<-c("Small")
        dumbsm$Small<-dumbsm$Small/mean(dumbsm$Small)
      }
    else {
      dumbsm<-data.frame(Year=dtyear,Survey=datSurvey,Ship=unique(dumb$Ship),HaulNo=levels(as.factor(dumb$HaulNo)),Small=0)
      }
      dumblg<-dumb[dumb$LngtClas>=SpeciesCodes[match(i,SpeciesCodes$Code),"LengthSplitMM"],]
    if (nrow(dumblg)>0) {
      dumblg<-aggregate(CPUE~Year+Survey+Ship+HaulNo,dumblg,sum,na.rm=T)
      colnames(dumblg)[match("CPUE",colnames(dumblg))]<-c("Large")
      dumblg$Large<-dumblg$Large/mean(dumblg$Large)
      }
    else dumblg<-data.frame(Year=dtyear,Survey=datSurvey,Ship=unique(dumb$Ship),HaulNo=levels(as.factor(dumb$HaulNo)),Large=0)
    datsize<-merge(dumbsm,dumblg,all.x=T,all.y=T)
    dattot<-merge(datsize,dumbtot,all.x=T,all.y=T)
    dattot<-data.frame(dattot[,1:4],SpeciesCode=i,dattot[,5:7])
    }
    dataIBTS.dat<-rbind(dataIBTS.dat,dattot)
    }
    dataIBTS.dat<-merge(dataIBTS.dat,dat.HH[,c("HaulNo","ShootLat","ShootLong")],by="HaulNo")
    dataIBTS.dat$Common_Name <- SpeciesCodes[match(dataIBTS.dat[,"SpeciesCode"],SpeciesCodes$Code),"Common"]
    dataIBTS.dat$Length_Split <- SpeciesCodes[match(dataIBTS.dat[,"SpeciesCode"],SpeciesCodes$Code),"LengthSplitMM"]
    dataIBTS.dat$time <- NA
    print(tapply(dataIBTS.dat$SpeciesCode,dataIBTS.dat$SpeciesCode,length))
    dataIBTS.dat<-dataIBTS.dat[,c(3,10:9,13,2,1,4,5,11:12,8,6:7)]
    #dataIBTS.dat
    #windows()
    if (plot) {
    IBTSNeAtl_map(load=F,leg=F,dens=0,nl=max(dataIBTS.dat$ShootLat)+.5,sl=min(dataIBTS.dat$ShootLat)-.5,xlims=c(min(dataIBTS.dat$ShootLong)-1,1+ifelse(max(dataIBTS.dat$ShootLong)>-8,max(dataIBTS.dat$ShootLong),-8)))
    if (ti) title(main=SpeciesCodes[match(esp,SpeciesCodes$Code),"Scientific"],font.main=4,line=1.5)
    if (zeros)  points(ShootLat~ShootLong,dat.HH,pch=20,cex=.8,col="black")
    if (!is.na(SpeciesCodes[match(esp,SpeciesCodes$Code),"LengthSplit"])) {
	     points(ShootLat~ShootLong,dataIBTS.dat,cex=sqrt(dataIBTS.dat[,"Small"]/(.1*max(dataIBTS.dat[dataIBTS.dat$SpeciesCode==esp,"Small"],na.rm=TRUE))),subset=SpeciesCode==esp,pch=21,col="red",lwd=2)
	     points(ShootLat~ShootLong,dataIBTS.dat,cex=sqrt(dataIBTS.dat[,"Large"]/(.1*max(dataIBTS.dat[dataIBTS.dat$SpeciesCode==esp,"Large"],na.rm=TRUE))),subset=SpeciesCode==esp,pch=21,col="blue",lwd=2)
	     if(zeros) {legend("bottomright",c("Large","Small","No Catch"),pch=c(21,21,20),
            col=c("blue","red","black"),inset=.1,bg="white",pt.lwd = 2,pt.cex = c(1.2,1.2,.8))}
	     else legend("bottomright",c("Large","Small"),pch=c(21,21),col=c("blue","red"),inset=.1,bg="white",pt.lwd = 2,pt.cex =1.2)
	     }
    else {
      points(ShootLat~ShootLong,dataIBTS.dat,cex=sqrt(dataIBTS.dat[,"Total"]/(.1*max(dataIBTS.dat[dataIBTS.dat$SpeciesCode==esp,"Total"],na.rm=TRUE))),subset=SpeciesCode==esp,pch=21,bg="blue",lwd=1)
      legend("bottomright",c(paste(esp,"Catches")),pch=21,pt.bg=c("blue"),inset=.1,bg="white",pt.lwd = 2,pt.cex = 1.2)
      }
    }
    if (save.dat)  write.csv(dataIBTS.dat,paste0("IBTSdata",datSurvey,substr(dtyear,3,4),"Q",dtq,".csv"),row.names=F)
    if (out.dat)  dataIBTS.dat
#    if (out.dat)  write.csv(dataIBTS.dat,paste0("IBTSdata",datSurvey,substr(dtyear,3,4),"Q",dtq,".csv"),row.names=F)
}
