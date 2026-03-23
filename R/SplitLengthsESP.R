#' Function SplitLengths extracts from DATRAS the data to produce the IBTS maps with two length ranges for a fix set of species see file SpeciesCodes.csv
#' @param datSurvey: The Survey to be downloaded from DATRAS (see details), or a data frame with the HH information with  the DATRAS HH format  and the years and quarter selected in years and quarter 
#' @param dtyear: year to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in yellow, it depends on the order of years, not the actual chronological year.
#' @param dtq: the quarter of the survey to be downloaded
#' @param esp: species to be included in the resulting map if plot=True, name as scientific upto date with WoRMS
#' @param plot: if TRUE a map with the data selected in the species esp is presented
#' @param legpos: by default "bottomright", but could be should be one of "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center"
#' @param ti: includes a title with the name of the species in the plot
#' @param save.dat: by default set to FALSE, if TRUE saves the data in a file: IBTSdataSURVEYyrQX.csv
#' @param out.dat: by default set to FALSE, if TRUE gives the data as output of the function
#' @param zeros: if TRUE the map includes small points with the hauls with out catch of the species
#' @details Surveys available in DATRAS recent years and not discontinued: EVHOE, FR-CGFS, 
#' FR-WCGFS, IE-IAMS, IE-IGFS, NIGFS, NS-IBTS, PT-IBTS, SCOROC, SCOWCGFS, SP-ARSA, SP-NORTH, SP-PORC
#' @return during the calculations shows the types of data in the file "C", "P", "R", And the species of the set present in the Survey/year
#' @examples SplitLengths("NS-IBTS",2023,3,esp="HKE",zeros=T)
#' @examples SplitLengths("NS-IBTS",2023,3,esp="MEG",zeros=T)
#' @export
#setwd("D:/FVG/Campanas/IBTS/IBTS_2024/mapping/DATOS")
SplitLengthsESP<-function(datSurvey,dtyear,dtq,esp="Merluccius merluccius",aphia=FALSE,L_Split=21,plot=TRUE,legpos="bottomright",ti=TRUE,save.dat=FALSE,out.dat=FALSE,zeros=FALSE) {
  if (aphia) Aphia_esp<-worrms::wm_name2id(esp) else Aphia_esp<-esp  
  dat.HH<-icesDatras::getHHdata(datSurvey,dtyear,dtq)
  dat.HL<-dplyr::filter(icesDatras::getHLdata(datSurvey,dtyear,dtq),Valid_Aphia==Aphia_esp)
  #print(unique(dat.HH$DataType)) # if only "C" it would already be CPUE, with "R" or "P" has to be weighted to hour: subfactor*60/hauldur already in the code.
  L_SplitMM<-L_Split*10
  #dat.HL<-subset(dat.HL,Valid_Aphia %in% SpeciesCodes$WoRMScode)
  dat.HL$HaulVal<-dat.HH$HaulVal[match(dat.HL$StNo,dat.HH$StNo)]
  dat.HL$HaulDur<-dat.HH$HaulDur[match(dat.HL$StNo,dat.HH$StNo)]
  dat.HL$DataType<-dat.HH$DataType[match(dat.HL$StNo,dat.HH$StNo)]
  if (nrow(dplyr::filter(dat.HL,LngtCode=="1"))>0) {dat.HL[dat.HL$LngtCode=="1","LngtClass"]<-dat.HL[dat.HL$LngtCode=="1","LngtClass"]*10}
# str(dat.HH)
# str(dat.HL)
  dat.HL<-dat.HL[dat.HL$HaulVal=="V",]
  if (nrow(dplyr::filter(dat.HL,is.na(SubFactor)))>0) {print(dplyr::filter(dat.HL,is.na(SubFactor)))}
  # else {stop(paste0("No catches of ",esp," in ",dtyear," ",datSurvey,"-Q",dtq),call. = F)}
  dat.HL$CPUE<-NA
  for (i in 1:nrow(dat.HL)) {
    if (dat.HL$DataType[i]=="C") dat.HL$CPUE[i]<-dat.HL$HLNoAtLngt[i]*dat.HL$SubFactor[i]
    else dat.HL$CPUE[i]<-dat.HL$HLNoAtLngt[i]*dat.HL$SubFactor[i]*60/dat.HL$HaulDur[i]
  }
  dat.HL$Species <- esp
  dat.HL$SizeRange <- L_Split
#  str(dat.HL)
  dat.HL$Size<-NULL
#  print(sort(unique(as.character(dat.HL$Species))))
#  print(tapply(datexch23$Species,datexch23[,c("Species")],length))
#  i<-sort(unique(as.character(datexch23$Species)))[1]
  #if (nrow(datexch23[datexch23$Species==i,])>0) i<-levels(as.factor(as.character(SpeciesCodes$Code)))[1]
  #else i<-levels(as.factor(as.character(SpeciesCodes$Code)))[2]
  dumb<-dat.HL[dat.HL$Species==esp,]
  dumbtot<-aggregate(CPUE~Year+Survey+Ship+HaulNo,dumb,sum)
  colnames(dumbtot)[match("CPUE",colnames(dumbtot))]<-c("Total")
  if (is.na(L_Split)) dattot<-data.frame(dumbtot[,1:4],Small=NA,Large=NA,Total=dumbtot[,5])
  if (!is.na(L_Split)) {
    dumbsm<-dumb[dumb$LngtClass<L_SplitMM,]
    if (nrow(dumbsm)>0) {
      dumbsm<-aggregate(CPUE~Year+Survey+Ship+HaulNo,dumbsm,sum)
      colnames(dumbsm)[match("CPUE",colnames(dumbsm))]<-c("Small")
      }
  else {
    dumbsm<-data.frame(Year=dtyear,Survey=datSurvey,Ship=unique(dumb$Ship),HaulNo=levels(as.factor(dumb$HaulNo)),Small=0)
    }
  dumblg<-dumb[dumb$LngtClas>=L_SplitMM,]
  if (nrow(dumblg)>0) {
     dumblg<-aggregate(CPUE~Year+Survey+Ship+HaulNo,dumblg,sum,na.rm=T)
     colnames(dumblg)[match("CPUE",colnames(dumblg))]<-c("Large")
    }
  else  {
    dumblg<-data.frame(Year=dtyear,Survey=datSurvey,Ship=unique(dumb$Ship),HaulNo=levels(as.factor(dumb$HaulNo)),Large=0)
    }
    datsize<-merge(dumbsm,dumblg,all.x=T,all.y=T)
    dattot<-merge(datsize,dumbtot,all.x=T,all.y=T)
    dattot<-data.frame(dattot[,1:4],dattot[5:7])
    }
  dataIBTS.dat<-merge(dattot,dat.HH[,c("HaulNo","ShootLat","ShootLong")],by="HaulNo")
   #dataIBTS.dat$Common_Name <- worrms::wm_id_ SpeciesCodes[match(dataIBTS.dat[,"SpeciesCode"],SpeciesCodes$Code),"Common"]
  dataIBTS.dat$Length_Split <- L_Split
  dataIBTS.dat$time <- NA
  #print(tapply(dataIBTS.dat$SpeciesCode,dataIBTS.dat$SpeciesCode,length))
  #dataIBTS.dat<-dataIBTS.dat[,c(3,10:9,13,2,1,4,5,11:12,8,6:7)]
  #dataIBTS.dat
  #if (out.dat)  write.csv(dataIBTS.dat,paste0("IBTSdata",datSurvey,substr(dtyear,3,4),"Q",dtq,".csv"),row.names=F)
    #windows()
    if (plot) {
    IBTSNeAtl_map(load=F,leg=F,dens=0,nl=max(dataIBTS.dat$ShootLat)+.5,sl=min(dataIBTS.dat$ShootLat)-.5,xlims=c(min(dataIBTS.dat$ShootLong)-1,1+ifelse(max(dataIBTS.dat$ShootLong)>-8,max(dataIBTS.dat$ShootLong),-8)))
    if (ti) title(main= ifelse(!aphia,worrms::wm_name2id(esp),esp),font.main=4,line=2,sub=bquote(" ">=.(format(paste0(L_Split," cm")))))
#      bquote(" ">=.(format(paste0(tmin,ifelse(unid.camp(gr,esp)$MED==2," mm"," cm")))))
    if (zeros)  points(ShootLat~ShootLong,dat.HH,pch=20,cex=.8,col="black")  
    if (!is.na(L_Split)) {
	     points(ShootLat~ShootLong,dataIBTS.dat,cex=sqrt(dataIBTS.dat[,"Small"]/(.1*hablar::max_(dataIBTS.dat[,"Small"])))*1,pch=21,col="red",lwd=2)
	     points(ShootLat~ShootLong,dataIBTS.dat,cex=sqrt(dataIBTS.dat[,"Large"]/(.1*hablar::max_(dataIBTS.dat[,"Large"])))*1,pch=21,col="blue",lwd=2)
       if (zeros) {
         legend(legpos,c("Small","Large","No Catch"),pch=c(21,21,20),
                col=c("red","blue","black"),inset=.1,bg="white",pt.lwd = 2,pt.cex = c(1.2,1.2,.8))         
       }
	     else legend(legpos,c("Small","Large"),pch=c(21,21),col=c("red","blue"),inset=.1,bg="white",pt.lwd = 2,pt.cex =1.2)
	     }
    else {
      points(ShootLat~ShootLong,dataIBTS.dat,cex=sqrt(dataIBTS.dat[,"Total"]/(.1*hablar::max_(dataIBTS.dat[,"Total"])))*1,pch=21,col="navy blue",lwd=2)
      if (zeros) {
        legend(legpos,c("Catch","No Catch"),pch=c(21,20),
               col=c("navy blue","black"),inset=.1,bg="white",pt.lwd = 2,pt.cex = c(1.2,.8))         
      }
      else legend(legpos,c("Catch"),pch=c(21),col=c("navy blue"),inset=.1,bg="white",pt.lwd = 2,pt.cex =1.2)
      }
    }
    #if (save.dat)  write.csv(dataIBTS.dat,paste0("IBTSdata",datSurvey,substr(dtyear,3,4),"Q",dtq,".csv"),row.names=F)
    if (out.dat)  dataIBTS.dat
  }
