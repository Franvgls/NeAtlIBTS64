#' Function qcHaulsDist checks consistency between hauls distance, speed and course parameters
#' 
#' Produces different plots comparing expected values and those included. 
#' Data are taken directly from DATRAS using function getDATRAS from library(icesDatras)
#' It only produces plots for surveys with HH files uploaded in DATRAS
#' @param Survey: either the Survey to be downloaded from DATRAS (see details), or a data frame with the HH information with  the DATRAS HH format  and the years and quarter selected in years and quarter 
#' @param years: years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in steelblue2, it depends on the order of years, not the actual chronological year.
#' @param quarter: the quarter of the survey to be ploted
#' @param allHauls Default is FALSE if TRUE lists values for all hauls, if not only those where pc.error>error
#' @param pc.error Acceptable error rate for not displaying hauls as erroneous
#' @param error.rb if FALSE does not take into account course errors
#' @param getICES: Should the data be downloaded from DATRAS? If T, default, the data are taken from DATRAS through the icesDatras package.
#' @param graf if FALSE the graph goes to screen, if its a file name (i.e. "graf") a .png file with that name is created and a message with location (wd) is shown in screen.
#' @param xpng width file png if graf is the name of the file
#' @param ypng height file png if graf is the name of the file
#' @param ppng points png parameter if graf is the name of the file
#' @details Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @family quality control
#' @references distHaversine function gives the haversine calculation of distance between two geographic points \code{\link[geosphere]{distHaversine}}
#' @return data.frame with survey, haul, distance and estimated values different errors and hours of dawn/sunrise/dusk
#' @examples qcHaulsDist("SP-NORTH",c(2024),4,error="Speed"); qcHaulsDist("IE-IAMS",2024,quarter=c(1,1),error="Course")
#' @export
qcHaulsDist<-function(Survey="NS-IBTS",years,quarter,pc.error=2,error.rb=TRUE,allHauls=FALSE,plots=TRUE,getICES=TRUE,error=c("Dist"),esc.mult=1,graf=FALSE,xpng=800,ypng=800,ppng=15) {
  if (!error %in% c("Dist","Speed","Course")) {stop("Options for error are Dist, Speed or Course")}
  if (getICES) {
      dumb<-icesDatras::getDATRAS("HH",Survey,years,quarter)
      if (identical(dumb, FALSE)) {
        stop("Survey and quarter combination do not exist")
      }
  }
  if (!getICES) {
    dumb<-Survey
    if (!all(unique(years) %in% unique(dumb$Year))) stop(paste0("Not all years selected in years are present in the data.frame, check: ",unique(years)[which(!(unique(years) %in% unique(dumb$Year)))]))
    if (unique(dumb$Quarter)!=quarter) stop(paste0("Quarter selected ",quarter," is not available in the data.frame, check please"))
  }
  quarter<-unique(dumb$Quarter)
  dumb<-dplyr::filter(dumb,HaulVal=="V")
#  if (any(dumb$HaulLat==c(-9))) stop("Some latitude/longitude data are missing, this function could not be used, Check data")
  if (any(is.na(dumb$Distance))) {
    datafromNA<-dumb[is.na(dumb$Distance),]
    dumb[is.na(dumb$Distance),"Distance"]<-geosphere::distHaversine(datafromNA[,c("ShootLong","ShootLat")],datafromNA[,c("HaulLong","HaulLat")])
    message(paste("Haul with NA value in Distance for HaulNo",datafromNA$HaulNo,"from",datafromNA$Country,"replaced with haversine distance between points"))
  }
  for(i in 1:nrow(dumb)) {
    if(dumb$HaulLat[i]==c(-9)) {dumb$HaulLat[i]<-NA}
    if(dumb$HaulLong[i]==c(-9)) {dumb$HaulLong[i]<-NA}
  }  
  countries<-unique(dumb$Country)
  #dumb<-dplyr::filter(dumb,Country==country)
  if (length(unique(dumb$Year))>1) stop("Only one year can be shown in this function")
  # print(tapply(dumb$Country,dumb[,c("Country","Year")],"length"))
  if (!is.logical(graf)) png(filename=paste0(graf,".png"),width = xpng,height = ypng, pointsize = ppng)
  if (is.logical(graf)) par(mar=c(2,2.5,2, 2.5) + 0.3,xaxs="i",yaxs="i")
  dumb$dist.vel<-dumb$HaulDur/60*dumb$GroundSpeed*1852 
  dumb$dist.hf<-round(geosphere::distHaversine(dumb[,c("ShootLong","ShootLat")],dumb[,c("HaulLong","HaulLat")]))
  dumb$vel.dist<-round((dumb$dist.hf/1852)/(dumb$HaulDur/60),1)
  dumb$error.vel<-round((dumb$dist.vel-dumb$Distance)*100/dumb$Distance,2)
  dumb$error.dist<-round((dumb$dist.hf-dumb$Distance)*100/dumb$Distance,2)
  dumb$rumb<-round(geosphere::bearingRhumb(dumb[,c("ShootLong","ShootLat")],dumb[c("HaulLong","HaulLat")]),0)
  dumb$error.rumb<-round(dumb$rumb-dumb$TowDir)
  dumb$date<-as.Date(paste0(dumb$Year,"-",dumb$Month,"-",dumb$Day))
  if (any(nchar(dumb$TimeShot)<3)) {
    message(paste("Some TimeShot are invalid, hauls",paste(dumb[nchar(dumb$TimeShot)<3,]$HaulNo,collapse = ", "),"will be ignored"))
    dumb<-dumb[nchar(dumb$TimeShot)>2,]
  }
  dumb$quarter<-quarter
  dumb$time<-ifelse(nchar(dumb$TimeShot)==3,paste0(0,dumb$TimeShot),dumb$TimeShot)
  dumb$time_l<-data.table::as.ITime(paste0(substr(dumb$time,1,2),":",substr(dumb$time,4,5)))
  dumb$time_v<-dumb$time_l+dumb$HaulDur*60
  dumb$lon<-dumb$ShootLong
  dumb$lat<-dumb$ShootLat
  dumb<-as.data.frame(cbind(dumb,suncalc::getSunlightTimes(data = dumb[,c("date","lat","lon")],tz="GMT",keep=c("dawn","sunrise","solarNoon"))[,c("dawn","sunrise","solarNoon")]))
  dumb$lon<-dumb$HaulLong
  dumb$lat<-dumb$HaulLat
  dumb<-as.data.frame(cbind(dumb,suncalc::getSunlightTimes(data = dumb[,c("date","lat","lon")],tz="GMT",keep=c("sunset","dusk"))[,c("sunset","dusk")]))
  dumb$daynight<-ifelse(data.table::as.ITime(dumb$sunrise)<dumb$time_l & dumb$time_l<data.table::as.ITime(dumb$sunset),"D","N")
  dumb$dawn<-data.table::as.ITime(dumb$dawn)
  dumb$sunrise<-data.table::as.ITime(dumb$sunrise)
  dumb$solarNoon<-data.table::as.ITime(dumb$solarNoon)
  dumb$sunset<-data.table::as.ITime(dumb$sunset)
  dumb$dusk<-data.table::as.ITime(dumb$dusk)
  dumb$dayhour<-NA
  for (i in 1:nrow(dumb)) { if (dumb$time_l[i]<dumb$dawn[i] | dumb$time_v[i]>dumb$dusk[i]) dumb$dayhour[i]<-"N" }
  for (i in 1:nrow(dumb)) { if (dumb$time_l[i]>dumb$dawn[i] & dumb$time_l[i]<dumb$sunrise[i]) dumb$dayhour[i]<-"S" }
  for (i in 1:nrow(dumb)) { if (dumb$time_l[i]>dumb$sunrise[i] & dumb$time_l[i]<dumb$solarNoon[i]) dumb$dayhour[i]<-"M" }
  for (i in 1:nrow(dumb)) { if (dumb$time_l[i]>dumb$solarNoon[i] & dumb$time_l[i]<dumb$sunset[i]) dumb$dayhour[i]<-"T" }
  for (i in 1:nrow(dumb)) { if (dumb$time_l[i]>dumb$sunset[i] & dumb$time_v[i]<dumb$dusk[i]) dumb$dayhour[i]<-"A" }
  # if (out.dat) return(dumb)
  # else return(dplyr::filter(dumb,daynight=="N")[,c("date","lance","daynight","sunrise","time_l","sunset","time_v")])
  dumb[abs(dumb$error.dist)>pc.error | abs(dumb$error.vel)>pc.error | abs(dumb$error.rumb)>pc.error,c("Survey","HaulNo","Distance","dist.hf","vel.dist","GroundSpeed","error.dist","error.vel","error.rumb")]
  op<-par(no.readonly = T)
  if (plots) {
    temp<-dumb[order(dumb$Year,dumb$HaulNo),c("Year","quarter","HaulNo","Distance","dist.hf","dist.vel","GroundSpeed","HaulDur","vel.dist","error.dist","error.vel","TowDir","rumb","error.rumb","Country")]
    par(mfrow=c(1,1),mar=c(5,4,4,2))
    if (error=="Dist") {
      ylims<-hablar::max_(abs(temp$error.dist))*1.1
      if (length(countries)==1) {
      plot(error.dist~HaulNo,temp,cex=sqrt(1+abs(error.dist)),pch=21,
         bg= dplyr::if_else(error.dist<0,"red","blue"),type="o",xlim=c(0,max(HaulNo)+1),
         ylim=c(-ylims,ylims),ylab="Error",xlab="Haul number",cex.lab=1*esc.mult,cex.axis=1*esc.mult)
      }
      else {
        plot(error.dist~HaulNo,temp,cex=sqrt(1+abs(error.dist)),pch=21,xlim=c(0,max(HaulNo)+1),
             bg= as.factor(Country),type="o",
             ylim=c(-ylims,ylims),ylab="Error",xlab="Haul number",cex.lab=1*esc.mult,cex.axis=1*esc.mult)
        legend("bottomright",unique(temp$Country),pch=21,cex=1,pt.bg=as.factor(temp$Country),inset = .05)
      }
      mtext(paste("Survey",unique(dumb$Survey)),side=3,line =0,adj =0,cex=0.8*esc.mult,font=2)
      abline(h=c(-quantile(temp$error.dist+mean(temp$error.dist,na.rm=T),pc.error/10),0,quantile(temp$error.dist+mean(temp$error.dist,na.rm=T),pc.error/10)),lty=c(3,2,3),lwd=c(.5,1,.5))
      title(main="Distance-Points error",cex.main=1.1*esc.mult)
    }
    if (error=="Speed"){
    ylims<-hablar::max_(abs(temp$error.vel))*1.1
    if (length(countries)==1) {
      plot(error.vel~HaulNo,temp,cex=sqrt(abs(error.vel)),pch=21,
         bg=dplyr::if_else(error.vel<0,"red","blue"),type="o",ylim=c(-ylims,ylims),xlim=c(0,max(HaulNo)+1),
         ylab="Error",xlab="Haul Number",cex.lab=1*esc.mult,cex.axis=1*esc.mult)
    }
    else {
      plot(error.vel~HaulNo,temp,cex=sqrt(abs(error.vel)),pch=21,
           bg= as.factor(Country),type="o",ylim=c(-ylims,ylims),xlim=c(0,max(HaulNo)+1),
           ylab="Error",xlab="Haul Number",cex.lab=1*esc.mult,cex.axis=1*esc.mult)
      legend("bottomright",unique(temp$Country),pch=21,cex=1,pt.bg=as.factor(temp$Country),inset = .05)
    }
    mtext(paste("Survey",unique(dumb$Survey)),side=3,line =0,adj =0,cex=0.8*esc.mult,font=2)
    abline(h=c(-quantile(temp$error.vel,pc.error/10),0,quantile(temp$error.vel,pc.error/10)),lty=c(3,2,3),lwd=c(.5,1,.5))
    title(main="Distance-speed error",cex.main=1.1*esc.mult)
    }
    if (error=="Course"){
    ylims<-hablar::max_(abs(temp$error.rumb))*1.1
    if (length(countries)==1) {
      plot(error.rumb~HaulNo,temp,cex=sqrt(abs(error.rumb)),pch=21,
         bg=dplyr::if_else(error.rumb<0,"red","blue"),type="o",ylim=c(-ylims,ylims),xlim=c(0,max(HaulNo)+1),
         ylab="Error",xlab="Haul Number",cex.lab=1*esc.mult,cex.axis=1*esc.mult)
    }
    else {
      plot(error.rumb~HaulNo,temp,cex=sqrt(abs(error.rumb)),pch=21,
           bg= as.factor(Country),type="o",ylim=c(-ylims,ylims),xlim=c(0,max(HaulNo)+1),
           ylab="Error",xlab="Haul Number",cex.lab=1*esc.mult,cex.axis=1*esc.mult)
      legend("bottomright",unique(temp$Country),pch=21,cex=1,pt.bg=as.factor(temp$Country),inset = .05)
    }
    mtext(paste("Survey",unique(dumb$Survey)),side=3,line =0,adj =0,cex=0.8*esc.mult,font=2)
    abline(h=c(-quantile(temp$error.rumb,pc.error/10),0,quantile(temp$error.rumb,pc.error/10)),lty=c(3,2,3),lwd=c(.5,1,.5))
    title(main="Course vs. Shoot-end points error",cex.main=1.1*esc.mult)
    }
    }
  if (length(unique(lubridate::year(dumb$date)))>1) 
    print(paste("Hauls from different years found: ",paste(unique(dumb$Year),collapse = ", ")))
  if (allHauls & error.rb) return(dumb[order(dumb$Survey,dumb$HaulNo),c("Survey","quarter","Year","HaulNo","Distance","dist.hf","dist.vel","GroundSpeed","HaulDur","vel.dist","error.dist","error.vel","TowDir","rumb","error.rumb","sunrise","time_l","sunset","time_v","dusk","daynight")])
  if (!allHauls & error.rb) {lt<-list(lances=(dumb[abs(dumb$error.dist)>pc.error | abs(dumb$error.vel)>pc.error*3 | abs(dumb$error.rumb)>pc.error*3,
                                                   c("Survey","quarter","Year","HaulNo","Distance","dist.hf","dist.vel","GroundSpeed","HaulDur","TowDir","rumb","vel.dist","error.dist","error.vel","error.rumb")]),
                                   daynight=dplyr::filter(dumb,daynight=="N")[,c("date","HaulNo","daynight","sunrise","time_l","sunset","time_v")])
  return(lt)}
  if (!error.rb) return(dumb[abs(dumb$error.dist)>pc.error | abs(dumb$error.vel)>pc.error*3,
                                c("Survey","quarter","HaulNo","Distance","dist.hf","dist.vel","GroundSpeed","HaulDur","vel.dist","error.dist","error.vel")])
}  
  
  
