#' Function IBTSNeAtl_map_porc plots the map with only Porcupine
#'
#' Produces a map from the shapefiles that define the IBTSNeAtl solo Porcupine
#' @param nl = 54.5 northernmost limit of the map
#' @param sl = 50.5 Southernmost limit of the map
#' @param leg = TRUE if TRUE includes the legend with the colors of the surveys
#' @param cex.leg = .7 Size of the legend
#' @param dens = 30 density of the shading lines for all the surveys
#' @param ICESdiv = TRUE if TRUE plots the IBTS divisions behind the shapefiles
#' @param ICESrect = FALSE if TRUE plots the lines of the ICES statistic rectangles
#' @param bathy = TRUE if TRUE plots the isobaths under the behind the shapefiles
#' @param out = format of the output, can be "def" default device, "pdf", "tiff" or "png"
#' @param nfile = name for the output file
#' @param shpdir = path to the folder with the shapefiles
#' @param load = T or F to load all the shapes files
#' @param places =T if T incluye letreros de Irlanda y Galway
#' @param es = F if F texts in English, if TRUE in Spanish
#' @examples IBTSNeAtl_map_porc(dens=0,leg=F,ICESrect = T);text(stat_y~stat_x,Area,labels=ICESNAME,cex=.8,font=4);text(stat_y~stat_x,Area,labels=Area,cex=.6,pos=1,col=2)
#' @export
IBTSNeAtl_map_porc<-function(nl=54.5,sl=50.5,xlims=c(-15.5,-8.5),leg=FALSE,es=FALSE,places=T,cex.leg=.7,dens=30,ICESdiv=TRUE,ICESrect=FALSE,bathy=TRUE,out="def",
                             nfile="NeAtlIBTS_map_porc",lwdl=.1,load=TRUE,shpdir="c:/GitHubRs/shapes/") {
  library(mapdata)
  library(maps)
  largo=(nl-sl)*10
  if (xlims[2] < 0) {
    ancho<- diff(rev(abs(xlims)))*10
  } else ancho<- diff(xlims)*10
  ices.div<-rgdal::readOGR(paste0(shpdir,"ices_div.dbf"),"ices_div",verbose = F)
  bath100<-rgdal::readOGR(paste0(shpdir,"100m.dbf"),"100m",verbose = F)
  bathy.geb<-rgdal::readOGR(paste0(shpdir,"bathy_geb.dbf"),"bathy_geb",verbose = F)
  Porc<-rgdal::readOGR(paste0(shpdir,"Porcupine.dbf"),"Porcupine",verbose = F)
  Porc_w84<-sp::spTransform(Porc,CRS("+proj=longlat +datum=WGS84"))
  switch(out,
         "pdf" = pdf(file = paste0(nfile,".pdf")),
         "tiff" = tiff(filename=paste0(nfile,".tiff"),width=660*ancho/largo,height=800*largo/ancho),
         "png" = png(filename=paste0(nfile,".png"),bg="transparent",type="cairo",width=round(800*ancho/largo),height=round(800*largo/ancho)))
  par(mar=c(3.5,2,2,2)+0.1)
  #  windows()
  maps::map(database = "worldHires", xlim = xlims, ylim = c(sl,nl),type="n")
#  maps::map(database = "worldHires", xlim = xlims, ylim = c(sl,nl),type="n")
  if (bathy) {
    sp::plot(bath100,add=T,col=gray(.85),lwd=.1)
    sp::plot(bathy.geb[bathy.geb$DEPTH!=100,],add=T,col=gray(.85),lwd=.1)
  }
  grid(col=gray(.8),lwd=.5)
  if (ICESdiv) sp::plot(ices.div,add=T,col=NA,border="burlywood")
  if (xlims[2] > 0) {
    degs = seq(xlims[1],-1,ifelse(abs(diff(xlims))>10,4,1))
    alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ W))
    axis(1, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
    axis(3, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
    degs = seq(3,xlims[2],ifelse(abs(diff(xlims))>1,4,1))
    alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ E))
    axis(1, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
    axis(3, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
    degs = c(0)
    alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ ""))
    axis(1, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
    axis(3, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
  } else {
    degs = seq(xlims[1],xlims[2],ifelse(ancho>10,4,1))
    alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ W))
    axis(1, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
    axis(3, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
  }
  degs = seq(sl,nl,ifelse(abs(diff(c(sl,nl)))>10,5,2))
  alt = sapply(degs,function(x) bquote(.(x)*degree ~ N))
  axis(2, at=degs, lab=do.call(expression,alt),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
  axis(4, at=degs, lab=do.call(expression,alt),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
  if (ICESrect) {
    abline(h=seq(30,65,by=.5),col=gray(.3),lwd=.2)
    abline(v=seq(-44,68),col=gray(.3),lwd=.2)
    }
  rug(seq(c(sl+.5),c(nl+.5),by=1),.005,side=2,lwd=lwdl,quiet=TRUE)
  rug(seq(c(xlims[1]+.5),c(xlims[2]+.5),by=1),.005,side=1,lwd=lwdl,quiet=TRUE)
  rug(seq(c(xlims[1]+.5),c(xlims[2]+.5),by=1),.005,side=3,lwd=lwdl,quiet=TRUE)
  rug(seq(c(sl+.5),c(nl+.5),by=1),.005,side=4,lwd=lwdl,quiet=TRUE)
  if (load) sp::plot(Porc_w84,add=T,col=5,lwd=.01,dens=dens,angle=180)
  maps::map(database = "worldHires",xlim = xlims, ylim = c(sl,nl),fill=T,col="gray",add=T,bg="blue")
  if (places) {
    points(-(9+.0303/.6),(53+.1623/.6),pch=16,col=1)
    text(-(9+.0303/.6),(53+.1623/.6),label="Galway",pos=3,cex=.7,font=2)
    text(-(9.1),(52.2),label=ifelse(es,"IRLANDA","IRELAND"),cex=1.3,font=2)
  }
  box()
  if (out!="def") dev.off()
}

# windows()
# IBTSNeAtl_map_porc(xlims=c(-15.55,-8.5),ICESdiv = F,load=T,dens=0)
# points(lat~long,lances,subset=camp=="P08",col=1,pch=16)
# points(lat~long,lances,subset=camp=="P09",col=2,pch=16)
# points(lat~long,lances,subset=camp=="P10",col=3,pch=16)
# points(lat~long,lances,subset=camp=="P11",col=4,pch=16)
# points(lat~long,lances,subset=camp=="P12",col=5,pch=16)
# legend("bottomright",legend=c(2008:2012),pch=16,col=1:5,bg="white",bty="o",
#        inset=c(.3,.04),title="Surveys",cex=1.2)
# points(-(9+.0303/.6),(53+.1623/.6),pch=16,col=1)
# text(-(9+.0303/.6),(53+.1623/.6),label="Galway",pos=3,cex=.7,font=2)
# text(-(9.1),(52.2),label="IRELAND",cex=1.3,font=2)
