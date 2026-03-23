#' Function SpeciesCAperYear Takes a CA file for one survey and produces a table with number of biological samples per year and species
#' @param Survey: Surveys available in DATRAS see details
#' @param years: years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in yellow, it depends on the order of years, not the actual chronological year.
#' @param  quarter: the quarter of the survey to be ploted
#' @details Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @examples SpeciesCAperYear("SP-NORTH",c(2014:2016),4)
#' @export
SpeciesCAperYear<-function(Survey,years,quarter){
#  library(SSOAP)
  listSurveys<-c("EVHOE","FR-CGFS","IE-IAMS","IE-IGFS","NIGFS","NS-IBTS","PT-IBTS","ROCKALL","SCOROC",
  "SCOWCGFS","SP-ARSA","SP-NORTH","SP-PORC","SWC-IBTS")
  if (!Survey %in% listSurveys) { stop(paste("Survey",Survey,"does not exist")) }       
  CA<-icesDatras::getDATRAS("CA",Survey,years,quarter)
  species<-data.frame(Aphia=unique(CA$Valid_Aphia),SpecName=worms::wormsbyid(unique(CA$Valid_Aphia))$valid_name)
  species$SpecName<-as.character(species$SpecName)
  CA$SpecName<-NA
  for (i in 1:nrow(CA)) {
  CA$SpecName[i]<-species[species$Aphia==CA$Valid_Aphia[i],"SpecName"]
  }
  tapply(CA$Sex,CA[,c("SpecName","Sex")],"length")
  }
