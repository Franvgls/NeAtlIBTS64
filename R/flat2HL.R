#' Convierte capturas planas (una fila por ejemplar) a formato HL DATRAS
#'
#' @param flat data.frame con al menos: HaulNo, SpecCode, LngtClass, Weight_g
#' @param survey,country,ship,quarter,year metadatos comunes
#' @param gear,doorType,gearEx,lenMeasType valores fijos del survey
#' @param sex si NA se pone 3 (desconocido)
#' @return data.frame en formato HL listo para rbind con otros HL de DATRAS
flat2HL <- function(flat,
                    survey, country, ship, quarter, year,
                    gear = "NCT", doorType = "P", gearEx = "R",
                    lenMeasType = 1, sex = 3,
                    subFactor = 1, specVal = 1,
                    lngtCode = 1, devStage = -9) {

  # Agrega por lance + especie + talla
  agg <- aggregate(
    list(HLNoAtLngt = rep(1, nrow(flat))),
    by = list(HaulNo    = flat$HaulNo,
              SpecCode  = flat$SpecCode,
              LngtClass = flat$LngtClass),
    FUN = sum
  )

  # NoMeas por (lance, especie)
  noMeas <- aggregate(HLNoAtLngt ~ HaulNo + SpecCode, data = agg, FUN = sum)
  names(noMeas)[3] <- "NoMeas"

  # CatCatchWgt por (lance, especie) desde el peso de cada ejemplar
  catWgt <- aggregate(Weight_g ~ HaulNo + SpecCode, data = flat, FUN = sum)
  names(catWgt)[3] <- "CatCatchWgt"

  # Merges
  hl <- merge(agg, noMeas,  by = c("HaulNo","SpecCode"))
  hl <- merge(hl,  catWgt,  by = c("HaulNo","SpecCode"))

  # Completar columnas DATRAS
  data.frame(
    RecordType    = "HL",
    Survey        = survey,
    Quarter       = quarter,
    Country       = country,
    Ship          = ship,
    Gear          = gear,
    SweepLngt     = -9,
    GearEx        = gearEx,
    DoorType      = doorType,
    StNo          = -9,           # el real se recupera del HH por HaulNo
    HaulNo        = hl$HaulNo,
    Year          = year,
    SpecCodeType  = "W",
    SpecCode      = hl$SpecCode,
    SpecVal       = specVal,
    Sex           = sex,
    TotalNo       = hl$NoMeas,
    CatIdentifier = 1,
    NoMeas        = hl$NoMeas,
    SubFactor     = subFactor,
    SubWgt        = -9,
    CatCatchWgt   = hl$CatCatchWgt,
    LngtCode      = lngtCode,
    LngtClass     = hl$LngtClass,
    HLNoAtLngt    = hl$HLNoAtLngt,
    DevStage      = devStage,
    LenMeasType   = lenMeasType,
    DateofCalculation = format(Sys.Date(), "%Y%m%d"),
    Valid_Aphia   = hl$SpecCode,
    stringsAsFactors = FALSE
  )
}
