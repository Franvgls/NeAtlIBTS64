#' Construye la clave talla-edad desde DATRAS
#'
#' Descarga los datos CA (Age) de DATRAS para una especie, survey,
#' año y trimestre concretos, y los reformatea en una tabla
#' \code{data.frame} con \code{LngtClasscm} y \code{Age} lista
#' para usar en \code{\link{grafalk.NeAtl64}}.
#'
#' @param survey  Survey de DATRAS, p.ej. "SP-NORTH", "EVHOE", "IE-IGFS",
#'   "SP-PORC", "SP-ARSA", "PT-IBTS".
#' @param year    Año.
#' @param quarter Trimestre (1-4).
#' @param Aphia   Valid_Aphia ID de la especie. Para sardina = 126421,
#'   merluza = 126484, etc. Ver \code{\link[worrms]{wm_records_taxamatch}}.
#' @param by_cm   Lógico. Si TRUE (default) agrupa por cm enteros
#'   (\code{trunc(LngtClass/10)}). Si FALSE mantiene 0.5 cm
#'   (\code{LngtClass/10}).
#' @param rdx reduce los campos a los relacionados con la edad
#' @return \code{data.frame} con columnas \code{LngtClasscm}, \code{Age},
#'   y atributos \code{survey}, \code{year}, \code{quarter}, \code{Aphia}.
#'   Lista para pasarse directamente a \code{\link{grafalk.NeAtl64}}.
#' @seealso \code{\link{grafalk.NeAtl64}}
#' @family ALK DATRAS
#' @export
#' @examples
#' \dontrun{
#' # Sardina, North Spain Q4 2023
#' ca <- GetAlkDTR.NeAtl64("SP-NORTH", 2023, 4, Aphia = 126421)
#' head(ca)
#' grafalk.NeAtl64(ca)
#' }
GetAlkDTR.NeAtl64 <- function(survey, year, quarter, Aphia,
                              by_cm = TRUE) {

  ca <- icesDatras::getCAdata(survey, year, quarter)
  if (is.null(ca) || nrow(ca) == 0)
    stop(sprintf("DATRAS no devuelve CA para %s %d-Q%d",
                 survey, year, quarter))

  ca <- ca[ca$Valid_Aphia == Aphia, , drop = FALSE]
  if (nrow(ca) == 0)
    stop(sprintf("Sin datos CA para Aphia=%s en %s %d-Q%d",
                 Aphia, survey, year, quarter))

  ca$LngtClasscm <- if (by_cm) trunc(ca$LngtClass / 10)
  else        ca$LngtClass / 10

  attr(ca, "survey")  <- survey
  attr(ca, "year")    <- year
  attr(ca, "quarter") <- quarter
  attr(ca, "Aphia")   <- Aphia
  attr(ca, "by_cm")   <- by_cm

  ca

}
