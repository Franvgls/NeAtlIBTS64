#' Species list in DATRAS HL
#'
#' @param Survey survey name from those present in DATRAS
#' @param Year Year of the survey
#' @param Quarter Quarter of the survey
#'
#' @importFrom icesDatras getHLdata
#' @importFrom worrms wm_id2name_
#' @returns A vector with the species list in the HL
#' @export
#'
#' @examples
#' \dontrun{
#' SpeciesInHL("SP-NORTH", 2025, 4)
#' }
SpeciesInHL <- function(Survey, Year, Quarter) {
  HLsurvey <- icesDatras::getHLdata(Survey, Year, Quarter)
  sort(unname(unlist(worrms::wm_id2name_(unique(HLsurvey$SpecCode)))))
}
