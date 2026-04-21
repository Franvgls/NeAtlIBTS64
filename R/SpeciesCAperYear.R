#' Function SpeciesCAperYear
#' Summary of biological samples from DATRAS CA records for one survey.
#'
#' By default produces a species x sex count of CA records. If `esp` is given,
#' restricts to that species and returns a length(cm) x sex matrix of numbers
#' at length (CANoAtLngt).
#'
#' @param Survey Survey available in DATRAS (see details)
#' @param year   Year(s) to download from DATRAS
#' @param quarter Quarter of the survey
#' @param esp   Optional: scientific name or AphiaID to restrict to a single
#'   species. If supplied, output is a length(cm) x sex matrix for that species.
#' @details Surveys available in DATRAS: EVHOE, FR-CGFS, FR-WCGFS, IE-IAMS,
#'   IE-IGFS, NIGFS, NS-IBTS, PT-IBTS, ROCKALL, SCOROC, SCOWCGFS, SP-ARSA,
#'   SP-NORTH, SP-PORC, SWC-IBTS.
#' @return A data.frame. Without `esp`: species x sex. With `esp`: length(cm)
#'   x sex, summing CANoAtLngt.
#' @examples
#' \dontrun{
#' SpeciesCAperYear("SP-NORTH", 2024, 4)
#' SpeciesCAperYear("SP-NORTH", 2024, 4, esp = "Merluccius merluccius")
#' SpeciesCAperYear("SP-NORTH", 2024, 4, esp = 126484)
#' }
#' @export
SpeciesCAperYear <- function(Survey, year, quarter, esp = NULL) {

  listSurveys <- c("EVHOE","FR-CGFS","FR-WCGFS","IE-IAMS","IE-IGFS","NIGFS",
                   "NS-IBTS","PT-IBTS","ROCKALL","SCOROC","SCOWCGFS",
                   "SP-ARSA","SP-NORTH","SP-PORC","SWC-IBTS")
  if (!Survey %in% listSurveys) stop(paste("Survey", Survey, "does not exist"))

  CA <- icesDatras::getDATRAS("CA", Survey, year, quarter)

  # Lookup único de AphiaID -> nombre, sin merge (evita el problema de fix.by)
  aphia_ids <- unique(stats::na.omit(CA$Valid_Aphia))
  name_lookup <- vapply(aphia_ids, function(id) {
    tryCatch(worrms::wm_id2name(id),
             error = function(e) NA_character_)
  }, character(1))
  names(name_lookup) <- as.character(aphia_ids)
  CA$Name <- name_lookup[as.character(CA$Valid_Aphia)]

  if (is.null(esp)) {
    # Resumen por especie x sexo (filas de CA = lecturas)
    res <- tapply(CA$Name, CA[, c("Name", "Sex")], length)
    res[is.na(res)] <- 0
    return(as.data.frame.matrix(res))
  }

  # --- Caso con especie concreta ---
  if (is.numeric(esp)) {
    CA_sp   <- CA[!is.na(CA$Valid_Aphia) & CA$Valid_Aphia == esp, ]
    sp_name <- unique(CA_sp$Name)[1]
  } else {
    CA_sp   <- CA[!is.na(CA$Name) & CA$Name == esp, ]
    sp_name <- esp
  }

  if (nrow(CA_sp) == 0)
    stop(paste("No CA records found for species", esp))

  # Conversión de talla a cm según LngtCode:
  #   "."  o "0"  -> mm    -> dividir por 10
  #   "1"         -> cm    -> dejar igual
  #   "5"         -> 5 mm  -> dividir por 10 (con resolución 0.5 cm)
  CA_sp$LngtCm <- ifelse(CA_sp$LngtCode %in% c(".", "0", "5"),
                         CA_sp$LngtClass / 10,
                         CA_sp$LngtClass)

  # Redondeo a cm enteros para que el rango de tallas sea manejable.
  # Si prefieres conservar medios cm (LngtCode == "5"), quítalo.
  CA_sp$LngtCm <- floor(CA_sp$LngtCm)

  res <- tapply(CA_sp$CANoAtLngt,
                CA_sp[, c("LngtCm", "Sex")],
                sum, na.rm = TRUE)
  res[is.na(res)] <- 0

  message(paste("Species:", sp_name,
                " | Length range:", min(CA_sp$LngtCm), "-", max(CA_sp$LngtCm), "cm",
                " | LngtCode:", paste(unique(CA_sp$LngtCode), collapse = "/")))
  as.data.frame.matrix(res)
}
