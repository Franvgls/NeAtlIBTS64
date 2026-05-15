#' Function SpeciesHLperYear
#' Summary of length-frequency data from DATRAS HL records for one survey.
#'
#' Default output is a per-species summary including number of HL records,
#' total measured fish, total caught (scaled), length range and the length
#' coding used. Useful to explore which species and length ranges are
#' available in a given survey-year-quarter combination.
#'
#' @param Survey Survey available in DATRAS (see details).
#' @param year Year to download from DATRAS.
#' @param quarter Quarter of the survey.
#' @param esp Optional: scientific name or AphiaID to restrict to a single
#'   species. If supplied, output is a length(cm) x sex matrix summing
#'   HLNoAtLngt for that species.
#' @param out Output type when \code{esp} is NULL: \code{"summary"}
#'   (default, per-species data.frame) or \code{"matrix"} (species x sex
#'   matrix of HL record counts).
#' @param sort_by When \code{out = "summary"}, row order: \code{"n"}
#'   (descending total measured, default), \code{"name"} alphabetical,
#'   \code{"records"} (descending HL records) or \code{"none"}.
#' @details Surveys available in DATRAS: EVHOE, FR-CGFS, FR-WCGFS, IE-IAMS,
#'   IE-IGFS, NIGFS, NS-IBTS, PT-IBTS, ROCKALL, SCOROC, SCOWCGFS, SP-ARSA,
#'   SP-NORTH, SP-PORC, SWC-IBTS.
#' @return A data.frame. Format depends on \code{esp} and \code{out}.
#' @seealso \code{\link{SpeciesCAperYear}}, \code{\link{GetAlkDTR.NeAtl64}}
#' @family DATRAS
#' @examples
#' \dontrun{
#' # Resumen de todas las especies
#' SpeciesHLperYear("SP-NORTH", 2020, 4)
#'
#' # Distribución de tallas de una especie concreta
#' SpeciesHLperYear("SP-NORTH", 2020, 4, esp = "Scomber scombrus")
#' SpeciesHLperYear("SP-NORTH", 2020, 4, esp = 127023)
#' }
#' @export
SpeciesHLperYear <- function(Survey, year, quarter, esp = NULL,
                             out = c("summary", "matrix"),
                             sort_by = c("n", "name", "records", "none")) {

  listSurveys <- c("EVHOE","FR-CGFS","FR-WCGFS","IE-IAMS","IE-IGFS","NIGFS",
                   "NS-IBTS","PT-IBTS","ROCKALL","SCOROC","SCOWCGFS",
                   "SP-ARSA","SP-NORTH","SP-PORC","SWC-IBTS")
  if (!Survey %in% listSurveys) stop(paste("Survey", Survey, "does not exist"))
  out     <- match.arg(out)
  sort_by <- match.arg(sort_by)

  HL <- icesDatras::getHLdata(Survey, year, quarter)
  if (is.null(HL) || nrow(HL) == 0) {
    message("DATRAS no devuelve HL para ", Survey, " ", year, "-Q", quarter)
    return(invisible(NULL))
  }

  # Lookup único de AphiaID -> nombre
  aphia_ids <- unique(stats::na.omit(HL$Valid_Aphia))
  name_lookup <- vapply(aphia_ids, function(id) {
    tryCatch(worrms::wm_id2name(id),
             error = function(e) NA_character_)
  }, character(1))
  names(name_lookup) <- as.character(aphia_ids)
  HL$Name <- name_lookup[as.character(HL$Valid_Aphia)]

  # Talla en cm según LngtCode:
  #   "." / "0" / "5"  -> en mm o 5mm -> /10
  #   "1"              -> ya en cm
  HL$LngtCm <- ifelse(HL$LngtCode %in% c(".", "0", "5"),
                      HL$LngtClass / 10,
                      HL$LngtClass)

  # ── Caso especie concreta: distribución talla x sexo ────────────────
  if (!is.null(esp)) {
    if (is.numeric(esp)) {
      HL_sp   <- HL[!is.na(HL$Valid_Aphia) & HL$Valid_Aphia == esp, ]
      sp_name <- unique(HL_sp$Name)[1]
    } else {
      HL_sp   <- HL[!is.na(HL$Name) & HL$Name == esp, ]
      sp_name <- esp
    }
    if (nrow(HL_sp) == 0)
      stop(paste("No HL records found for species", esp))

    HL_sp$LngtCm <- floor(HL_sp$LngtCm)

    res <- tapply(HL_sp$HLNoAtLngt,
                  HL_sp[, c("LngtCm", "Sex")],
                  sum, na.rm = TRUE)
    res[is.na(res)] <- 0

    message(paste("Species:", sp_name,
                  " | Length range:", min(HL_sp$LngtCm), "-",
                  max(HL_sp$LngtCm), "cm",
                  " | LngtCode:",
                  paste(unique(HL_sp$LngtCode), collapse = "/")))
    return(as.data.frame.matrix(res))
  }

  # ── Caso matriz (legacy-style): especie x sexo ─────────────────────
  if (out == "matrix") {
    res <- tapply(HL$Name, HL[, c("Name", "Sex")], length)
    res[is.na(res)] <- 0
    return(as.data.frame.matrix(res))
  }

  # ── Caso resumen ───────────────────────────────────────────────────
  spp <- split(HL, HL$Name)
  resumen <- do.call(rbind, lapply(names(spp), function(nm) {
    s <- spp[[nm]]
    data.frame(
      Name       = nm,
      Aphia      = unique(s$Valid_Aphia)[1],
      N_records  = nrow(s),
      N_meas     = sum(s$HLNoAtLngt, na.rm = TRUE),
      N_caught   = round(sum(s$TotalNo, na.rm = TRUE)),
      N_hauls    = length(unique(s$HaulNo)),
      Lmin       = if (any(!is.na(s$LngtCm))) min(s$LngtCm, na.rm = TRUE) else NA_real_,
      Lmax       = if (any(!is.na(s$LngtCm))) max(s$LngtCm, na.rm = TRUE) else NA_real_,
      LngtCode   = paste(sort(unique(s$LngtCode)), collapse = "/"),
      M          = sum(s$Sex == "M", na.rm = TRUE),
      F          = sum(s$Sex == "F", na.rm = TRUE),
      U          = sum(s$Sex == "U", na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))

  resumen <- switch(sort_by,
                    n       = resumen[order(-resumen$N_meas, -resumen$N_records), ],
                    name    = resumen[order(resumen$Name), ],
                    records = resumen[order(-resumen$N_records), ],
                    none    = resumen
  )
  rownames(resumen) <- NULL
  resumen
}
