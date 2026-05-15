#' Function SpeciesCAperYear
#' Summary of biological samples from DATRAS CA records for one survey.
#'
#' Default output is a per-species summary including how many records
#' have age readings, length range and age range. Useful to explore which
#' species have biological data and ALK data available in a given
#' survey-year-quarter combination.
#'
#' @param Survey Survey available in DATRAS (see details)
#' @param year   Year(s) to download from DATRAS
#' @param quarter Quarter of the survey
#' @param esp Optional: scientific name or AphiaID to restrict to a single
#'   species. If supplied, output is a length(cm) x sex matrix of numbers
#'   at length (CANoAtLngt) for that species.
#' @param out Output type when \code{esp} is NULL:
#'   \itemize{
#'     \item \code{"summary"} (default): data.frame with one row per species
#'       and columns \code{Name, Aphia, N_total, N_aged, pct_aged, Lmin,
#'       Lmax, Amin, Amax, M, F, U}.
#'     \item \code{"matrix"}: species x sex matrix of CA record counts
#'       (legacy behaviour).
#'   }
#' @param sort_by When \code{out = "summary"}, how to order rows:
#'   \code{"aged"} (descending by N_aged), \code{"name"} alphabetical,
#'   \code{"n"} (descending by N_total), or \code{"none"}.
#' @details Surveys available in DATRAS: EVHOE, FR-CGFS, FR-WCGFS, IE-IAMS,
#'   IE-IGFS, NIGFS, NS-IBTS, PT-IBTS, ROCKALL, SCOROC, SCOWCGFS, SP-ARSA,
#'   SP-NORTH, SP-PORC, SWC-IBTS.
#' @return A data.frame. Format depends on \code{esp} and \code{out}.
#' @examples
#' \dontrun{
#' # Resumen completo, ordenado por especies con más otolitos leídos
#' SpeciesCAperYear("SP-NORTH", 2024, 4)
#'
#' # Solo especies con edades disponibles
#' x <- SpeciesCAperYear("SP-NORTH", 2024, 4)
#' x[x$N_aged > 0, ]
#'
#' # Matriz especie x sexo (comportamiento legacy)
#' SpeciesCAperYear("SP-NORTH", 2024, 4, out = "matrix")
#'
#' # Distribución de tallas para una especie
#' SpeciesCAperYear("SP-NORTH", 2024, 4, esp = "Merluccius merluccius")
#' SpeciesCAperYear("SP-NORTH", 2024, 4, esp = 126484)
#' }
#' @export
SpeciesCAperYear <- function(Survey, year, quarter, esp = NULL,
                             out = c("summary", "matrix"),
                             sort_by = c("aged", "name", "n", "none")) {

  listSurveys <- c("EVHOE","FR-CGFS","FR-WCGFS","IE-IAMS","IE-IGFS","NIGFS",
                   "NS-IBTS","PT-IBTS","ROCKALL","SCOROC","SCOWCGFS",
                   "SP-ARSA","SP-NORTH","SP-PORC","SWC-IBTS")
  if (!Survey %in% listSurveys) stop(paste("Survey", Survey, "does not exist"))
  out     <- match.arg(out)
  sort_by <- match.arg(sort_by)

  CA <- icesDatras::getDATRAS("CA", Survey, year, quarter)
  if (is.null(CA) || nrow(CA) == 0) {
    message("DATRAS no devuelve CA para ", Survey, " ", year, "-Q", quarter)
    return(invisible(NULL))
  }

  # Lookup único de AphiaID -> nombre
  aphia_ids <- unique(stats::na.omit(CA$Valid_Aphia))
  name_lookup <- vapply(aphia_ids, function(id) {
    tryCatch(worrms::wm_id2name(id),
             error = function(e) NA_character_)
  }, character(1))
  names(name_lookup) <- as.character(aphia_ids)
  CA$Name <- name_lookup[as.character(CA$Valid_Aphia)]

  # Talla en cm (común para resumen y caso esp)
  CA$LngtCm <- ifelse(CA$LngtCode %in% c(".", "0", "5"),
                      CA$LngtClass / 10,
                      CA$LngtClass)

  # ── Caso especie concreta: distribución talla x sexo ────────────────
  if (!is.null(esp)) {
    if (is.numeric(esp)) {
      CA_sp   <- CA[!is.na(CA$Valid_Aphia) & CA$Valid_Aphia == esp, ]
      sp_name <- unique(CA_sp$Name)[1]
    } else {
      CA_sp   <- CA[!is.na(CA$Name) & CA$Name == esp, ]
      sp_name <- esp
    }
    if (nrow(CA_sp) == 0)
      stop(paste("No CA records found for species", esp))

    CA_sp$LngtCm <- floor(CA_sp$LngtCm)

    res <- tapply(CA_sp$CANoAtLngt,
                  CA_sp[, c("LngtCm", "Sex")],
                  sum, na.rm = TRUE)
    res[is.na(res)] <- 0

    message(paste("Species:", sp_name,
                  " | Length range:", min(CA_sp$LngtCm), "-",
                  max(CA_sp$LngtCm), "cm",
                  " | LngtCode:",
                  paste(unique(CA_sp$LngtCode), collapse = "/")))
    return(as.data.frame.matrix(res))
  }

  # ── Caso resumen ───────────────────────────────────────────────────
  if (out == "matrix") {
    res <- tapply(CA$Name, CA[, c("Name", "Sex")], length)
    res[is.na(res)] <- 0
    return(as.data.frame.matrix(res))
  }

  # out == "summary"
  spp <- split(CA, CA$Name)
  resumen <- do.call(rbind, lapply(names(spp), function(nm) {
    s    <- spp[[nm]]
    aged <- s[!is.na(s$Age), ]
    data.frame(
      Name     = nm,
      Aphia    = unique(s$Valid_Aphia)[1],
      N_total  = nrow(s),
      N_aged   = nrow(aged),
      pct_aged = round(100 * nrow(aged) / nrow(s), 1),
      Lmin     = if (any(!is.na(s$LngtCm)))    min(s$LngtCm, na.rm = TRUE) else NA_real_,
      Lmax     = if (any(!is.na(s$LngtCm)))    max(s$LngtCm, na.rm = TRUE) else NA_real_,
      Amin     = if (nrow(aged) > 0)           min(aged$Age, na.rm = TRUE) else NA_integer_,
      Amax     = if (nrow(aged) > 0)           max(aged$Age, na.rm = TRUE) else NA_integer_,
      M        = sum(s$Sex == "M", na.rm = TRUE),
      F        = sum(s$Sex == "F", na.rm = TRUE),
      U        = sum(s$Sex == "U", na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))

  resumen <- switch(sort_by,
                    aged = resumen[order(-resumen$N_aged, -resumen$N_total), ],
                    name = resumen[order(resumen$Name), ],
                    n    = resumen[order(-resumen$N_total), ],
                    none = resumen
  )
  rownames(resumen) <- NULL
  resumen
}
