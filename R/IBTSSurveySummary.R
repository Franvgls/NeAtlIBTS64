#' Descarga y resume los datos HH de todas las campañas NeAtl IBTS de un año
#'
#' @param year Año a consultar
#' @param surveys Named list con survey -> vector de quarters. Si NULL usa la
#'   tabla por defecto del IBTSWG
#' @param only_valid Si TRUE filtra solo HaulVal=="V"
#' @param local_HH data.frame en formato DATRAS HH con datos locales (ej. PT-IBTS Q4
#'   no subido a DATRAS). Columnas \code{Survey} y \code{Quarter} obligatorias
#'   (\code{Survey} no viene en un export HH crudo: hay que anadirla a mano con
#'   el codigo exacto del survey, ej. \code{local_HH$Survey <- "PT-IBTS"}).
#'   Si faltan columnas, o si contiene datos de un Survey/Quarter que no se usa
#'   en ningun momento, se lanza un \code{warning()} avisando de ello.
#' @param extra_surveys Named list con surveys adicionales o forzados fuera de la
#'   lista por defecto. Mismo formato: \code{list("NS-IBTS" = 1, "BALTIC" = c(1,4))}.
#' @return Lista con un data.frame por survey y un data.frame resumen global
#' @export
IBTSSurveySummary <- function(year,
                              surveys       = NULL,
                              only_valid    = FALSE,
                              local_HH      = NULL,
                              extra_surveys = NULL) {

  # Tabla por defecto: surveys conocidas del NeAtl IBTS y sus quarters
  if (is.null(surveys)) {
    surveys <- list(
      "NS-IBTS"    = c(1, 3),
      "SCOWCGFS"   = c(1, 4),
      "SCOROC"     = 3,
      "NIGFS"      = c(1, 4),
      "IE-IAMS"    = c(1, 2),
      "IE-IGFS"    = 4,
      "FR-WCGFS"   = 3,
      "FR-CGFS"    = 4,
      "EVHOE"      = 4,
      "SP-PORC"    = 3,
      "SP-NORTH"   = 4,
      "PT-IBTS"    = 4,
      "SP-ARSA"    = c(1, 4)
    )
  }

  # Merge extra_surveys
  if (!is.null(extra_surveys)) {
    for (sv in names(extra_surveys)) {
      if (sv %in% names(surveys)) {
        surveys[[sv]] <- unique(c(surveys[[sv]], extra_surveys[[sv]]))
      } else {
        surveys[[sv]] <- extra_surveys[[sv]]
      }
    }
  }

  # --- Validar local_HH y preparar aviso de combinaciones Survey/Quarter no usadas ---
  # (para detectar, ej., que el Survey/Quarter de local_HH no coincide con ningun
  # survey solicitado por un typo, columnas con nombre distinto, etc.)
  local_combos <- character(0)
  local_used   <- character(0)
  if (!is.null(local_HH)) {
    faltan <- setdiff(c("Survey", "Quarter"), names(local_HH))
    if (length(faltan) > 0) {
      warning("local_HH no tiene la(s) columna(s) '", paste(faltan, collapse = "', '"),
              "' necesarias para identificar campana/quarter: sus datos NO se usaran ",
              "y se intentara descargar todo de DATRAS.", call. = FALSE)
      local_HH <- NULL
    } else {
      local_HH$Quarter <- as.character(local_HH$Quarter)
      local_combos <- unique(paste(local_HH$Survey, local_HH$Quarter))
    }
  }

  results <- list()
  summary_rows <- list()

  for (surv in names(surveys)) {
    for (q in surveys[[surv]]) {
      key <- paste0(surv, "_Q", q)
      message("Descargando ", key, "...")

      # Datos locales tienen prioridad sobre DATRAS
      has_local <- !is.null(local_HH) &&
        any(local_HH$Survey == surv & local_HH$Quarter == as.character(q))

      if (has_local) local_used <- c(local_used, paste(surv, as.character(q)))

      if (!has_local) {
        # try() en lugar de tryCatch por bug rbindlist/Doortype en icesDatras 1.4.1
        quarters_ok <- try(suppressMessages(suppressWarnings(
          icesDatras::getSurveyYearQuarterList(surv, year)
        )), silent = TRUE)
        if (inherits(quarters_ok, "try-error")) quarters_ok <- NULL
        if (is.null(quarters_ok) || !(q %in% quarters_ok)) {
          message("  Sin datos en DATRAS: ", key)
          next
        }
      }

      hh <- if (has_local) {
        message("  -> HH de local_HH")
        local_HH[local_HH$Survey == surv & local_HH$Quarter == as.character(q), ]
      } else {
        tryCatch({
          suppressMessages(suppressWarnings(
            icesDatras::getHHdata(surv, year, q)
          ))
        },
        error   = function(e) { message("  Sin datos: ", surv, " Q", q); return(NULL) },
        warning = function(w) { message("  Aviso: ", surv, " Q", q, " - ", conditionMessage(w)); return(NULL) }
        )
      }

      if (is.null(hh) || nrow(hh) == 0) next

      # Fechas de inicio y fin
      hh_ord <- dplyr::arrange(hh, Month, Day)
      n_total <- nrow(hh)
      n_valid <- sum(hh$HaulVal == "V", na.rm = TRUE)
      date_start <- paste0(hh_ord$Month[1], "/", hh_ord$Day[1])
      date_end   <- paste0(hh_ord$Month[n_total], "/", hh_ord$Day[n_total])
      n_days     <- length(unique(paste(hh$Month, hh$Day)))

      results[[key]] <- hh

      summary_rows[[key]] <- data.frame(
        Survey    = surv,
        Quarter   = q,
        Year      = year,
        Start     = date_start,
        End       = date_end,
        Days      = n_days,
        N_hauls   = n_total,
        N_valid   = n_valid,
        N_invalid = n_total - n_valid,
        stringsAsFactors = FALSE
      )

      message("  ", n_valid, " validos / ", n_total, " totales | ",
              date_start, " - ", date_end)
    }
  }

  summary_df <- do.call(rbind, summary_rows)
  rownames(summary_df) <- NULL

  # Avisar de Survey/Quarter presentes en local_HH que no se han usado: indica
  # que el nombre de Survey no coincide con la lista de surveys/extra_surveys,
  # o que ese quarter no estaba entre los solicitados para esa campana.
  no_usados <- setdiff(local_combos, local_used)
  if (length(no_usados) > 0) {
    warning("local_HH contiene datos para ", paste(no_usados, collapse = "; "),
            " (Survey Quarter) que no se han usado: revisa que el nombre de Survey ",
            "coincida exactamente con la lista de 'surveys' y que ese quarter este ",
            "incluido (usa 'extra_surveys' si hace falta anadirlo).", call. = FALSE)
  }

  list(data = results, summary = summary_df)
}
