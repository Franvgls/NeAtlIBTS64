#' Descarga y resume los datos HH de todas las campañas NeAtl IBTS de un año
#'
#' @param year Año a consultar
#' @param surveys Named list con survey -> vector de quarters. Si NULL usa la
#'   tabla por defecto del IBTSWG
#' @param only_valid Si TRUE filtra solo HaulVal=="V"
#' @param local_HH data.frame opcional en formato DATRAS HH con datos locales
#'   (e.g. PT-IBTS Q4 no subido aun a DATRAS). Debe tener columnas \code{Survey}
#'   y \code{Quarter}. Si hay filas para un \code{surv+q}, se usan en vez de
#'   descargar de DATRAS y se omite el chequeo de disponibilidad para esa clave.
#' @return Lista con un data.frame por survey y un data.frame resumen global
#' @export
IBTSSurveySummary <- function(year,
                              surveys = NULL,
                              only_valid = FALSE,
                              local_HH   = NULL) {

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

  results <- list()
  summary_rows <- list()

  for (surv in names(surveys)) {
    for (q in surveys[[surv]]) {
      key <- paste0(surv, "_Q", q)
      message("Descargando ", key, "...")

      # Comprobar si hay datos locales para esta clave
      has_local <- !is.null(local_HH) &&
        any(local_HH$Survey == surv & local_HH$Quarter == as.character(q))

      if (has_local) {
        hh <- local_HH[local_HH$Survey == surv &
                         local_HH$Quarter == as.character(q), ]
        message("  -> HH de local_HH (", nrow(hh), " filas), omitiendo DATRAS.")
      } else {
        # Comprobar si existe el año Y el trimestre antes de descargar
        quarters_ok <- tryCatch({
          suppressMessages(suppressWarnings(
            icesDatras::getSurveyYearQuarterList(surv, year)
          ))
        }, error = function(e) NULL)

        if (is.null(quarters_ok) || !(q %in% quarters_ok)) {
          message("  Sin datos en DATRAS: ", key)
          next
        }

        hh <- tryCatch({
          suppressMessages(
            suppressWarnings(
              icesDatras::getHHdata(surv, year, q)
            )
          )
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

  list(data = results, summary = summary_df)
}
