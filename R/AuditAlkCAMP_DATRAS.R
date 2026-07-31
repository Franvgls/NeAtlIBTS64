#' Auditoría de consistencia entre la ALK del CAMP local y DATRAS
#'
#' Compara el número de otolitos por clase de talla entre los datos de
#' edad del CAMP local (via \code{GetAlk.camp64}) y los datos CA de DATRAS,
#' detectando discrepancias y marcando las tallas con datos imputados en
#' el CAMP (flag 99/100).
#'
#' @param gr        Grupo de la especie (CampR64).
#' @param esp       Código de la especie (CampR64).
#' @param camp_code Código de campaña para el CAMP, p.ej. \code{"N22"}.
#' @param zona      Zona del CAMP, p.ej. \code{"cant"}.
#' @param dns       Origen datos CAMP: \code{"local"} o \code{"serv"}.
#' @param survey    Survey de DATRAS, p.ej. \code{"SP-NORTH"}.
#' @param year      Año de DATRAS.
#' @param quarter   Trimestre de DATRAS (1-4).
#' @param Aphia     Valid_Aphia ID de la especie en DATRAS.
#' @param plus      Edad del grupo terminal. Por defecto 8.
#' @param verbose   Si TRUE (default) imprime el resumen en consola.
#' @return Invisible: data.frame con columnas \code{talla}, \code{n_camp},
#'   \code{imputado}, \code{n_dtr}, \code{n_dtr_na}, \code{n_dtr_raw},
#'   \code{dif} (n_camp - n_dtr con edad; NA en tallas imputadas).
#' @seealso \code{\link{GetAlkDTR.NeAtl64}}, \code{\link{GrafAlk.NeAtl64}}
#' @family ALK DATRAS
#' @export
#' @examples
#' \dontrun{
#' audit <- AuditAlkCAMP_DATRAS(
#'   gr=1, esp=42, camp_code="N22", zona="cant", dns="local",
#'   survey="SP-NORTH", year=2022, quarter=4, Aphia=127145
#' )
#'
#' # Solo tallas con discrepancia real
#' audit[!is.na(audit$dif) & audit$dif != 0, ]
#'
#' # Exportar para revisar con SIRENO
#' write.csv(audit, "auditoria_boscii_N22.csv", row.names=FALSE)
#' }
AuditAlkCAMP_DATRAS <- function(gr, esp, camp_code, zona, dns,
                                 survey, year, quarter, Aphia,
                                 plus = 8, verbose = TRUE) {

  # ── 1. ALK del CAMP con conteos ────────────────────────────────────
  camp <- CampR64::GetAlk.camp64(gr, esp, camp_code, zona, dns,
                                  plus = plus, n.ots = TRUE)
  age_cols_c <- grep("^E", names(camp), value = TRUE)
  n_camp_raw <- rowSums(camp[, age_cols_c])

  # Detectar tallas imputadas: celda con 99 o 100, o suma múltiplo de
  # 99/100 (convenio IEO para tallas sin otolito leído)
  imputado <- apply(camp[, age_cols_c], 1, function(r)
    any(r == 99) || any(r == 100) ||
    (sum(r) %% 99 == 0 && sum(r) >= 99) ||
    (sum(r) %% 100 == 0 && sum(r) >= 100 && sum(r) != 0)
  )

  # n real del CAMP: NA en filas imputadas para no confundir
  n_camp <- ifelse(imputado, NA_real_, as.numeric(n_camp_raw))

  # ── 2. CA raw de DATRAS ────────────────────────────────────────────
  ca_raw <- icesDatras::getCAdata(survey, year, quarter)
  if (is.null(ca_raw) || nrow(ca_raw) == 0)
    stop(sprintf("DATRAS no devuelve CA para %s %d-Q%d",
                 survey, year, quarter))

  ca_sp <- ca_raw[!is.na(ca_raw$Valid_Aphia) &
                  ca_raw$Valid_Aphia == Aphia, , drop = FALSE]
  if (nrow(ca_sp) == 0)
    stop(sprintf("Sin registros CA para Aphia=%s en %s %d-Q%d",
                 Aphia, survey, year, quarter))

  ca_sp$LngtClasscm <- trunc(ca_sp$LngtClass / 10)

  n_dtr_raw  <- tapply(ca_sp$LngtClasscm,
                       ca_sp$LngtClasscm, length)
  n_dtr_aged <- tapply(!is.na(ca_sp$Age),
                       ca_sp$LngtClasscm, sum)
  n_dtr_na   <- tapply(is.na(ca_sp$Age),
                       ca_sp$LngtClasscm, sum)

  # ── 3. Merge CAMP + DATRAS ─────────────────────────────────────────
  res <- merge(
    data.frame(talla    = camp$talla,
               n_camp   = n_camp,
               imputado = imputado),
    data.frame(talla     = as.numeric(names(n_dtr_raw)),
               n_dtr     = as.numeric(n_dtr_aged),
               n_dtr_na  = as.numeric(n_dtr_na),
               n_dtr_raw = as.numeric(n_dtr_raw)),
    by = "talla", all = TRUE
  )

  # Ordenar por talla
  res <- res[order(res$talla), ]
  rownames(res) <- NULL

  # ── 4. Rellenar NAs estructurales ──────────────────────────────────
  # Tallas que solo están en DATRAS (no en CAMP): imputado=FALSE, n_camp=0
  res$imputado[is.na(res$imputado)]                    <- FALSE
  res$n_camp[is.na(res$n_camp) & !res$imputado]        <- 0
  res$n_dtr[is.na(res$n_dtr)]                          <- 0
  res$n_dtr_na[is.na(res$n_dtr_na)]                    <- 0
  res$n_dtr_raw[is.na(res$n_dtr_raw)]                  <- 0

  # Recalcular dif con NAs rellenados (NA solo en imputadas)
  res$dif <- ifelse(res$imputado, NA_real_, res$n_camp - res$n_dtr)

  # ── 5. Resumen ──────────────────────────────────────────────────────
  if (verbose) {
    cat(sprintf(
      "\n=== Auditoría ALK: %s %d-Q%d (Aphia %s) vs CAMP %s ===\n",
      survey, year, quarter, Aphia, camp_code))
    cat(sprintf("Total otolitos CAMP (no imputados): %d\n",
                sum(res$n_camp, na.rm = TRUE)))
    cat(sprintf("Tallas imputadas en CAMP:           %d\n",
                sum(res$imputado, na.rm = TRUE)))
    cat(sprintf("Total registros CA DATRAS:          %d\n",
                sum(res$n_dtr_raw, na.rm = TRUE)))
    cat(sprintf("  con edad leída:                   %d\n",
                sum(res$n_dtr, na.rm = TRUE)))
    cat(sprintf("  sin edad (Age=NA):                %d\n",
                sum(res$n_dtr_na, na.rm = TRUE)))
    cat(sprintf("Diferencia neta CAMP-DATRAS:        %+d\n",
                sum(res$dif, na.rm = TRUE)))

    disc <- res[!is.na(res$dif) & res$dif != 0, ]
    if (nrow(disc) > 0) {
      cat(sprintf("Tallas con discrepancia (%d):\n", nrow(disc)))
      cat(sprintf("  CAMP > DATRAS: %d tallas\n", sum(disc$dif > 0)))
      cat(sprintf("  DATRAS > CAMP: %d tallas\n", sum(disc$dif < 0)))
      cat("\n")
      print(disc[, c("talla","n_camp","n_dtr","n_dtr_na","dif")])
    } else {
      cat("Sin discrepancias — CAMP y DATRAS coinciden exactamente.\n")
    }

    imp <- res[!is.na(res$imputado) & res$imputado, ]
    if (nrow(imp) > 0) {
      cat(sprintf(
        "\nTallas imputadas en CAMP (excluidas de la comparación):\n"))
      print(imp[, c("talla","n_dtr","n_dtr_na")])
    }

    only_dtr <- res[!res$imputado & res$n_camp == 0 & res$n_dtr_raw > 0, ]
    if (nrow(only_dtr) > 0) {
      cat(sprintf(
        "\nTallas solo en DATRAS (no en CAMP):\n"))
      print(only_dtr[, c("talla","n_dtr","n_dtr_na")])
    }
    cat("\n")
  }

  invisible(res)
}
