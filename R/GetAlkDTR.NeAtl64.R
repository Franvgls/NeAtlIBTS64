#' Construye la clave talla-edad desde DATRAS
#'
#' Descarga los datos CA (Age) de DATRAS para una especie, survey,
#' año y trimestre concretos, y los agrega en una tabla talla x edad
#' equivalente a la que devuelve \code{GetAlk.camp64} de CampR64.
#' Una fila por clase de talla, columnas E0..Eplus+.
#'
#' @param survey  Survey de DATRAS, p.ej. "SP-NORTH", "EVHOE", "IE-IGFS".
#' @param year    Año.
#' @param quarter Trimestre (1-4).
#' @param Aphia   Valid_Aphia ID de la especie.
#' @param by_cm   Lógico. Si TRUE (default) agrupa por cm enteros
#'   (\code{trunc(LngtClass/10)}). Si FALSE mantiene 0.5 cm.
#' @param plus    Edad del grupo terminal. Por defecto 8.
#' @param n.ots   Lógico. Si FALSE (default) devuelve proporciones por fila
#'   (ALK). Si TRUE devuelve conteos de otolitos.
#' @return \code{data.frame} con columnas \code{talla}, \code{sexo},
#'   \code{E0}, ..., \code{Eplus+}. Mismo formato que \code{GetAlk.camp64}.
#'   Incluye atributos \code{survey}, \code{year}, \code{quarter},
#'   \code{Aphia}, \code{by_cm} y \code{n_por_talla} (conteos, disponibles
#'   incluso cuando \code{n.ots = FALSE}, para mostrar el n en los gráficos).
#' @seealso \code{\link{GrafAlk.NeAtl64}}, \code{\link{SpeciesCAperYear}},
#'   \code{\link{AuditAlkCAMP_DATRAS}}
#' @family ALK DATRAS
#' @export
#' @examples
#' \dontrun{
#' # ALK como proporciones (para graficar)
#' alk <- GetAlkDTR.NeAtl64("SP-NORTH", 2022, 4, Aphia = 127145)
#' GrafAlk.NeAtl64(alk)
#'
#' # Anidado directo — funciona gracias al atributo n_por_talla
#' GrafAlk.NeAtl64(GetAlkDTR.NeAtl64("SP-NORTH", 2022, 4, 127145))
#'
#' # Conteos para comparar con GetAlk.camp64
#' alk_n <- GetAlkDTR.NeAtl64("SP-NORTH", 2022, 4, 127145, n.ots = TRUE)
#' }
GetAlkDTR.NeAtl64 <- function(survey, year, quarter, Aphia,
                               by_cm = TRUE, plus = 8,
                               n.ots = FALSE) {

  # ── 1. Descarga CA desde DATRAS ────────────────────────────────────
  ca <- icesDatras::getCAdata(survey, year, quarter)
  if (is.null(ca) || nrow(ca) == 0)
    stop(sprintf("DATRAS no devuelve CA para %s %d-Q%d",
                 survey, year, quarter))

  # ── 2. Filtrar especie ─────────────────────────────────────────────
  ca <- ca[!is.na(ca$Valid_Aphia) & ca$Valid_Aphia == Aphia, ,
           drop = FALSE]
  if (nrow(ca) == 0)
    stop(sprintf("Sin datos CA para Aphia=%s en %s %d-Q%d",
                 Aphia, survey, year, quarter))

  # ── 3. Talla en cm ────────────────────────────────────────────────
  ca$LngtClasscm <- if (by_cm) trunc(ca$LngtClass / 10)
                    else        ca$LngtClass / 10

  # ── 4. Filtrar registros sin edad leída ───────────────────────────
  n_total <- nrow(ca)
  ca <- ca[!is.na(ca$Age), , drop = FALSE]
  n_aged <- nrow(ca)
  if (n_aged == 0)
    stop(sprintf(
      "Todos los registros CA tienen Age = NA para Aphia=%s en %s %d-Q%d",
      Aphia, survey, year, quarter))
  if (n_aged < n_total)
    message(sprintf("  %d de %d registros CA tienen Age = NA y se excluyen",
                    n_total - n_aged, n_total))

  # ── 5. Agrupar edades > plus en clase terminal ────────────────────
  ca$AgeGr <- factor(pmin(ca$Age, plus), levels = 0:plus)

  # ── 6. Tabla talla x edad (conteos de otolitos) ───────────────────
  tab <- tapply(ca$LngtClasscm,
                list(LngtClasscm = ca$LngtClasscm,
                     Age         = ca$AgeGr),
                length, default = 0L)
  storage.mode(tab) <- "numeric"

  # Eliminar tallas sin ningún otolito
  n_por_talla <- rowSums(tab)
  tab <- tab[n_por_talla > 0, , drop = FALSE]
  n_por_talla <- n_por_talla[n_por_talla > 0]

  if (nrow(tab) == 0)
    stop("La tabla talla-edad quedó vacía tras filtrar.")

  # ── 7. Construir data.frame con mismo formato que GetAlk.camp64 ───
  age_cols <- c(paste0("E", 0:(plus - 1)), paste0("E", plus, "+"))

  df <- data.frame(
    talla = as.numeric(rownames(tab)),
    sexo  = 3L,
    as.data.frame(tab, stringsAsFactors = FALSE),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  names(df)[3:ncol(df)] <- age_cols

  # ── 8. Guardar n por talla ANTES de calcular proporciones ─────────
  # Esto permite que GrafAlk.NeAtl64 muestre el n encima de las barras
  # incluso cuando se llama de forma anidada con n.ots = FALSE
  attr(df, "n_por_talla") <- as.numeric(n_por_talla)

  # ── 9. Proporciones o conteos ──────────────────────────────────────
  if (!n.ots) {
    totales <- n_por_talla
    totales[totales == 0] <- 1
    df[, age_cols] <- df[, age_cols] / totales
  }

  # ── 10. Atributos de origen ────────────────────────────────────────
  attr(df, "survey")  <- survey
  attr(df, "year")    <- year
  attr(df, "quarter") <- quarter
  attr(df, "Aphia")   <- Aphia
  attr(df, "by_cm")   <- by_cm

  df
}
