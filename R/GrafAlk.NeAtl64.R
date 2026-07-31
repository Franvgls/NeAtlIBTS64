#' Clave talla-edad gráfica (ALK) para datos DATRAS
#'
#' Barras apiladas que suman 1 por talla, mostrando la composición por
#' edad en cada clase de talla. Acepta dos formatos de entrada:
#' \itemize{
#'   \item \strong{Formato agregado} (salida de \code{\link{GetAlkDTR.NeAtl64}}):
#'     data.frame con columnas \code{talla}, \code{sexo}, \code{E0}, ...,
#'     \code{Eplus+}. Mismo formato que \code{GetAlk.camp64}.
#'   \item \strong{Formato individual} (CA records crudos): data.frame con
#'     columnas \code{LngtClasscm} y \code{Age}, un registro por otolito.
#' }
#'
#' @param ed.tal data.frame. Salida de \code{\link{GetAlkDTR.NeAtl64}}
#'   (formato agregado) o CA records con columnas \code{LngtClasscm}
#'   y \code{Age} (formato individual).
#' @param plus  Edad del grupo terminal. Solo se usa en formato individual;
#'   en formato agregado las columnas ya definen el plus.
#' @param ti    Título: \code{TRUE} construye el título a partir de los
#'   atributos del data.frame; string con el título; \code{FALSE} sin título.
#' @param leg   Lógico: dibujar leyenda de edades en la parte superior.
#' @param n.tal Lógico: añadir el n muestreado encima de cada barra.
#' @param cexleg Tamaño relativo de leyenda y título.
#' @param es    Idioma: \code{TRUE} español, \code{FALSE} inglés.
#' @param cols  Vector de colores (longitud plus+1); si NULL, rainbow.
#' @param out.dat Si \code{TRUE} devuelve invisible \code{list(prop, n)}.
#' @return Invisible: \code{NULL}, o \code{list(prop, n)} si
#'   \code{out.dat = TRUE}.
#' @seealso \code{\link{GetAlkDTR.NeAtl64}}
#' @family ALK DATRAS
#' @export
#' @examples
#' \dontrun{
#' # Formato agregado — anidado directo
#' GrafAlk.NeAtl64(GetAlkDTR.NeAtl64("SP-NORTH", 2022, 4, 127145))
#'
#' # Guardando primero
#' alk <- GetAlkDTR.NeAtl64("SP-NORTH", 2022, 4, Aphia = 127145)
#' GrafAlk.NeAtl64(alk)
#' GrafAlk.NeAtl64(alk, es = FALSE, ti = "L. boscii SP-N 2022 Q4")
#' }
GrafAlk.NeAtl64 <- function(ed.tal, plus = 8,
                             ti = TRUE, leg = TRUE, n.tal = TRUE,
                             cexleg = 1, es = TRUE,
                             cols = NULL, out.dat = FALSE) {

  # ── Detectar formato de entrada ──────────────────────────────────────
  formato_agregado <- "talla" %in% names(ed.tal) &&
                      any(grepl("^E[0-9]", names(ed.tal)))

  if (formato_agregado) {
    # ── Formato agregado: talla, sexo, E0, E1, ..., Eplus+ ────────────
    age_cols <- grep("^E[0-9]", names(ed.tal), value = TRUE)
    plus_det <- length(age_cols) - 1L

    mat <- as.matrix(ed.tal[, age_cols])
    rownames(mat) <- ed.tal$talla
    storage.mode(mat) <- "numeric"

    row_sums <- rowSums(mat, na.rm = TRUE)

    # Determinar si son proporciones o conteos
    es_prop <- all(abs(row_sums[row_sums > 0] - 1) < 0.01)

    if (es_prop) {
      prop   <- mat
      # Recuperar n del atributo guardado por GetAlkDTR.NeAtl64
      n_real <- attr(ed.tal, "n_por_talla")
    } else {
      n_real <- row_sums
      prop   <- mat / ifelse(row_sums > 0, row_sums, 1)
    }

    # Eliminar filas todo-cero
    filas_ok <- row_sums > 0
    prop   <- prop[filas_ok, , drop = FALSE]
    n_real <- if (!is.null(n_real)) n_real[filas_ok] else NULL

    age_labs           <- paste0("E", 0:plus_det)
    age_labs[plus_det + 1] <- paste0("E", plus_det, "+")

  } else {
    # ── Formato individual: LngtClasscm, Age ──────────────────────────
    ed.tal <- ed.tal[!is.na(ed.tal$Age) & !is.na(ed.tal$LngtClasscm), ]
    if (nrow(ed.tal) == 0)
      stop("No hay registros con Age y LngtClasscm.")

    ed.tal$Age <- pmin(ed.tal$Age, plus)

    tab <- tapply(ed.tal$LngtClasscm,
                  list(LngtClasscm = ed.tal$LngtClasscm,
                       Age         = factor(ed.tal$Age, levels = 0:plus)),
                  length, default = 0)
    storage.mode(tab) <- "numeric"

    n_real <- rowSums(tab)
    tab    <- tab[n_real > 0, , drop = FALSE]
    n_real <- n_real[n_real > 0]
    if (nrow(tab) == 0) stop("Todas las tallas están vacías.")

    prop     <- prop.table(tab, margin = 1)
    age_labs <- paste0("E", 0:plus)
    age_labs[plus + 1] <- paste0("E", plus, "+")
  }

  # ── Estética ────────────────────────────────────────────────────────
  if (is.null(cols)) cols <- rainbow(ncol(prop), end = 5/6)

  # Título
  tit <- if (isTRUE(ti)) {
    s <- attr(ed.tal, "survey")
    y <- attr(ed.tal, "year")
    q <- attr(ed.tal, "quarter")
    a <- attr(ed.tal, "Aphia")
    sp_name <- tryCatch(
      aphia_to_name(a),
      error = function(e) NULL
    )
    if (!is.null(sp_name) && length(sp_name) > 0 && !is.na(sp_name) &&
        !is.null(s) && !is.null(y) && !is.null(q))
      sprintf("%s \u2014 %s %d-Q%d", sp_name, s, y, q)
    else if (!is.null(s) && !is.null(y) && !is.null(q))
      sprintf("%s %d-Q%d (Aphia %s)", s, y, q, a)
    else ""
  } else if (is.character(ti)) ti
    else ""

  # ── Plot ─────────────────────────────────────────────────────────────
  top_mar <- if (leg) 6.0 else 3.5
  op <- par(mar = c(4.2, 4.5, top_mar, 1.0), mgp = c(2.6, .7, 0))
  on.exit(par(op), add = TRUE)

  mp <- barplot(t(prop), col = cols, border = "gray30",
                ylim = c(0, 1.08),
                xlab = ifelse(es, "talla (cm)", "length (cm)"),
                ylab = ifelse(es, "proporci\u00f3n", "proportion"),
                main = "", cex.main = cexleg, las = 1, space = 0)
  abline(h = c(.25, .5, .75), col = "gray80", lty = 3)

  if (nchar(tit) > 0) {
    parts <- strsplit(tit, " \u2014 ", fixed = TRUE)[[1]]
    if (length(parts) == 2) {
      title(main = bquote(italic(.(parts[1])) ~ "\u2014" ~ .(parts[2])),
            cex.main = cexleg)
    } else {
      title(main = tit, cex.main = cexleg)
    }
  }

  if (n.tal && !is.null(n_real))
    text(mp, 1.03, labels = n_real,
         cex = 0.7 * cexleg, xpd = NA, col = "gray30")

  if (leg) {
    usr <- par("usr")
    legend(x      = mean(usr[1:2]),
           y      = usr[4] + (usr[4] - usr[3]) * 0.10,
           xjust  = 0.5, yjust = 0.5,
           horiz  = TRUE, bty = "n",
           legend = age_labs, fill = cols, border = "gray30",
           cex    = 0.8 * cexleg, xpd = NA, x.intersp = 0.6)
  }

  invisible(if (out.dat) list(prop = prop, n = n_real) else NULL)
}
