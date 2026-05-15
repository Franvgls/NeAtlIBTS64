#' Clave talla-edad gráfica (ALK) para datos DATRAS
#'
#' Barras apiladas que suman 1 por talla, mostrando la composición por
#' edad en cada clase de talla, usando datos individuales de otolitos
#' descargados de DATRAS. Análoga a \code{grafalk.camp64} de CampR64
#' pero adaptada al formato CA records.
#'
#' @param ed.tal \code{data.frame} con al menos columnas \code{LngtClasscm}
#'   y \code{Age}. Habitualmente la salida de
#'   \code{\link{GetAlkDTR.NeAtl64}}.
#' @param plus Edad del grupo terminal; las edades >= plus se agrupan.
#' @param ti Título: \code{TRUE} construye el título a partir de los
#'   atributos del data.frame (survey, año, quarter, Aphia); un string
#'   con el título; \code{FALSE} sin título.
#' @param leg Lógico: dibujar leyenda de edades en la parte superior.
#' @param n.tal Lógico: añadir el n muestreado encima de cada barra.
#' @param cexleg Tamaño relativo de leyenda y título.
#' @param es Idioma: \code{TRUE} español, \code{FALSE} inglés.
#' @param cols Vector de colores (longitud plus+1); si NULL, rainbow.
#' @param out.dat Si \code{TRUE} devuelve invisible la matriz de
#'   proporciones y los \emph{n} reales por talla.
#' @return Invisible: \code{NULL}, o \code{list(prop, n)} si
#'   \code{out.dat = TRUE}.
#' @seealso \code{\link{GetAlkDTR.NeAtl64}}
#' @family ALK DATRAS
#' @export
#' @examples
#' \dontrun{
#' # Sardina, North Spain Q4 2023
#' ca <- GetAlkDTR.NeAtl64("SP-NORTH", 2023, 4, Aphia = 126421)
#' GrafAlk.NeAtl64(ca)
#' GrafAlk.NeAtl64(ca, ti = "Sardina, SP-N 2023 Q4", es = FALSE)
#'
#' # Sardina con resolución de 0.5 cm
#' ca05 <- GetAlkDTR.NeAtl64("SP-NORTH", 2023, 4,
#'                            Aphia = 126421, by_cm = FALSE)
#' GrafAlk.NeAtl64(ca05)
#' }
GrafAlk.NeAtl64 <- function(ed.tal, plus = 8,
                            ti = TRUE, leg = TRUE, n.tal = TRUE,
                            cexleg = 1, es = TRUE,
                            cols = NULL, out.dat = FALSE) {

  ed.tal <- ed.tal[!is.na(ed.tal$Age) & !is.na(ed.tal$LngtClasscm), ]
  if (nrow(ed.tal) == 0) stop("No hay registros con Age y LngtClasscm.")

  ed.tal$Age <- pmin(ed.tal$Age, plus)

  # ── Tabla y proporciones ────────────────────────────────────────────
  tab <- tapply(ed.tal$LngtClasscm,
                list(LngtClasscm = ed.tal$LngtClasscm,
                     Age         = factor(ed.tal$Age, levels = 0:plus)),
                length, default = 0)
  storage.mode(tab) <- "numeric"

  n_real <- rowSums(tab)
  tab    <- tab[n_real > 0, , drop = FALSE]
  n_real <- n_real[n_real > 0]
  if (nrow(tab) == 0) stop("Todas las tallas están vacías.")

  prop <- prop.table(tab, margin = 1)

  # ── Estética ────────────────────────────────────────────────────────
  if (is.null(cols)) cols <- rainbow(plus + 1, end = 5/6)
  age_labs           <- paste0("E", 0:plus)
  age_labs[plus + 1] <- paste0("E", plus, "+")

  # Título: TRUE → atributos del df; carácter → tal cual; FALSE → sin
  tit <- if (isTRUE(ti)) {
    s <- attr(ed.tal, "survey")
    y <- attr(ed.tal, "year")
    q <- attr(ed.tal, "quarter")
    a <- attr(ed.tal, "Aphia")
    sp_name <- aphia_to_name(a)
    #   if (!is.null(a))
    #   tryCatch(unname(unlist(worrms::wm_id2name_(as.numeric(a)))),
    #            error = function(e) NULL)
    # else NULL
    #
    # if (!is.null(sp_name) && length(sp_name) > 0)
    #   sprintf("%s — %s %d-Q%d", sp_name, s, y, q)
    # else if (!is.null(s) && !is.null(y) && !is.null(q))
    #   sprintf("%s %d-Q%d (Aphia %s)", s, y, q, a)
    # else ""
  } else if (is.character(ti)) ti
  else                       ""

  # ── Plot ────────────────────────────────────────────────────────────
  top_mar <- if (leg) 6.0 else 3.5
  op <- par(mar = c(4.2, 4.5, top_mar, 1.0), mgp = c(2.6, .7, 0))
  on.exit(par(op), add = TRUE)

  mp <- barplot(t(prop), col = cols, border = "gray30",
                ylim = c(0, 1.08),
                xlab = ifelse(es, "talla (cm)",  "length (cm)"),
                ylab = ifelse(es, "proporción", "proportion"),
                main = "", cex.main = cexleg, las = 1, space = 0)
  abline(h = c(.25, .5, .75), col = "gray80", lty = 3)
  if (nchar(tit)) {
    parts <- strsplit(tit, " — ", fixed = TRUE)[[1]]
    if (length(parts) == 2) {
      title(main = bquote(italic(.(parts[1])) ~ "—" ~ .(parts[2])),
            cex.main = cexleg)
    } else {
      title(main = tit, cex.main = cexleg)
    }
  }
  if (n.tal)
    text(mp, 1.03, labels = n_real,
         cex = 0.7 * cexleg, xpd = NA, col = "gray30")

  if (leg) {
    usr <- par("usr")
    legend(x = mean(usr[1:2]),
           y = usr[4] + (usr[4] - usr[3]) * 0.10,
           xjust = 0.5, yjust = 0.5,
           horiz = TRUE, bty = "n",
           legend = age_labs, fill = cols, border = "gray30",
           cex = 0.8 * cexleg, xpd = NA, x.intersp = 0.6)
  }

  invisible(if (out.dat) list(prop = prop, n = n_real) else NULL)
}
