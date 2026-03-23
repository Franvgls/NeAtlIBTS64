#' Diagrama de Gantt de las campanas IBTS del Atlantico Nordeste
#'
#' @param summary_df data.frame con el resumen de IBTSSurveySummary(), o la lista completa
#' @param year anio del diagrama. Si NULL se infiere del summary
#' @param IBTSsurvs data.frame con columnas survey y color (dataset del paquete). Si NULL colores por quarter
#' @return invisible: el data.frame ordenado usado para el grafico
#' @export
IBTSGantt <- function(summary_df, year = NULL, IBTSsurvs = NULL) {

  # Aceptar lista completa de IBTSSurveySummary()
  if (is.list(summary_df) && "summary" %in% names(summary_df))
    summary_df <- summary_df$summary

  if (is.null(year)) year <- unique(summary_df$Year)[1]

  # --- Tabla de colores ---
  extra_cols <- c("NS-IBTS" = "red", "IE-IAMS" = "darkgreen", "NIGFS" = "cyan4")
  if (!is.null(IBTSsurvs)) {
    col_map <- setNames(as.character(IBTSsurvs$color), IBTSsurvs$survey)
    col_map <- c(col_map, extra_cols[!names(extra_cols) %in% names(col_map)])
  } else {
    col_map <- c("1" = "steelblue", "2" = "skyblue", "3" = "tomato", "4" = "salmon")
  }

  # --- Preparar datos ---
  gdf <- summary_df
  gdf$start_date <- as.Date(paste0(year, "/", gdf$Start), format = "%Y/%m/%d")
  gdf$end_date   <- as.Date(paste0(year, "/", gdf$End),   format = "%Y/%m/%d")
  gdf$label      <- paste0(gdf$Survey, " Q", gdf$Quarter)

  if (!is.null(IBTSsurvs)) {
    gdf$bar_color <- ifelse(gdf$Survey %in% names(col_map),
                            col_map[gdf$Survey], "gray50")
  } else {
    gdf$bar_color <- col_map[as.character(gdf$Quarter)]
  }

  # Ordenar cronologico de arriba (primero) a abajo (ultimo)
  gdf <- gdf[order(gdf$start_date, decreasing = TRUE), ]
  n   <- nrow(gdf)

  # --- Rangos ---
  x_min <- min(gdf$start_date) - 5
  x_max <- max(gdf$end_date)   + 5

  meses <- seq.Date(as.Date(paste0(year, "-01-01")),
                    as.Date(paste0(year, "-12-31")), by = "month")
  meses <- meses[meses >= x_min & meses <= x_max]

  # --- Lienzo ---
  par(mar = c(4, 10, 3, 2))
  plot(NULL,
       xlim = c(x_min, x_max), ylim = c(0.5, n + 0.5),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")

  # Fondo alternado por fila
  for (i in seq_len(n)) {
    if (i %% 2 == 0)
      rect(x_min, i - .45, x_max, i + .45, col = gray(.96), border = NA)
  }

  # Grid vertical en meses
  abline(v = as.numeric(meses), col = gray(.85), lwd = .5)

  # --- Barras, textos y rotulos Y ---
  lbl_w <- 12  # ancho del rectangulo de rotulo en dias — ajustar si hace falta

  # Justo antes del bucle, calcular lbl_w dinamicamente
  rango_x <- as.numeric(x_max) - as.numeric(x_min)
  lbl_w   <- rango_x * 0.13   # mas ancho
  lbl_sep  <- rango_x * 0.01  # separacion entre rotulo y borde del plot

  for (i in seq_len(n)) {
    for (i in seq_len(n)) {

      # Barra
      rect(as.numeric(gdf$start_date[i]), i - .35,
           as.numeric(gdf$end_date[i]),   i + .35,
           col = gdf$bar_color[i], border = NA)

      # Texto interior: validos/totales
      mid <- as.numeric(gdf$start_date[i]) +
        (as.numeric(gdf$end_date[i]) - as.numeric(gdf$start_date[i])) / 2
      text(mid, i, paste0(gdf$N_valid[i], "/", gdf$N_hauls[i]),
           col = "white", cex = .7, font = 2)

      # Fechas fuera de la barra
      text(as.numeric(gdf$start_date[i]), i, gdf$Start[i], pos = 2, cex = .65, font = 2)
      text(as.numeric(gdf$end_date[i]),   i, gdf$End[i],   pos = 4, cex = .65, font = 2)

      # Rotulo eje Y — UN SOLO text(), justificado a la izquierda
      rect_izq <- as.numeric(x_min) - lbl_w - lbl_sep
      rect_der <- as.numeric(x_min) - lbl_sep
      rect(rect_izq, i - .35, rect_der, i + .35,
           col = gdf$bar_color[i], border = NA, xpd = TRUE)
      text(rect_izq + rango_x * 0.005, i, gdf$label[i],
           adj = c(0, 0.5), cex = .75, font = 2, col = "white", xpd = TRUE)
      }
    }

  # --- Ejes y titulo ---
  axis(1, at = as.numeric(meses), labels = format(meses, "%b"), cex.axis = .8)
  title(paste("IBTSWG NeAtl", year), font.main = 2, cex.main = 1.1)
  box(bty = "l")

  invisible(gdf)
}
