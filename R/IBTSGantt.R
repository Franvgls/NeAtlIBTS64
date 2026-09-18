#' Diagrama de Gantt de las campanas IBTS del Atlantico Nordeste
#'
#' @param summary_df data.frame con el resumen de IBTSSurveySummary(), o la lista completa
#' @param year anio del diagrama. Si NULL se infiere del summary
#' @param IBTSsurvs data.frame con columnas survey y color (dataset del paquete). Si NULL colores por quarter
#' @param cex Factor global de escala de texto. Default 1. Aumentar (ej. 1.4)
#'   para png de alta resolucion o presentaciones.
#' @return invisible: el data.frame ordenado usado para el grafico
#' @export
IBTSGantt <- function(summary_df, year = NULL, IBTSsurvs = NULL, cex = 1) {

  # Aceptar lista completa de IBTSSurveySummary()
  if (is.list(summary_df) && "summary" %in% names(summary_df))
    summary_df <- summary_df$summary

  if (is.null(year)) year <- unique(summary_df$Year)[1]

  # --- Funcion auxiliar: color de texto segun luminancia del fondo ---
  # Devuelve "black" o "white" segun si el fondo es claro u oscuro
  txt_col <- function(bg) {
    rgb <- grDevices::col2rgb(bg) / 255
    lum <- 0.299 * rgb[1,] + 0.587 * rgb[2,] + 0.114 * rgb[3,]
    ifelse(lum > 0.45, "black", "white")
  }

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
    # unname() evita que el resultado herede el nombre del vector y cause problemas
    gdf$bar_color <- unname(ifelse(gdf$Survey %in% names(col_map),
                                   col_map[gdf$Survey], "gray50"))
  } else {
    gdf$bar_color <- unname(col_map[as.character(gdf$Quarter)])
  }

  # Ordenar cronologico de arriba (primero) a abajo (ultimo)
  gdf <- gdf[order(gdf$start_date, decreasing = TRUE), ]
  n   <- nrow(gdf)

  # --- Rangos ---
  # Grafico anual: siempre se muestra el ano completo, tenga o no campanas en enero
  x_min <- as.Date(paste0(year, "-01-01"))
  x_max <- as.Date(paste0(year, "-12-31"))

  meses <- seq.Date(x_min, x_max, by = "month")

  rango_x <- as.numeric(x_max) - as.numeric(x_min)

  # --- Tamanos de letra (todos relativos al cex global) ---
  cex_lbl  <- .85 * cex   # rotulo Y (nombre de campana)
  cex_in   <- .80 * cex   # texto interior de barra (validos/totales)
  cex_date <- .82 * cex   # fechas de inicio/fin
  cex_axis <- .95 * cex   # eje de meses

  # --- Margen izquierdo: medido con el ancho real del rotulo mas largo ---
  # (en pulgadas, via strwidth) en vez de una heuristica basada en nchar().
  # Asi el rectangulo de color del rotulo Y coincide siempre con el margen
  # real reservado por par(), sea cual sea el tamano/resolucion del dispositivo.
  par(mar = c(4, 4, 3, 2))
  plot.new()   # lienzo provisional, solo para poder medir texto con strwidth()
  label_w_in <- max(strwidth(gdf$label, units = "inches", cex = cex_lbl, font = 2))
  mai <- par("mai")
  mai[2] <- label_w_in + 0.12
  par(mai = mai)

  # --- Lienzo definitivo ---
  # xaxs/yaxs = "i" quita el padding automatico del 4%, para que fondos,
  # barras y linea inferior coincidan exactamente con x_min/x_max y el rango Y.
  plot(NULL,
       xlim = c(x_min, x_max), ylim = c(0.5, n + 0.5),
       xaxs = "i", yaxs = "i",
       xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")

  # Borde izquierdo real de la figura (coordenadas de usuario), para que el
  # rotulo Y quede pegado exactamente al margen fijado arriba
  usr <- par("usr"); plt <- par("plt")
  x_scale  <- (usr[2] - usr[1]) / (plt[2] - plt[1])
  rect_izq <- usr[1] - plt[1] * x_scale
  rect_der <- usr[1]

  # Fondo alternado por fila
  for (i in seq_len(n)) {
    if (i %% 2 == 0)
      rect(x_min, i - .45, x_max, i + .45, col = gray(.96), border = NA)
  }

  # Grid vertical en meses
  abline(v = as.numeric(meses), col = gray(.85), lwd = .5)

  # --- Barras, textos y rotulos Y (UN SOLO bucle) ---
  for (i in seq_len(n)) {

    bc <- gdf$bar_color[i]   # color de fondo de esta fila
    tc <- txt_col(bc)        # "black" o "white" segun luminancia

    x0 <- as.numeric(gdf$start_date[i])
    x1 <- as.numeric(gdf$end_date[i])

    # Barra
    rect(x0, i - .35, x1, i + .35, col = bc, border = NA)

    # Texto interior: validos/totales — encogido si no cabe en la barra
    lab_in <- paste0(gdf$N_valid[i], "/", gdf$N_hauls[i])
    mid    <- x0 + (x1 - x0) / 2
    bar_w  <- x1 - x0
    cex_i  <- cex_in
    w_txt  <- strwidth(lab_in, units = "user", cex = cex_i, font = 2)
    if (w_txt > bar_w * 0.9)
      cex_i <- max(cex_i * (bar_w * 0.9) / w_txt, 0.35 * cex)
    text(mid, i, lab_in, col = tc, cex = cex_i, font = 2)

    # Fechas fuera de la barra (omitir inicio si no hay espacio)
    if (x0 - as.numeric(x_min) > rango_x * 0.04)
      text(x0, i, gdf$Start[i], pos = 2, cex = cex_date, font = 2)
    text(x1, i, gdf$End[i], pos = 4, cex = cex_date, font = 2)

    # Rotulo eje Y — rectangulo del color de la campana, texto adaptado
    rect(rect_izq, i - .35, rect_der, i + .35,
         col = bc, border = NA, xpd = TRUE)
    text(rect_izq + (rect_der - rect_izq) * 0.05, i, gdf$label[i],
         adj = c(0, 0.5), cex = cex_lbl, font = 2, col = tc, xpd = TRUE)
  }

  # --- Ejes y titulo ---
  axis(1, at = as.numeric(meses), labels = format(meses, "%b"), cex.axis = cex_axis,
       lwd = 0, lwd.ticks = 0.8)
  title(paste("IBTSWG NeAtl", year), font.main = 2, cex.main = 1.2 * cex)
  # Solo linea horizontal inferior (box bty='l' cortaria las etiquetas Y)
  segments(as.numeric(x_min), 0.5, as.numeric(x_max), 0.5, col = "black", lwd = 1)

  invisible(gdf)
}
