IBTSPositMap <- function(ibts_data, year = NULL,
                         quarters = NULL,
                         IBTSsurvs = NULL,
                         no_NS = FALSE,    # excluye NS-IBTS para mapas de especies
                         add = FALSE,
                         show_invalid = TRUE,  # muestra los I con simbolo distinto
                         cex = 0.8,
                         leg = TRUE,
                         ...) {

  if (is.list(ibts_data) && "data" %in% names(ibts_data))
    hh_list <- ibts_data$data
  else
    stop("ibts_data debe ser el resultado de IBTSSurveySummary()")

  if (is.null(year)) year <- ibts_data$summary$Year[1]

  # Excluir NS-IBTS si no_NS=TRUE
  if (no_NS)
    hh_list <- hh_list[!grepl("^NS-IBTS", names(hh_list))]

  # Filtrar quarters si se especifica
  if (!is.null(quarters)) {
    pat <- paste0("_Q(", paste(quarters, collapse="|"), ")$")
    hh_list <- hh_list[grepl(pat, names(hh_list))]
  }

  # Colores por survey
  extra_cols <- c("NS-IBTS" = "red", "IE-IAMS" = "darkgreen", "NIGFS" = "cyan4")
  if (!is.null(IBTSsurvs)) {
    col_map <- setNames(as.character(IBTSsurvs$color), IBTSsurvs$survey)
    col_map <- c(col_map, extra_cols[!names(extra_cols) %in% names(col_map)])
  } else {
    col_map <- extra_cols
  }

  # Mapa base
  if (!add) {
    if (!no_NS) {xlims=c(-13,14)}
    IBTSNeAtl_map64(load = FALSE, leg = FALSE, ...)
    title(paste("IBTSWG NeAtl", year,
                if (!is.null(quarters)) paste0("Q", paste(quarters, collapse="+"))),
          font.main = 2,line = 2)
  }

  # Pintar lances
  survs_pintados <- character(0)
  cols_pintados  <- character(0)

  for (key in names(hh_list)) {
    hh <- hh_list[[key]]
    if (is.null(hh) || nrow(hh) == 0) next

    surv  <- hh$Survey[1]
    color <- unname(if (surv %in% names(col_map)) col_map[surv] else "gray50")

    # V y A validos, I invalidos
    hh_ok  <- hh[hh$HaulVal %in% c("V", "A"), ]
    hh_inv <- hh[hh$HaulVal == "I", ]

    if (nrow(hh_ok) > 0)
      points(hh_ok$ShootLong, hh_ok$ShootLat,
             pch = 21, bg = color, col = "black", cex = cex)

    if (show_invalid && nrow(hh_inv) > 0)
      points(hh_inv$ShootLong, hh_inv$ShootLat,
             pch = 4, col = color, cex = cex, lwd = 1.5)

    survs_pintados <- c(survs_pintados, surv)
    cols_pintados  <- c(cols_pintados, color)
  }

  # Leyenda
  if (leg) {
    idx <- !duplicated(survs_pintados)
    legend("bottomright",
           legend = survs_pintados[idx],
           pt.bg  = cols_pintados[idx],
           pch = 21, col = "black",
           cex = 0.8, pt.cex = 1.2,
           inset = c(.03, .03), bg = "white")
  }

  invisible(NULL)
}
