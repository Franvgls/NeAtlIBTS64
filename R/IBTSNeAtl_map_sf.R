# ==========================================================
#   IBTSNeAtl_map_sf (R 4.4+)
#   - worldHires (mapdata) con fallback a world (maps)
#   - sf para lectura/reproyecci\u00f3n
#   - batimetr\u00eda (100m, bathy_geb) seg\u00fan 'bathy'
#   - ICESdiv opcional (ICESdiv=TRUE/FALSE)
#   - campa\u00f1as todas o ninguna (load=TRUE/FALSE)
#   - ejes como en el original (W/E y N/S, cero incluido)
#   - marco inicial type="n" para evitar errores polygon()
# ==========================================================

IBTSNeAtl_map_sf <- function(
    nl = 60.5, sl = 36.0, xlims = c(-18, 3),
    leg = TRUE, legpos = c("bottomright"), cex.leg = .7, dens = 30,
    load = TRUE, ICESdiv = TRUE, ICESrect = FALSE, ICESlab = FALSE, ICESlabcex = .8,
    NS = FALSE, bathy = TRUE, bw = FALSE, axlab = .8, bords = TRUE, lwdl = .1,
    shpdir = "c:/GitHubRs/shapes/", places = FALSE, minpop = 200000,
    graf = FALSE, xpng = 1200, ypng = 800, ppng = 15,
    IBTSsurvs = NULL,
    use_worldHires = TRUE
) {
  # ------------------------
  # Dependencias
  # ------------------------
  stopifnot(requireNamespace("sf", quietly = TRUE))
  stopifnot(requireNamespace("maps", quietly = TRUE))
  has_mapdata <- requireNamespace("mapdata", quietly = TRUE)  # <- FALTA ESTA
  has_sp <- requireNamespace("sp", quietly = TRUE)            # <- Y ESTA

  if (has_mapdata && use_worldHires) {
    .mapdata_env <- new.env(parent = emptyenv())
    lazyLoad(file.path(system.file("data", package = "mapdata"), "Rdata"),
             envir = .mapdata_env)
    worldHires_db <- .mapdata_env$worldHiresMapEnv
  } else {
    worldHires_db <- "world"
  }

  # ------------------------
  # Utilidades
  # ------------------------
  shpdir <- normalizePath(shpdir, mustWork = FALSE)

  # Lee .gpkg o .shp seg\u00fan exista (basenames fijos)
  read_vec <- function(base) {
    g <- file.path(shpdir, paste0(base, ".gpkg"))
    s <- file.path(shpdir, paste0(base, ".shp"))
    if (file.exists(g)) return(sf::st_read(g, quiet = TRUE))
    if (file.exists(s)) return(sf::st_read(s, quiet = TRUE))
    stop(sprintf("No encuentro %s.[gpkg|shp] en %s", base, shpdir))
  }

  # Tramados con sp si disponible; si no, relleno semi-transparente
  plot_hatched <- function(x, col, lwd = .1, dens = NA, angle = NA) {
    if (has_sp && !is.na(dens) && !is.na(angle)) {
      xs <- methods::as(x, "Spatial")
      sp::plot(xs, add = TRUE, col = col, lwd = lwd,
               density = dens, angle = angle)
    } else {
      graphics::plot(sf::st_geometry(x), add = TRUE,
                     col = grDevices::adjustcolor(col, alpha.f = 0.35),
                     border = col, lwd = lwd)
    }
  }

  # Dimensiones para la l\u00f3gica de ejes
  largo <- if ((sl < 0 && nl < 0) || (sl > 0 && nl > 0)) rev(abs(nl - sl)) * 1 else (nl - sl)
  ancho <- if (xlims[2] < 0) diff(rev(abs(xlims))) * 1 else diff(xlims) * 1

  # ------------------------
  # Capas base
  # ------------------------
  if (ICESdiv) ices_div <- read_vec("ices_div")

  if (bathy) {
    bath100   <- read_vec("100m")
    bathy_geb <- read_vec("bathy_geb")
    if ("DEPTH" %in% names(bathy_geb)) {
      bathy_geb <- bathy_geb[bathy_geb$DEPTH != 100, ]
    }
  }

  # ------------------------
  # Capas campa\u00f1as (todas o ninguna, seg\u00fan load)
  # ------------------------
  if (load) {
    SWC_Q1   <- sf::st_transform(read_vec("SWC_Q1"), 4326)
    SCOROC   <- sf::st_transform(read_vec("SCOROC"), 4326)
    IGFS     <- sf::st_transform(read_vec("IGFS"),   4326)
    NIGFS    <- sf::st_transform(read_vec("NI_IBTS"), 4326)
    CGFS     <- read_vec("CGFS_stratum")
    WCGFS    <- read_vec("CGFS_Western_Channel_stratification-2023")
    Porc     <- sf::st_transform(read_vec("Porcupine"), 4326)
    EVHOE    <- sf::st_transform(read_vec("EVHOE"), 4326)
    Sp_North <- read_vec("Sp_North.WGS84")
    PT_IBTS  <- read_vec("PT_IBTS_2015")
    Sp_Cadiz <- sf::st_transform(read_vec("Sp_Cadiz"), 4326)
  }

  # ------------------------
  # Salida PNG si graf es un nombre
  # ------------------------
  if (!is.logical(graf)) {
    png(filename = paste0(graf, ".png"),
        width = xpng, height = ypng, pointsize = ppng)
    on.exit(dev.off(), add = TRUE)
  }

  # ------------------------
  # Lienzo
  # ------------------------
  par(mar = c(3.5, 2, 2, 2) + 0.1)

  # (1) Marco inicial del mapa (OBLIGATORIO: crea el plot frame)

  maps::map(worldHires_db, xlim = xlims, ylim = c(sl, nl), type = "n")

  # Fondo azul si no es BN
  if (!bw) {
    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4], col = "lightblue1", border = NA)
  }

  # ICES divisions (opcional)
  if (ICESdiv) {
    graphics::plot(sf::st_geometry(ices_div), add = TRUE, col = NA, border = "burlywood")
  }

  # Batimetria
  if (bathy) {
    graphics::plot(sf::st_geometry(bath100), add = TRUE,
                   col = if (bw) gray(.85) else gray(.50), lwd = .1, border = NA)
    graphics::plot(sf::st_geometry(bathy_geb), add = TRUE,
                   col = if (bw) gray(.85) else gray(.50), lwd = .1, border = NA)
  }

  # Cuadr\u00edcula ICES rectangular (opcional)
  if (ICESrect) {
    abline(h = seq(30, 65, by = .5), v = seq(-44, 68, by = 1), col = gray(.8), lwd = .2)
  }

  # ------------------------
  # Etiquetas ICES (opcional)
  # ------------------------
  if (ICESlab && ICESdiv) {
    cent <- suppressWarnings(sf::st_centroid(ices_div))
    xy   <- sf::st_coordinates(cent)
    nm   <- if ("ICESNAME" %in% names(ices_div)) ices_div$ICESNAME else seq_len(nrow(ices_div))
    text(stat_y~stat_x,Area,label = ICESNAME, cex = ICESlabcex, font = 2)
  }


  maps::map(worldHires_db,
            xlim = xlims, ylim = c(sl, nl),
            fill = TRUE, col = if (bw) "gray" else "burlywood3",
            add = TRUE, fg = "blue",
            interior = TRUE, boundary = TRUE,
            lty = 1, lwd = .05)

  # ------------------------
  # Ejes (conservando la lógica original completa)
  # ------------------------

  # Longitudes (W/E)
  if (all(xlims < 0)) {
    degs <- seq(round(xlims[1], 0), round(xlims[2], 0), ifelse(ancho > 10, 4, 1))
    alg  <- sapply(degs, function(x) bquote(.(abs(x)) * degree ~ W))
    axis(1, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -0.01, mgp = c(1, .4, 0))
    axis(3, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -0.01, mgp = c(1, .4, 0))
  }
  if (all(xlims > 0)) {
    degs <- seq(round(xlims[1], 0), round(xlims[2], 0), ifelse(ancho > 10, 4, 1))
    alg  <- sapply(degs, function(x) bquote(.(abs(x)) * degree ~ E))
    axis(1, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -0.01, mgp = c(1, .4, 0))
    axis(3, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -0.01, mgp = c(1, .4, 0))
  }
  if (xlims[1] < 0 && xlims[2] > 0) {
    degs <- seq(round(xlims[1], 0), -1, ifelse(abs(diff(xlims)) > 10, 4, 1))
    alg  <- sapply(degs, function(x) bquote(.(abs(x)) * degree ~ W))
    axis(1, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -0.01, mgp = c(1, .4, 0))
    axis(3, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -0.01, mgp = c(1, .4, 0))
    degs <- seq(2, round(xlims[2], 0), ifelse(abs(diff(xlims)) > 1, 4, 1))
    alg  <- sapply(degs, function(x) bquote(.(abs(x)) * degree ~ E))
    axis(1, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -0.01, mgp = c(1, .4, 0))
    axis(3, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -0.01, mgp = c(1, .4, 0))
    degs <- 0
    alg  <- sapply(degs, function(x) bquote(.(abs(x)) * degree ~ ""))
    axis(1, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -0.01, mgp = c(1, .4, 0))
    axis(3, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -0.01, mgp = c(1, .4, 0))
  }

  # Latitudes (N/S)
  if (all(c(sl, nl) < 0)) {
    degs <- seq(round(sl, 0), round(nl, 0), ifelse(largo > 10, 4, 1))
    alg  <- sapply(degs, function(x) bquote(.(abs(x)) * degree ~ S))
    axis(2, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -0.01, las = 2, mgp = c(1, .5, 0))
    axis(4, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -0.01, las = 2, mgp = c(1, .5, 0))
  }
  if (all(c(sl, nl) > 0)) {
    degs <- seq(round(sl, 0), round(nl, 0), ifelse(largo > 10, 4, 1))
    alg  <- sapply(degs, function(x) bquote(.(abs(x)) * degree ~ N))
    axis(2, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -0.01, las = 2, mgp = c(1, .5, 0))
    axis(4, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -0.01, las = 2, mgp = c(1, .5, 0))
  }
  if (!all(c(sl, nl) > 0)) {
    degs <- seq(round(sl, 0), -5, ifelse(largo > 10, 4, 1))
    alg  <- sapply(degs, function(x) bquote(.(abs(x)) * degree ~ S))
    axis(2, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -0.01, las = 2, mgp = c(1, .5, 0))
    axis(4, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -0.01, las = 2, mgp = c(1, .5, 0))
    degs <- seq(5, nl, ifelse(largo > 10, 4, 1))
    alg  <- sapply(degs, function(x) bquote(.(abs(x)) * degree ~ N))
    axis(2, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -0.01, las = 2, mgp = c(1, .5, 0))
    axis(4, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -0.01, las = 2, mgp = c(1, .5, 0))
    degs <- 0
    alg  <- sapply(degs, function(x) bquote(.(abs(x)) * degree ~ ""))
    axis(2, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -0.01, las = 2, mgp = c(1, .5, 0))
    axis(4, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -0.01, las = 2, mgp = c(1, .5, 0))
  }

  # Rejilla NS opcional
  if (NS) {
    rect(-4, 55.5, 9.5, 60.2, col = "tomato1", border = NA)
    rect(-2, 50.0, 9.5, 60.2, col = "tomato1", border = NA)
    for (lat in seq(55, 66, by = .5)) segments(-4, lat, 12, lat, col = gray(.85), lwd = .01)
    for (lat in seq(49.5, 55, by = .5)) segments(-2, lat, 12, lat, col = gray(.85), lwd = .01)
    for (long in seq(-4, 12, by = 1))  segments(long, 55, long, 65, col = gray(.85), lwd = .01)
    for (long in seq(-2, 12, by = 1))  segments(long, 50, long, 55, col = gray(.85), lwd = .01)
  }

  # Rug marks

  rug(seq(round(sl, 0) + .5, round(nl, 0) + .5, by = 1), .005, side = 2, lwd = lwdl, quiet = TRUE)
  rug(seq(round(xlims[1], 0) + .5, round(xlims[2], 0) + .5, by = 1), .005, side = 1, lwd = lwdl, quiet = TRUE)
  rug(seq(round(xlims[1], 0) + .5, round(xlims[2], 0) + .5, by = 1), .005, side = 3, lwd = lwdl, quiet = TRUE)
  rug(seq(round(sl, 0) + .5, round(nl, 0) + .5, by = 1), .005, side = 4, lwd = lwdl, quiet = TRUE)

  # ------------------------
  # Campanas (tramado)
  # ------------------------
  if (load) {
    plot_hatched(SWC_Q1,    "yellow",     lwd = .01, dens = dens, angle =   0)
    plot_hatched(SCOROC,    "yellow4",    lwd = .01, dens = dens, angle =  32)
    plot_hatched(NIGFS,     "cyan4",      lwd = .01, dens = dens, angle =  64)
    plot_hatched(IGFS,      "green",      lwd = .01, dens = dens, angle =  96)
    plot_hatched(Porc,      "red",        lwd = .01, dens = dens, angle = 128)
    plot_hatched(CGFS,      "navy",       lwd = .10, dens = dens, angle = 160)
    plot_hatched(WCGFS,     "violet",     lwd = .10, dens = dens, angle = 192)
    plot_hatched(EVHOE,     "blue",       lwd = .10, dens = dens, angle = 224)
    plot_hatched(Sp_North,  "orange",     lwd = .10, dens = dens, angle = 256)
    plot_hatched(PT_IBTS,   "lightgreen", lwd = .10, dens = dens, angle = 288)
    plot_hatched(Sp_Cadiz,  "sienna",     lwd = .10, dens = dens, angle = 320)
  }

  # ------------------------
  # Ciudades (opcional)
  # ------------------------
  if (places) {
    points(-6.260278, 53.349722, pch = 18); text(-6.260278, 53.349722, "Dublin",      cex = .5 * cex.leg, pos = 2, font = 2)
    points(-9.1393,   38.7223,   pch = 18); text(-9.1393,   38.7223,   "Lisbon",      cex = .5 * cex.leg, pos = 4, font = 2)
    points( 2.3488,   48.85341,  pch = 18); text( 2.3488,   48.85341,  "Paris",       cex = .5 * cex.leg, pos = 3, font = 2)
    points(-3.713,    40.2085,   pch = 18); text(-3.713,    40.2085,   "Madrid",      cex = .5 * cex.leg, pos = 3, font = 2)
    points( 4.34878,  50.85045,  pch = 18); text( 4.34878,  50.85045,  "Brussels",    cex = .5 * cex.leg, pos = 3, font = 2)
    points(-0.12776,  51.50735,  pch = 18); text(-0.12776,  51.50735,  "London",      cex = .5 * cex.leg, pos = 3, font = 2)
    points(12.56833,  55.67611,  pch = 18); text(12.56833,  55.67611,  "Copenhagen",  cex = .5 * cex.leg, pos = 3)
  }

  box()

  # ------------------------
  # Leyenda (si se proporciona IBTSsurvs)
  # ------------------------
  if (leg && !is.null(IBTSsurvs)) {
    survs <- if (NS)
      c("NS-IBTS","SCOWCGFS","SCOROC","NIGFS","IE-IGFS","SP-PORC",
        "FR-CGFS","FR-WCGFS","EVHOE","SP-NORTH","PT-IBTS","SP-ARSA")
    else
      c("SCOWCGFS","SCOROC","NIGFS","IE-IGFS","SP-PORC",
        "FR-CGFS","FR-WCGFS","EVHOE","SP-NORTH","PT-IBTS","SP-ARSA")

    keep <- IBTSsurvs$survey %in% survs

    legend(legpos,
           legend = IBTSsurvs$survey[keep],
           fill   = IBTSsurvs$color[keep],
           cex = cex.leg, inset = c(.03, .03), bg = "white", text.col = "black",
           density = dens, angle = seq(0, 350, by = 32))
  }

  if (!is.logical(graf)) {
    message(sprintf("Figura generada: %s/%s.png", getwd(), graf))
  }

  invisible(TRUE)
}


