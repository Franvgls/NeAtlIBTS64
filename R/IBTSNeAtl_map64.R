#' Function IBTSNeAtl_map plots the map with all the NeAtl IBTS surveys
#'
#' @description
#' Produces a map from the shapefiles that define the IBTSNeAtl Surveys from
#' Scotland to Cadiz. Requires shapefiles in shpdir.
#'
#' @details
#' The default limits cover the full NeAtl IBTS area. The aspect ratio of the
#' plot window is calculated automatically from xlims and ylims using the
#' cosine correction at the mean latitude, so the map is always geographically
#' correct regardless of the limits used.
#'
#' @param nl northernmost limit of the map (default 60.5)
#' @param sl southernmost limit of the map (default 36.0)
#' @param xlims longitude limits as c(west, east) (default c(-18, 3))
#' @param leg if TRUE includes the legend (default TRUE)
#' @param legpos position of the legend (default "bottomright")
#' @param cex.leg size of the legend text (default 0.9)
#' @param dens density of shading lines for the survey polygons (default 30)
#' @param load if TRUE loads and plots the survey shapefiles (default TRUE)
#' @param ICESdiv if TRUE plots the ICES divisions (default TRUE)
#' @param ICESrect if TRUE plots ICES statistical rectangle grid (default FALSE)
#' @param ICESlab if TRUE plots ICES rectangle labels (default FALSE)
#' @param ICESlabcex size of ICES rectangle labels in cex (default 0.8)
#' @param NS if TRUE adds the North Sea ICES rectangle grid (default FALSE)
#' @param bathy if TRUE plots the isobaths (default TRUE)
#' @param bathy_col colour for isobath lines (default "gray55")
#' @param bathy_lwd line width for isobath lines (default 0.4)
#' @param bw if TRUE land in grey, if FALSE in burlywood3 (default FALSE)
#' @param bords if TRUE plots country borders (default TRUE)
#' @param axlab size of axis labels (default 0.8)
#' @param lwdl line width for shading (default 0.1)
#' @param shpdir path to the folder with the shapefiles
#' @param places if TRUE adds main cities (default FALSE)
#' @param minpop minimum population for cities to plot (default 200000)
#' @param newdev if TRUE opens a new graphics device with correct aspect ratio
#' @param graf if FALSE plots to screen; if a filename string, saves as PNG
#' @param xpng width of PNG in pixels (default 1200)
#' @param ypng height of PNG in pixels (default 800)
#' @param ppng pointsize for PNG (default 15)
#' @examples
#' \dontrun{
#' IBTSNeAtl_map(dens = 0, leg = TRUE, load = TRUE)
#' IBTSNeAtl_map(dens = 0, load = FALSE, ICESrect = TRUE)
#' }
#' @export
IBTSNeAtl_map64 <- function(nl = 60.5, sl = 36.0, xlims = c(-18, 3),
                          leg = TRUE, legpos = "bottomright", cex.leg = 0.9,
                          dens = 30, load = TRUE,
                          ICESdiv = TRUE, ICESrect = FALSE,
                          ICESlab = FALSE, ICESlabcex = 0.8,
                          NS = FALSE, bathy = TRUE, bathy_col = "gray55", bathy_lwd = 0.4,
                          bw = FALSE,
                          bords = TRUE, axlab = 0.8, lwdl = 0.1,
                          shpdir = system.file("shapes", package = "NeAtlIBTS64"),
                          places = FALSE, minpop = 200000,
                          newdev = TRUE,
                          graf = FALSE, xpng = 1200, ypng = 800, ppng = 15) {

  # --- Dependencias ---
  stopifnot(requireNamespace("sf",      quietly = TRUE))
  stopifnot(requireNamespace("maps",    quietly = TRUE))
  stopifnot(requireNamespace("sp",      quietly = TRUE))
  has_mapdata <- requireNamespace("mapdata", quietly = TRUE)

  # worldHires via data() en entorno local
  # worldHires: asignar temporalmente al globalenv donde maps::map() lo encuentra
  if (has_mapdata) {
    .e <- new.env(parent = emptyenv())
    utils::data("worldHiresMapEnv", package = "mapdata", envir = .e)
    assign("worldHiresMapEnv", .e$worldHiresMapEnv, envir = globalenv())
    on.exit(suppressWarnings(rm("worldHiresMapEnv", envir = globalenv())),
            add = TRUE)
    world_db <- "worldHires"
  } else {
    warning("mapdata no disponible, usando maps::world")
    world_db <- "world"
  }

  shpdir <- normalizePath(shpdir, mustWork = FALSE)

  # --- Funcion interna de lectura ---
  read_shp <- function(base, layer = NULL) {
    # Intenta .shp, .gpkg y .dbf en ese orden
    for (ext in c(".shp", ".gpkg", ".dbf")) {
      f <- file.path(shpdir, paste0(base, ext))
      if (file.exists(f)) {
        sf_obj <- sf::st_read(f, quiet = TRUE)
        sf_obj <- sf_obj[!sf::st_is_empty(sf_obj), ]
        if (is.na(sf::st_crs(sf_obj))) sf::st_crs(sf_obj) <- 4326
        sf_obj <- tryCatch(sf::st_transform(sf_obj, 4326), error = function(e) sf_obj)
        return(sf_obj)
      }
    }
    stop(sprintf("No se encuentra %s.[shp|gpkg|dbf] en %s", base, shpdir))
  }

  # Convierte sf a Spatial para sp::plot (tramado density/angle)
  to_sp <- function(x) methods::as(x, "Spatial")

  # --- Calculo de dimensiones ---
  largo <- if ((sl < 0 && nl < 0) || (sl > 0 && nl > 0)) abs(nl - sl) else nl - sl
  ancho <- if (xlims[2] < 0) diff(rev(abs(xlims))) else diff(xlims)

  # Aspect ratio geografico: correccion coseno en latitud media
  lat_med  <- mean(c(sl, nl))
  asp_geo  <- largo / (ancho * cos(lat_med * pi / 180))

  # --- Apertura de dispositivo grafico ---
  if (!is.logical(graf)) {
    # Guardar en PNG con dimensiones calculadas para asp correcto
    h_calc <- round(xpng * asp_geo)
    png(filename = paste0(graf, ".png"),
        width = xpng, height = h_calc, pointsize = ppng)
    on.exit(dev.off(), add = TRUE)
  } else if (newdev) {
    # Nueva ventana con asp correcto
    h_win <- 600
    w_win <- round(h_win / asp_geo)
    grDevices::dev.new(width = w_win / 72, height = h_win / 72,
                       noRStudioGD = TRUE)
  }

  # --- Lectura de capas base ---
  ices.div_sp  <- to_sp(read_shp("ices_div"))
  bath100_sp   <- to_sp(read_shp("100m"))
  bathy_geb_sf <- read_shp("bathy_geb")
  if ("DEPTH" %in% names(bathy_geb_sf))
    bathy_geb_sf <- bathy_geb_sf[bathy_geb_sf$DEPTH != 100, ]
  bathy_geb_sp <- to_sp(bathy_geb_sf)

  # --- Lectura de campanas si load=TRUE ---
  if (load) {
    SWC_Q1_sp  <- to_sp(read_shp("SWC_Q1"))
    SCOROC_sp  <- to_sp(read_shp("SCOROC"))
    NIGFS_sp   <- to_sp(read_shp("NI_IBTS"))
    IGFS_sp    <- to_sp(read_shp("IGFS"))
    Porc_sp    <- to_sp(read_shp("Porcupine"))
    CGFS_sp    <- to_sp(read_shp("CGFS_stratum"))
    WCGFS_sp   <- to_sp(read_shp("CGFS_Western_Channel_stratification-2023"))
    EVHOE_sp   <- to_sp(read_shp("EVHOE"))
    SpNorth_sp <- to_sp(read_shp("Sp_North.WGS84"))
    PTIBTS_sp  <- to_sp(read_shp("PT_IBTS_2015"))
    SpCadiz_sp <- to_sp(read_shp("Sp_Cadiz"))
  }

  # --- Lienzo ---
  par(mar = c(3.5, 3.5, 2, 3.5) + 0.1)
  maps::map(world_db, xlim = xlims, ylim = c(sl, nl), type = "n")

  # Fondo mar
  usr <- par("usr")
  if (!bw) rect(usr[1], usr[3], usr[2], usr[4], col = "lightblue1", border = NA)
  clip(usr[1], usr[2], usr[3], usr[4])

  if (bathy) {
    bc <- if (bw) gray(.85) else bathy_col
    sp::plot(bath100_sp[1],   add = TRUE, col = bc, lwd = bathy_lwd)
    sp::plot(bathy_geb_sp[1], add = TRUE, col = bc, lwd = bathy_lwd)
  }

  # Divisiones ICES
  if (ICESdiv) sp::plot(ices.div_sp[1], add = TRUE, col = NA, border = "burlywood")

  # Cuadricula ICES rectangular
  if (ICESrect)
    abline(h = seq(30, 65, by = .5), v = seq(-44, 68, by = 1),
           col = gray(.8), lwd = .2)

  # Cuadricula NS
  if (NS) {
    rect(-4, 55.5, 9.5, 60.2, col = "tomato1", border = NA)
    rect(-2, 50.0, 9.5, 60.2, col = "tomato1", border = NA)
    for (lat in seq(55, 66, by = .5)) segments(-4, lat, 12, lat, col = gray(.85), lwd = .01)
    for (lat in seq(49.5, 55, by = .5)) segments(-2, lat, 12, lat, col = gray(.85), lwd = .01)
    for (lon in seq(-4, 12, by = 1))  segments(lon, 55, lon, 65, col = gray(.85), lwd = .01)
    for (lon in seq(-2, 12, by = 1))  segments(lon, 50, lon, 55, col = gray(.85), lwd = .01)
  }

  # --- Ejes W/E ---
  if (all(xlims < 0)) {
    degs <- seq(round(xlims[1], 0), round(xlims[2], 0), ifelse(ancho > 10, 4, 1))
    alg  <- sapply(degs, function(x) bquote(.(abs(x)) * degree ~ W))
    axis(1, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -.01, mgp = c(1, .4, 0))
    axis(3, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -.01, mgp = c(1, .4, 0))
  }
  if (xlims[1] < 0 && xlims[2] > 0) {
    degs <- seq(round(xlims[1], 0), -1, ifelse(abs(diff(xlims)) > 10, 4, 1))
    alg  <- sapply(degs, function(x) bquote(.(abs(x)) * degree ~ W))
    axis(1, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -.01, mgp = c(1, .4, 0))
    axis(3, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -.01, mgp = c(1, .4, 0))
    degs <- seq(2, round(xlims[2], 0), ifelse(abs(diff(xlims)) > 10, 4, 1))
    alg  <- sapply(degs, function(x) bquote(.(abs(x)) * degree ~ E))
    axis(1, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -.01, mgp = c(1, .4, 0))
    axis(3, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -.01, mgp = c(1, .4, 0))
    alg0 <- sapply(0, function(x) bquote(.(abs(x)) * degree ~ ""))
    axis(1, at = 0, lab = do.call(expression, alg0), font.axis = 2, cex.axis = axlab, tck = -.01, mgp = c(1, .4, 0))
    axis(3, at = 0, lab = do.call(expression, alg0), font.axis = 2, cex.axis = axlab, tck = -.01, mgp = c(1, .4, 0))
  }
  if (all(xlims > 0)) {
    degs <- seq(round(xlims[1], 0), round(xlims[2], 0), ifelse(ancho > 10, 4, 1))
    alg  <- sapply(degs, function(x) bquote(.(abs(x)) * degree ~ E))
    axis(1, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -.01, mgp = c(1, .4, 0))
    axis(3, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, tck = -.01, mgp = c(1, .4, 0))
  }

  # --- Ejes N/S ---
  if (all(c(sl, nl) > 0)) {
    degs <- seq(round(sl, 0), round(nl, 0), ifelse(largo > 10, 4, 1))
    alg  <- sapply(degs, function(x) bquote(.(abs(x)) * degree ~ N))
    axis(2, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, las = 2, tck = -.01, mgp = c(1, .5, 0))
    axis(4, at = degs, lab = do.call(expression, alg), font.axis = 2, cex.axis = axlab, las = 2, tck = -.01, mgp = c(1, .5, 0))
  }

  # Rug marks de medios grados
  rug(seq(round(sl, 0) + .5, round(nl, 0) + .5, by = 1), .005, side = 2, lwd = lwdl, quiet = TRUE)
  rug(seq(round(sl, 0) + .5, round(nl, 0) + .5, by = 1), .005, side = 4, lwd = lwdl, quiet = TRUE)
  rug(seq(round(xlims[1], 0) + .5, round(xlims[2], 0) + .5, by = 1), .005, side = 1, lwd = lwdl, quiet = TRUE)
  rug(seq(round(xlims[1], 0) + .5, round(xlims[2], 0) + .5, by = 1), .005, side = 3, lwd = lwdl, quiet = TRUE)

  # --- Campanas (tramado) ---
  if (load) {
    sp::plot(SWC_Q1_sp,  add = TRUE, col = "yellow",     lwd = lwdl, density = dens, angle =   0)
    sp::plot(SCOROC_sp,  add = TRUE, col = "yellow4",    lwd = lwdl, density = dens, angle =  32)
    sp::plot(NIGFS_sp,   add = TRUE, col = "cyan4",      lwd = lwdl, density = dens, angle =  64)
    sp::plot(IGFS_sp,    add = TRUE, col = "green",      lwd = lwdl, density = dens, angle =  96)
    sp::plot(Porc_sp,    add = TRUE, col = "red",        lwd = lwdl, density = dens, angle = 128)
    sp::plot(CGFS_sp,    add = TRUE, col = "navy",       lwd = lwdl, density = dens, angle = 160)
    sp::plot(WCGFS_sp,   add = TRUE, col = "violet",     lwd = lwdl, density = dens, angle = 192)
    sp::plot(EVHOE_sp,   add = TRUE, col = "blue",       lwd = lwdl, density = dens, angle = 224)
    sp::plot(SpNorth_sp, add = TRUE, col = "orange",     lwd = lwdl, density = dens, angle = 256)
    sp::plot(PTIBTS_sp,  add = TRUE, col = "lightgreen", lwd = lwdl, density = dens, angle = 288)
    sp::plot(SpCadiz_sp, add = TRUE, col = "sienna",     lwd = lwdl, density = dens, angle = 320)
  }

  # --- Costas ---
  maps::map(world_db, xlim = xlims, ylim = c(sl, nl),
            fill = TRUE, col = ifelse(bw, "gray", "burlywood3"),
            add = TRUE, fg = "blue",
            interior = TRUE, boundary = TRUE,
            lty = 1, lwd = 0.05)

  # --- Ciudades ---
  if (places) {
    pts <- data.frame(
      lon  = c(-6.26, -9.14,  2.35, -3.71,  4.35, -0.13, 12.57),
      lat  = c(53.35, 38.72, 48.85, 40.21, 50.85, 51.51, 55.68),
      name = c("Dublin","Lisbon","Paris","Madrid","Brussels","London","Copenhagen"),
      pos  = c(2, 4, 3, 3, 3, 3, 3)
    )
    points(pts$lon, pts$lat, pch = 18)
    text(pts$lon, pts$lat, pts$name, cex = .5 * cex.leg, font = 2, pos = pts$pos)
  }

  box()

  # --- Leyenda ---
  if (leg) {
    colores <- c("tomato1","yellow","yellow4","cyan4","green","red",
                 "navy","violet","blue","orange","lightgreen","sienna")
    if (NS) {
      survs <- c("NS-IBTS","SCOWCGFS","SCOROC","NIGFS","IE-IGFS","SP-PORC",
                 "FR-CGFS","FR-WCGFS","EVHOE","SP-NORTH","PT-IBTS","SP-ARSA")
    } else {
      survs   <- c("SCOWCGFS","SCOROC","NIGFS","IE-IGFS","SP-PORC",
                   "FR-CGFS","FR-WCGFS","EVHOE","SP-NORTH","PT-IBTS","SP-ARSA")
      colores <- colores[2:length(colores)]
    }
    legend(legpos,
           legend  = survs,
           fill    = colores,
           density = dens,
           angle   = seq(0, 350, by = 32),
           cex     = cex.leg,
           inset   = c(.03, .03),
           title   = "SURVEYS",
           bg      = "white",
           text.col = "black")
  }

  invisible(list(asp = asp_geo, xlims = xlims, ylims = c(sl, nl)))
}
