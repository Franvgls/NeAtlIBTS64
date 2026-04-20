#' Map species distribution from IBTS NeAtl surveys (R 4.4+)
#'
#' Phase 2 of the species distribution mapping pipeline. Plots bubble maps of
#' species CPUE (numbers per hour) from all IBTS NeAtl surveys on the
#' \code{IBTSNeAtl_map64()} base map.  Outliers (the top N values) are shown
#' with open circles so that the rest of the data uses a consistent size scale.
#'
#' Replaces the legacy \code{MapIBTS()} function from \code{mapIBTS_csv_IBTSWG2024.R},
#' removing all dependency on maptools, rgdal, sp and the hardcoded colour vector.
#'
#' @param data data.frame. Output of \code{IBTSGetSpeciesCPUE()}, or any
#'   data.frame with columns:
#'   Survey_Code, Longitude, Latitude, Survey_Year, Haul_Code, Vessel_Code,
#'   Species_Code, Common_Name, Length_Split, Group1, Group2, Total.
#' @param esp Character. Species code to plot (e.g. \code{"HKE"}).
#'   Use \code{"Stations"} to plot station positions only.
#' @param year Integer. Year to select. If NULL uses all rows in \code{data}.
#' @param heading Character. Species description for the legend title
#'   (e.g. \code{"Hake <20 cm"}). If NULL derived from Common_Name.
#' @param dato Integer. Which column to plot: 1 = Group1 (small / pre-recruit),
#'   2 = Group2 (large / post-recruit), 3 = Total. Default 1.
#' @param NS Logical. If TRUE (default) includes North Sea (wider xlim).
#' @param noutliers Integer. Number of top-value hauls to treat as outliers
#'   (drawn as open circles with a "> N" label). Default 5.
#' @param scaler Numeric. Bubble-size divisor. Increase to shrink all bubbles.
#'   Default 0.35 (same as legacy MapIBTS).
#' @param sl Numeric. Southern latitude limit (degrees N). Default 35.
#' @param nl Numeric. Northern latitude limit (degrees N). Default 61.5.
#' @param metafile Logical. If TRUE saves output to a PNG file. Default FALSE.
#' @param leg Logical. Draw the legend. Default TRUE.
#' @param IBTSsurvs data.frame. Survey colour table (columns: survey, color, ...).
#'   If NULL tries the package dataset \code{IBTSsurvs}.
#' @param extra_colors Named character vector of colours for surveys not in
#'   \code{IBTSsurvs} (e.g. NS-IBTS, IE-IAMS, NIGFS Q1).
#'   Default: \code{c("NS-IBTS"="red","IE-IAMS"="darkgreen","NIGFS"="cyan4")}.
#' @param nfile Character. Output PNG filename without extension. Auto-generated
#'   from \code{esp} + \code{dato} + \code{NS} if NA.
#' @param pt Numeric. PNG pointsize. Default 18.
#' @param png_w Integer. PNG width in pixels. Default 600 (NS=FALSE) or 800 (NS=TRUE).
#' @param png_h Integer. PNG height in pixels. Default 1200 (NS=FALSE) or 1100 (NS=TRUE).
#' @param png_res Integer. PNG resolution in dpi. Default NULL (uses R's default ~72 dpi).
#'   Set to 150 or 300 for high-resolution output.
#' @param bathy_col Colour for isobath lines passed to IBTSNeAtl_map64().
#'   Default "gray55" (visible but light over lightblue1 background).
#' @param bathy_lwd Line width for isobath lines. Default 0.4.
#' @param newdev Logical. Open a new graphics window. Default TRUE.
#'   Set to FALSE when drawing into an already-open device (PNG, mfrow, etc.).
#' @param add Logical. If TRUE skips drawing the base map and only adds points
#'   and legend to the current device. Default FALSE.
#' @param Std.Surv Logical. If TRUE standardises each survey to its own maximum
#'   (use with care -- breaks cross-survey comparability). Default FALSE.
#' @param Ord.Survs Logical. If TRUE (default) orders legend north to south.
#' @param cexleg Numeric. Character expansion for legend text. Default 0.7.
#'
#' @return Invisibly, a list with elements:
#'   \code{data} (the filtered/annotated data.frame),
#'   \code{m} (the non-outlier maximum used for scaling),
#'   \code{ml} (the rounded scale maximum),
#'   \code{col_map} (named colour vector by survey).
#'
#' @examples
#' \dontrun{
#' # ---- Typical single-species map ----------------------------------------
#' devtools::load_all("d:/FVG/GitHubRs/NeAtlIBTS64")
#' ibts25  <- IBTSSurveySummary(2025)
#' cpue25  <- IBTSGetSpeciesCPUE(2025, species = read.csv("SpeciesCodes.csv"),
#'                                HHdata = ibts25)
#'
#' # Pre-recruit hake (Group1 = <20 cm)
#' MapIBTS64(cpue25, esp = "HKE", year = 2025,
#'           heading = "Hake <20 cm", dato = 1, NS = FALSE)
#'
#' # Post-recruit hake (Group2 = >=20 cm)
#' MapIBTS64(cpue25, esp = "HKE", year = 2025,
#'           heading = "Hake 20+ cm", dato = 2, NS = FALSE)
#'
#' # ---- PNG with two panels (Q1 left, Q3+Q4 right) ----------------------
#' png("HKE_2025.png", width = 1600, height = 2000, pointsize = 18)
#' par(mfrow = c(1, 2))
#' MapIBTS64(cpue25, esp = "HKE", year = 2025, heading = "Hake <20cm Q1",
#'           dato = 1, NS = FALSE, newdev = FALSE)
#' MapIBTS64(cpue25, esp = "HKE", year = 2025, heading = "Hake <20cm Q3+Q4",
#'           dato = 1, NS = FALSE, newdev = FALSE)
#' dev.off()
#'
#' # ---- Station positions only -------------------------------------------
#' MapIBTS64(cpue25, esp = "Stations", year = 2025)
#' }
#'
#' @export
MapIBTS64 <- function(data,
                       esp         = "HKE",
                       year        = NULL,
                       heading     = NULL,
                       dato        = 1,
                       NS          = TRUE,
                       noutliers   = 5,
                       scaler      = 0.35,
                       sl          = 35,
                       nl          = 61.5,
                       metafile    = FALSE,
                       leg         = TRUE,
                       IBTSsurvs   = NULL,
                       extra_colors = c("NS-IBTS" = "red",
                                         "IE-IAMS"  = "darkgreen",
                                         "NIGFS"    = "cyan4"),
                       nfile       = NA,
                       pt          = 18,
                       png_w       = NULL,
                       png_h       = NULL,
                       png_res     = NULL,
                       bathy_col   = "gray55",
                       bathy_lwd   = 0.4,
                       newdev      = TRUE,
                       add         = FALSE,
                       Std.Surv    = FALSE,
                       Ord.Survs   = TRUE,
                       cexleg      = 0.7) {

  # ---- 0. Resolve column names --------------------------------------------
  dato_col <- switch(as.character(dato),
    "1" = "Group1",
    "2" = "Group2",
    "3" = "Total",
    stop("'dato' must be 1 (Group1/small), 2 (Group2/large) or 3 (Total)")
  )
  dato1 <- switch(as.character(dato), "1" = "_SM_", "2" = "_XL_", "3" = "_Tot_")

  # ---- 1. Load IBTSsurvs if not provided ----------------------------------
  if (is.null(IBTSsurvs)) {
    ns_env <- tryCatch(asNamespace("NeAtlIBTS64"), error = function(e) NULL)
    if (!is.null(ns_env) && exists("IBTSsurvs", envir = ns_env, inherits = FALSE))
      IBTSsurvs <- get("IBTSsurvs", envir = ns_env)
  }

  # ---- 2. Filter data -----------------------------------------------------
  esp_n <- as.character(esp)
  df    <- as.data.frame(data)

  if (!is.null(year)) df <- df[df$Survey_Year == year, ]
  df <- df[!is.na(df$Longitude) & !is.na(df$Latitude), ]

  xlims <- if (NS) c(-15.7, 13) else c(-18.6, 2.5)

  if (esp_n == "Stations") {
    srv_haul       <- paste(df$Survey_Code, df$Vessel_Code, df$Haul_Code, sep = "_")
    df             <- df[!duplicated(srv_haul), ]
    df[, dato_col] <- 1
    stations       <- TRUE
  } else {
    stations <- FALSE
    df       <- df[!is.na(df$Species_Code) & as.character(df$Species_Code) == esp_n, ]
    if (nrow(df) == 0) stop("No data found for species code: '", esp_n, "'")
    df[is.na(df[, dato_col]), dato_col] <- 0
  }

  # Geographic clip
  df <- df[df$Latitude  >= sl & df$Latitude  <= nl, ]
  df <- df[df$Longitude >= xlims[1] & df$Longitude <= xlims[2], ]
  if (!NS) df <- df[df$Survey_Code != "NS-IBTS", ]

  if (nrow(df) == 0) stop("No data within the requested geographic extent.")

  # ---- 3. Legend heading --------------------------------------------------
  if (is.null(heading)) {
    if (esp_n == "Stations") {
      heading <- "Stations Sampled"
    } else if ("Common_Name" %in% names(df) && !all(is.na(df$Common_Name))) {
      cn <- df$Common_Name[!is.na(df$Common_Name)][1]
      heading <- if (dato == 1 && "Length_Split" %in% names(df) &&
                     !is.na(df$Length_Split[1]))
        paste0(cn, " <", df$Length_Split[1], " cm")
      else if (dato == 2 && "Length_Split" %in% names(df) &&
               !is.na(df$Length_Split[1]))
        paste0(cn, " \u2265", df$Length_Split[1], " cm")
      else cn
    } else {
      heading <- esp_n
    }
  }

  # ---- 4. Survey colour mapping -------------------------------------------
  df$Survey_Code <- as.character(df$Survey_Code)

  # Order surveys N to S (by max latitude) if requested
  if (Ord.Survs) {
    surv_order <- names(sort(tapply(df$Latitude, df$Survey_Code, max, na.rm = TRUE),
                             decreasing = TRUE))
  } else {
    surv_order <- unique(df$Survey_Code)
  }
  df$Survey_Code <- factor(df$Survey_Code, levels = surv_order)

  get_color <- function(s, ibs = IBTSsurvs, ec = extra_colors) {
    if (!is.null(ibs) && s %in% ibs$survey)
      return(as.character(ibs$color[ibs$survey == s]))
    if (!is.null(ec) && s %in% names(ec))
      return(unname(ec[s]))
    return("gray50")
  }
  col_map   <- stats::setNames(sapply(surv_order, get_color), surv_order)
  df$bgs    <- unname(col_map[as.character(df$Survey_Code)])

  # ---- 5. Optional per-survey standardisation ------------------------------
  if (Std.Surv && !stations) {
    for (lv in levels(df$Survey_Code)) {
      idx <- df$Survey_Code == lv
      mx  <- max(df[idx, dato_col], na.rm = TRUE)
      if (mx > 0) df[idx, dato_col] <- 100 * df[idx, dato_col] / mx
    }
  }

  # ---- 6. Identify positive hauls and outliers ----------------------------
  df_pos  <- df[df[, dato_col] > 0, ]
  mxd     <- max(df_pos[, dato_col], na.rm = TRUE)
  pout    <- integer(0)
  OUTLIERS <- numeric(0)
  outliers <- (!stations) && (nrow(df_pos) > noutliers) && (noutliers > 0)

  if (outliers) {
    threshold <- sort(df_pos[, dato_col])[nrow(df_pos) - noutliers]
    pout      <- which(df_pos[, dato_col] > threshold)
    OUTLIERS  <- df_pos[pout, dato_col]
    minpout   <- min(OUTLIERS)

    # Non-outlier maximum, rounded to a clean number
    m <- trunc(max(df_pos[-pout, dato_col], na.rm = TRUE))
    if ((signif(m, 1) / (10^nchar(m))) > 0.25) {
      ml <- 5 * 10^(nchar(m) - 1)
    } else {
      ml <- 10^(nchar(m) - 1)
    }
    # Ensure ml < minpout (otherwise the legend would be misleading)
    if (ml >= minpout) {
      ml <- trunc(minpout / 10^(nchar(trunc(minpout)) - 2)) *
            10^(nchar(trunc(minpout)) - 2)
    }
    if (ml <= 0 || ml >= minpout) {
      warning("Cannot separate outliers cleanly; setting outliers=FALSE.")
      outliers <- FALSE
      pout     <- integer(0)
      OUTLIERS <- numeric(0)
    }
  }

  if (!outliers) {
    m <- trunc(mxd)
    if ((signif(m, 1) / (10^nchar(m))) > 0.25) {
      ml <- 5 * 10^(nchar(m) - 1)
    } else {
      ml <- 10^(nchar(m) - 1)
    }
    if (!stations && mxd < 5) { ml <- 5; m <- 5 }
  }

  scaler_use <- if (stations) 1.5 else scaler

  # ---- 7. Output filename -------------------------------------------------
  if (metafile) {
    if (is.na(nfile))
      nfile <- paste0(esp_n, dato1, if (NS) "NS" else "WS", ".png")
    else
      nfile <- paste0(nfile, ".png")
  }

  # ---- 8. Open device and base map ----------------------------------------
  if (metafile) {
    w <- if (!is.null(png_w)) png_w else if (NS) 800 else 600
    h <- if (!is.null(png_h)) png_h else if (NS) 1100 else 1200
    png_args <- list(filename  = nfile,
                     width     = w,
                     height    = h,
                     type      = "cairo",
                     bg        = "white",
                     pointsize = pt)
    if (!is.null(png_res)) png_args$res <- png_res
    do.call(grDevices::png, png_args)
    newdev <- FALSE   # device already open
  }

  if (!add) {
    # IBTSNeAtl_map64 draws worldHires + shapefiles + grid + ICES lines + land fill
    # load=FALSE skips the campana polygons; leg=FALSE omits survey legend
    do.call(IBTSNeAtl_map64, c(list(
      load      = FALSE,
      leg       = FALSE,
      newdev    = newdev,
      NS        = NS,
      ylims     = c(sl, nl),
      bathy     = TRUE,
      bathy_col = bathy_col,
      bathy_lwd = bathy_lwd,
      graf      = FALSE
    )))
  }

  # ---- 9. Plot CPUE bubbles -----------------------------------------------
  # Denominador comun: ml (maximo redondeado de la escala).
  # Esto garantiza que ningun no-outlier supere el tamano del circulo outlier,
  # ya que los no-outliers tienen CPUE <= ml por definicion.
  denom <- ml * scaler_use

  # Non-outlier filled circles
  df_plot <- if (outliers && length(pout) > 0) df_pos[-pout, ] else df_pos
  if (nrow(df_plot) > 0) {
    graphics::points(
      df_plot$Longitude, df_plot$Latitude,
      cex = sqrt(df_plot[, dato_col] / denom),
      pch = 21, bg = df_plot$bgs, lwd = 0.5
    )
  }

  # Outliers: open circles fijos al tamano maximo (sqrt(ml/denom) = sqrt(1/scaler_use))
  if (outliers && length(pout) > 0) {
    graphics::points(
      df_pos[pout, "Longitude"], df_pos[pout, "Latitude"],
      cex = sqrt(ml / denom),
      pch = 21, col = df_pos$bgs[pout], bg = NA, lwd = 2
    )
  }

  # ---- 10. Legend ---------------------------------------------------------
  if (leg) {
    surv_in_data <- levels(droplevels(df$Survey_Code))
    pts          <- length(surv_in_data)
    leg_colors   <- unname(col_map[surv_in_data])

    if (!stations) {
      # Size-scale part of the legend — mismo denominador denom = ml * scaler_use
      l1      <- trunc(ml * c(0.1, 0.3, 0.5, 1))
      pt.size <- sqrt(l1 / denom)
      pt.simb <- rep(21, 4)
      pt.col  <- rep(grDevices::grey(0.8), 4)
      pt.lwd  <- rep(1, 4)

      if (outliers) {
        l1      <- c(l1, paste(">", ml))
        pt.size <- c(pt.size, sqrt(ml / denom))
        pt.simb <- c(pt.simb, 21)
        pt.col  <- c(pt.col, "white")
        pt.lwd  <- c(pt.lwd, 2)
      }
    } else {
      heading <- "Stations Sampled"
      l1 <- NULL; pt.size <- NULL; pt.simb <- NULL
      pt.col <- NULL; pt.lwd <- NULL
    }

    l1      <- c(NA, l1, "SURVEYS:", surv_in_data)
    pt.size <- c(NA, pt.size, NA, rep(1.5, pts))
    pt.simb <- c(NA, pt.simb, NA, rep(22, pts))
    pt.col  <- c(NA, pt.col, NA, leg_colors)
    pt.lwd  <- c(NA, pt.lwd, NA, rep(1, pts))

    n_size_rows <- if (!stations) (if (outliers) 5 else 4) else 0

    temp <- graphics::legend(
      if (NS) "bottomright" else "bottomleft",
      l1,
      pch        = pt.simb,
      pt.cex     = pt.size,
      pt.bg      = pt.col,
      pt.lwd     = pt.lwd,
      title      = format("LEGEND",
                           width   = max(36, nchar(heading) * 1.2),
                           justify = "centre"),
      cex        = cexleg,
      bg         = "white",
      y.intersp  = if (outliers) 1.2 else 1.1,
      inset      = 0.03,
      trace      = FALSE,
      xjust      = 0.5,
      yjust      = 1
    )
    # Species heading centrado sobre la caja de la leyenda
    graphics::text(temp$rect$left + temp$rect$w / 2, temp$text$y[1],
                   heading, adj = c(0.5, 0.5), cex = cexleg, font = 2)
  }

  # ---- 11. Outlier footnote -----------------------------------------------
  if (outliers && length(OUTLIERS) > 0) {
    n_out <- length(OUTLIERS)
    if (n_out > 1)
      graphics::mtext(
        paste(n_out, "outliers between", round(min(OUTLIERS)), "and", round(mxd)),
        side = 1, line = 1.5, font = 1, cex = 0.8)
    else
      graphics::mtext(
        paste(n_out, "outlier with", round(mxd), "individuals"),
        side = 1, line = 1.5, font = 1, cex = 0.8)
    message("Outliers: ", paste(sort(OUTLIERS), collapse = ", "))
  }

  if (metafile) grDevices::dev.off()

  invisible(list(data    = df,
                 m       = m,
                 ml      = ml,
                 col_map = col_map))
}
