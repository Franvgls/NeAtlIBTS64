#' Function qcHaulsDist checks consistency between hauls distance, speed and course parameters
#'
#' Produces different plots comparing expected values and those included.
#' Data are taken directly from DATRAS using function getDATRAS from library(icesDatras).
#' It only produces plots for surveys with HH files uploaded in DATRAS.
#'
#' @param Survey Either the survey name to download from DATRAS (see details),
#'   or a data.frame with HH information in DATRAS HH format.
#' @param years Years to download and use; must be available in DATRAS.
#' @param quarter Quarter of the survey.
#' @param allHauls Default FALSE; if TRUE lists values for all hauls,
#'   otherwise only those where pc.error>error.
#' @param pc.error Acceptable error rate (in percent) for displaying hauls
#'   as erroneous.
#' @param error.rb If FALSE does not take into account course errors.
#' @param getICES Logical. If TRUE (default), data are downloaded from DATRAS via
#'   icesDatras; if FALSE, uses the HH data.frame passed in as Survey.
#' @param graf If FALSE the graph goes to screen; if it is a file name
#'   (e.g. "graf") a .png file with that name is created.
#' @param xpng Width of png in pixels.
#' @param ypng Height of png in pixels.
#' @param ppng Pointsize parameter for png.
#' @param error Which error to plot: "Dist", "Speed" or "Course".
#' @param esc.mult Multiplicative scale factor for cex of labels/axes.
#' @param plots Logical; if TRUE produces graphics.
#' @details Surveys available in DATRAS: NS-IBTS, SWC-IBTS, ROCKALL, NIGFS,
#'   IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS, SP-ARSA.
#' @family quality control
#' @references distHaversine function gives the haversine calculation of distance
#'   between two geographic points \code{\link[geosphere]{distHaversine}}
#' @return data.frame with survey, haul, distance, estimated values, different
#'   errors and times of dawn/sunrise/dusk.
#' @examples
#' \dontrun{
#' qcHaulsDist("SP-NORTH", 2024, 4, error = "Speed")
#' qcHaulsDist("IE-IAMS", 2024, quarter = c(1, 1), error = "Course")
#' qcHaulsDist("NS-IBTS", 2024, 1, error = "Speed")   # multi-country: one panel per country
#' }
#' @export
qcHaulsDist <- function(Survey = "NS-IBTS", years, quarter,
                        pc.error = 2, error.rb = TRUE, allHauls = FALSE,
                        plots = TRUE, getICES = TRUE,
                        error = c("Dist"), esc.mult = 1,
                        graf = FALSE, xpng = 800, ypng = 800, ppng = 15) {

  if (!error %in% c("Dist", "Speed", "Course"))
    stop("Options for error are Dist, Speed or Course")

  if (getICES) {
    dumb <- icesDatras::getDATRAS("HH", Survey, years, quarter)
    if (is.null(dumb) || !is.data.frame(dumb) || nrow(dumb) == 0)
      stop("Survey and quarter combination do not exist or returned no data")
  } else {
    dumb <- Survey
    if (!all(unique(years) %in% unique(dumb$Year)))
      stop(paste0("Not all years selected are present in the data.frame, check: ",
                  paste(unique(years)[!(unique(years) %in% unique(dumb$Year))],
                        collapse = ", ")))
    if (unique(dumb$Quarter) != quarter)
      stop(paste0("Quarter selected ", quarter,
                  " is not available in the data.frame, check please"))
  }

  quarter <- unique(dumb$Quarter)
  dumb <- dplyr::filter(dumb, HaulVal == "V")

  # DATRAS uses -9 as NA marker in numeric fields. Normalise to NA and warn.
  na_cols <- c("Distance", "GroundSpeed", "TowDir", "HaulDur",
               "ShootLat", "ShootLong", "HaulLat", "HaulLong")
  for (col in na_cols) {
    if (col %in% names(dumb)) {
      bad <- !is.na(dumb[[col]]) & dumb[[col]] == -9
      if (any(bad)) {
        message(sprintf(
          "Found %d haul(s) with -9 (DATRAS NA) in '%s' (countries: %s) - converted to NA",
          sum(bad), col,
          paste(unique(dumb$Country[bad]), collapse = ", ")))
        dumb[[col]][bad] <- NA
      }
    }
  }

  if (any(is.na(dumb$Distance))) {
    datafromNA <- dumb[is.na(dumb$Distance), ]
    has_pos <- !is.na(datafromNA$ShootLong) & !is.na(datafromNA$ShootLat) &
      !is.na(datafromNA$HaulLong)  & !is.na(datafromNA$HaulLat)
    if (any(has_pos)) {
      dumb[is.na(dumb$Distance), "Distance"][has_pos] <-
        geosphere::distHaversine(
          datafromNA[has_pos, c("ShootLong", "ShootLat")],
          datafromNA[has_pos, c("HaulLong",  "HaulLat")])
      message(paste("Distance NA replaced with haversine for haul(s)",
                    paste(datafromNA$HaulNo[has_pos], collapse = ", "),
                    "from", paste(unique(datafromNA$Country[has_pos]), collapse = ", ")))
    }
  }

  countries <- unique(dumb$Country)
  if (length(unique(dumb$Year)) > 1) stop("Only one year can be shown in this function")

  if (!is.logical(graf)) png(filename = paste0(graf, ".png"),
                             width = xpng, height = ypng, pointsize = ppng)

  # --- Calculos derivados ---
  dumb$dist.vel   <- dumb$HaulDur / 60 * dumb$GroundSpeed * 1852
  dumb$dist.hf    <- round(geosphere::distHaversine(
    dumb[, c("ShootLong", "ShootLat")],
    dumb[, c("HaulLong",  "HaulLat")]))
  dumb$vel.dist   <- round((dumb$dist.hf / 1852) / (dumb$HaulDur / 60), 1)
  dumb$error.vel  <- round((dumb$dist.vel - dumb$Distance) * 100 / dumb$Distance, 2)
  dumb$error.dist <- round((dumb$dist.hf  - dumb$Distance) * 100 / dumb$Distance, 2)
  dumb$rumb       <- round(geosphere::bearingRhumb(
    dumb[, c("ShootLong", "ShootLat")],
    dumb[, c("HaulLong",  "HaulLat")]), 0)
  dumb$error.rumb <- round(dumb$rumb - dumb$TowDir)
  dumb$date       <- as.Date(paste0(dumb$Year, "-", dumb$Month, "-", dumb$Day))

  if (any(nchar(dumb$TimeShot) < 3)) {
    message(paste("Some TimeShot are invalid, hauls",
                  paste(dumb[nchar(dumb$TimeShot) < 3, ]$HaulNo, collapse = ", "),
                  "will be ignored"))
    dumb <- dumb[nchar(dumb$TimeShot) > 2, ]
  }
  dumb$quarter <- quarter
  dumb$time    <- ifelse(nchar(dumb$TimeShot) == 3,
                         paste0(0, dumb$TimeShot), dumb$TimeShot)
  dumb$time_l  <- data.table::as.ITime(paste0(substr(dumb$time, 1, 2), ":",
                                              substr(dumb$time, 4, 5)))
  dumb$time_v  <- dumb$time_l + dumb$HaulDur * 60

  dumb$lon <- dumb$ShootLong; dumb$lat <- dumb$ShootLat
  dumb <- as.data.frame(cbind(dumb,
                              suncalc::getSunlightTimes(data = dumb[, c("date", "lat", "lon")],
                                                        tz = "GMT",
                                                        keep = c("dawn", "sunrise", "solarNoon"))[
                                                          , c("dawn", "sunrise", "solarNoon")]))
  dumb$lon <- dumb$HaulLong; dumb$lat <- dumb$HaulLat
  dumb <- as.data.frame(cbind(dumb,
                              suncalc::getSunlightTimes(data = dumb[, c("date", "lat", "lon")],
                                                        tz = "GMT",
                                                        keep = c("sunset", "dusk"))[
                                                          , c("sunset", "dusk")]))
  dumb$daynight <- ifelse(data.table::as.ITime(dumb$sunrise) < dumb$time_l &
                            dumb$time_l < data.table::as.ITime(dumb$sunset),
                          "D", "N")
  dumb$dawn      <- data.table::as.ITime(dumb$dawn)
  dumb$sunrise   <- data.table::as.ITime(dumb$sunrise)
  dumb$solarNoon <- data.table::as.ITime(dumb$solarNoon)
  dumb$sunset    <- data.table::as.ITime(dumb$sunset)
  dumb$dusk      <- data.table::as.ITime(dumb$dusk)
  dumb$dayhour <- NA
  for (i in seq_len(nrow(dumb))) {
    if (dumb$time_l[i] < dumb$dawn[i]    | dumb$time_v[i] > dumb$dusk[i])    dumb$dayhour[i] <- "N"
    if (dumb$time_l[i] > dumb$dawn[i]    & dumb$time_l[i] < dumb$sunrise[i]) dumb$dayhour[i] <- "S"
    if (dumb$time_l[i] > dumb$sunrise[i] & dumb$time_l[i] < dumb$solarNoon[i]) dumb$dayhour[i] <- "M"
    if (dumb$time_l[i] > dumb$solarNoon[i] & dumb$time_l[i] < dumb$sunset[i]) dumb$dayhour[i] <- "T"
    if (dumb$time_l[i] > dumb$sunset[i]  & dumb$time_v[i] < dumb$dusk[i])    dumb$dayhour[i] <- "A"
  }

  # --- Graficos ---
  if (plots) {
    temp <- dumb[order(dumb$Year, dumb$HaulNo),
                 c("Year", "quarter", "HaulNo", "Distance", "dist.hf",
                   "dist.vel", "GroundSpeed", "HaulDur", "vel.dist",
                   "error.dist", "error.vel", "TowDir", "rumb",
                   "error.rumb", "Country")]

    # Helper: dibuja un panel de error.
    # show_country_main = FALSE -> no pinta titulo de pais en el panel
    # (necesario en mono-pais para que no choque con title(main=...))
    draw_panel <- function(temp_c, ctry, error_var, error_lab,
                           show_country_main = TRUE) {
      err <- temp_c[[error_var]]
      if (any(is.finite(err))) {
        ylims <- max(abs(err), na.rm = TRUE) * 1.1
      } else {
        ylims <- 1
      }
      n_valid <- sum(!is.na(err))
      n_na    <- sum(is.na(err))

      # xlim al rango real de HaulNo de este pais, con padding
      x_min <- min(temp_c$HaulNo, na.rm = TRUE)
      x_max <- max(temp_c$HaulNo, na.rm = TRUE)
      x_pad <- max(1, ceiling((x_max - x_min) * 0.03))

      panel_main <- if (show_country_main) {
        paste0(ctry, "  (n = ", n_valid,
               if (n_na > 0) paste0(", ", n_na, " NA") else "",
               ")")
      } else {
        ""
      }

      plot(temp_c$HaulNo, err,
           cex  = sqrt(1 + abs(err)),
           pch  = 21,
           bg   = dplyr::if_else(err < 0, "red", "blue"),
           type = "o",
           xlim = c(x_min - x_pad, x_max + x_pad),
           ylim = c(-ylims, ylims),
           ylab = error_lab, xlab = "Haul Number",
           cex.lab = 1 * esc.mult, cex.axis = 1 * esc.mult,
           main = panel_main)
      if (n_valid >= 2) {
        q <- stats::quantile(err, pc.error / 10, na.rm = TRUE)
        abline(h = c(-q, 0, q), lty = c(3, 2, 3), lwd = c(.5, 1, .5))
      } else {
        abline(h = 0, lty = 2)
      }
    }

    # Layout: panel por pais si multi-pais
    n_countries <- length(countries)
    if (n_countries > 1) {
      nc <- ceiling(sqrt(n_countries))
      nr <- ceiling(n_countries / nc)
      par(mfrow = c(nr, nc), mar = c(4, 4, 2.5, 1), oma = c(0, 0, 3, 0))
    } else {
      par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
    }

    error_var <- switch(error,
                        "Dist"   = "error.dist",
                        "Speed"  = "error.vel",
                        "Course" = "error.rumb")
    error_lab <- switch(error,
                        "Dist"   = "Distance error (%)",
                        "Speed"  = "Speed-distance error (%)",
                        "Course" = "Course error (degrees)")
    main_title <- switch(error,
                         "Dist"   = "Distance-Points error",
                         "Speed"  = "Distance-speed error",
                         "Course" = "Course vs. Shoot-end points error")

    if (n_countries == 1) {
      # show_country_main = FALSE para que el panel no pinte main propio
      draw_panel(temp, countries, error_var, error_lab,
                 show_country_main = FALSE)
      mtext(paste("Survey", unique(dumb$Survey)),
            side = 3, line = 0, adj = 0,
            cex = 0.8 * esc.mult, font = 2)
      title(main = main_title, cex.main = 1.1 * esc.mult)
    } else {
      for (ctry in countries) {
        temp_c <- temp[temp$Country == ctry, ]
        if (nrow(temp_c) == 0) next
        draw_panel(temp_c, ctry, error_var, error_lab,
                   show_country_main = TRUE)
      }
      mtext(paste0("Survey ", unique(dumb$Survey),
                   " - ", main_title),
            outer = TRUE, line = 0.8, font = 2, cex = 1 * esc.mult)
    }
  }

  if (!is.logical(graf)) dev.off()

  if (length(unique(lubridate::year(dumb$date))) > 1)
    message(paste("Hauls from different years found:",
                  paste(unique(dumb$Year), collapse = ", ")))

  # --- Returns ---
  if (allHauls & error.rb) {
    return(dumb[order(dumb$Survey, dumb$HaulNo),
                c("Survey", "quarter", "Year", "HaulNo", "Country",
                  "Distance", "dist.hf", "dist.vel", "GroundSpeed",
                  "HaulDur", "vel.dist", "error.dist", "error.vel",
                  "TowDir", "rumb", "error.rumb",
                  "sunrise", "time_l", "sunset", "time_v", "dusk",
                  "daynight")])
  }
  if (!allHauls & error.rb) {
    lt <- list(
      lances = dumb[
        (!is.na(dumb$error.dist) & abs(dumb$error.dist) > pc.error)     |
          (!is.na(dumb$error.vel)  & abs(dumb$error.vel)  > pc.error * 3) |
          (!is.na(dumb$error.rumb) & abs(dumb$error.rumb) > pc.error * 3),
        c("Survey", "quarter", "Year", "HaulNo", "Country",
          "Distance", "dist.hf", "dist.vel", "GroundSpeed", "HaulDur",
          "TowDir", "rumb", "vel.dist",
          "error.dist", "error.vel", "error.rumb")],
      daynight = dplyr::filter(dumb, daynight == "N")[
        , c("date", "HaulNo", "Country", "daynight",
            "sunrise", "time_l", "sunset", "time_v")])
    return(lt)
  }
  if (!error.rb) {
    return(dumb[
      (!is.na(dumb$error.dist) & abs(dumb$error.dist) > pc.error) |
        (!is.na(dumb$error.vel)  & abs(dumb$error.vel)  > pc.error * 3),
      c("Survey", "quarter", "HaulNo", "Country",
        "Distance", "dist.hf", "dist.vel", "GroundSpeed",
        "HaulDur", "vel.dist", "error.dist", "error.vel")])
  }
}
