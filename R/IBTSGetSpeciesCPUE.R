#' Get species CPUE by haul from DATRAS for all IBTS NeAtl surveys
#'
#' Phase 1 of the species distribution mapping pipeline.  Downloads HL + HH
#' exchange data from DATRAS, handles mixed DataType (R/C) within the same
#' survey (as in NS-IBTS where different nations submit different formats),
#' converts all length classes to cm following the DATRAS specification, and
#' returns a standardised data.frame ready for \code{MapIBTS64()}.
#'
#' @section DataType R vs C:
#' DATRAS \code{DataType} in HH indicates how \code{HLNoAtLngt} was recorded.
#' Both types still require the SubFactor correction:
#' \describe{
#'   \item{R (raw counts)}{Most surveys (NS-IBTS many vessels, SP-NORTH,
#'     EVHOE, SP-PORC, ...).
#'     \code{CPUE = HLNoAtLngt * SubFactor * 60 / HaulDur}}
#'   \item{C (pre-calculated CPUE/h)}{Scottish surveys (SCOWCGFS, SCOROC) and
#'     some vessels within NS-IBTS.
#'     \code{CPUE = HLNoAtLngt * SubFactor}
#'     (haul-duration already applied; SubFactor still needed for any
#'     subsampling performed before the CPUE was computed)}
#' }
#' NS-IBTS pools vessels from many nations so the same download often mixes R
#' and C rows.  DataType is matched row-by-row from HH via HaulNo.
#'
#' @section Length units (TS_LngtCode vocabulary, ICES vocab server):
#' The \code{LngtCode} field in HL uses the \code{TS_LngtCode} vocabulary
#' (3 codes).  The rule is uniform across all IBTS NeAtl surveys:
#' \describe{
#'   \item{"."}{1 mm length class -- \code{LngtClass} in \strong{mm} -> divide by 10.}
#'   \item{"0"}{0.5 cm length class -- \code{LngtClass} in \strong{mm} -> divide by 10.}
#'   \item{"1"}{1 cm length class  -- \code{LngtClass} in \strong{cm} -> keep as-is.}
#' }
#' Any other code is treated as mm (divide by 10) and triggers a \code{warning()}.
#' All values are converted to a single internal field \code{LngtClas_cm} BEFORE
#' the Group1/Group2 split, so that \code{LengthSplit} in the species table
#' (always in cm) is applied correctly regardless of survey or species.
#'
#' @section NS-IBTS LngtCode note:
#' The reference script \code{filterNSIBTSdata_cpue.R} divides \code{"."} rows
#' by 10 for NS-IBTS.  This is correct: \code{"."} means 1 mm class (stored in mm)
#' per the \code{TS_LngtCode} vocabulary, consistent with all other surveys.
#'
#' @param year Integer. Year to process.
#' @param species data.frame with columns:
#'   \code{Code} (3-letter code e.g. \code{"HKE"}),
#'   \code{WoRMSCode} (integer, Valid_Aphia),
#'   \code{Common} (common name),
#'   \code{LengthSplit} (numeric, cm; NA if no size split defined).
#'   If NULL every species with a Valid_Aphia in HL is returned.
#' @param surveys List of lists each with \code{surv} and \code{q}.
#'   NULL uses the full NeAtl IBTS default list (Q1 / Q3 / Q4).
#' @param quarters Integer vector.  Subset of quarters from the default list.
#'   Ignored when \code{surveys} is provided explicitly.
#'   E.g. \code{c(3, 4)} skips all Q1 surveys.  Default NULL = all quarters.
#' @param valid_only Logical.  Keep only HaulVal \%in\% c("V","A").
#'   Default TRUE.
#' @param local_HL data.frame opcional en formato DATRAS HL con datos locales
#'   (e.g. especies no subidas aún a DATRAS). Se filtra por \code{Survey} y
#'   \code{Quarter} y se añade mediante \code{rbind} al HL descargado antes
#'   de cualquier procesamiento. \code{DataType} se fuerza a \code{"R"}.
#'   El \code{HaulDur} se tomará del HH existente vía \code{HaulNo}.
#' @param local_HH data.frame opcional en formato DATRAS HH con datos locales
#'   (e.g. campanas no subidas aun a DATRAS como PT-IBTS Q4). Debe tener
#'   columnas \code{Survey} y \code{Quarter} para el filtro por clave.
#'   Si hay filas para un \code{surv+q}, se usan en vez de descargar HH de
#'   DATRAS Y se omite el chequeo de disponibilidad en DATRAS para esa clave.
#' @param HHdata Output of \code{IBTSSurveySummary()} to reuse cached HH data.
#'   NULL downloads fresh HH from DATRAS for each survey.
#' @param verbose Logical.  Print progress messages.  Default TRUE.
#'
#' @return data.frame with columns:
#'   \code{Survey_Code}, \code{Longitude}, \code{Latitude},
#'   \code{Survey_Year}, \code{Quarter},
#'   \code{Haul_Code}, \code{Vessel_Code}, \code{DataType},
#'   \code{Species_Code}, \code{Common_Name}, \code{Length_Split},
#'   \code{Group1}, \code{Group2}, \code{Total}.
#'   All CPUE values are numbers per hour.
#'   \code{Group1}: CPUE for fish < \code{LengthSplit} cm.
#'   \code{Group2}: CPUE for fish >= \code{LengthSplit} cm.
#'   NA in both columns when \code{LengthSplit} is NA for that species.
#'
#' @examples
#' \dontrun{
#' # SpeciesCodes.csv columns: Code, WoRMSCode, Common, LengthSplit (in cm)
#' spcodes <- read.csv("SpeciesCodes.csv")
#'
#' # Full run, reusing HH from IBTSSurveySummary() to avoid double download
#' ibts25  <- IBTSSurveySummary(2025)
#' cpue25  <- IBTSGetSpeciesCPUE(2025, species = spcodes, HHdata = ibts25)
#'
#' # Western/southern area only (Q3+Q4, no NS-IBTS)
#' cpue25_ws <- IBTSGetSpeciesCPUE(2025, species = spcodes, quarters = c(3, 4))
#'
#' # NS-IBTS only, both quarters
#' cpue25_ns <- IBTSGetSpeciesCPUE(
#'   2025, species = spcodes,
#'   surveys = list(list(surv = "NS-IBTS", q = 1),
#'                  list(surv = "NS-IBTS", q = 3))
#' )
#' }
#'
#' @export
IBTSGetSpeciesCPUE <- function(year,
                               species    = NULL,
                               surveys    = NULL,
                               quarters   = NULL,
                               valid_only = TRUE,
                               HHdata     = NULL,
                               local_HL   = NULL,
                               local_HH   = NULL,
                               verbose    = TRUE) {

  # ---- 0. Default survey list -----------------------------------------------
  default_surveys <- list(
    list(surv = "NS-IBTS",   q = 1),
    list(surv = "NS-IBTS",   q = 3),
    list(surv = "SCOWCGFS",  q = 1),
    list(surv = "SCOWCGFS",  q = 4),
    list(surv = "NIGFS",     q = 1),
    list(surv = "NIGFS",     q = 4),
    list(surv = "IE-IAMS",   q = 1),
    list(surv = "IE-IAMS",   q = 2),
    list(surv = "IE-IGFS",   q = 4),
    list(surv = "SP-ARSA",   q = 1),
    list(surv = "SP-ARSA",   q = 4),
    list(surv = "SCOROC",    q = 3),
    list(surv = "FR-WCGFS",  q = 3),
    list(surv = "SP-PORC",   q = 3),
    list(surv = "FR-CGFS",   q = 4),
    list(surv = "EVHOE",     q = 4),
    list(surv = "SP-NORTH",  q = 4),
    list(surv = "PT-IBTS",   q = 4)
  )
  if (is.null(surveys)) {
    surveys <- default_surveys
    if (!is.null(quarters))
      surveys <- surveys[sapply(surveys, function(s) s$q %in% quarters)]
  }

  # ---- 1. LngtClass -> cm conversion ----------------------------------------
  #
  # TS_LngtCode vocabulary (ICES vocab server) -- uniform for all surveys:
  #   "."  = 1 mm class    -> LngtClass in mm -> divide by 10
  #   "0"  = 0.5 cm class  -> LngtClass in mm -> divide by 10
  #   "1"  = 1 cm class    -> LngtClass in cm -> keep as-is
  #   other / NA           -> assume mm -> divide by 10 (conservative, warning)
  #
  lngt_to_cm <- function(lngt_clas, lngt_code) {
    ifelse(lngt_code == "1", lngt_clas, lngt_clas / 10)
  }

  # ---- 2. Loop over surveys -------------------------------------------------
  out_list <- vector("list", length(surveys))

  for (s_idx in seq_along(surveys)) {
    surv <- surveys[[s_idx]]$surv
    q    <- surveys[[s_idx]]$q
    key  <- paste0(surv, "_Q", q)

    if (verbose) message("[ ", key, " ] checking DATRAS...")

    # 2a. Check for local data (bypasses DATRAS availability check)
    has_local_HH <- !is.null(local_HH) &&
      any(local_HH$Survey == surv & local_HH$Quarter == as.character(q))
    has_local_HL <- !is.null(local_HL) &&
      any(local_HL$Survey == surv & local_HL$Quarter == as.character(q))

    # Availability check omitido — el bug de rbindlist/Doortype en icesDatras 1.4.1
    # puede ocurrir tambien en getSurveyYearQuarterList. Se delega la deteccion
    # de datos ausentes al propio intento de descarga (getDATRAS / getHLdata).
    if (has_local_HH || has_local_HL) {
      if (verbose) message("  -> local data found, skipping DATRAS availability check.")
    }

    # 2b. Download HL (skip if all HL comes from local_HL)
    if (has_local_HL) {
      # Use only local HL for this survey+quarter
      hl <- local_HL[
        local_HL$Survey  == surv &
          local_HL$Quarter == as.character(q), ]
      hl$DataType <- "R"
      if (verbose) message("  -> HL from local_HL (", nrow(hl), " rows), skipping DATRAS HL download.")
    } else {
      # Bug icesDatras 1.4.1: rbindlist/Doortype lanza errores C-level que
      # tryCatch no atrapa. Se usa try() que es mas robusto para estos casos.
      hl <- try(icesDatras::getDATRAS("HL", surv, year, q), silent = TRUE)
      if (inherits(hl, "try-error")) {
        if (verbose) message("  -> getDATRAS HL error, reintentando con getHLdata()...")
        hl <- try(icesDatras::getHLdata(surv, year, q), silent = TRUE)
      }
      if (inherits(hl, "try-error")) hl <- NULL
      if (is.null(hl) || !is.data.frame(hl) || nrow(hl) == 0) {
        if (verbose) message("  -> no HL data, skipping.")
        next
      }
    }

    # 2b2. Inject local HL rows — solo si HL vino de DATRAS (has_local_HL=FALSE)
    #       Si has_local_HL=TRUE el HL ya viene integro de local_HL (ver 2b).
    if (!has_local_HL && !is.null(local_HL)) {
      loc <- local_HL[
        local_HL$Survey  == surv &
          local_HL$Quarter == as.character(q), ]
      if (nrow(loc) > 0) {
        # Alinear columnas con el HL de DATRAS
        common_cols <- intersect(names(hl), names(loc))
        loc <- loc[, common_cols, drop = FALSE]
        # Columnas que faltan en loc → NA
        missing <- setdiff(names(hl), common_cols)
        for (mc in missing) loc[[mc]] <- NA
        loc$DataType <- "R"   # datos locales siempre son raw counts
        hl <- rbind(hl, loc[, names(hl)])
        if (verbose) message("  -> injected ", nrow(loc),
                             " local HL rows for ", surv, " Q", q)
      }
    }
    # 2c. Get HH: local_HH > IBTSSurveySummary cache > DATRAS download
    hh <- NULL
    if (has_local_HH) {
      hh <- local_HH[
        local_HH$Survey  == surv &
          local_HH$Quarter == as.character(q), ]
      if (verbose) message("  -> HH from local_HH (", nrow(hh), " rows)")
    }
    if (is.null(hh) && !is.null(HHdata) && !is.null(HHdata$data[[key]])) {
      hh <- HHdata$data[[key]]
      if (verbose) message("  -> HH from IBTSSurveySummary cache")
    }
    if (is.null(hh)) {
      hh <- tryCatch(
        icesDatras::getDATRAS("HH", surv, year, q),
        error = function(e) { message("  -> HH error: ", conditionMessage(e)); NULL }
      )
    }
    if (is.null(hh) || !is.data.frame(hh) || nrow(hh) == 0) {
      if (verbose) message("  -> no HH data, skipping.")
      next
    }

    # 2d. Valid hauls
    # NOTA: en NS-IBTS HaulNo NO es unico — cada pais usa numeracion propia.
    # La clave unica es HaulNo + Country. Se usa paste() para el filtro.
    hh_use <- if (valid_only) hh[hh$HaulVal %in% c("V", "A"), ] else hh
    hl_key <- paste(hl$HaulNo,     hl$Country)
    hh_key <- paste(hh_use$HaulNo, hh_use$Country)
    hl     <- hl[hl_key %in% hh_key, ]
    if (nrow(hl) == 0) {
      if (verbose) message("  -> no valid hauls in HL.")
      next
    }

    # -------------------------------------------------------------------------
    # KEY STEP 1 — merge DataType + HaulDur from HH row-by-row
    #
    # NS-IBTS: multiple nations in one download -> same file has R and C rows.
    # Each HL row gets the DataType of its parent haul via HaulNo.
    # -------------------------------------------------------------------------
    hh_cols <- intersect(
      c("HaulNo","Country","HaulDur","ShootLat","ShootLong","Ship","DataType"),
      names(hh_use)
    )
    hl <- merge(hl, hh_use[, hh_cols, drop = FALSE],
                by = c("HaulNo","Country"), all.x = TRUE)

    if (!"DataType" %in% names(hl)) {
      if (verbose) message("  -> DataType absent from HH, defaulting to 'R'")
      hl$DataType <- "R"
    }
    hl$DataType <- trimws(as.character(hl$DataType))

    # Drop R-type rows with missing/zero HaulDur
    bad_R <- (hl$DataType == "R") & (is.na(hl$HaulDur) | hl$HaulDur <= 0)
    if (any(bad_R)) {
      if (verbose) message("  -> dropping ", sum(bad_R),
                           " R-type rows with invalid HaulDur")
      hl <- hl[!bad_R, ]
    }
    if (nrow(hl) == 0) next

    # -------------------------------------------------------------------------
    # KEY STEP 2 — convert LngtClass to cm
    #
    # Drop rows where LngtCode is NA (unit unknown, cannot convert safely).
    # Apply survey-specific rule via lngt_to_cm().
    # -------------------------------------------------------------------------
    if (!"LngtCode" %in% names(hl)) {
      if (verbose) message("  -> LngtCode absent, defaulting to '.'")
      hl$LngtCode <- "."
    }
    n_na_lngt <- sum(is.na(hl$LngtCode))
    if (n_na_lngt > 0) {
      if (verbose) message("  -> dropping ", n_na_lngt,
                           " rows with NA LngtCode (unit unknown)")
      hl <- hl[!is.na(hl$LngtCode), ]
    }
    hl$LngtCode    <- trimws(as.character(hl$LngtCode))

    # Warn if any LngtCode other than ".", "0", "1" is present.
    # Known codes (TS_LngtCode vocab): "." = 1mm, "0" = 0.5cm, "1" = 1cm.
    # Unknown codes are treated as mm (divide by 10) conservatively.
    unknown_codes <- setdiff(unique(hl$LngtCode), c(".", "0", "1"))
    if (length(unknown_codes) > 0) {
      for (wc in unknown_codes) {
        n_wc  <- sum(hl$LngtCode == wc, na.rm = TRUE)
        sp_wc <- if (!is.null(species) && "Valid_Aphia" %in% names(hl)) {
          aph <- unique(hl$Valid_Aphia[hl$LngtCode == wc])
          m   <- match(aph, species$WoRMSCode)
          paste(ifelse(is.na(m), paste0("Aphia:", aph), species$Code[m]),
                collapse = ", ")
        } else "unknown species"
        warning(key, ": unknown LngtCode='", wc, "' in ", n_wc,
                " rows for species [", sp_wc, "]. ",
                "Division by 10 applied as conservative default. ",
                "Please check these data in DATRAS.",
                call. = FALSE)
      }
    }

    hl$LngtClas_cm <- lngt_to_cm(hl$LngtClass, hl$LngtCode)

    if (nrow(hl) == 0) next

    # LngtCode distribution log (QC)
    if (verbose) {
      lc_tab <- sort(table(hl$LngtCode), decreasing = TRUE)
      message("  -> LngtCode: ",
              paste(names(lc_tab), lc_tab, sep = "=", collapse = "  "))
    }

    # -------------------------------------------------------------------------
    # KEY STEP 3 — CPUE in numbers per hour
    #
    # Formulas from filterNSIBTSdata_cpue.R lines 17-18:
    #   C: CPUE = HLNoAtLngt * SubFactor
    #      (HaulDur already baked in; SubFactor corrects for any subsampling
    #      done before the per-hour CPUE was computed by the submitting lab)
    #   R: CPUE = HLNoAtLngt * SubFactor * 60 / HaulDur
    # -------------------------------------------------------------------------
    hl$CPUE_n_h <- ifelse(
      hl$DataType == "C",
      hl$HLNoAtLngt * hl$SubFactor,
      hl$HLNoAtLngt * hl$SubFactor * 60 / hl$HaulDur
    )

    if (verbose) {
      dt_tab <- sort(table(hl$DataType), decreasing = TRUE)
      message("  -> DataType distribution: ",
              paste(names(dt_tab), dt_tab, sep = "=", collapse = "  "))
    }

    # ---- 3. Filter to target species ----------------------------------------
    hl <- hl[!is.na(hl$Valid_Aphia), ]
    if (!is.null(species)) hl <- hl[hl$Valid_Aphia %in% species$WoRMSCode, ]
    if (nrow(hl) == 0) {
      if (verbose) message("  -> no target species found.")
      next
    }

    if (!is.null(species)) {
      hl$Species_Code <- species$Code[match(hl$Valid_Aphia, species$WoRMSCode)]
      hl$Length_Split <- species$LengthSplit[match(hl$Valid_Aphia, species$WoRMSCode)]
    } else {
      hl$Species_Code <- as.character(hl$Valid_Aphia)
      hl$Length_Split <- NA_real_
    }
    hl <- hl[!is.na(hl$Species_Code), ]
    if (nrow(hl) == 0) next

    if (!"Ship" %in% names(hl)) {
      hl$Ship <- "unknown"
    } else {
      hl$Ship[is.na(hl$Ship)] <- "unknown"
    }
    if (!"Country" %in% names(hl)) hl$Country <- "UNK"
    hl$Country[is.na(hl$Country)] <- "UNK"
    hl$Ship[is.na(hl$Ship)]       <- "unknown"
    # ---- 4. Aggregate by HaulNo x Species: Group1 / Group2 / Total ----------
    #
    # Group1 (small / pre-recruit):  LngtClas_cm <  LengthSplit
    # Group2 (large / post-recruit): LngtClas_cm >= LengthSplit
    #
    # Both use LngtClas_cm consistently (the original scripts had a typo
    # mixing LngtClass and LngtClas for the two groups).
    # -------------------------------------------------------------------------
    sp_codes     <- unique(hl$Species_Code)
    haul_sp_list <- vector("list", length(sp_codes))

    for (sp_i in seq_along(sp_codes)) {
      i    <- sp_codes[sp_i]
      dumb <- hl[hl$Species_Code == i, ]
      if (nrow(dumb) == 0) next

      len_spl <- dumb$Length_Split[1]   # cm, or NA

      # Total CPUE per haul (all lengths)
      dumbtot <- stats::aggregate(
        CPUE_n_h ~ HaulNo + Country + Ship + DataType,
        data = dumb, FUN = sum, na.rm = TRUE
      )
      names(dumbtot)[names(dumbtot) == "CPUE_n_h"] <- "Total"

      if (is.na(len_spl)) {
        # No size split for this species
        dattot <- data.frame(
          HaulNo = dumbtot$HaulNo, Country = dumbtot$Country,
          Ship = dumbtot$Ship,
          DataType = dumbtot$DataType, Species_Code = i,
          Group1 = NA_real_, Group2 = NA_real_, Total = dumbtot$Total,
          stringsAsFactors = FALSE
        )
      } else {
        # Group1: small (< LengthSplit cm)
        dumbsm <- dumb[dumb$LngtClas_cm < len_spl, ]
        if (nrow(dumbsm) > 0) {
          dumbsm <- stats::aggregate(
            CPUE_n_h ~ HaulNo + Country + Ship, dumbsm, sum, na.rm = TRUE)
          names(dumbsm)[names(dumbsm) == "CPUE_n_h"] <- "Group1"
        } else {
          haul_uniq <- unique(dumb[, c("HaulNo","Country","Ship")])
          dumbsm <- data.frame(haul_uniq, Group1 = 0, stringsAsFactors = FALSE)
        }

        # Group2: large (>= LengthSplit cm)
        dumblg <- dumb[dumb$LngtClas_cm >= len_spl, ]
        if (nrow(dumblg) > 0) {
          dumblg <- stats::aggregate(
            CPUE_n_h ~ HaulNo + Country + Ship, dumblg, sum, na.rm = TRUE)
          names(dumblg)[names(dumblg) == "CPUE_n_h"] <- "Group2"
        } else {
          haul_uniq <- unique(dumb[, c("HaulNo","Country","Ship")])
          dumblg <- data.frame(haul_uniq, Group2 = 0, stringsAsFactors = FALSE)
        }

        datsize <- merge(dumbsm, dumblg, by = c("HaulNo","Country","Ship"), all = TRUE)
        dattot  <- merge(datsize,
                         dumbtot[, c("HaulNo","Country","Ship","DataType","Total")],
                         by = c("HaulNo","Country","Ship"), all = TRUE)
        dattot  <- data.frame(
          dattot[, c("HaulNo","Country","Ship","DataType")],
          Species_Code = i,
          dattot[, c("Group1","Group2","Total")],
          stringsAsFactors = FALSE
        )
      }
      haul_sp_list[[sp_i]] <- dattot
    }

    survey_data <- do.call(rbind, haul_sp_list)
    if (is.null(survey_data) || nrow(survey_data) == 0) next

    # ---- 5. Merge coordinates -----------------------------------------------
    coord_cols  <- intersect(c("HaulNo","Country","ShootLat","ShootLong"), names(hh_use))
    survey_data <- merge(survey_data, hh_use[, coord_cols, drop = FALSE],
                         by = c("HaulNo","Country"), all.x = TRUE)

    survey_data$Survey_Code  <- surv
    survey_data$Survey_Year  <- as.integer(year)
    survey_data$Quarter      <- as.integer(q)

    if (!is.null(species)) {
      survey_data$Common_Name  <- species$Common[
        match(survey_data$Species_Code, species$Code)]
      survey_data$Length_Split <- species$LengthSplit[
        match(survey_data$Species_Code, species$Code)]
    } else {
      survey_data$Common_Name  <- NA_character_
      survey_data$Length_Split <- NA_real_
    }

    names(survey_data)[names(survey_data) == "ShootLong"] <- "Longitude"
    names(survey_data)[names(survey_data) == "ShootLat"]  <- "Latitude"
    names(survey_data)[names(survey_data) == "HaulNo"]    <- "Haul_Code"
    names(survey_data)[names(survey_data) == "Ship"]      <- "Vessel_Code"

    col_order   <- c("Survey_Code","Longitude","Latitude",
                     "Survey_Year","Quarter",
                     "Haul_Code","Country","Vessel_Code","DataType",
                     "Species_Code","Common_Name","Length_Split",
                     "Group1","Group2","Total")
    survey_data <- survey_data[, intersect(col_order, names(survey_data)),
                               drop = FALSE]
    out_list[[s_idx]] <- survey_data

    if (verbose) {
      nR <- sum(survey_data$DataType == "R", na.rm = TRUE)
      nC <- sum(survey_data$DataType == "C", na.rm = TRUE)
      message("  -> OK | records: ", nrow(survey_data),
              " | species: ", length(unique(survey_data$Species_Code)),
              " | hauls: ",   length(unique(survey_data$Haul_Code)),
              "  [R:", nR, " C:", nC, "]")
    }
  } # end survey loop

  # ---- 6. Combine -----------------------------------------------------------
  result <- do.call(rbind, out_list)
  if (is.null(result) || nrow(result) == 0) {
    warning("IBTSGetSpeciesCPUE: no data retrieved for year ", year)
    return(invisible(NULL))
  }
  result <- result[!is.na(result$Longitude) & !is.na(result$Latitude), ]
  rownames(result) <- NULL

  if (verbose) {
    message("\n=== IBTSGetSpeciesCPUE DONE ===")
    message("Total records : ", nrow(result))
    message("Surveys       : ", paste(unique(result$Survey_Code), collapse = ", "))
    message("Species       : ", length(unique(result$Species_Code)))
    dt_tab <- table(result$Survey_Code, result$DataType)
    dt_tab <- dt_tab[rowSums(dt_tab) > 0, , drop = FALSE]
    if (nrow(dt_tab) > 0) {
      message("DataType per survey (haul-species rows):")
      print(dt_tab)
    }
  }
  result
}
