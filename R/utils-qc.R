#' Label hauls with missing values for QC warnings (internal)
#'
#' Builds a compact "Country-HaulNo" list for the warnings of the qcHauls*
#' functions, adding which of \code{cols} are missing in each haul.
#' @param df data.frame with the affected hauls (HH format)
#' @param cols optional columns to report as missing for each haul
#' @return a single string, e.g. "ES-5 (HaulLat/HaulLong), ES-12 (ShootLong)"
#' @keywords internal
posMissingLabel <- function(df, cols = NULL) {
  lab <- paste0(df$Country, "-", df$HaulNo)
  if (!is.null(cols)) {
    miss <- is.na(as.matrix(df[, cols, drop = FALSE]))
    lab <- paste0(lab, " (", apply(miss, 1, function(r) paste(cols[r], collapse = "/")), ")")
  }
  paste(lab, collapse = ", ")
}
