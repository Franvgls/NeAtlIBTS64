#' Wrap icesDatras::getDATRAS with friendly errors
#'
#' Downloads data from DATRAS and handles missing data gracefully, both from
#' the console (stop with a helpful message) and from Shiny (validate+need).
#' `shiny` is only required when `context = "shiny"`; it is listed as Suggests.
#'
#' @param datatype Record type: "HH" (default), "HL" or "CA".
#' @param survey Survey to download (see details).
#' @param years Year or vector of years to download, must be available in DATRAS.
#' @param quarter Quarter or vector of quarters of the survey.
#' @param context Execution context: "console" (raises stop) or "shiny"
#'   (uses shiny::validate). Defaults to "console".
#' @family quality control
#' @return data.frame with DATRAS data for the selected survey, years and
#'   quarter(s).
#' @details Surveys available in DATRAS: SWC-IBTS, ROCKALL, NIGFS, IE-IGFS,
#'   SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS, SP-ARSA and others.
#' @examples
#' \dontrun{
#' safe_getDATRAS("HH", "SP-NORTH", 2022, 4, context = "console")
#' }
#' @export
safe_getDATRAS <- function(datatype = "HH", survey, years, quarter,
                           context = c("console", "shiny")) {
  context <- match.arg(context)

  res <- suppressMessages(
    icesDatras::getDATRAS(datatype, survey, years, quarter)
  )

  if (is.null(res) || !is.data.frame(res) || nrow(res) == 0) {
    msg <- paste0(
      "No DATRAS data found for:\n",
      "  record:  ", datatype, "\n",
      "  survey:  ", survey, "\n",
      "  years:   ", paste(years, collapse = ", "), "\n",
      "  quarter: ", paste(quarter, collapse = ", "), "\n",
      "Check available years/quarters with:\n",
      "  icesDatras::getSurveyYearList('", survey, "')\n",
      "  icesDatras::getSurveyYearQuarterList('", survey, "', <year>)"
    )
    if (context == "console") {
      stop(msg, call. = FALSE)
    } else {
      if (!requireNamespace("shiny", quietly = TRUE)) {
        stop(msg, call. = FALSE)   # fallback si shiny no está instalado
      }
      shiny::validate(shiny::need(FALSE, msg))
    }
  }

  res
}
