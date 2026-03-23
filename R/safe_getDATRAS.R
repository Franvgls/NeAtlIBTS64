#' Function safe_getDATRAS gets data from DATRAS and shows original messages to the users but inside R and shiny
#' 
#' Downloads data from DATRAS or sends its errors
#' @param Survey: either the Survey to be downloaded from DATRAS (see details), or a data frame with the HH information with  the DATRAS HH format  and the years and quarter selected in years and quarter 
#' @param years: years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in steelblue2, it depends on the order of years, not the actual chronological year.
#' @param quarter: the quarter of the survey to be ploted
#' @param context: evaluates if running within a normal script or within shiny app
#' @family quality control
#' @export
#' @return data.frame withtthe DATRAS data for the selected survey, year and quaerter
#' @details Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @examples safe_getDATRAS("SP-NORTH",2022,4,context="console")
#' @export
safe_getDATRAS <- function(datatype = "HH", survey, years, quarter,
                           context = c("console","shiny")) {
  context <- match.arg(context)
  
  res <- suppressMessages(
    icesDatras::getDATRAS(datatype, survey, years, quarters)
  )
  
  if (identical(res, FALSE)) {
    msg <- paste0("Survey ", survey,
                  " with Year ", years,
                  " and Quarter ", paste(quarters, collapse = ","),
                  " does not exist.\n",
                  "Check available options with:\n",
                  "icesDatras::getDATRAS('", datatype, "', '", survey,
                  "', ", years, ", 1:4)")
    
    if (context == "console") {
      stop(msg, call. = FALSE)
    } else {
      shiny::validate(shiny::need(FALSE, msg))
    }
  }
  
  res
}
