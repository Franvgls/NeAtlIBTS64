#' Is there enough history to fit a gear model without the last year? (internal)
#'
#' The gearPlotHH.* functions fit their models on all years but the last one,
#' which is then drawn on top as the year being checked. When a group (e.g. a
#' sweep length) has no data before the last year, as happens right after a
#' gear change, that fit would be empty and fail, so the group must be fitted
#' with all its years instead. This helper makes that decision and tells the user.
#' @param d data.frame with the hauls of one group (all hauls, one sweep length...)
#' @param years years requested by the user; the last one is the year being checked
#' @param resp names of the columns that must be valid (> 0) for a haul to count
#' @param label name of the group, used in the message
#' @param min.n minimum number of valid hauls before the last year needed to fit on the history only
#' @return TRUE if the model can be fitted without the last year, FALSE (with a message) if not
#' @keywords internal
gearHasHist<-function(d,years,resp,label="all data",min.n=5) {
  last<-years[length(years)]
  ok<-rep(TRUE,nrow(d))
  for (v in resp) ok<-ok & !is.na(d[[v]]) & d[[v]]>0
  has<-sum(ok & d$Year!=last,na.rm=TRUE)>=min.n
  if (!has) message("gearPlotHH: fewer than ",min.n," valid hauls before ",last," for ",label,
                    "; its model is fitted with all years available (",last," included), so there is no history to compare ",last," against.")
  has
}
