#' Confidence/prediction band for gear-relationship models (internal)
#'
#' Computes a statistically valid pointwise band for the single-predictor lm/nls models
#' used across the gearPlot* functions, replacing the previous approach of combining the
#' independent confint() bounds of each coefficient (which does not correspond to any
#' well-defined confidence level for the fitted curve). For lm objects it delegates to
#' predict.lm. For nls objects fitted as y~a+b*log(x) it applies the delta method
#' (gradient (1, log(x)) on vcov(model)), since predict.nls does not support interval=
#' in every R version this package needs to support.
#' @param model an lm or nls object with a single continuous predictor
#' @param xname name of the predictor variable in model, as a character string
#' @param newx numeric vector of predictor values at which to evaluate the band
#' @param level the confidence/prediction level (e.g. .95)
#' @param type "prediction" (range for a new individual observation, does not shrink with sample size,
#' appropriate for flagging hauls outside the expected gear geometry), "confidence" (range for the mean
#' curve, shrinks as data accumulates) or "both"
#' @return a list with fit, lwr, upr (prediction, when type is "prediction" or "both") and
#' lwr.conf, upr.conf (confidence, when type is "confidence" or "both")
#' @keywords internal
gearBand<-function(model,xname,newx,level=.95,type=c("prediction","confidence","both")) {
  type<-match.arg(type)
  nd<-setNames(data.frame(newx),xname)
  out<-list()
  if (inherits(model,"lm") & !inherits(model,"nls")) {
    out$fit<-as.numeric(predict(model,newdata=nd))
    if (type %in% c("prediction","both")) {
      pr<-predict(model,newdata=nd,interval="prediction",level=level)
      out$lwr<-pr[,"lwr"]; out$upr<-pr[,"upr"]
    }
    if (type %in% c("confidence","both")) {
      cr<-predict(model,newdata=nd,interval="confidence",level=level)
      out$lwr.conf<-cr[,"lwr"]; out$upr.conf<-cr[,"upr"]
    }
  } else if (inherits(model,"nls")) {
    b<-coef(model)
    V<-vcov(model)
    g<-cbind(1,log(newx))
    fit<-as.numeric(g %*% b)
    se.fit<-sqrt(rowSums((g %*% V) * g))
    df<-summary(model)$df[2]
    sigma<-summary(model)$sigma
    tval<-qt(1-(1-level)/2,df)
    out$fit<-fit
    if (type %in% c("prediction","both")) {
      se.pred<-sqrt(sigma^2+se.fit^2)
      out$lwr<-fit-tval*se.pred; out$upr<-fit+tval*se.pred
    }
    if (type %in% c("confidence","both")) {
      out$lwr.conf<-fit-tval*se.fit; out$upr.conf<-fit+tval*se.fit
    }
  } else stop("gearBand: model must be an lm or nls object")
  out
}

#' Build the on-plot CI/PI label text for gear plots (internal)
#'
#' Formats the level and type actually used to draw a band, so the plotted
#' label always reflects what \code{\link{gearBand}} computed instead of a
#' hardcoded string.
#' @param level the confidence/prediction level (e.g. .95)
#' @param type "prediction", "confidence" or "both", as passed to \code{\link{gearBand}}
#' @return a single string, e.g. "95% prediction interval"
#' @keywords internal
gearIntLabel<-function(level,type=c("prediction","confidence","both")) {
  type<-match.arg(type)
  pct<-round(level*100)
  switch(type,
         prediction=paste0(pct,"% prediction interval"),
         confidence=paste0(pct,"% confidence interval"),
         both=paste0(pct,"% prediction & confidence interval"))
}
