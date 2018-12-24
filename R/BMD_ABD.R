#' @method AIC fit
#' @export
AIC.fit<-function(object,...)
{
  return(object$AIC)
}

#' @method residuals fit
#' @export
residuals.fit<-function(object,...)
{
  return(object$obs.freq-object$exp.freq)
}

#' @method fitted fit
#' @export
fitted.fit<-function(object,...)
{
  return(object$exp.freq)
}
