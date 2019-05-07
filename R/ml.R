#' @method AIC ml
#' @export
AIC.ml<-function(object,...)
{
  AICvalue<-object$AIC
  return(AICvalue)
}

