#'
#' @export
Overdispersion<-function(object)
{
  UseMethod("Overdispersion",object)
}

#' @method Overdispersion fitTB
#' @export
Overdispersion.fitTB<-function(object)
{
  return(object$over.dis.para)
}

#' @method Overdispersion fitBB
#' @export
Overdispersion.fitBB<-function(object)
{
  return(object$over.dis.para)
}

#' @method Overdispersion fitKB
#' @export
Overdispersion.fitKB<-function(object)
{
  return(object$over.dis.para)
}

#' @method Overdispersion fitGB
#' @export
Overdispersion.fitGB<-function(object)
{
  return(object$over.dis.para)
}

#' @method Overdispersion fitMB
#' @export
Overdispersion.fitMB<-function(object)
{
  return(object$over.dis.para)
}
