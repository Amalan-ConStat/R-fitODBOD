#' Gamma distribution
#'
#' These functions provide the ability for generating probability density values,
#' cumulative probability density values and moment about zero values for
#' Gamma Distribution bounded between [0,1]
#'
#' @usage
#' dGAMMA(p,c,l)
#'
#' @param p                vector of probabilities.
#' @param c                single value for shape parameter c.
#' @param l                single value for shape parameter l.
#'
#' @details
#' The probability density function and cumulative density function of a
#' unit bounded Gamma distribution with random variable P are given by
#'
#' \deqn{g_{P}(p) = \frac{ c^l p^{c-1}}{\gamma(l)} [ln(1/p)]^{l-1} } ;    \eqn{0 \le p \le 1}
#' \deqn{G_{P}(p) = \frac{ Ig(l,cln(1/p))}{\gamma(l) } ; \eqn{0 \le p \le 1}
#' \deqn{l,c > 0}
#'
#' The mean the variance are denoted by
#' \deqn{E[P] = (\frac{c}{c+1})^l }
#' \deqn{var[P] = (\frac{c}{c+2})^l - (\frac{c}{c+1})^{2l} }
#'
#' The moments about zero is denoted as
#' \deqn{E[P^r]=(\frac{c}{c+r})^l }
#' \eqn{r = 1,2,3,...}
#'
#' Defined as \eqn{\gamma(l)= } is the gamma function
#' Defined as \eqn{Ig(l,cln(1/p))= \int_0^{cln(1/p)} t^{l-1} e^{-t}dt } is the Lower incomplete gamma function
#'
#'
#' \strong{NOTE} : If input parameters are not in given domain  conditions necessary error
#' messages will be provided to go further.
#'
#'  @return
#'  The output of \code{dGAMMA} gives a list format consisting
#'
#'  \code{pdf}                   probability density values in vector form.
#'
#'  \code{mean}                  mean of the Gamma distribution.
#'
#'  \code{var}                   variance of Gamma distribution.
#'
#'  @references
#'
#'
#'
#'
#'
#'  @seealso
#'  \code{\link[stats]{dgamma}}
#'
#'  \code{\link[base]{gamma}}
#'
#' @examples
#' plotting the random variables and probability values
#' col<-rainbow(4)
#' a<-c(1,2,5,10)
#' plot(0,0,main="Probability density graph",xlab="Random variable",ylab="Probability density values",
#' xlim = c(0,1),ylim = c(0,4))
#' for (i in 1:4)
#' {
#' lines(seq(0,1,by=0.01),dGAMMA(seq(0,1,by=0.01),a[i],a[i])$pdf,col = col[i])
#' }
#'
#' dGAMMA(seq(0,1,by=0.01),2,3)$pdf   #extracting the pdf values
#' dGAMMA(seq(0,1,by=0.01),2,3)$mean  #extracting the mean
#' dGAMMA(seq(0,1,by=0.01),2,3)$var   #extracting the variance
#'
#' #plotting the random variables and cumulative probability values
#' col<-rainbow(4)
#' a<-c(1,2,5,10)
#' plot(0,0,main="Cumulative density graph",xlab="Random variable",ylab="Cumulative density values",
#' xlim = c(0,1),ylim = c(0,1))
#' for (i in 1:4)
#' {
#' lines(seq(0,1,by=0.01),pGAMMAseq(0,1,by=0.01),a[i],a[i]),col = col[i])
#' }
#'
#' pGAMMA(seq(0,1,by=0.01),2,3)   #acquiring the cumulative probability values
#' mazGAMMA(1.4,3,2)              #acquiring the moment about zero values
#' mazGAMMA(2,3,2)-mazGAMMA(1,3,2)^2 #acquiring the variance for a=3,b=2
#'
#' #only the integer value of moments is taken here because moments cannot be decimal
#' mazGAMMA(1.9,5.5,6)
#'
#' @export
dGAMMA<-function(p,c,l)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(p,c,l))) | any(is.infinite(c(p,c,l))) | any(is.nan(c(p,c,l))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if shape parameters are greater than zero, if not providing error message and
    #stopping the function progress
    if(c <= 0 | l <= 0)
    {
      stop("Shape parameters cannot be less than or equal to zero")
    }
    else
    {
      ans<-NULL
      #for each input values in the vector necessary calculations and conditions are applied
      for (i in 1:length(p))
      {
        if(p[i]<0 |p[i]>1)
        {
          stop("Invalid values in the input")
        }
        else
        {
          ans[i]<-(c^l* p[i]^(c-1)*(log(1/p[i]))^(l-1))/gamma(l)
        }
      }
    }
    mean<-(c/(c+1))^l             #according to theory the mean value
    variance<-(c/(c+2))^l - (c/(c+1))^(2*l)     #according to theory the variance value
    # generating an output in list format consisting pdf,mean and variance
    output<-list("pdf"=ans,"mean"=mean,"var"=variance)
    return(output)
  }
}

#' @rdname dGAMMA
#' @export
pGAMMA<-function(p,c,l)
{

}

#' @rdname dGAMMA
#' @export
mazGAMMA<-function(r,c,l)
{

}

#' @export
dGamma1Bin<-function()
{

}

#' @export
pGamma1Bin<-function()
{

}

#' @export
NegLLGamma1Bin<-function()
{

}

#' @export
EstMLEGamma1Bin<-function()
{

}

#' @export
fitGamma1Bin<-function()
{

}

#' @export
dGamma2Bin<-function()
{

}

#' @export
pGamma2Bin<-function()
{

}

#' @export
NegLLGamma2Bin<-function()
{

}

#' @export
EstMLEGamma2Bin<-function()
{

}

#' @export
fitGamma2Bin<-function()
{

}
