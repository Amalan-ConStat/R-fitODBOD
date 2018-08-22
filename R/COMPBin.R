#' COM Poisson Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the COM Poisson  Binomial Distribution.
#'
#' @usage
#' dCOMPBin(x,n,p,v)
#'
#' @param x        vector of binomial random variables
#' @param n        single value for no of binomial trials
#' @param p        single value for probability of success
#' @param v        single value for  v
#'
#' @details
#' The probability function and cumulative function can be constructed and are denoted below
#'
#' The cumulative probability function is the summation of probability function values
#'
#' \deqn{P_{COMPBin}(x) = \frac{{n \choose x}^v p^x (1-p)^{n-x}}{\sum_{j=0}^{n} {n \choose j}^v p^j (1-p)^{(n-j)}}}
#' \deqn{x = 0,1,2,3,...n}
#' \deqn{n = 1,2,3,...}
#' \deqn{0 < p < 1}
#' \deqn{-\infty < v < +\infty }
#'
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further
#'
#' @return
#' The output of \code{dCOMPBin} gives a list format consisting
#'
#' \code{pdf}           probability function values in vector form
#'
#' \code{mean}          mean of COM Poisson  Binomial Distribution
#'
#' \code{var}           variance of COM Poisson  Binomial Distribution
#'
#'
#' @references
#' Extracted from
#'
#' Borges, P., Rodrigues, J., Balakrishnan, N. and Bazan, J., 2014. A COM-Poisson type
#' generalization of the binomial distribution and its properties and applications.
#' Statistics & Probability Letters, 87, pp.158-166.
#'
#' Available at: \url{http://conteudo.icmc.usp.br/CMS/Arquivos/arquivos_enviados/BIBLIOTECA_113_NSE_90.pdf}
#'
#' @examples
#' #plotting the random variables and probability values
#' col<-rainbow(5)
#' a<-c(0.58,0.59,0.6,0.61,0.62)
#' b<-c(0.022,0.023,0.024,0.025,0.026)
#' plot(0,0,main="COM Poisson Binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,0.5))
#' for (i in 1:5)
#' {
#' lines(0:10,dCOMPBin(0:10,10,a[i],b[i])$pdf,col = col[i],lwd=2.85)
#' points(0:10,dCOMPBin(0:10,10,a[i],b[i])$pdf,col = col[i],pch=16)
#' }
#' dCOMPBin(0:10,10,0.58,0.022)$pdf      #extracting the pdf values
#' dCOMPBin(0:10,10,0.58,0.022)$mean     #extracting the mean
#' dCOMPBin(0:10,10,0.58,0.022)$var      #extracting the variance
#'
#' #plotting the random variables and cumulative probability values
#' col<-rainbow(5)
#' a<-c(0.58,0.59,0.6,0.61,0.62)
#' b<-c(0.022,0.023,0.024,0.025,0.026)
#' plot(0,0,main="COM Poisson Binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,1))
#' for (i in 1:5)
#' {
#' lines(0:10,pCOMPBin(0:10,10,a[i],b[i]),col = col[i],lwd=2.85)
#' points(0:10,pCOMPBin(0:10,10,a[i],b[i]),col = col[i],pch=16)
#' }
#' pCOMPBin(0:10,10,0.58,0.022)      #acquiring the cumulative probability values
#'
#' @export
dCOMPBin<-function(x,n,p,v)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,n,p,v))) | any(is.infinite(c(x,n,p,v))) | any(is.nan(c(x,n,p,v))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if at any chance the binomial random variable is greater than binomial trial value
    #if so providing an error message and stopping the function progress
    if(max(x) > n )
    {
      stop("Binomial random variable cannot be greater than binomial trial value")
    }
    #checking if any random variable or trial value is negative if so providig an error message
    #and stopping the function progress
    else if(any(x<0) | n<0)
    {
      stop("Binomial random variable or binomial trial value cannot be negative")
    }
    else
    {
      #checking the probability value is inbetween zero and one
      if( p <= 0 | p >= 1 )
      {
        stop("Probability value doesnot satisfy conditions")
      }
      else
      {
        value<-NULL
        #constructing the probability values for all random variables
        y<-0:n
        value1<-NULL
        for(i in 1:length(y))
        {
          value1[i]<-(((choose(n,y[i]))^v)*(p^y[i])*((1-p)^(n-y[i])))/
                        (sum(((choose(n,y))^v)*(p^y)*((1-p)^(n-y))))
        }
        check1<-sum(value1)

        #checking if the sum of all probability values leads upto one
        #if not providing an error message and stopping the function progress
        if(check1 < 0.9999 | check1 >1.0001 | any(value1 < 0) | any(value1 >1))
        {
          stop("Input parameter combinations of probability of success and covariance does
               not create proper probability function")
        }
        else
        {
          #for each random variable in the input vector below calculations occur
          for (i in 1:length(x))
          {
            value[i]<-(((choose(n,x[i]))^v)*(p^x[i])*((1-p)^(n-x[i])))/
                              (sum(((choose(n,y))^v)*(p^y)*((1-p)^(n-y))))
          }
          mean<-sum(value1*y)
          variance<-sum((y^2)*value1)-mean^2
          # generating an output in list format consisting pdf,mean and variance
          output<-list("pdf"=value,"mean"=mean,"var"=variance)
          return(output)
        }
      }
    }
  }
}

#' COM Poisson Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the COM Poisson  Binomial Distribution.
#'
#' @usage
#' dCOMPBin(x,n,p,v)
#'
#' @param x        vector of binomial random variables
#' @param n        single value for no of binomial trials
#' @param p        single value for probability of success
#' @param v        single value for  v
#'
#' @details
#' The probability function and cumulative function can be constructed and are denoted below
#'
#' The cumulative probability function is the summation of probability function values
#'
#' \deqn{P_{COMPBin}(x) = \frac{{n \choose x}^v p^x (1-p)^{n-x}}{\sum_{j=0}^{n} {n \choose j}^v p^j (1-p)^{(n-j)}}}
#' \deqn{x = 0,1,2,3,...n}
#' \deqn{n = 1,2,3,...}
#' \deqn{0 < p < 1}
#' \deqn{-\infty < v < +\infty }
#'
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further
#'
#' @return
#' The output of \code{dCOMPBin} gives a list format consisting
#'
#' \code{pdf}           probability function values in vector form
#'
#' \code{mean}          mean of COM Poisson  Binomial Distribution
#'
#' \code{var}           variance of COM Poisson  Binomial Distribution
#'
#'
#' @references
#' Extracted from
#'
#' Borges, P., Rodrigues, J., Balakrishnan, N. and Bazan, J., 2014. A COM-Poisson type
#' generalization of the binomial distribution and its properties and applications.
#' Statistics & Probability Letters, 87, pp.158-166.
#'
#' Available at: \url{http://conteudo.icmc.usp.br/CMS/Arquivos/arquivos_enviados/BIBLIOTECA_113_NSE_90.pdf}
#'
#' @examples
#' #plotting the random variables and probability values
#' col<-rainbow(5)
#' a<-c(0.58,0.59,0.6,0.61,0.62)
#' b<-c(0.022,0.023,0.024,0.025,0.026)
#' plot(0,0,main="COM Poisson Binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,0.5))
#' for (i in 1:5)
#' {
#' lines(0:10,dCOMPBin(0:10,10,a[i],b[i])$pdf,col = col[i],lwd=2.85)
#' points(0:10,dCOMPBin(0:10,10,a[i],b[i])$pdf,col = col[i],pch=16)
#' }
#' dCOMPBin(0:10,10,0.58,0.022)$pdf      #extracting the pdf values
#' dCOMPBin(0:10,10,0.58,0.022)$mean     #extracting the mean
#' dCOMPBin(0:10,10,0.58,0.022)$var      #extracting the variance
#'
#' #plotting the random variables and cumulative probability values
#' col<-rainbow(5)
#' a<-c(0.58,0.59,0.6,0.61,0.62)
#' b<-c(0.022,0.023,0.024,0.025,0.026)
#' plot(0,0,main="COM Poisson Binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,1))
#' for (i in 1:5)
#' {
#' lines(0:10,pCOMPBin(0:10,10,a[i],b[i]),col = col[i],lwd=2.85)
#' points(0:10,pCOMPBin(0:10,10,a[i],b[i]),col = col[i],pch=16)
#' }
#' pCOMPBin(0:10,10,0.58,0.022)      #acquiring the cumulative probability values
#'
#' @export
pCOMPBin<-function(x,n,p,v)
{
  ans<-NULL
  #for each binomial random variable in the input vector the cumulative proability function
  #values are calculated
  for(i in 1:length(x))
  {
    j<-0:x[i]
    ans[i]<-sum(dCOMPBin(j,n,p,v)$pdf)
  }
  #generating an ouput vector cumulative probability function values
  return(ans)
}

NegLLCOMPBin<-function(x,freq,p,v)
{

}

EstMLECOMPBin<-function(x,freq,p,v)
{

}

fitCOMPBin<-function(x,obs.freq,p,v,print=T)
{

}
