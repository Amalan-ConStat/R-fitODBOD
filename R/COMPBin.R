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
#' pCOMPBin(x,n,p,v)
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
#' The output of \code{pCOMPBin} gives  cumulative probability  values in vector form.
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

#' Negative Log Likelihood value of COM Poisson Binomial distribution
#'
#' This function will calculate the negative log likelihood value when the vector of binomial random
#' variables and vector of corresponding frequencies are given with the input parameters.
#'
#' @usage
#' NegLLCOMPBin(x,freq,p,v)
#'
#' @param x                 vector of binomial random variables
#' @param freq              vector of frequencies
#' @param p                 single value for probability of success
#' @param v                 single value for  v
#'
#' @details
#' \deqn{freq \ge 0}
#' \deqn{x = 0,1,2,..}
#' \deqn{0 < p < 1}
#' \deqn{-\infty < v < +\infty}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further
#'
#' @return
#' The output of \code{NegLLCOMPBin} will produce a single numeric value
#'
#' @references
#' Borges, P., Rodrigues, J., Balakrishnan, N. and Bazan, J., 2014. A COM-Poisson type
#' generalization of the binomial distribution and its properties and applications.
#' Statistics & Probability Letters, 87, pp.158-166.
#'
#' Available at: \url{http://conteudo.icmc.usp.br/CMS/Arquivos/arquivos_enviados/BIBLIOTECA_113_NSE_90.pdf}
#'
#' @examples
#' No.D.D=0:7         #assigning the random variables
#' Obs.fre.1=c(47,54,43,40,40,41,39,95)      #assigning the corresponding frequencies
#' NegLLCOMPBin(No.D.D,Obs.fre.1,.5,.03)     #acquiring the negative log likelihood value
#'
#' @export
NegLLCOMPBin<-function(x,freq,p,v)
{
  #constructing the data set using the random variables vector and frequency vector
  n<-max(x)
  data<-rep(x,freq)
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,freq,p,v))) | any(is.infinite(c(x,freq,p,v))) |
     any(is.nan(c(x,freq,p,v))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if any of the random variables of frequencies are less than zero if so
    #creating a error message as well as stopping the function progress
    if(any(c(x,freq) < 0) )
    {
      stop("Binomial random variable or frequency values cannot be negative")
    }
    #checking the probability value is inbetween zero and one or covariance is greater than zero
    else if( p <= 0 | p >= 1)
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
        j<-1:sum(freq)
        term1<-v*sum(log(choose(n,data[j])))
        term2<-log(p)*sum(data[j])
        term3<-log(1-p)*sum(n-data[j])
        term4<-sum(freq)*log(sum(((choose(n,y))^v)*(p^y)*((1-p)^(n-y))))
        COMPBinLL<-term1+term2+term3-term4
        #calculating the negative log likelihood value and representing as a single output value
        return(-COMPBinLL)
      }
    }
  }
}

#' Estimating the probability of success and v parameter for COM Poisson Binomial
#' Distribution
#'
#' The function will estimate the probability of success and v parameter using the maximum log
#' likelihood method for the COM Poisson Binomial distribution when the binomial random
#' variables and corresponding frequencies are given
#'
#' @usage
#' EstMLECOMPBin(x,freq,p,v)
#'
#'
#' @param x       vector of binomial random variables
#' @param freq    vector of frequencies
#' @param p       single value for probability of success
#' @param v       single value for v
#'
#' @details
#' \deqn{x = 0,1,2,...}
#' \deqn{freq \ge 0}
#' \deqn{0 < p < 1}
#' \deqn{-\infty < v < +\infty}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further
#'
#' @return
#' \code{EstMLECOMPBin} here is used as a input parameter for the \code{mle2} function of \pkg{bbmle} package
#' therefore output is of class of mle2.
#'
#' @references
#' Borges, P., Rodrigues, J., Balakrishnan, N. and Bazan, J., 2014. A COM-Poisson type
#' generalization of the binomial distribution and its properties and applications.
#' Statistics & Probability Letters, 87, pp.158-166.
#'
#' Available at: \url{http://conteudo.icmc.usp.br/CMS/Arquivos/arquivos_enviados/BIBLIOTECA_113_NSE_90.pdf}
#'
#'
#' @examples
#' No.D.D=0:7               #assigning the random variables
#' Obs.fre.1=c(47,54,43,40,40,41,39,95)     #assigning the corresponding frequencies
#'
#' #estimating the parameters using maximum log likelihood value and assigning it
#' parameters=suppressWarnings(bbmle::mle2(EstMLECOMPBin,start = list(p=0.5,v=0.1),
#'                        data = list(x=No.D.D,freq=Obs.fre.1)))
#' bbmle::coef(parameters)           #extracting the parameters
#'
#' @export
EstMLECOMPBin<-function(x,freq,p,v)
{
  #with respective to using bbmle package function mle2 there is no need impose any restrictions
  #therefor the output is directly a single numeric value for the negative log likelihood value of
  #COM Poisson Binomial distribution
  value<-NULL
  n<-max(x)
  y<-0:n
  data<-rep(x,freq)
  j<-1:sum(freq)
  term1<-v*sum(log(choose(n,data[j])))
  term2<-log(p)*sum(data[j])
  term3<-log(1-p)*sum(n-data[j])
  term4<-sum(freq)*log(sum(((choose(n,y))^v)*(p^y)*((1-p)^(n-y))))
  COMPBinLL<-term1+term2+term3-term4
  return(-COMPBinLL)
}

#' Fitting the COM Poisson Binomial Distribution when binomial
#' random variable, frequency, probability of success and v parameter are given
#'
#' The function will fit the COM Poisson binomial Distribution
#' when random variables, corresponding frequencies, probability of success and v parameter are given.
#' It will provide the expected frequencies, chi-squared test statistics value, p value,
#' and degree of freedom so that it can be seen if this distribution fits the data.
#'
#' @usage
#' fitCOMPBin(x,obs.freq,p,v,print)
#'
#' @param x                  vector of binomial random variables
#' @param obs.freq           vector of frequencies
#' @param p                  single value for probability of success
#' @param v                  single value for v
#' @param print              logical value for print or not
#'
#' @details
#' \deqn{obs.freq \ge 0}
#' \deqn{x = 0,1,2,..}
#' \deqn{0 < p < 1}
#' \deqn{-\infty < v < +\infty}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further
#'
#' @return
#' The output of \code{fitCOMPBin} gives a list format consisting
#'
#' \code{bin.ran.var} binomial random variables
#'
#' \code{obs.freq} corresponding observed frequencies
#'
#' \code{exp.freq} corresponding expected frequencies
#'
#' \code{statistic} chi-squared test statistics
#'
#' \code{df} degree of freedom
#'
#' \code{p.value} probability value by chi-squared test statistic
#'
#'
#' @references
#' Borges, P., Rodrigues, J., Balakrishnan, N. and Bazan, J., 2014. A COM-Poisson type
#' generalization of the binomial distribution and its properties and applications.
#' Statistics & Probability Letters, 87, pp.158-166.
#'
#' Available at: \url{http://conteudo.icmc.usp.br/CMS/Arquivos/arquivos_enviados/BIBLIOTECA_113_NSE_90.pdf}
#'
#'
#' @examples
#' No.D.D=0:7                    #assigning the random variables
#' Obs.fre.1=c(47,54,43,40,40,41,39,95)      #assigning the corresponding frequencies
#'
#' #estimating the parameters using maximum log likelihood value and assigning it
#' parameters=suppressWarnings(bbmle::mle2(EstMLECOMPBin,start = list(p=0.5,v=0.050),
#'            data = list(x=No.D.D,freq=Obs.fre.1)))
#' pCOMPBin=bbmle::coef(parameters)[1]
#' vCOMPBin=bbmle::coef(parameters)[2]
#' #fitting when the random variable,frequencies,probability and v parameter are given
#' fitCOMPBin(No.D.D,Obs.fre.1,pCOMPBin,vCOMPBin)
#' #extracting the expected frequencies
#' fitCOMPBin(No.D.D,Obs.fre.1,pCOMPBin,vCOMPBin,FALSE)$exp.freq
#' @export
fitCOMPBin<-function(x,obs.freq,p,v,print=T)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,obs.freq,p,v))) | any(is.infinite(c(x,obs.freq,p,v))) |
     any(is.nan(c(x,obs.freq,p,v))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #for given random variables and parameters calculating the estimated probability values
    est.prob<-dCOMPBin(x,max(x),p,v)$pdf
    #using the estimated probability values the expected frequencies are calculated
    exp.freq<-round((sum(obs.freq)*est.prob),2)
    #chi-squared test statistics is calculated with observed frequency and expected frequency
    statistic<-sum(((obs.freq-exp.freq)^2)/exp.freq)
    #degree of freedom is calculated
    df<-length(x)-3
    #p value of chi-squared test statistic is calculated
    p.value<-1-stats::pchisq(statistic,df)
    #all the above information is mentioned as a message below
    #and if the user wishes they can print or not to
    if(print==TRUE)
    {
      cat("\nChi-squared test for COM Poisson Binomial Distribution\n\n
                 Observed Frequency : ",obs.freq,"\n
                 expected Frequency : ",exp.freq,"\n
                 X-squared =",round(statistic,4),"df =",df,"  p-value =",round(p.value,4),"\n")
    }
    #checking if any of the expected frequencies are less than five and greater than zero, if so
    #a warning message is provided in interpreting the results
    if(min(exp.freq)<5 && min(exp.freq) > 0)
    {
      warning("Chi-squared approximation may be doubtful because expected frequency is less than 5")
    }
    #checking if df is less than or equal to zero
    if(df<0 | df==0)
    {
      warning("Degrees of freedom cannot be less than or equal to zero")
    }
    #checking if expected frequency is zero, if so providing a warning message in interpreting
    #the results
    if(min(exp.freq)==0)
    {
      warning("Chi-squared approximation is not suitable because expected frequency approximates to zero")
    }
    #the final output is in a list format containing the calculated values
    final<-list("bin.ran.var"=x,"obs.freq"=obs.freq,"exp.freq"=exp.freq,"statistic"=round(statistic,4),
                "df"=df,"p.value"=round(p.value,4))
  }
}
