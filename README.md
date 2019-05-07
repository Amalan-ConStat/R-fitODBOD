R-fitODBOD
================

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version-last-release/fitODBOD)](https://cran.r-project.org/package=fitODBOD)
[![packageversion](https://img.shields.io/badge/Package%20version-1.2.0-orange.svg?style=flat-square)](commits/master)
[![Rdoc](http://www.rdocumentation.org/badges/version/fitODBOD)](http://www.rdocumentation.org/packages/fitODBOD)

![downloads](http://cranlogs.r-pkg.org/badges/grand-total/fitODBOD)
![downloads](https://cranlogs.r-pkg.org/badges/fitODBOD)
![downloads](http://cranlogs.r-pkg.org/badges/last-week/fitODBOD)

[![Licence](https://img.shields.io/badge/licence-GPL--2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)
[![GitHub
license](https://img.shields.io/github/license/Amalan-ConStat/R-fitODBOD.svg?style=popout)](https://github.com/Amalan-ConStat/R-fitODBOD/blob/master/LICENSE)

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![GitHub
issues](https://img.shields.io/github/issues/Amalan-ConStat/R-fitODBOD.svg?style=popout)](https://github.com/Amalan-ConStat/R-fitODBOD/issues)

[![rpackages.io
rank](http://www.rpackages.io/badge/fitODBOD.svg)](http://www.rpackages.io/package/fitODBOD)

# fitODBOD <img src="man/figures/logo.png" align="right" alt="" width="150" />

## How to engage with “fitODBOD” the first time ?

``` r
## Installing the package from GitHub
devtools::install_github("Amalan-ConStat/R-fitODBOD")

## Installing the package from CRAN
install.packages("fitODBOD")
```

## Key Phrases

  - BOD (Binomial Outcome Data)
  - Over Dispersion
  - Under Dispersion
  - BMD (Binomial Mixture Distributions)
  - ABD (Alternate Binomial Distributions)
  - PMF (Probability Mass Function)
  - CPMF (Cumulative Probability Mass Function)

## What does “fitODBOD” ?

You can understand BMD & ABD with PMF & CPMF. Further, BOD can be
modeled using these
Distributions

## Distributions

| Alternate Binomial Distributions                | Binomial Mixture Distributions                                   |
| :---------------------------------------------- | :--------------------------------------------------------------- |
| 1.Additive Binomial Distribution                | 1.Uniform Binomial Distribution                                  |
| 2.Beta-Correlated Binomial Distribution         | 2.Triangular Binomial Distribution                               |
| 3.COM Poisson Binomial Distribution             | 3.Beta-Binomial Distribution                                     |
| 4.Correlated Binomial Distribution              | 4.Kumaraswamy Binomial Distribution                              |
| 5.Multiplicative Binomial Distribution          | 5.Gaussian Hypergeometric Generalized Beta-Binomial Distribution |
| 6.Lovinson Multiplicative Binomial Distribution | 6.McDonald Generalized Beta-Binomial Distribution                |
|                                                 | 7.Gamma Binomial Distribution                                    |
|                                                 | 8.Grassia II Binomial Distribution                               |

## Just an example

### Modelling BOD using Beta-Binomial Distribution

``` r
library(fitODBOD)

#Looking at the BOD
Alcohol_data
```

    ##   Days week1 week2
    ## 1    0    47    42
    ## 2    1    54    47
    ## 3    2    43    54
    ## 4    3    40    40
    ## 5    4    40    49
    ## 6    5    41    40
    ## 7    6    39    43
    ## 8    7    95    84

Hypothesis to check if above data follows Binomial Distribution

H<sub>0</sub> : The Data follows the Binomial Distribution.

H<sub>1</sub> : The Data does not follow the Binomial
Distribution.

``` r
#Checking if the above data  of Days and week2 follows Binomial Distribution
fitB<-fitBin(Alcohol_data$Days,Alcohol_data$week2)

print(fitB)
```

    ## Call: 
    ## fitBin(x = Alcohol_data$Days, obs.freq = Alcohol_data$week2)
    ## 
    ## Chi-squared test for Binomial Distribution 
    ##  
    ##       Observed Frequency :  42 47 54 40 49 40 43 84 
    ##  
    ##       expected Frequency :  1.66 13.79 49.19 97.48 115.89 82.67 32.76 5.56 
    ##  
    ##       estimated probability value : 0.5431436 
    ##  
    ##       X-squared : 2265.111   ,df : 6   ,p-value : 0

According to p-value= 0, H<sub>0</sub> is rejected at 5% significance
level.

``` r
#Estimating the parameters a and b using bbmle package mle2 function
BetaBin=bbmle::mle2(EstMLEBetaBin,data=list(x=Alcohol_data$Days,
                        freq=Alcohol_data$week2), start=list(a=100.1,b=100.1))
a_est=bbmle::coef(BetaBin)[1]                    
b_est=bbmle::coef(BetaBin)[2]
```

Now, Checking if above data follows Beta-Binomial Distribution

H<sub>0</sub> : The Data follows the Beta-Binomial Distribution.

H<sub>1</sub> : The Data does not follow the Beta-Binomial Distribution.

``` r
#Checking if the above data follows Beta-Binomial Distribution
fitBB<-fitBetaBin(Alcohol_data$Days,Alcohol_data$week2,a_est,b_est)

print(fitBB)
```

    ## Call: 
    ## fitBetaBin(x = Alcohol_data$Days, obs.freq = Alcohol_data$week2, 
    ##     a = a_est, b = b_est)
    ## 
    ## Chi-squared test for Beta-Binomial Distribution 
    ##  
    ##           Observed Frequency :  42 47 54 40 49 40 43 84 
    ##  
    ##           expected Frequency :  47.91 42.92 41.95 42.5 44.3 47.81 54.89 76.73 
    ##  
    ##           estimated a parameter : 0.8575354   ,estimated b parameter : 0.7007619 
    ##  
    ##           X-squared : 9.7641   ,df : 5   ,p-value : 0.0822 
    ##  
    ##           over dispersion : 0.390885

p-value=0.1524 indicates H<sub>0</sub> is not rejected at 5%
significance level. Clearly Beta-Binomial Distribution is a better suit
for the Alcohol BOD.

Further

``` r
#Actual Variance
Act_Var<-var(rep(Alcohol_data$Days,Alcohol_data$week2))
#Estimated Variance of Binomial Distribution
Est_Var_Bin<-var(rep(Alcohol_data$Days,fitted(fitB)))
#Estimated Variance of Beta-Binomial Distribution
Est_Var_BetaBin<-var(rep(Alcohol_data$Days,fitted(fitBB)))
```

| Type of Variance                                                  |   Values |
| :---------------------------------------------------------------- | -------: |
| Actual                                                            | 5.787333 |
| From Expected frequencies of estimated Binomial Distribution      | 1.694534 |
| From Expected frequencies of estimated Beta-Binomial Distribution | 5.804344 |

![](README_files/figure-gfm/Printing%20variance%20and%20plotting%20frequencies-1.png)<!-- -->

Variance(5.8043) from Beta-Binomial Distribution’s estimated frequencies
is closer to the Actual Variance(5.7873) of Alcohol BOD week 2 than
variance(1.6945) of expected frequencies by Binomial Distribution.

According to the plot, it is clearly seen that Beta-Binomial estimated
frequencies behave very close to actual frequency values than the
estimate frequencies from Binomial distribution. Or the Red line is very
similar and close to the Blue line than the Green
line.

## Thank You

[![Twitter](https://img.shields.io/twitter/url/https/github.com/Amalan-ConStat/R-fitODBOD.svg?style=social)](https://twitter.com/intent/tweet?text=Wow:&url=https%3A%2F%2Fgithub.com%2FAmalan-ConStat%2FR-fitODBOD)

[![](https://img.shields.io/badge/LinkedIn-Amalan%20Mahendran-black.svg?style=flat)](https://www.linkedin.com/in/amalan-mahendran-72b86b37/)
[![](https://img.shields.io/badge/Research%20Gate-Amalan%20Mahendran-black.svg?style=flat)](https://www.researchgate.net/profile/Amalan_Mahendran)
