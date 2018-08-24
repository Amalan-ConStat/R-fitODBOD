# R-fitODBOD

```{r badges, include=FALSE, echo=FALSE}
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/fitODBOD)](https://cran.r-project.org/package=fitODBOD)
[![Licence](https://img.shields.io/badge/licence-GPL--2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)
[![packageversion](https://img.shields.io/badge/Package%20version-1.2.0-orange.svg?style=flat-square)](commits/master)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
# badge for code coverage
# badge from license for github or CRAN 
# badge for downloads
# badge for size 
# badge for twitter and follow
# badge for commits for year
# badge for No of contributors
# badge for github release date
```

## Installing the package from GitHub

devtools::install_github("Amalan-ConStat/R-fitODBOD")

## Installing the package from CRAN

install.packages("fitODBOD")

## Key Phrases
* BOD (Binomial Outcome Data)
* Over Dispersion
* Under Dispersion
* BMD (Binomial Mixture Distributions)
* ABD (Alternate Binomial Distributions)
* PMF (Probability Mass Function)
* CPMF (Cumulative Probability Mass Function)

## What does "fitODBOD" ?
You can understand BMD & ABD with PMF & CPMF. Further, BOD can be modeled using these Distributions

## Distributions
|Left|Left|
|-----------------------|----------------------------------|
| Alternate Binomial Distrbutions | Binomial Mixture Distributions|
|1.Additive Binomial Distribution|1.Uniform Binomial Distribution |
|2.Beta-Correlated Binomial Distribution|2.Triangular Binomial Distribution|
|3.COM Poisson Binoial Distribution|3.Beta-Binomial Distribution|
|4.Correlated Binomial Distribution|4.Kumaraswamy Binomial Distribution|
|5.Multiplicative Binomial Distribution|5. Gaussian Hypergeometric Generalized Beta-Binomial Distribution|
||6.McDonald Generalized Beta-Binomial Distribution|


## Just an example
### Modelling BOD using Beta-Binomial Distribution
```{r Male Children Data}
#Looking at the BOD
Male_Children
```

Hypothesis to check if above data follows Binomial Distribution

H~0~ : The Data follows the Binomial Distribution. 

H~1~ : The Data does not follow the Binomial Distribution.

```{r Fitting Binomial Distribution}
#Checking if the above data follows Binomial Distribution
fitBin(Male_Children$No_of_Males,Male_Children$freq)
```

According to p-value=0, H~0~ is rejected at 5% significance level.

```{r Estimating shape parameters a,b using given BOD}
BetaBin=bbmle::mle2(EstMLEBetaBin,data=list(x=Male_Children$No_of_Males,
                    freq=Male_Children$freq), start=list(a=100.1,b=100.1))

a_est=bbmle::coef(BetaBin)[1]                    
b_est=bbmle::coef(BetaBin)[2]
```

Now, Checking if above data follows Beta-Binomial Distribution

H~0~ : The Data follows the Beta-Binomial Distribution. 

H~1~ : The Data does not follow the Beta-Binomial Distribution.

```{r Fitting Beta-Binomial Distribution}
#Checking if the above data follows Binomial Distribution
fitBetaBin(Male_Children$No_of_Males,Male_Children$freq,a_est,b_est)
```

p-value=0.1524 indicates H~0~ is not rejected at 5% significance level.
Clearly Beta-Binomial Distribution is a better suit for the Male_Children BOD.

Further 
```{r Comparing Variances}
#Actual Variance


#Estimated Variance of Binomial Distribution


#Estimated Variance of Beta-Binomial Distribution


```
Variance from Beta-Binomial Distribution's estimated frequencies is closer to 
the Actual Variance of Data than Binomial Distribution.
