R-fitODBOD
================

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version-last-release/fitODBOD)](https://cran.r-project.org/package=fitODBOD)
[![packageversion](https://img.shields.io/badge/Package%20version-1.4.1-orange.svg?style=flat-square)](commits/master)
[![Rdoc](http://www.rdocumentation.org/badges/version/fitODBOD)](http://www.rdocumentation.org/packages/fitODBOD)
[![GitHub
license](https://img.shields.io/github/license/Amalan-ConStat/R-fitODBOD.svg?style=popout)](https://github.com/Amalan-ConStat/R-fitODBOD/blob/master/LICENSE)

![downloads](http://cranlogs.r-pkg.org/badges/grand-total/fitODBOD)
![downloads](https://cranlogs.r-pkg.org/badges/fitODBOD)
![downloads](http://cranlogs.r-pkg.org/badges/last-week/fitODBOD)
[![status](http://joss.theoj.org/papers/388fc2f4d7c1e0ae83cf0de13ac038a4/status.svg)](http://joss.theoj.org/papers/388fc2f4d7c1e0ae83cf0de13ac038a4)

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![GitHub
issues](https://img.shields.io/github/issues/Amalan-ConStat/R-fitODBOD.svg?style=popout)](https://github.com/Amalan-ConStat/R-fitODBOD/issues)
[![Travis build
status](https://travis-ci.org/Amalan-ConStat/R-fitODBOD.svg?branch=master)](https://travis-ci.org/Amalan-ConStat/R-fitODBOD)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/Amalan-ConStat/R-fitODBOD?branch=master&svg=true)](https://ci.appveyor.com/project/Amalan-ConStat/R-fitODBOD)
[![Codecov test
coverage](https://codecov.io/gh/Amalan-ConStat/R-fitODBOD/branch/master/graph/badge.svg)](https://codecov.io/gh/Amalan-ConStat/R-fitODBOD?branch=master)

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
  - FBMD (Family of Binomial Mixture Distributions)
  - ABD (Alternate Binomial Distributions)
  - PMF (Probability Mass Function)
  - CPMF (Cumulative Probability Mass Function)

## What does “fitODBOD” ?

You can understand BMD & ABD with PMF & CPMF. Further, BOD can be
modeled using these Distributions

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

## Modelling

To demonstrate the process the Alcohol Consumption Data, which is the
most commonly used data-set by the researchers to explain
Over-dispersion will be taken {lemmens1988}. In this data-set, the
number of alcohol consumption days in two reference weeks is separately
self-reported by a randomly selected sample of 399 respondents from the
Netherlands in 1983. Here, the number of days a given individual
consumes alcohol out of seven days a week can be treated as a Binomial
variable. The collection of all such variables from all respondents
would be defined as “Binomial Outcome Data”.

### Step 1

The Alcohol consumption data is already in the necessary format to apply
steps 2 to 5 and hence, step 1 can be avoided. The steps 2 to 5 can be
applied only if the data-set is in the form of a frequency table as
follows.

``` r
library("fitODBOD")     ## Loading packages
# Hello, This is Amalan. For more details refer --> https://amalan-constat.github.io/R-fitODBOD/index.html
print(Alcohol_data)     ## print the alcohol consumption data set
#   Days week1 week2
# 1    0    47    42
# 2    1    54    47
# 3    2    43    54
# 4    3    40    40
# 5    4    40    49
# 6    5    41    40
# 7    6    39    43
# 8    7    95    84
sum(Alcohol_data$week1) ## No of respondents or N
# [1] 399
Alcohol_data$Days       ## Binomial random variables or x 
# [1] 0 1 2 3 4 5 6 7
```

Suppose your data-set is not a frequency table as shown in the following
data-set called `datapoints`. Then the function `BODextract` can be used
to prepare the appropriate format as follows.

``` r
datapoints <- sample(0:7, 340, replace = TRUE) ## creating a set of raw BOD 
head(datapoints)  ## first few observations of datapoints dataset
# [1] 6 0 5 6 6 7
    
## extracting and printing BOD in a usable way for the package
new_data <- BODextract(datapoints)
matrix(c(new_data$RV, new_data$Freq), ncol=2, byrow = FALSE)
#      [,1] [,2]
# [1,]    0   46
# [2,]    1   34
# [3,]    2   48
# [4,]    3   39
# [5,]    4   45
# [6,]    5   41
# [7,]    6   45
# [8,]    7   42
```

### Step 2

As in the second step we test whether the Alcohol Consumption data
follows the Binomial distribution based on the hypothesis given below:

Null Hypothesis : The data follows Binomial Distribution.

Alternate Hypothesis : The data does not follow Binomial Distribution.

Alcohol Consumption data consists of frequency information for two weeks
but only the first week is considered for computation. By doing so the
researcher can verify if the results acquired from the functions are
similar to the results acquired from previous researchers work.

``` r
BinFreq <- fitBin(x=Alcohol_data$Days,obs.fre=Alcohol_data$week1)
# Chi-squared approximation may be doubtful because expected frequency is less than 5
print(BinFreq)
# Call: 
# fitBin(x = Alcohol_data$Days, obs.freq = Alcohol_data$week1)
# 
# Chi-squared test for Binomial Distribution 
#   
#       Observed Frequency :  47 54 43 40 40 41 39 95 
#   
#       expected Frequency :  1.59 13.41 48.3 96.68 116.11 83.66 33.49 5.75 
#   
#       estimated probability value : 0.5456498 
#   
#       X-squared : 2911.434   ,df : 6   ,p-value : 0
```

Looking at the p-value it is clear that null hypothesis is rejected at
5% significance level. This indicates that data does not fit the
Binomial distribution. The reason for a warning message is that one of
the expected frequencies in the results is less than five. Now we
compare the actual and the fitting Binomial variances.

``` r
## Actual variance of observed frequencies
var(rep(Alcohol_data$Days, times = Alcohol_data$week1))
# [1] 6.253788
## Calculated variance for frequencies of  fitted Binomial distribution 
var(rep(BinFreq$bin.ran.var, times = fitted(BinFreq)))
# [1] 1.696035
```

The variance of observed frequencies and the variance of fitting
frequencies are 6.253788 and 1.696035 respectively, which indicates
Over-dispersion.

### Step 3 and 4

Since the Over-dispersion exists in the data now it is necessary to fit
the Binomial Mixture distributions Triangular Binomial, Beta-Binomial,
Kumaraswamy Binomial, Gamma Binomial, Grassia II Binomial, GHGBB and
McGBB using the package, and select the best-fitting distribution using
Negative Log likelihood value, p-value and by comparing observed and
expected frequencies. Modelling these distributions are given in the
next sub-sections.

#### a) Triangular Binomial distribution.

Maximizing the log likelihood value or in our case minimizing the
negative log likelihood is used in the `EstMLExxx` functions. The
estimation of the `mode` parameter can be done by using the
`EstMLETriBin` function, and then the estimated value has to be applied
to `fitTriBin` function to check whether the data fit the Triangular
Binomial distribution.

``` r
## estimating the mode
modeTB <- EstMLETriBin(x=Alcohol_data$Days,freq=Alcohol_data$week1) 
coef(modeTB)  ## printing the estimated mode
#  mode 
#  0.944444
## printing the Negative log likelihood value which is minimized
NegLLTriBin(x=Alcohol_data$Days,freq=Alcohol_data$week1,mode=modeTB$mode)
# [1] 880.6167
```

To fit the Triangular Binomial distribution for estimated mode parameter
the following hypothesis is used

Null Hypothesis : The data follows Triangular Binomial Distribution.

Alternate Hypothesis : The data does not follow Triangular Binomial
Distribution.

``` r
## fitting the Triangular Binomial Distribution for the estimated mode value
fTB <- fitTriBin(x=Alcohol_data$Days,obs.freq=Alcohol_data$week1,mode=modeTB$mode)
print(fTB)
# Call: 
# fitTriBin(x = Alcohol_data$Days, obs.freq = Alcohol_data$week1, 
#     mode = modeTB$mode)
# 
# Chi-squared test for Triangular Binomial Distribution 
#   
#       Observed Frequency :  47 54 43 40 40 41 39 95 
#   
#       expected Frequency :  11.74 23.47 35.21 46.94 58.66 70.2 79.57 73.21 
#   
#       estimated Mode value: 0.944444 
#   
#       X-squared : 193.6159   ,df : 6   ,p-value : 0 
#   
#       over dispersion : 0.2308269
AIC(fTB) 
# [1] 882.6167
var(rep(fTB$bin.ran.var, times = fitted(fTB)))
# [1] 3.786005
```

Since the `p-value` is 0 which is less than 0.05 it is clear that the
null hypothesis is rejected, and the estimated `Over-dispersion` is
0.2308269. Therefore, it is necessary to fit a better flexible
distribution than the Triangular Binomial distribution.

#### b) Beta-Binomial distribution.

To estimate the two shape parameters of the Beta-Binomial distribution
Methods of Moments or Maximum Likelihood estimation can be used. Using
the function `EstMLEBetaBin`(wrapper function of `mle2` from package
`bbmle`) the Negative Log likelihood value will be minimized. In order
to estimate the shape parameters `a` and `b`, initial shape parameter
values have to be given by the user to this function. These initial
values have to be in the domain of the shape parameters. Below given is
the pair of estimates for initial values where `a=0.1` and `b=0.1`.

``` r
## estimating the shape parameters a, b
estimate <- EstMLEBetaBin(x=Alcohol_data$Days,freq = Alcohol_data$week1,a=0.1,b=0.1)
estimate@min ## extracting the minimized Negative log likelihood value 
# [1] 813.4571
## extracting the estimated shape parameter a, b
a1 <- bbmle::coef(estimate)[1] ; b1 <- bbmle::coef(estimate)[2]  
print(c(a1,b1))        ## printing the estimated shape parameters
#         a         b 
# 0.7229420 0.5808483
```

To fit the Beta-Binomial distribution for estimated (Maximum Likelihood
Estimation method) shape parameters the following hypothesis is used

Null Hypothesis : The data follows Beta-Binomial Distribution by the
Maximum Likelihood Estimates.

Alternate Hypothesis: The data does not follow Beta-Binomial
Distribution by the Maximum Likelihood Estimates.

``` r
## fitting Beta Binomial Distribution for estimated shape parameters
fBB1 <- fitBetaBin(x=Alcohol_data$Days,obs.fre=Alcohol_data$week1,a=a1,b=b1)
print(fBB1)
# Call: 
# fitBetaBin(x = Alcohol_data$Days, obs.freq = Alcohol_data$week1, 
#     a = a1, b = b1)
# 
# Chi-squared test for Beta-Binomial Distribution 
#   
#           Observed Frequency :  47 54 43 40 40 41 39 95 
#   
#           expected Frequency :  54.62 42 38.9 38.54 40.07 44 53.09 87.78 
#   
#           estimated a parameter : 0.722942   ,estimated b parameter : 0.5808483 
#   
#           X-squared : 9.5171   ,df : 5   ,p-value : 0.0901 
#   
#           over dispersion : 0.4340673
AIC(fBB1) 
#        a 
# 817.4571
var(rep(fBB1$bin.ran.var, times = fitted(fBB1)))
# [1] 6.24275
```

The `p-value` of 0.0901 \> 0.05 indicates that the null hypothesis is
not rejected. Current estimated shape parameters fit the Beta-Binomial
distribution. Note that the estimated `Over-dispersion` parameter is
0.4340673.

Function `EstMGFBetaBin` is used as below to estimate shape parameters
`a` and `b` using Methods of Moments.

``` r
## estimating the shape parameter a, b
estimate <- EstMGFBetaBin(Alcohol_data$Days, Alcohol_data$week1)
print(c(estimate$a, estimate$b))  ## printing the estimated parameters a, b
# [1] 0.7161628 0.5963324
## finding the minimized negative log likelihood value 
NegLLBetaBin(x=Alcohol_data$Days,freq=Alcohol_data$week1,a=estimate$a,b=estimate$b)
# [1] 813.5872
```

To fit the Beta-Binomial distribution for estimated (Method of Moments)
shape parameters the following hypothesis is used

Null Hypothesis : The data follows Beta-Binomial Distribution by the
Method of Moments.

Alternate Hypothesis: The data does not follow Beta-Binomial
Distribution by the Method of Moments.

``` r
## fitting Beta-Binomial Distribution to estimated shape parameters
fBB2 <- fitBetaBin(x=Alcohol_data$Days,obs.fre=Alcohol_data$week1,a=estimate$a,b=estimate$b)
print(fBB2)
# Call: 
# fitBetaBin(x = Alcohol_data$Days, obs.freq = Alcohol_data$week1, 
#     a = estimate$a, b = estimate$b)
# 
# Chi-squared test for Beta-Binomial Distribution 
#   
#           Observed Frequency :  47 54 43 40 40 41 39 95 
#   
#           expected Frequency :  56.6 43.01 39.57 38.97 40.27 43.89 52.39 84.29 
#   
#           estimated a parameter : 0.7161628   ,estimated b parameter : 0.5963324 
#   
#           X-squared : 9.7362   ,df : 5   ,p-value : 0.0831 
#   
#           over dispersion : 0.4324333
AIC(fBB2) 
# [1] 817.5872
var(rep(fBB2$bin.ran.var, times = fitted(fBB2)))
# [1] 6.273084
```

Results from Method of Moments to estimate the parameters have led to a
`p-value` of 0.0831 which is greater than 0.05 indicates that the null
hypothesis is not rejected. The parameters estimated through Method of
Moments fit the Beta-Binomial distribution for an estimated
`Over-dispersion` of 0.4324333.

#### c) Kumaraswamy Binomial distribution.

The shape parameters `a`, `b` and `it` are estimated and fitted below.
Suppose the selected input parameters are `a=10.1`, `b=1.1` and
`it=20000`.

``` r
## estimating the shape parameters and iteration value
estimate <- EstMLEKumBin(x=Alcohol_data$Days,freq=Alcohol_data$week1,a=10.1,b=1.1,it=20000)
estimate@min ## extracting the minimized negative log likelihood value 
# [1] 814.2052
## extracting the shape parameter a and b
a1 <- bbmle::coef(estimate)[1] ; b1 <- bbmle::coef(estimate)[2] ; it1 <- bbmle::coef(estimate)[3] 
print(c(a1, b1, it1))   ## print shape parameters and iteration value
#            a            b           it 
# 7.231022e-01 6.099952e-01 2.000000e+04
```

To fit the Kumaraswamy Binomial distribution for estimated shape
parameters the following hypothesis is used

Null Hypothesis : The data follows Kumaraswamy Binomial Distribution.

Alternate Hypothesis : The data does not follow Kumaraswamy Binomial
Distribution.

``` r
## fitting Kumaraswamy Binomial Distribution to estimated shape parameters
fKB <- fitKumBin(x=Alcohol_data$Days,obs.fre=Alcohol_data$week1,a=a1,b=b1,it=it1*100)
print(fKB)
# Call: 
# fitKumBin(x = Alcohol_data$Days, obs.freq = Alcohol_data$week1, 
#     a = a1, b = b1, it = it1 * 100)
# 
# Chi-squared test for Kumaraswamy Binomial Distribution 
#   
#       Observed Frequency :  47 54 43 40 40 41 39 95 
#   
#       expected Frequency :  53.6 42.05 39.4 39.3 40.97 44.91 53.66 85.09 
#   
#       estimated a parameter : 0.7231022   ,estimated b parameter : 0.6099952 ,
#   
#       estimated it value : 2e+06 
#   
#       X-squared : 10.0728   ,df : 5   ,p-value : 0.0732 
#   
#       over dispersion : 0.4252887
AIC(fKB) 
#        a 
# 817.7668
var(rep(fKB$bin.ran.var, times = fitted(fKB)))
# [1] 6.181906
```

The null hypothesis is not rejected at 5% significance level
(`p-value`=0.0732 )for the estimated parameters `it=20000`,
`a=0.7231022`, `b=0.6099952` and the estimated `Over-dispersion` of
0.4252887.

#### d) Gamma Binomial distribution.

The shape parameters `c` and `l` are estimated and fitted below. Suppose
the selected input parameters are `c=10.1` and `l=5.1`.

``` r
## estimating the shape parameters
estimate <- EstMLEGammaBin(x=Alcohol_data$Days,freq=Alcohol_data$week1,c=10.1,l=5.1)
estimate@min ## extracting the minimized negative log likelihood value 
# [1] 814.0045
## extracting the shape parameter c and l
c1 <- bbmle::coef(estimate)[1] ; l1 <- bbmle::coef(estimate)[2]  
print(c(c1, l1))   ## print shape parameters
#         c         l 
# 0.6036061 0.6030777
```

To fit the Gamma Binomial distribution for estimated shape parameters
the following hypothesis is used

Null Hypothesis : The data follows Gamma Binomial Distribution.

Alternate Hypothesis : The data does not follow Gamma Binomial
Distribution.

``` r
## fitting Gamma Binomial Distribution to estimated shape parameters
fGB <- fitGammaBin(x=Alcohol_data$Days,obs.fre=Alcohol_data$week1,c=c1,l=l1)
print(fGB)
# Call: 
# fitGammaBin(x = Alcohol_data$Days, obs.freq = Alcohol_data$week1, 
#     c = c1, l = l1)
# 
# Chi-squared test for Gamma Binomial Distribution 
#   
#       Observed Frequency :  47 54 43 40 40 41 39 95 
#   
#       expected Frequency :  54.59 41.39 38.7 38.71 40.54 44.69 53.79 86.58 
#   
#       estimated c parameter : 0.6036061   ,estimated l parameter : 0.6030777  
#   
#       X-squared : 10.6152   ,df : 5   ,p-value : 0.0596 
#   
#       over dispersion : 0.4308113
AIC(fGB) 
# [1] 818.0045
var(rep(fGB$bin.ran.var, times = fitted(fGB)))
# [1] 6.228652
```

The null hypothesis is not rejected at 5% significance level
(`p-value`=0.0596)for the estimated parameters `c=0.6036061`,
`l=0.6030777` and the estimated `Over-dispersion` of 0.4308113.

#### e) Grassia II Binomial distribution.

The shape parameters `a` and `b` are estimated and fitted below using
the `EstMLEGammaBin` function. Suppose the selected input parameters are
`a=1.1` and `b=5.1`.

``` r
## estimating the shape parameters
estimate <- EstMLEGrassiaIIBin(x=Alcohol_data$Days,freq=Alcohol_data$week1,a=1.1,b=5.1)
estimate@min ## extracting the minimized negative log likelihood value 
# [1] 813.0395
# extracting the shape parameter a and b
a1 <- bbmle::coef(estimate)[1] ; b1 <- bbmle::coef(estimate)[2]  
print(c(a1, b1))   #print shape parameters
#         a         b 
# 0.7285039 2.0251513
```

To fit the Grassia II Binomial distribution for estimated shape
parameters the following hypothesis is used

Null Hypothesis : The data follows Grassia II Binomial Distribution.

Alternate Hypothesis : The data does not follow Grassia II Binomial
Distribution.

``` r
#fitting Grassia II Binomial Distribution to estimated shape parameters
fGB2 <- fitGrassiaIIBin(x=Alcohol_data$Days,obs.fre=Alcohol_data$week1,a=a1,b=b1)
print(fGB2)
# Call: 
# fitGrassiaIIBin(x = Alcohol_data$Days, obs.freq = Alcohol_data$week1, 
#     a = a1, b = b1)
# 
# Chi-squared test for Grassia II Binomial Distribution 
#   
#       Observed Frequency :  47 54 43 40 40 41 39 95 
#   
#       expected Frequency :  55.02 42.36 39.08 38.51 39.78 43.39 52.13 88.74 
#   
#       estimated a parameter : 0.7285039   ,estimated b parameter : 2.025151  
#   
#       X-squared : 8.6999   ,df : 5   ,p-value : 0.1216 
#   
#       over dispersion : 0.259004
AIC(fGB2) 
# [1] 817.0395
var(rep(fGB2$bin.ran.var, times = fitted(fGB2)))
# [1] 6.299827
```

The null hypothesis is not rejected at 5% significance level
(`p-value`=0.0596)for the estimated parameters `a=0.7285039`,
`b=2.0251513` and the estimated `Over-dispersion` of 0.259004.

#### f) GHGBB distribution.

Now we estimate the shape parameters and fit the GHGBB distribution for
the first set of randomly selected initial input shape parameters of
`a=10.1`, `b=1.1` and `c=5`.

``` r
#estimating the shape parameters 
estimate <- EstMLEGHGBB(x=Alcohol_data$Days,freq=Alcohol_data$week1,a=10.1,b=1.1,c=5)
estimate@min #extracting the minimized negative log likelihood value
# [1] 809.2767
#extracting the shape parameter a, b and c
a1 <- bbmle::coef(estimate)[1] ; b1 <- bbmle::coef(estimate)[2] ; c1 <- bbmle::coef(estimate)[3]   
print(c(a1, b1, c1))      #printing the shape parameters
#         a         b         c 
# 1.3506836 0.3245421 0.7005210
```

To fit the GHGBB distribution for estimated shape parameters the
following hypothesis is used.

Null Hypothesis : The data follows Gaussian Hypergeometric Generalized
Beta-Binomial Distribution.

Alternate Hypothesis : The data does not follow Gaussian Hypergeometric
Generalized Beta-Binomial Distribution.

``` r
#fitting GHGBB distribution for estimated shape parameters
fGG <- fitGHGBB(Alcohol_data$Days, Alcohol_data$week1, a1, b1, c1)  
print(fGG)
# Call: 
# fitGHGBB(x = Alcohol_data$Days, obs.freq = Alcohol_data$week1, 
#     a = a1, b = b1, c = c1)
# 
# Chi-squared test for Gaussian Hypergeometric Generalized Beta-Binomial Distribution 
#   
#       Observed Frequency :  47 54 43 40 40 41 39 95 
#   
#       expected Frequency :  47.88 50.14 46.52 42.08 38.58 37.32 41.78 94.71 
#   
#       estimated a parameter : 1.350684   ,estimated b parameter : 0.3245421 ,
#   
#       estimated c parameter : 0.700521 
#   
#       X-squared : 1.2835   ,df : 4   ,p-value : 0.8642 
#   
#       over dispersion : 0.4324874
AIC(fGG) 
#        a 
# 815.2767
var(rep(fGG$bin.ran.var, times = fitted(fGG)))
# [1] 6.249335
```

The null hypothesis is not rejected at 5% significance level
(`p-value`=0.8642). The estimated shape parameters are `a=1.3506836`,
`b=0.3245421` and `c=0.7005210`, where the estimated `Over-dispersion`
of 0.4324874.

#### g) McGBB distribution.

Given below is the results generated for the randomly selected initial
input parameters where `a=1.1`, `b=5` and `c=10`.

``` r
#estimating the shape parameters
estimate <- EstMLEMcGBB(x = Alcohol_data$Days,freq = Alcohol_data$week1,a = 1.1, b = 5, c = 10)
estimate@min  #extracting the negative log likelihood value which is minimized
# [1] 809.7134
#extracting the shape parameter a, b and c
a1 <- bbmle::coef(estimate)[1] ; b1 <- bbmle::coef(estimate)[2] ; c1 <- bbmle::coef(estimate)[3] 
print(c(a1, b1, c1))    #printing the shape parameters
#           a           b           c 
#  0.04099005  0.21082788 21.67349031
```

To fit the McGBB distribution for estimated shape parameters the
following hypothesis is used

Null Hypothesis : The data follows McDonald Generalized Beta-Binomial
Distribution.

Alternate Hypothesis : The data does not follow McDonald Generalized
Beta-Binomial Distribution.

``` r
#fitting the MCGBB distribution for estimated shape parameters
fMB <- fitMcGBB(x=Alcohol_data$Days,obs.fre=Alcohol_data$week1,a=a1,b=b1,c=c1)
print(fMB)
# Call: 
# fitMcGBB(x = Alcohol_data$Days, obs.freq = Alcohol_data$week1, 
#     a = a1, b = b1, c = c1)
# 
# Chi-squared test for Mc-Donald Generalized Beta-Binomial Distribution 
#   
#       Observed Frequency :  47 54 43 40 40 41 39 95 
#   
#       expected Frequency :  51.37 45.63 43.09 41.5 40.42 39.97 42.11 94.91 
#   
#       estimated a parameter : 0.04099005   ,estimated b parameter : 0.2108279 ,
#   
#       estimated c parameter : 21.67349 
#   
#       X-squared : 2.2222   ,df : 4   ,p-value : 0.695 
#   
#       over dispersion : 0.4359023
AIC(fMB) 
#        a 
# 815.7134
var(rep(fMB$bin.ran.var, times = fitted(fMB)))
# [1] 6.288273
```

The null hypothesis is not rejected at 5% significance level
(`p-value`=0.695 \> 0.05). The estimated shape parameters are
`a=0.04099005` `b=0.2108279` and `c=21.67349`, and the estimated
`Over-dispersion` of 0.4359023.

### Step 5

Below table presents the expected frequencies, p-values, Negative Log
Likelihood values, AIC values, Variance and Over-dispersion of the
Binomial Mixture distributions obtained above for the Alcohol
Consumption data. Further, for the above fitting distributions
difference values (the difference between observed frequency and
expected frequency) are calculated and included in the table in brackets
with next to the expected frequencies of that distribution.

![](man/figures/table1.JPG)

## Conclusion

The best-fitting distribution is chosen by comparing three main
measurements extracted from the results shown in the above table which
are p-value, Negative Log Likelihood value, the count of difference
between expected and observed frequencies in the range of +/-5, variance
difference and AIC values.

Then the following three criteria will be considered for the selection
procedure

1.  The `p-value` \>0.05 from the hypothesis test.
2.  The Negative Log Likelihood value.
3.  The AIC value.
4.  The number of difference values within the range of +/-5.  
5.  The Variance difference between expected and observed frequency.

The following table shows the respective values of the above criteria
and the corresponding Over-dispersion value for each distribution
constructed for Alcohol Consumption data.

![](man/figures/table2.JPG)

Triangular Binomial and Binomial distributions cannot be fitted since
its p-value \< 0.05. The Negative Log Likelihood values of GHGBB and
McGBB distributions are the lowest and are quite similar. Similarly AIC
values are lowest for GHGBB and McGBB, also highest AIC values is for
Triangular Binomial distribution. Based on the count of difference
values for the Beta-Binomial distribution it is four out of eight and
similar for distributions Gamma Binomial, Grassia II Binomial, and
Kumaraswamy Binomial. But for the McGBB distribution it is seven out of
eight counts.

Further, Over-dispersion parameters of all four fitted distributions are
same for the second decimal point (Over-dispersion = 0.43) except
Triangular Binomial and Grassia II Binomial distributions where they are
similar for the first decimal point (Over-dispersion = 0.2). Clearly
variance difference is highest for Binomial distribution and lowest for
GHGBB distribution, while others are significant only from the second
decimal point.

The best-fitting distribution GHGBB has the highest p-value of 0.8642,
the lowest Negative Log Likelihood value of 809.2767 and AIC value of
815.2767, the count of difference values is eight out of eight and
indicates an estimated Over-dispersion of 0.4324875. The variance
difference between observed and expected frequencies of GHGBB leads to
the smallest value of 0.004453.

#### Thank You

[![Twitter](https://img.shields.io/twitter/url/https/github.com/Amalan-ConStat/R-fitODBOD.svg?style=social)](https://twitter.com/intent/tweet?text=Wow:&url=https%3A%2F%2Fgithub.com%2FAmalan-ConStat%2FR-fitODBOD)

[![](https://img.shields.io/badge/LinkedIn-Amalan%20Mahendran-black.svg?style=flat)](https://www.linkedin.com/in/amalan-mahendran-72b86b37/)
[![](https://img.shields.io/badge/Research%20Gate-Amalan%20Mahendran-black.svg?style=flat)](https://www.researchgate.net/profile/Amalan_Mahendran)
