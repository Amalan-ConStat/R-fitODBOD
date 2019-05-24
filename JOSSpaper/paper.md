---
title: 'fitODBOD: An R Package to Model Binomial Outcome Data using Binomial Mixture
  Distributions and Alternate Binomial Distributions.'
authors:
- affiliation: 1
  name: Amalan Mahendran
  orcid: 0000-0002-0643-9052
- affiliation: 1
  name: Prof. Pushpakanthie Wijekoon
  orcid: 0000-0003-4242-1017
date: '2019-05-24'
bibliography: paper.bib
tags:
- R
- fitODBOD
- BOD
- Over-dispersion
- FBMD
- ABD
affiliations:
- index: 1
  name: Department of Statistics and Computer Science, Faculty of Science, University
    of Peradeniya.
---

# Summary

The **R** package **fitODBOD** can be used to identify the best-fitted
model for Over-dispersed Binomial Outcome Data (BOD). The Triangular
Binomial (TriBin), Beta-Binomial (BetaBin), Kumaraswamy Binomial
(KumBin), Gaussian Hypergeometric Generalized Beta-Binomial (GHGBB),
Gamma Binomial (GammaBin), Grassia II Binomial (GrassiaIIBin) and
McDonald Generalized Beta-Binomial (McGBB) distributions in the Family
of Binomial Mixture Distributions (FBMD) are considered for model
fitting in this package. Also, Alternate Binomial Distributions such as
Additive Binomial (AddBin), Beta-Correlated Binomial (BetaCorrBin), COM
Poisson Binomial (COMPBin), Correlated Binomial (CorrBin), Lovinson
Multiplicative Binomial (LMBin) and Multiplicative Binomial (MultiBin)
distributions are of use as well replacing the traditional Binomial
distribution. Further, Probability Mass Function (PMF), Cumulative
Probability Mass Function (CPMF), Negative Log Likelihood,
Over-dispersion and parameter estimation (shape and distribution
distinct parameters) can be explored for each fitted model by using
**fitODBOD** package. **fitODBOD** is a free open-source **R** package
available via GitHub at <https://github.com/Amalan-ConStat/R-fitODBOD>

# Introduction

Statistical methods are widely used in the areas of research in most of
the current disciplines. There is more focus towards fitting
distributions to given data since the distributions of data depends on
the method of data collection. For example, consider a Binomial
experiment that a fair coin is being tossed *n* number of times. Let the
event of falling head be defined as the success of probability *p*.
Then, the number of heads out of *n* tosses is considered to be a single
Binomial variable, *Y*. Also if similar Binomial experiments occur in
*N* number of different clusters, a collection of *Y <sub>1<sub>*,
*Y<sub>2<sub>*, *Y<sub>3<sub>*, ……, *Y<sub>N<sub>* would form the BOD.
Such data are frequently mentioned in fields of Toxicology, Biology,
Clinical Medicine, Epidemiology and much more. One may attempt to fit
the BOD using the traditional Binomial distribution, as it is
characterized using the number of identical trials *n* and the
probability of success parameter *p*. The parameter *p* (*p* 
![](http://latex.codecogs.com/gif.latex?%5Cfn_jvn%20%5Clarge%20%5Cin) [0, 1]) 
is usually assumed to be a constant from trial to trial and
the trials are independent. In many empirical situations, it has been
frequently observed that the actual observed variance of the BOD is
greater than the assumed theoretical Binomial variance. This outcome is
typically known as “Over-dispersion” (Cox 1983); (Anderson 1988).
Over-dispersion in BOD can occur either with a probability of success
parameter *p* varying from trial to trial or there is a correlation
among binary trials. However, (Collett 1991) argued that the above two
cases of Over-dispersion are frequently the same.

New distributions emerged to fit the BOD replacing the traditional
Binomial distribution. Li, Huang, and Zhao (2011) has developed the
Kumaraswamy Binomial distribution, Rodriguez-Avi et al. (2007) has
constructed the Gaussian Hypergeometric Generalized Beta-Binomial
distribution, Karlis and Xekalaki (2008) wrote the article on the
Triangular Binomial distribution. Initially the concept of mixing the
Binomial distribution with an unit bounded continuous distribution was
initially done by Horsnell (1957), which led to the Uniform Binomial
distribution. Recently, Manoj, Wijekoon, and Yapa (2013) had developed
the McDonald Generalized Beta-Binomial distribution. Based on this
research only the development of **fitODBOD**(version 1.1.0) package was
developed in February, 2018. Recently the package **fitODBOD** went
through severe changes twice which included new distributions,
functions, class objects and methods. This had made the package more
flexible and convenient for researchers who intend to use it.

Further new types of Binomial distributions were developed replacing the
traditional Binomial distribution, which are called as Alternate
Binomial distributions. S.R.Paul (1985) has developed the Multiplicative
Binomial distribution, while recently Elamir (2013) has done some more
research to form the Lovinson Multiplicative Binomial distribution. COM
Poisson Binomial distribution was introduced first by Borges et al.
(2014). The comparison of Beta-Correlated Binomial distribution with
Correlated Binomial distribution was done by Kupper and Haseman (1978).
Until now this package holds all the distributions mentioned above and
in the future more distributions which were constructed to fit BOD will
be added to the **fitODBOD** package as version updates.

# Analysis

To fit a Binomial Mixture distribution for a raw BOD set, the following
steps have to be used when using this package.

1.  Extract the data in a meaningful way (**BODextract** function).
2.  Check whether the Binomial distribution can be fitted and if not
    test the Over-dispersion (**fitBin** function) by using Pearson
    Chi-square Goodness of Fit test.
3.  If Over-dispersion exists, estimate the parameters for each
    distribution for the given data separately (**EstTriBin**,
    **EstMLEBetaBin**, **EstMGFBetaBin**, **EstMLEKumBin**,
    **EstMLEGammaBin**, **EstMLEGrassiaIIBin**, **EstMLEGHGBB**,
    **EstMLEMcGBB**, **EstMLEAddBin**, **EstMLEBetaCorrBin**,
    **EstMLECOMPBin**, **EstMLECorrBin**, **EstMLELMBin**,
    **EstMLEMultiBin** functions).
4.  Based on the above estimated parameters corresponding models can be
    fitted (**fitTriBin**, **fitBetaBin**, **fitKumBin**, **fitGHGBB**,
    **fitGammaBin**, **fitGrassiaIIBin**, **fitMcGBB**, **fitAddBin**,
    **fitBetaCorrBin**, **fitCOMPBin**, **fitCorrBin**, **fitLMBin**,
    **fitMultiBin** functions).
5.  Finally, compare the results and choose the best-fitted distribution
    for the data by using a plot or table.

Below is the series of code to complete the steps from 1 to 4 for the
Beta-Binomial distribution. Similarly, it is possible to fit other FBMDs
and ABDs for the purpose of finding the most suitable distribution to
fit the given BOD. Using a table to summarize the result is quite
useful, where it will contain the expected frequencies, chi-squared
values, p-values, estimated parameters, Negative Log Likelihood values
and Over-dispersion of the Binomial Mixture distributions.

``` r
library("bbmle") # Loading packages 
library("fitODBOD")

# step 1: print the alcohol consumption data set 
print(Alcohol_data)
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

``` r
# step 2: Checking if the Binomial Distribution can be fitted for Week 1
fitBin(Alcohol_data$Days, Alcohol_data$week1)
```

    ## Call: 
    ## fitBin(x = Alcohol_data$Days, obs.freq = Alcohol_data$week1)
    ## 
    ## Chi-squared test for Binomial Distribution 
    ##  
    ##       Observed Frequency :  47 54 43 40 40 41 39 95 
    ##  
    ##       expected Frequency :  1.59 13.41 48.3 96.68 116.11 83.66 33.49 5.75 
    ##  
    ##       estimated probability value : 0.5456498 
    ##  
    ##       X-squared : 2911.434   ,df : 6   ,p-value : 0

``` r
# step 3: estimating the shape parameters a,b for Week 1
estimate = bbmle::mle2(EstMLEBetaBin, start = list(a = 0.1, b = 0.1),  
                       data = list(x = Alcohol_data$Days, freq = Alcohol_data$week1))

# extracting the estimated shape parameters a and b for Week 1
a1 = bbmle::coef(estimate)[1] ; b1 = bbmle::coef(estimate)[2]
print(c(a1,b1)) #printing the estimated shape parameters a and b 
```

    ##         a         b 
    ## 0.7229420 0.5808483

``` r
# step 4: fitting the Beta Binomial Distribution for estimated shape parameters to Week 1
fitBetaBin(Alcohol_data$Days, Alcohol_data$week1, a1, b1)
```

    ## Call: 
    ## fitBetaBin(x = Alcohol_data$Days, obs.freq = Alcohol_data$week1, 
    ##     a = a1, b = b1)
    ## 
    ## Chi-squared test for Beta-Binomial Distribution 
    ##  
    ##           Observed Frequency :  47 54 43 40 40 41 39 95 
    ##  
    ##           expected Frequency :  54.62 42 38.9 38.54 40.07 44 53.09 87.78 
    ##  
    ##           estimated a parameter : 0.722942   ,estimated b parameter : 0.5808483 
    ##  
    ##           X-squared : 9.5171   ,df : 5   ,p-value : 0.0901 
    ##  
    ##           over dispersion : 0.4340673

# Conclusion

The **fitODBOD** package is constructed for the main purpose of fitting
the given BOD and being able to choose the best-fitted Binomial Mixture
and Alternate Binomial Distributions. The package has functions to
calculate PMF, CPMF and Negative Log Likelihood of Triangular Binomial,
Beta-Binomial, Kumaraswamy Binomial, Gamma Binomial, Grassia II
Binomial, GHGBB, McGBB, Additive Binomial, Beta-Correlated Binomial, COM
Poisson Binomial, Correlated Binomial, Lovinson Multiplicative and
Multiplicative Binomial Binomial distributions. Further, there are
functions for probability density, cumulative density and moment about
zero values for Triangular, Beta, Kumaraswamy, Gamma, Grassia II,
Gaussian Hypergeometric Generalized Beta and Generalized Beta of First
kind distributions. Using the above steps mentioned of the article, the
most fitted Binomial Mixture distribution or/and Alternate Binomial
distribution is decided.

# References

