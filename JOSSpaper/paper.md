---
title: '``fitODBOD``: An R Package to Model Binomial Outcome Data using Binomial Mixture
  and Alternate Binomial Distributions.'
authors:
- affiliation: 1
  name: Amalan Mahendran
  orcid: 0000-0002-0643-9052
- affiliation: 1
  name: Pushpakanthie Wijekoon
  orcid: 0000-0003-4242-1017
date: '2019-06-13'
output:
  html_document:
    df_print: paged
  pdf_document: default
bibliography: Ref.bib
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

The **R** package **``fitODBOD``** can be used to identify the best-fitting
model for Over-dispersed Binomial Outcome Data (BOD). The Triangular
Binomial (TriBin), Beta-Binomial (BetaBin), Kumaraswamy Binomial
(KumBin), Gaussian Hypergeometric Generalized Beta-Binomial (GHGBB),
Gamma Binomial (GammaBin), Grassia II Binomial (GrassiaIIBin) and
McDonald Generalized Beta-Binomial (McGBB) distributions in the Family
of Binomial Mixture Distributions (FBMD) are considered for model
fitting in this package. Alternate Binomial Distributions such as
Additive Binomial (AddBin), Beta-Correlated Binomial (BetaCorrBin), COM
Poisson Binomial (COMPBin), Correlated Binomial (CorrBin), Lovinson
Multiplicative Binomial (LMBin) and Multiplicative Binomial (MultiBin)
distributions are used as well, replacing the traditional Binomial
distribution. Further, Probability Mass Function (PMF), Cumulative
Probability Mass Function (CPMF), Negative Log Likelihood,
Over-dispersion and parameter estimation (shape and distribution
distinct parameters) can be explored for each fitted model with the
**``fitODBOD``** package. 

# Introduction

Statistical methods are widely used for research in most disciplines. There is a focus towards fitting
distributions to given data since the distributions of data depends on
the method of data collection. For example, consider a Binomial
experiment where a fair coin is being tossed *n* times. Let the
event of landing heads-up be defined as the success of probability *p*.
Then, the number of heads out of *n* tosses is considered to be a single
Binomial variable, *Y*. Also if similar Binomial experiments occur in
*N* different clusters, a collection of *Y<sub>1<sub>*,
*Y<sub>2<sub>*, *Y<sub>3<sub>*, ..., *Y<sub>N<sub>* would form the BOD.
Such data are frequently mentioned in fields of toxicology, biology,
clinical medicine, epidemiology and many more. One may attempt to fit
the BOD using the traditional Binomial distribution, as it is
characterized using the number of identical trials *n* and the
probability of success parameter *p*. The parameter *p* (*p* $\in$ [0, 1]) 
is usually assumed to be a constant from trial to trial and
the trials are independent. In many empirical situations, it has been
frequently observed that the actual observed variance of the BOD is
greater than the assumed theoretical Binomial variance. This outcome is
typically known as "over-dispersion" [@Cox1983; @anderson1988].
Over-dispersion in BOD can occur either with a probability of success
parameter *p* varying from trial to trial or if there is a correlation
among binary trials. However, @collett1991 argued that the above two
cases of over-dispersion are frequently the same.

New distributions emerged to fit the BOD replacing the traditional
Binomial distribution. @Xiaohu2011 has developed the
Kumaraswamy Binomial distribution, @Rodriguez-Avi2007 has
constructed the Gaussian Hypergeometric Generalized Beta-Binomial
distribution, @Karlis2008 wrote the article on the
Triangular Binomial distribution. Also, @grassia1977 mentioned the 
Gamma Binomial and Grassia II Binomial distributions. The Beta-Binomial 
distribution is clearly explained in @johnson1995. Initially the concept 
of mixing the Binomial distribution with a unit bounded continuous distribution 
was done by @Horsnell, which led to the Uniform Binomial distribution. 
Recently, @Manoj2013 had developed the McDonald Generalized Beta-Binomial 
distribution. Based on this research only the development of **``fitODBOD``**
(version 1.1.0) package was released to CRAN in February, 2018. 
Recently this package became available on [GitHub](https://github.com/Amalan-ConStat/R-fitODBOD) 
and has its own [website](https://amalan-constat.github.io/R-fitODBOD/index.html), 
which has made the package more convenient for researchers who intend to use it.

Further, new types of Binomial distributions were developed replacing the
traditional Binomial distribution, which are called Alternate
Binomial distributions. @Multiplicative has developed the Multiplicative
Binomial distribution, while recently @Lovinson has done more
research to form the Lovinson Multiplicative Binomial distribution. COM
Poisson Binomial distribution was introduced first by @COMPoisson. 
The comparison of Beta-Correlated Binomial distribution with
Correlated Binomial distribution was done by @Correlated.
Version 1.4.0 of **``fitODBOD``**(@fitODBOD) holds all the distributions mentioned 
above and in the future more distributions developed to fit the BOD will
be added to the package as major version updates.

# Modelling

To fit a Binomial Mixture distribution for a raw BOD set, the following
steps have to be used when using this package.

1.  Extract the data in a meaningful way (**BODextract** function).
2.  Check whether the Binomial distribution can be fitted and if not
    test the over-dispersion (**fitBin** function) by using Pearson
    Chi-square goodness of fit test.
3.  If over-dispersion exists, estimate the parameters for each
    distribution for the given data separately (**EstTriBin**,
    **EstMLEBetaBin**, **EstMGFBetaBin**, **EstMLEKumBin**,
    **EstMLEGammaBin**, **EstMLEGrassiaIIBin**, **EstMLEGHGBB**,
    **EstMLEMcGBB**, **EstMLEAddBin**, **EstMLEBetaCorrBin**,
    **EstMLECOMPBin**, **EstMLECorrBin**, **EstMLELMBin**,
    **EstMLEMultiBin** functions).
4.  Based on the above estimated parameters corresponding models can be
    fitted (**fitTriBin**, **fitBetaBin**, **fitKumBin**, **fitGammaBin**, 
    **fitGrassiaIIBin**,**fitGHGBB**, **fitMcGBB**, **fitAddBin**,
    **fitBetaCorrBin**, **fitCOMPBin**, **fitCorrBin**, **fitLMBin**,
    **fitMultiBin** functions).
5.  Finally, compare the results and choose the best-fitting distribution
    for the data by using a plot or table.

Below is the series of code to complete the steps from 1 to 4 for the
Beta-Binomial distribution. Similarly, it is possible to fit other FBMD
and ABD for the purpose of finding the most suitable distribution to
fit the given BOD. 

```{r}
library("bbmle") # Loading packages 
library("fitODBOD")

# step 1: print the alcohol consumption data set 
print(Alcohol_data)
```
According to the below hypothesis Binomial distribution will be tested for the 
given BOD.

<p align= "center"> Null Hypothesis : Data follows the Binomial Distribution. </p>
<p align= "center"> Alternate Hypothesis : Data does not follow the Binomial Distribution. </p>

```r
# step 2: Checking if the Binomial Distribution can be fitted for Week 2
fdBin<-fitBin(Alcohol_data$Days, Alcohol_data$week2)
print(fdBin)

# step 3: estimating the shape parameters a,b for Week 2
estimate = bbmle::mle2(EstMLEBetaBin, start = list(a = 0.1, b = 0.1),  
                       data = list(x = Alcohol_data$Days, freq = Alcohol_data$week2))

# extracting the estimated shape parameters a and b for Week 2
a1 = bbmle::coef(estimate)[1] ; b1 = bbmle::coef(estimate)[2]
print(c(a1,b1)) #printing the estimated shape parameters a and b 
```

Below hypothesis will check the suitability to fit the Beta-Binomial distribution.

<p align= "center"> Null Hypothesis : Data follows the Beta-Binomial Distribution. </p>
<p align= "center"> Alternate Hypothesis : Data does not follow the Beta-Binomial Distribution. </p>

```r
# step 4: fitting the Beta Binomial Distribution for estimated shape parameters to Week 2
fdBB<-fitBetaBin(Alcohol_data$Days, Alcohol_data$week2, a1, b1)
print(fdBB)

# step 5: Using a table to summarize the result is quite useful, where it will contain 
# the expected frequencies, chi-squared values, p-values, estimated parameters, 
# Negative Log Likelihood values and Over-dispersion of the Binomial Mixture distributions.
```

The best distribution to fit the given BOD from ABD and/or FBMD is thoroughly discussed in the
[website](https://amalan-constat.github.io/R-fitODBOD/articles/BMDs_and_ABDs_fitxxxBin.html) 
with the help of plots. 

# Conclusion

The **``fitODBOD``** package is constructed for the main purpose of fitting
the given BOD and being able to choose the best-fitted Binomial Mixture
and/or Alternate Binomial Distributions. The package has functions to
calculate PMF, CPMF and Negative Log Likelihood of Triangular Binomial,
Beta-Binomial, Kumaraswamy Binomial, Gamma Binomial, Grassia II
Binomial, GHGBB, McGBB, Additive Binomial, Beta-Correlated Binomial, COM
Poisson Binomial, Correlated Binomial, Lovinson Multiplicative and
Multiplicative Binomial distributions. Further, there are
functions for probability density, cumulative density and moment about
zero values for Triangular, Beta, Kumaraswamy, Gamma, Gaussian Hypergeometric 
Generalized Beta and Generalized Beta of First kind distributions. Using the 
above steps mentioned of the article, the best-fitting Binomial Mixture 
distribution and/or Alternate Binomial distribution is decided.

# References
