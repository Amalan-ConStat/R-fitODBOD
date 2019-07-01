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
  name: Department of Statistics and Computer Science, Faculty of Science, University of Peradeniya.
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
distributions are used as well, replacing the traditional binomial
distribution. Further, Probability Mass Function (PMF), Cumulative
Probability Mass Function (CPMF), Negative Log Likelihood,
Over-dispersion and parameter estimation (shape and distribution
distinct parameters) can be explored for each fitted model with the
**``fitODBOD``** package. 

# Introduction

Statistical methods are widely used for research in most disciplines. There is a 
focus towards fitting distributions to given data since the distributions of 
data depends on the method of data collection. For example, consider a binomial
experiment where a fair coin is being tossed *n* times. Let the
event of landing heads-up be defined as the success of probability *p*.
Then, the number of heads out of *n* tosses is considered to be a single
binomial variable, *Y*. Also if similar binomial experiments occur in
*N* different clusters, a collection of *Y<sub>1<sub>*,
*Y<sub>2<sub>*, *Y<sub>3<sub>*, ..., *Y<sub>N<sub>* would form the BOD.
Such data are frequently mentioned in fields of toxicology, biology,
clinical medicine, epidemiology and many more. One may attempt to fit
the BOD using the traditional binomial distribution, as it is
characterized using the number of identical trials *n* and the
probability of success parameter *p*. The parameter *p* (*p* $\in$ [0, 1]) 
is usually assumed to be a constant from trial to trial and
the trials are independent. In many empirical situations, it has been
frequently observed that the actual observed variance of the BOD is
greater than the assumed theoretical binomial variance. This outcome is
typically known as "over-dispersion" [@Cox1983; @anderson1988].
Over-dispersion in BOD can occur either with a probability of success
parameter *p* varying from trial to trial or if there is a correlation
among binary trials. However, @collett1991 argued that the above two
cases of over-dispersion are frequently the same.

New distributions emerged to fit the BOD replacing the traditional
binomial distribution. @Xiaohu2011 have developed the
Kumaraswamy Binomial distribution, @Rodriguez-Avi2007 have
constructed the Gaussian Hypergeometric Generalized Beta-Binomial
distribution, @Karlis2008 wrote the article on the
Triangular Binomial distribution. Also, @grassia1977 mentioned the 
Gamma Binomial and Grassia II Binomial distributions. The Beta-Binomial 
distribution is clearly explained in @johnson1995. Initially the concept 
of mixing the binomial distribution with a unit bounded continuous distribution 
was done by @Horsnell, which led to the Uniform Binomial distribution. 
Recently, @Manoj2013 had developed the McDonald Generalized Beta-Binomial 
distribution. Based on this research only the **``fitODBOD``** (version 1.1.0) 
package was released to CRAN in February, 2018. Recently this package became 
available on [GitHub](https://github.com/Amalan-ConStat/R-fitODBOD) 
and has its own [website](https://amalan-constat.github.io/R-fitODBOD/index.html), 
which has made the package more convenient for researchers who intend to use it.

Further, new types of binomial distributions were developed replacing the
traditional binomial distribution, which are called Alternate
Binomial Distributions. @Multiplicative has developed the Multiplicative
Binomial distribution, while recently @Lovinson has done more
research to form the Lovinson Multiplicative Binomial distribution. COM
Poisson Binomial distribution was introduced first by @COMPoisson. 
The comparison of Beta-Correlated Binomial distribution with
Correlated Binomial distribution was done by @Correlated.
Version 1.4.0 of **``fitODBOD``** (@fitODBOD) holds all the distributions mentioned 
above and in the future more distributions developed to fit the BOD will
be added to the package as major version updates.

# Modelling

To fit a Binomial Mixture distribution for a raw BOD set, the following
steps have to be used when using this package.

1.  Extract the data in a meaningful way (**BODextract** function).
2.  Check whether the binomial distribution can be fitted and if not
    test the over-dispersion (**fitBin** function) by using Pearson
    Chi-square goodness of fit test.
3.  If over-dispersion exists, estimate the parameters for each
    distribution for the given data separately (**EstMLETriBin**,
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

Series of code to complete the steps from 1 to 5 are thoroughly discussed in 
the [README file](https://github.com/Amalan-ConStat/R-fitODBOD/blob/master/README.md) in the GitHub repository.

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
steps outlined above, the best-fitting Binomial Mixture 
Distribution and/or Alternate Binomial Distribution is determined.

# Main Dependencies

**``fitODBOD``** package has three main dependencies from CRAN. Functions from 
**``hypergeo``** are used for applications of GHGBB and Gaussian Hypergeometric 
Generalized Beta distribution. **``stats``** functions are used for integration 
situations for the Triangular Binomial distribution. Finally, **``bbmle``** 
package is used for the parameter estimation of ABD and FBMD under the concept
of Maximum Likelihood Estimation.

# References
