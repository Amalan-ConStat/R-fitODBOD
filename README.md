R-fitODBOD
================

How to engage with "fitODBOD" the first time ?
----------------------------------------------

``` r
## Installing the package from GitHub
devtools::install_github("Amalan-ConStat/R-fitODBOD")

## Installing the package from CRAN
install.packages("fitODBOD")
```

Key Phrases
-----------

-   BOD (Binomial Outcome Data)
-   Over Dispersion
-   Under Dispersion
-   BMD (Binomial Mixture Distributions)
-   ABD (Alternate Binomial Distributions)
-   PMF (Probability Mass Function)
-   CPMF (Cumulative Probability Mass Function)

What does "fitODBOD" ?
----------------------

You can understand BMD & ABD with PMF & CPMF. Further, BOD can be modeled using these Distributions

Distributions
-------------

<table style="width:93%;">
<colgroup>
<col width="47%" />
<col width="45%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">Alternate Binomial Distrbutions</th>
<th align="left">Binomial Mixture Distributions</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">1.Additive Binomial Distribution</td>
<td align="left">1.Uniform Binomial Distribution</td>
</tr>
<tr class="even">
<td align="left">2.Beta-Correlated Binomial Distribution</td>
<td align="left">2.Triangular Binomial Distribution</td>
</tr>
<tr class="odd">
<td align="left">3.COM Poisson Binomial Distribution</td>
<td align="left">3.Beta-Binomial Distribution</td>
</tr>
<tr class="even">
<td align="left">4.Correlated Binomial Distribution</td>
<td align="left">4.Kumaraswamy Binomial Distribution</td>
</tr>
<tr class="odd">
<td align="left">5.Multiplicative Binomial Distribution</td>
<td align="left">5.Gaussian Hypergeometric Generalized Beta-Binomial Distribution</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left">6.McDonald Generalized Beta-Binomial Distribution</td>
</tr>
</tbody>
</table>

Just an example
---------------

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

*H*<sub>0</sub> : The Data follows the Binomial Distribution.

*H*<sub>1</sub> : The Data does not follow the Binomial Distribution.

``` r
#Checking if the above data  of Days and week2 follows Binomial Distribution
fitBin(Alcohol_data$Days,Alcohol_data$week2)
```

    ## 
    ## Chi-squared test for Binomial Distribution
    ## 
    ## 
    ##                 Observed Frequency :  42 47 54 40 49 40 43 84 
    ## 
    ##                 expected Frequency :  1.66 13.79 49.19 97.48 115.89 82.67 32.76 5.56 
    ## 
    ##                 X-squared = 2265.111 df = 6 p-value = 0

    ## Warning in fitBin(Alcohol_data$Days, Alcohol_data$week2): Chi-squared
    ## approximation may be doubtful because expected frequency is less than 5

According to p-value=0, *H*<sub>0</sub> is rejected at 5% significance level.

``` r
#Estimating the parameters a and b using bbmle package mle2 function
BetaBin=bbmle::mle2(EstMLEBetaBin,data=list(x=Alcohol_data$Days,
                        freq=Alcohol_data$week2), start=list(a=100.1,b=100.1))
a_est=bbmle::coef(BetaBin)[1]                    
b_est=bbmle::coef(BetaBin)[2]
```

Now, Checking if above data follows Beta-Binomial Distribution

*H*<sub>0</sub> : The Data follows the Beta-Binomial Distribution.

*H*<sub>1</sub> : The Data does not follow the Beta-Binomial Distribution.

``` r
#Checking if the above data follows Beta-Binomial Distribution
fitBetaBin(Alcohol_data$Days,Alcohol_data$week2,a_est,b_est)
```

    ## 
    ## Chi-squared test for Beta-Binomial Distribution 
    ## 
    ## 
    ##                  Observed Frequency :  42 47 54 40 49 40 43 84 
    ## 
    ##                  expected Frequency :  47.91 42.92 41.95 42.5 44.3 47.81 54.89 76.73 
    ## 
    ##                  X-squared = 9.7641 df = 5   p-value = 0.0822 
    ## 
    ##                  over dispersion = 0.390885

p-value=0.1524 indicates *H*<sub>0</sub> is not rejected at 5% significance level. Clearly Beta-Binomial Distribution is a better suit for the Alcohol BOD.

Further

``` r
#Actual Variance
Act_Var<-var(rep(Alcohol_data$Days,Alcohol_data$week2))
#Estimated Variance of Binomial Distribution
Est_Var_Bin<-var(rep(Alcohol_data$Days,c(1.66,13.79,49.19,97.48,115.89,82.67,32.76,5.56)))
#Estimated Variance of Beta-Binomial Distribution
Est_Var_BetaBin<-var(rep(Alcohol_data$Days,c(47.91,42.92,41.95,42.5,44.3,47.81,54.89,76.73)))
```

| Type of Variance                                                  |    Values|
|:------------------------------------------------------------------|---------:|
| Actual                                                            |  5.787333|
| From Expected frequencies of estimated Binomial Distribution      |  1.694534|
| From Expected frequencies of estimated Beta-Binomial Distribution |  5.804344|

![](README_files/figure-markdown_github/Printing%20variance%20and%20plotting%20frequencies-1.png)

Variance(5.8043) from Beta-Binomial Distribution's estimated frequencies is closer to the Actual Variance(5.7873) of Alcohol BOD week 2 than variance(1.6945) of expected frequencies by Binomial Distribution.

According to the plot, it is clearly seen that Beta-Binomial estimated frequencies behave very close to actual frequency values than the estimate frequencies from Binomial distribution. Or the Red line is very similar and close to the Blue line than the Green line.

Thank You
---------
