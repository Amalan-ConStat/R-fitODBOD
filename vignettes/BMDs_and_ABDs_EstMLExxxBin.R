## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(fitODBOD)
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)
library(bbmle)

## ----Printing data-------------------------------------------------------
# estimating parameter for p and alpha using Additive Binomial Distribution of Male_children data
Male_Children

## ----printing estimated parameters for Additive Binomial Distribution,echo=FALSE----
# estimating parameter for p and alpha using Additive Binomial Distribution
Est_para<-suppressWarnings(EstMLEAddBin(Male_Children$No_of_Males,Male_Children$freq))
cat("Estimated parameter p for Male_children data=",Est_para$p, "\n")
cat("Estimated parameter alpha for Male_children data=",Est_para$alpha,"\n")

## ----printing estimated parameter for Triangular Binomial Distribution,echo=FALSE----
# estimating parameter for mode using Triangular Binomial Distribution of Male_children data
Est_para<-suppressWarnings(EstMLETriBin(Male_Children$No_of_Males,Male_Children$freq))
cat("Estimated parameter mode or c for Male_children data=",Est_para$mode, "\n")

## ----printing estimated parameters for Beta-Correlated Binomial Distribution, echo=FALSE----
#estimating parameters for Beta-Correlated Binomial Distribution of Male_children data
Est_para<-suppressWarnings(mle2(EstMLEBetaCorrBin,
                           data=list(x=Male_Children$No_of_Males,freq=Male_Children$freq),
                           start=list(cov=0.05,a=10,b=10)))
cat("Estimated parameter covariance for Male_children data=",coef(Est_para)[1],"\n")
cat("Estimated parameter a for Male_children data=",coef(Est_para)[2], "\n")
cat("Estimated parameter b for Male_children data=",coef(Est_para)[3], "\n")

## ----printing estimated parameters for Correlated Binomial Distribution,echo=FALSE----
#estimating parameters for Correlated Binomial Distribution of Male_children data
Est_para<-suppressWarnings(mle2(EstMLECorrBin,
                           data=list(x=Male_Children$No_of_Males,freq=Male_Children$freq),
                           start=list(cov=0.0005,p=0.51)))
cat("Estimated parameter p for Male_children data=",coef(Est_para)[1],"\n")
cat("Estimated parameter covariance for Male_children data=",coef(Est_para)[2], "\n")

## ----printing estimated parameters for COM Poisson Binomial Distribution,echo=FALSE----
#estimating parameters for COM Poisson Binomial Distribution of Male_children data
Est_para<-suppressWarnings(mle2(EstMLECOMPBin,
                           data=list(x=Male_Children$No_of_Males,freq=Male_Children$freq),
                           start=list(v=0.0005,p=0.51)))
cat("Estimated parameter p for Male_children data=",coef(Est_para)[1],"\n")
cat("Estimated parameter covariance for Male_children data=",coef(Est_para)[2], "\n")

## ----printing estimated parameters for Multiplicative Binomial Distribution,echo=FALSE----
#estimating parameters for Multiplicative Binomial Distribution of Male_children data
Est_para<-suppressWarnings(mle2(EstMLEMultiBin,
                           data=list(x=Male_Children$No_of_Males,freq=Male_Children$freq),
                           start=list(theta=15,p=0.51)))
cat("Estimated parameter p for Male_children data=",coef(Est_para)[1],"\n")
cat("Estimated parameter theta for Male_children data=",coef(Est_para)[2], "\n")

## ----printing estimated parameters for Beta-Binomial Distribution,echo=FALSE----
#estimating parameters for Beta-Binomial Distribution of Male_children data
Est_para<-suppressWarnings(mle2(EstMLEBetaBin,
                           data=list(x=Male_Children$No_of_Males,freq=Male_Children$freq),
                           start=list(a=15,b=0.51)))
cat("Estimated parameter a for Male_children data=",coef(Est_para)[1],"\n")
cat("Estimated parameter b for Male_children data=",coef(Est_para)[2], "\n")

## ----printing estimated parameters for Kumaraswamy Binomial Distribution, echo=FALSE----
#estimating parameters for Kumaraswamy Binomial Distribution of Male_children data
Est_para<-suppressWarnings(mle2(EstMLEKumBin,
                           data=list(x=Male_Children$No_of_Males,freq=Male_Children$freq),
                           start=list(a=15,b=10,it=5000)))
cat("Estimated parameter a for Male_children data=",coef(Est_para)[1],"\n")
cat("Estimated parameter b for Male_children data=",coef(Est_para)[2], "\n")
cat("Estimated parameter iteration for Male_children data=",coef(Est_para)[3], "\n")

## ----printing estimated parameters for GHGBB Distribution, echo=FALSE----
#estimating parameters for GHGBB Distribution of Male_children data
Est_para<-suppressWarnings(mle2(EstMLEGHGBB,
                           data=list(x=Male_Children$No_of_Males,freq=Male_Children$freq),
                           start=list(a=15,b=10,c=20)))
cat("Estimated parameter a for Male_children data=",coef(Est_para)[1],"\n")
cat("Estimated parameter b for Male_children data=",coef(Est_para)[2], "\n")
cat("Estimated parameter c for Male_children data=",coef(Est_para)[3], "\n")

## ----printing estimated parameters for McGBB Distribution, echo=FALSE----
#estimating parameters for McGBB Distribution of Male_children data
Est_para<-suppressWarnings(mle2(EstMLEMcGBB,
                           data=list(x=Male_Children$No_of_Males,freq=Male_Children$freq),
                           start=list(a=1,b=10,c=20)))
cat("Estimated parameter a for Male_children data=",coef(Est_para)[1],"\n")
cat("Estimated parameter b for Male_children data=",coef(Est_para)[2], "\n")
cat("Estimated parameter c for Male_children data=",coef(Est_para)[3], "\n")

