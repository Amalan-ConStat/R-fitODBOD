## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(fitODBOD)
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)
library(bbmle)

## ----Printing data-------------------------------------------------------
# estimating parameter for p and alpha using Additive Binomial Distribution of Chromosome data
Chromosome_data

## ----printing estimated parameters for Additive Binomial Distribution,echo=FALSE----
# estimating parameter for p and alpha using Additive Binomial Distribution
Est_para<-EstMLEAddBin(Chromosome_data$No.of.Asso,Chromosome_data$fre)
cat("Estimated parameter p for Chromosome data=",Est_para$p, "\n")
cat("Estimated parameter alpha for Chromosome data=",Est_para$alpha,"\n")

## ----printing estimated parameter for Triangular Binomial Distribution,echo=FALSE----
# estimating parameter for mode using Triangular Binomial Distribution of Chromosome data
Est_para<-EstMLETriBin(Chromosome_data$No.of.Asso,Chromosome_data$fre)
cat("Estimated parameter mode or c for Chromosome data=",Est_para$mode, "\n")

## ----printing estimated parameters for Beta-Correlated Binomial Distribution, echo=FALSE----
#estimating parameters for Beta-Correlated Binomial Distribution of Chromosome data
Est_para<-EstMLEBetaCorrBin(x=Chromosome_data$No.of.Asso,freq=Chromosome_data$fre,
                            cov=0.05,a=10,b=10)
cat("Estimated parameter covariance for Chromosome data=",coef(Est_para)[1],"\n")
cat("Estimated parameter a for Chromosome data=",coef(Est_para)[2], "\n")
cat("Estimated parameter b for Chromosome data=",coef(Est_para)[3], "\n")

## ----printing estimated parameters for Correlated Binomial Distribution,echo=FALSE----
#estimating parameters for Correlated Binomial Distribution of Chromosome data
Est_para<-EstMLECorrBin(x=Chromosome_data$No.of.Asso,freq=Chromosome_data$fre,
                        cov=0.0005,p=0.51)
cat("Estimated parameter p for Chromosome data=",coef(Est_para)[1],"\n")
cat("Estimated parameter covariance for Chromosome data=",coef(Est_para)[2], "\n")

## ----printing estimated parameters for COM Poisson Binomial Distribution,echo=FALSE----
#estimating parameters for COM Poisson Binomial Distribution of Chromosome data
Est_para<-EstMLECOMPBin(x=Chromosome_data$No.of.Asso,freq=Chromosome_data$fre,
                        v=0.0005,p=0.51)
cat("Estimated parameter p for Chromosome data=",coef(Est_para)[1],"\n")
cat("Estimated parameter covariance for Chromosome data=",coef(Est_para)[2], "\n")

## ----printing estimated parameters for Multiplicative Binomial Distribution,echo=FALSE----
#estimating parameters for Multiplicative Binomial Distribution of Chromosome data
Est_para<-EstMLEMultiBin(x=Chromosome_data$No.of.Asso,freq=Chromosome_data$fre,
                         theta=15,p=0.51)
cat("Estimated parameter p for Chromosome data=",coef(Est_para)[1],"\n")
cat("Estimated parameter theta for Chromosome data=",coef(Est_para)[2], "\n")

## ----printing estimated parameters for Lovinson Multiplicative Binomial Distribution,echo=FALSE----
#estimating parameters for Lovinson Multiplicative Binomial Distribution of Chromosome data
Est_para<-EstMLELMBin(x=Chromosome_data$No.of.Asso,freq=Chromosome_data$fre,
                      phi=15,p=0.51)
cat("Estimated parameter p for Chromosome data=",coef(Est_para)[1],"\n")
cat("Estimated parameter phi for Chromosome data=",coef(Est_para)[2], "\n")

## ----printing estimated parameters for Beta-Binomial Distribution,echo=FALSE----
#estimating parameters for Beta-Binomial Distribution of Chromosome data
Est_para<-EstMLEBetaBin(x=Chromosome_data$No.of.Asso,freq=Chromosome_data$fre,
                        a=15,b=0.51)
cat("Estimated parameter a for Chromosome data=",coef(Est_para)[1],"\n")
cat("Estimated parameter b for Chromosome data=",coef(Est_para)[2], "\n")

## ----printing estimated parameters for Kumaraswamy Binomial Distribution, echo=FALSE----
#estimating parameters for Kumaraswamy Binomial Distribution of Chromosome data
Est_para<-EstMLEKumBin(x=Chromosome_data$No.of.Asso,freq=Chromosome_data$fre,
                       a=15,b=10,it=7500)
cat("Estimated parameter a for Chromosome data=",coef(Est_para)[1],"\n")
cat("Estimated parameter b for Chromosome data=",coef(Est_para)[2], "\n")
cat("Estimated parameter iteration for Chromosome data=",coef(Est_para)[3], "\n")

## ----printing estimated parameters for GHGBB Distribution, echo=FALSE----
#estimating parameters for GHGBB Distribution of Chromosome data
Est_para<-EstMLEGHGBB(x=Chromosome_data$No.of.Asso,freq=Chromosome_data$fre,
                      a=15,b=10,c=20)
cat("Estimated parameter a for Chromosome data=",coef(Est_para)[1],"\n")
cat("Estimated parameter b for Chromosome data=",coef(Est_para)[2], "\n")
cat("Estimated parameter c for Chromosome data=",coef(Est_para)[3], "\n")

## ----printing estimated parameters for McGBB Distribution, echo=FALSE----
#estimating parameters for McGBB Distribution of Chromosome data
Est_para<-EstMLEMcGBB(x=Chromosome_data$No.of.Asso,freq=Chromosome_data$fre,
                      a=12,b=10,c=20)
cat("Estimated parameter a for Chromosome data=",coef(Est_para)[1],"\n")
cat("Estimated parameter b for Chromosome data=",coef(Est_para)[2], "\n")
cat("Estimated parameter c for Chromosome data=",coef(Est_para)[3], "\n")

## ----printing estimated parameters for Gamma Binomial Distribution,echo=FALSE----
#estimating parameters for Gamma Binomial Distribution of Chromosome data
Est_para<-EstMLEGammaBin(x=Chromosome_data$No.of.Asso,freq=Chromosome_data$fre,
                         c=15,l=0.51)
cat("Estimated parameter c for Chromosome data=",coef(Est_para)[1],"\n")
cat("Estimated parameter l for Chromosome data=",coef(Est_para)[2], "\n")

## ----printing estimated parameters for Grassia II Binomial Distribution,echo=FALSE----
#estimating parameters for Grassia II Binomial Distribution of Chromosome data
Est_para<-EstMLEGrassiaIIBin(x=Chromosome_data$No.of.Asso,freq=Chromosome_data$fre,
                             a=15,b=0.51)
cat("Estimated parameter a for Chromosome data=",coef(Est_para)[1],"\n")
cat("Estimated parameter b for Chromosome data=",coef(Est_para)[2], "\n")

