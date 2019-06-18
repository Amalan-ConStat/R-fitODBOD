## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(fitODBOD)
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)
library(bbmle)

## ----Estimating parameters and printing Negative Log Likelihood value, echo=FALSE----
Est_para<-EstMLEBetaBin(x=Male_Children$No_of_Males,freq=Male_Children$freq,
                        a=10,b=10)

Negllvalue<-NegLLBetaBin(Male_Children$No_of_Males,Male_Children$freq,
                         coef(Est_para)[1],coef(Est_para)[2])
cat("Minimized Negative Log Likelihood value for Male_children data =",Negllvalue)

