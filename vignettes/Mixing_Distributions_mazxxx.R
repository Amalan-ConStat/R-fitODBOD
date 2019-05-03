## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(fitODBOD)
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)

## ----Comparing the mean values, echo=FALSE-------------------------------
# a=3, b=9 and mean output
cat("Mean from dBETA function for (a=3, b=9) =",dBETA(0.5,3,9)$mean,"\n")

# a=3, b=9 and first moment 
cat("Mean from mazBETA function for (a=3, b=9) =",mazBETA(1,3,9))

## ----Comparing the variance, echo=FALSE----------------------------------
# a=3,b=9 and variance output
cat("Variance from dBETA function for (a=3,b=9) =",dBETA(0.5,3,9)$var,"\n")

# a=3, b=9, first moment and second moment
cat("Variance from mazBETA function for (a=3,b=9) =",mazBETA(2,3,9)-mazBETA(1,3,9)*mazBETA(1,3,9))

