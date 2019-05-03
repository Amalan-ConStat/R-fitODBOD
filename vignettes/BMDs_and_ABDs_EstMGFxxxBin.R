## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(fitODBOD)

## ----Estimating parameters a and b for Chromosome data,echo=FALSE--------
Chromosome_data #printing the Chromosome_data

# Estimating the parameters using EstMGFBetaBin and printing them
Est_para<-EstMGFBetaBin(Chromosome_data$No.of.Asso,Chromosome_data$fre)
cat("Estimated alpha parameter for Chromosome data =",Est_para$a,"\n")
cat("Estimated beta parameter for Chromosome data =",Est_para$b)

## ----Estimating parameters a and b for Male_children data,echo=FALSE-----
Male_Children #printing the Male Children data

# Estimating the parameters using EstMGFBetaBin and printing them
Est_para<-EstMGFBetaBin(Male_Children$No_of_Males,Male_Children$freq)
cat("Estimated alpha parameter Male_children data=",Est_para$a,"\n")
cat("Estimated beta parameter Male_children data=",Est_para$b)


