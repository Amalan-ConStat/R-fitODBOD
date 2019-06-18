## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(fitODBOD)
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)
library(bbmle)
library(tibble)
library(ggthemes)

## ----plotting Expected frequencies with actual frequency for Binomial Distribution, warning=FALSE----
Alcohol_data

BinRanVar <- Alcohol_data$Days
ActFreq <- Alcohol_data$week2

# Fitting Binomial Distribution
BinFreq <- fitBin(BinRanVar,ActFreq)

# printing the results of fitting Binomial distribution
print(BinFreq)

## ----plotting Expected frequencies with actual frequency for Additive Binomial Distribution, warning=FALSE----
# Estimating and fitting Additive Binomial distribution
Para_AddBin <- EstMLEAddBin(BinRanVar,ActFreq)

# printing the coefficients and using them
coef(Para_AddBin)

AddBin_p <- Para_AddBin$p
AddBin_alpha <- Para_AddBin$alpha

# Fitting Additive Binomial Distribution
AddBinFreq <- fitAddBin(BinRanVar, ActFreq, AddBin_p, AddBin_alpha)

# printing the results of fitting Additive Binomial Distribution
print(AddBinFreq)

## ----plotting Expected frequencies with actual frequency for Beta-Correlated Binomial Distribution, warning=FALSE----
# Estimating and fitting Beta Correlated Binoial Distribution
Para_BetaCorrBin <- EstMLEBetaCorrBin(x=BinRanVar, freq=ActFreq,
                                    cov=0.001,a=10,b=10)

# printing the coefficients and using them
coef(Para_BetaCorrBin)

BetaCorrBin_cov <- coef(Para_BetaCorrBin)[1]
BetaCorrBin_a <- coef(Para_BetaCorrBin)[2]
BetaCorrBin_b <- coef(Para_BetaCorrBin)[3]

# Fitting Beta-Correlated Binomial Distribution
BetaCorrBinFreq <- fitBetaCorrBin(BinRanVar,ActFreq,BetaCorrBin_cov,
                                BetaCorrBin_a,BetaCorrBin_b)

# printing the results of fitting Beta-Correlated Binomial Distribution
print(BetaCorrBinFreq)

## ----plotting Expected frequencies with actual frequency for COM-Poisson Binomial Distribution, warning=FALSE----
# Estimating and fitting COM Poisson Binomial Distribution
Para_COMPBin <- EstMLECOMPBin(x=BinRanVar, freq=ActFreq,
                            v=12.1,p=0.9)

# printing the coefficients and using them
coef(Para_COMPBin)

COMPBin_p <- coef(Para_COMPBin)[1]
COMPBin_v <- coef(Para_COMPBin)[2]

# Fitting COM-Poisson Binomial Distribution
COMPBinFreq <- fitCOMPBin(BinRanVar, ActFreq,COMPBin_p,COMPBin_v)

# printing the results of fitting COM-Poisson Binomial Distribution
print(COMPBinFreq)

## ----plotting Expected frequencies with actual frequency for Correlated Binomial Distribution, warning=FALSE----
# Estimating and fitting Correlated Binomial Distribution
Para_CorrBin <- EstMLECorrBin(x=BinRanVar, freq=ActFreq,
                            cov=0.0021,p=0.19)

# printing the coefficients and using them
coef(Para_CorrBin)

CorrBin_p <- coef(Para_CorrBin)[1]
CorrBin_cov <- coef(Para_CorrBin)[2]

# Fitting Correlated Binomial Distribution
CorrBinFreq <- fitCorrBin(BinRanVar, ActFreq,CorrBin_p,CorrBin_cov)

# printing the results of fitting Correlated Binomial Distribution
print(CorrBinFreq)

## ----plotting Expected frequencies with actual frequency for Multiplicative Binomial Distribution, warning=FALSE----
# Estimating and fitting Multiplicative Binomial Distribution
Para_MultiBin <- EstMLEMultiBin(x=BinRanVar, freq=ActFreq,
                              theta=21,p=0.19)

# printing the coefficients and using them
coef(Para_MultiBin)

MultiBin_p <- coef(Para_MultiBin)[1]
MultiBin_theta <- coef(Para_MultiBin)[2]

# Fitting Multiplicative Binomial Distribution
MultiBinFreq <- fitMultiBin(BinRanVar, ActFreq,MultiBin_p,MultiBin_theta)

# printing the results of fitting Multiplicative Binomial Distribution
print(MultiBinFreq)

## ----plotting Expected frequencies with actual frequency for Lovinson Multiplicative Binomial Distribution, warning=FALSE----
# Estimating and fitting Lovinson Multiplicative Binomial Distribution
Para_LMBin <- EstMLELMBin(x=BinRanVar, freq=ActFreq,
                        phi=21,p=0.19)

# printing the coefficients and using them
coef(Para_LMBin)

LMBin_p <- coef(Para_LMBin)[1]
LMBin_phi <- coef(Para_LMBin)[2]

# Fitting Lovinson Multiplicative Binomial Distribution
LMBinFreq <- fitLMBin(BinRanVar, ActFreq,LMBin_p,LMBin_phi)

# printing the results of fitting Multiplicative Binomial Distribution
print(LMBinFreq)

## ----plotting that dataset for ABD,fig.align='center',fig.height=7,fig.width=9,echo=FALSE----
# creating dataset for plotting
ABD_Data <- tibble(
                  w=BinRanVar,
                  x=ActFreq,
                  y=fitted(BinFreq),
                  z=fitted(AddBinFreq),
                  a=fitted(BetaCorrBinFreq),
                  b=fitted(COMPBinFreq),
                  c=fitted(CorrBinFreq),
                  d=fitted(MultiBinFreq),
                  e=fitted(LMBinFreq)
                )
names(ABD_Data) <- c("Bin_RV","Actual_Freq","EstFreq_BinD","EstFreq_AddBinD","EstFreq_BetaCorrBinD","EstFreq_COMPBinD",
                   "EstFreq_CorrBinD","EstFreq_MultiBinD","EstFreq_LMBinD")


ABD_freq <- ggplot(ABD_Data)+theme_get()+
                     geom_point(aes(x=Bin_RV,y=Actual_Freq),color="red",size=2)+
                     geom_line(aes(x=Bin_RV,y=Actual_Freq),color="red",size=1)+
                     geom_point(aes(x=Bin_RV,y=EstFreq_BinD),color="darkblue",size=2)+
                     geom_line(aes(x=Bin_RV,y=EstFreq_BinD),color="darkblue",size=1)+
                     geom_point(aes(x=Bin_RV,y=EstFreq_AddBinD),color="forestgreen",size=3.25)+
                     geom_line(aes(x=Bin_RV,y=EstFreq_AddBinD),color="forestgreen",size=2.25)+
                     geom_point(aes(x=Bin_RV,y=EstFreq_BetaCorrBinD),color="yellow4",size=2)+
                     geom_line(aes(x=Bin_RV,y=EstFreq_BetaCorrBinD),color="yellow4",size=1)+
                     geom_point(aes(x=Bin_RV,y=EstFreq_COMPBinD),color="purple",size=2)+
                     geom_line(aes(x=Bin_RV,y=EstFreq_COMPBinD),color="purple",size=1)+
                     geom_point(aes(x=Bin_RV,y=EstFreq_CorrBinD),color="orangered",size=2)+
                     geom_line(aes(x=Bin_RV,y=EstFreq_CorrBinD),color="orangered",size=1)+
                     geom_point(aes(x=Bin_RV,y=EstFreq_MultiBinD),color="deeppink",size=3.25)+
                     geom_line(aes(x=Bin_RV,y=EstFreq_MultiBinD),color="deeppink",size=2.25)+
                     geom_point(aes(x=Bin_RV,y=EstFreq_LMBinD),color="#008080", size=2)+
                     geom_line(aes(x=Bin_RV,y=EstFreq_LMBinD),color="#008080",size=1)+
                     ggtitle("Plot of fitted and actual frequencies of Alcohol BOD week2")+
                     xlab("Binomial Random Variables from 0 to 7")+
                     ylab("Frequency Values Estimated and Actual")+
                     theme_clean()+
                     scale_y_continuous(breaks=seq(0,120,5))+
                     scale_x_continuous(breaks=seq(0,7,1))+
                     annotate("text",x=0,y=120,label="Actual",color="red")+
                     annotate("text",x=0,y=116,label="Binomial",color="darkblue")+
                     annotate("text",x=0.35,y=112,label="Additive-Binomial",color="forestgreen")+
                     annotate("text",x=0.55,y=108,label="Beta Correlated Binomial",color="yellow4")+
                     annotate("text",x=6.45,y=120,label="COM Poisson Binomial",color="purple")+
                     annotate("text",x=6.45,y=116,label="Correlated Binomial",color="orangered")+
                     annotate("text",x=6.45,y=112,label="Multiplicative Binomial",color="deeppink")+
                     annotate("text",x=6.15,y=108,label="Lovinson Multiplicative Binomial",color="#008080")

plotly::plotly_build(ABD_freq)

## ----plotting Expected frequencies with actual frequency for Triangular Binomial Distribution,warning=FALSE----
# Estimating and fitting Triangular Binomial distribution
Para_TriBin <- EstMLETriBin(BinRanVar,ActFreq)

# printing the coefficients and using them
coef(Para_TriBin)

TriBin_c <- Para_TriBin$mode

# Fitting Triangular Binomial Distribution
TriBinFreq <- fitTriBin(BinRanVar, ActFreq, TriBin_c)

# printing the results of fitting Triangular Binomial Distribution
print(TriBinFreq)

## ----plotting Expected frequencies with actual frequency for Beta-Binomial Distribution,warning=FALSE----
# Estimating and fitting Beta Correlated Binoial Distribution
Para_BetaBin <- EstMLEBetaBin(x=BinRanVar, freq=ActFreq,
                            a=10,b=10)
# printing the coefficients and using them
coef(Para_BetaBin)

BetaBin_a <- coef(Para_BetaBin)[1]
BetaBin_b <- coef(Para_BetaBin)[2]

# Fitting Beta-Binomial Distribution
BetaBinFreq <- fitBetaBin(BinRanVar, ActFreq,BetaBin_a,BetaBin_b)

# printing the results of fitting Beta-Binomial Distribution
print(BetaBinFreq)

## ----plotting Expected frequencies with actual frequency for Kumaraswamy Binomial Distribution,warning=FALSE----
# Estimating and fitting Kumaraswamy Binomial Distribution
Para_KumBin <- EstMLEKumBin(x=BinRanVar, freq=ActFreq,
                          a=12.1,b=0.9,it=10000)

# printing the coefficients and using them
coef(Para_KumBin)

KumBin_a <- coef(Para_KumBin)[1]
KumBin_b <- coef(Para_KumBin)[2]
KumBin_it <- coef(Para_KumBin)[3]

# Fitting Kumaraswamy Binomial Distribution
KumBinFreq <- fitKumBin(BinRanVar, ActFreq,KumBin_a,KumBin_b,KumBin_it*10)

# printing the results of fitting Kumaraswamy Binomial Distribution
print(KumBinFreq)

## ----plotting Expected frequencies with actual frequency for GHGBB Distribution,warning=FALSE----
# Estimating and fitting GHGBB Distribution
Para_GHGBB <- EstMLEGHGBB(x=BinRanVar, freq=ActFreq,
                        a=0.0021,b=0.19,c=0.3)

# printing the coefficients and using them
coef(Para_GHGBB)

GHGBB_a <- coef(Para_GHGBB)[1]
GHGBB_b <- coef(Para_GHGBB)[2]
GHGBB_c <- coef(Para_GHGBB)[3]

# Fitting GHGBB Distribution
GHGBBFreq <- fitGHGBB(BinRanVar, ActFreq,GHGBB_a,GHGBB_b,GHGBB_c)

# printing the results of fitting GHGBB Distribution
print(GHGBBFreq)

## ----plotting Expected frequencies with actual frequency for McGBB Distribution,warning=FALSE----
# Estimating and fitting McGBB Distribution
Para_McGBB <- EstMLEMcGBB(x=BinRanVar, freq=ActFreq,
                        a=21,b=0.19,c=0.1)

# printing the coefficients and using them
coef(Para_McGBB)

McGBB_a <- coef(Para_McGBB)[1]
McGBB_b <- coef(Para_McGBB)[2]
McGBB_c <- coef(Para_McGBB)[3]

# Fitting McGBB Distribution
McGBBFreq <- fitMcGBB(BinRanVar, ActFreq,McGBB_a,McGBB_b,McGBB_c)

# printing the results of fitting McGBB Distribution
print(McGBBFreq)

## ----plotting Expected frequencies with actual frequency for Gamma Binomial Distribution,warning=FALSE----
# Estimating and fitting Gamma Binoial Distribution
Para_GammaBin <- EstMLEGammaBin(x=BinRanVar, freq=ActFreq,
                              c=10,l=10)

# printing the coefficients and using them
coef(Para_GammaBin)

GammaBin_c <- coef(Para_GammaBin)[1]
GammaBin_l <- coef(Para_GammaBin)[2]

# Fitting Gamma Binomial Distribution
GammaBinFreq <- fitGammaBin(BinRanVar, ActFreq,GammaBin_c,GammaBin_l)

# printing the results of fitting Beta-Binomial Distribution
print(GammaBinFreq)

## ----plotting Expected frequencies with actual frequency for Grassia II Binomial Distribution,warning=FALSE----
# Estimating and fitting Grassia II Binoial Distribution
Para_GrassiaIIBin <- EstMLEGrassiaIIBin(x=BinRanVar, freq=ActFreq,
                                      a=10,b=10)
# printing the coefficients and using them
coef(Para_GrassiaIIBin)

GrassiaIIBin_a <- coef(Para_GrassiaIIBin)[1]
GrassiaIIBin_b <- coef(Para_GrassiaIIBin)[2]

# Fitting Grassia II Binomial Distribution
GrassiaIIBinFreq <- fitGrassiaIIBin(BinRanVar, ActFreq,GrassiaIIBin_a,GrassiaIIBin_b)

# printing the results of fitting Grassia II Binomial Distribution
print(GrassiaIIBinFreq)

## ----plotting that dataset for BMD,fig.align='center',fig.height=7,fig.width=9,echo=FALSE----
BMD_Data <- tibble(
                  w=BinRanVar,
                  x=ActFreq,
                  y=fitted(BinFreq),
                  z=fitted(TriBinFreq),
                  a=fitted(BetaBinFreq),
                  b=fitted(KumBinFreq),
                  c=fitted(GHGBBFreq),
                  d=fitted(McGBBFreq),
                  e=fitted(GammaBinFreq),
                  f=fitted(GrassiaIIBinFreq)
                )
names(BMD_Data) <- c("Bin_RV","Actual_Freq","EstFreq_BinD","EstFreq_TriBinD","EstFreq_BetaBinD","EstFreq_KumBinD",
                   "EstFreq_GHGBBD","EstFreq_McGBBD","EstFreq_GammaBinD","EstFreq_GrassiaIIBinD")

BMD_freq <- ggplot(BMD_Data)+theme_get()+
                     geom_point(aes(x=Bin_RV,y=Actual_Freq),color="red",size=2)+
                     geom_line(aes(x=Bin_RV,y=Actual_Freq),color="red",size=1)+
                     geom_point(aes(x=Bin_RV,y=EstFreq_BinD),color="darkblue",size=2)+
                     geom_line(aes(x=Bin_RV,y=EstFreq_BinD),color="darkblue",size=1)+
                     geom_point(aes(x=Bin_RV,y=EstFreq_TriBinD),color="forestgreen",size=2)+
                     geom_line(aes(x=Bin_RV,y=EstFreq_TriBinD),color="forestgreen",size=1)+
                     geom_point(aes(x=Bin_RV,y=EstFreq_BetaBinD),color="yellow4",size=3.75)+
                     geom_line(aes(x=Bin_RV,y=EstFreq_BetaBinD),color="yellow4",size=2.75)+
                     geom_point(aes(x=Bin_RV,y=EstFreq_KumBinD),color="purple",size=3)+
                     geom_line(aes(x=Bin_RV,y=EstFreq_KumBinD),color="purple",size=2.25)+
                     geom_point(aes(x=Bin_RV,y=EstFreq_GHGBBD),color="orangered",size=2)+
                     geom_line(aes(x=Bin_RV,y=EstFreq_GHGBBD),color="orangered",size=1)+
                     geom_point(aes(x=Bin_RV,y=EstFreq_McGBBD),color="deeppink",size=2.5)+
                     geom_line(aes(x=Bin_RV,y=EstFreq_McGBBD),color="deeppink",size=1.5)+
                     geom_point(aes(x=Bin_RV,y=EstFreq_GammaBinD),color="#7DBDBD",size=2)+
                     geom_line(aes(x=Bin_RV,y=EstFreq_GammaBinD),color="#7DBDBD",size=1.15)+
                     geom_point(aes(x=Bin_RV,y=EstFreq_GrassiaIIBinD),color="#FC92A3",size=1)+
                     geom_line(aes(x=Bin_RV,y=EstFreq_GrassiaIIBinD),color="#FC92A3",size=0.75)+
                     ggtitle("Plot of fitted and actual frequencies of Alcohol BOD week2")+
                     xlab("Binomial Random Variables from 0 to 7")+
                     ylab("Frequency Values Estimated and Actual")+
                     theme_clean()+
                     scale_y_continuous(breaks=seq(0,120,5))+
                     scale_x_continuous(breaks=seq(0,7,1))+
                     annotate("text",x=0,y=120,label="Actual",color="red")+
                     annotate("text",x=0,y=116,label="Binomial",color="darkblue")+
                     annotate("text",x=0.35,y=112,label="Triangular Binomial",color="forestgreen")+
                     annotate("text",x=0.25,y=108,label="Beta-Binomial",color="yellow4")+
                     annotate("text",x=6.5,y=120,label="Kumaraswamy Binomial",color="purple")+
                     annotate("text",x=6.95,y=116,label="GHGBB",color="orangered")+
                     annotate("text",x=6.95,y=112,label="McGBB",color="deeppink")+
                     annotate("text",x=6.5,y=108,label="Gamma Binomial",color="#7DBDBD")+
                     annotate("text",x=6.5,y=104,label="Grassia II Binomial",color="#FC92A3")

plotly::plotly_build(BMD_freq)


