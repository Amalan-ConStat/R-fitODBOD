## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(fitODBOD)
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)
library(bbmle)
library(tibble)

## ----plotting Expected frequencies with actual frequency for ABD,include=FALSE, warning=FALSE----
Alcohol_data

BinRanVar<-Alcohol_data$Days
ActFreq<-Alcohol_data$week2

# Fitting Binoial Distribution
BinFreq<-suppressWarnings(fitBin(BinRanVar,ActFreq,print=F)$exp.freq)

# Estimating and fitting Additive Binomial distribution
Para_AddBin<-suppressWarnings(EstMLEAddBin(BinRanVar,ActFreq))
AddBin_p<-Para_AddBin$p
AddBin_alpha<-Para_AddBin$alpha

AddBinFreq<-suppressWarnings(fitAddBin(BinRanVar, ActFreq, AddBin_p, AddBin_alpha, print=F)$exp.freq)

# Estimating and fitting Beta Correlated Binoial Distribution
Para_BetaCorrBin<-suppressWarnings(mle2(EstMLEBetaCorrBin, 
                       data=list(x=BinRanVar, freq=ActFreq),
                       start=list(cov=0.001,a=10,b=10)))
BetaCorrBin_cov<-coef(Para_BetaCorrBin)[1]
BetaCorrBin_a<-coef(Para_BetaCorrBin)[2]
BetaCorrBin_b<-coef(Para_BetaCorrBin)[3]

BetaCorrBinFreq<-suppressWarnings(fitBetaCorrBin(BinRanVar, ActFreq,BetaCorrBin_cov,BetaCorrBin_a,BetaCorrBin_b,print=F)$exp.freq)

# Estimating and fitting COM Poisson Binomial Distribution

Para_COMPBin<-suppressWarnings(mle2(EstMLECOMPBin, 
                       data=list(x=BinRanVar, freq=ActFreq),
                       start=list(v=12.1,p=0.9)))
COMPBin_p<-coef(Para_COMPBin)[1]
COMPBin_v<-coef(Para_COMPBin)[2]

COMPBinFreq<-suppressWarnings(fitCOMPBin(BinRanVar, ActFreq,COMPBin_p,COMPBin_v,print=F)$exp.freq)

# Estimating and fitting Correlated Binomial Distribution

Para_CorrBin<-suppressWarnings(mle2(EstMLECorrBin, 
                       data=list(x=BinRanVar, freq=ActFreq),
                       start=list(cov=0.0021,p=0.19)))
CorrBin_p<-coef(Para_CorrBin)[1]
CorrBin_cov<-coef(Para_CorrBin)[2]

CorrBinFreq<-suppressWarnings(fitCorrBin(BinRanVar, ActFreq,CorrBin_p,CorrBin_cov,print=F)$exp.freq)

# Estimating and fitting Multiplicative Binomial Distribution

Para_MultiBin<-suppressWarnings(mle2(EstMLEMultiBin, 
                       data=list(x=BinRanVar, freq=ActFreq),
                       start=list(theta=21,p=0.19)))
MultiBin_p<-coef(Para_MultiBin)[1]
MultiBin_theta<-coef(Para_MultiBin)[2]

MultiBinFreq<-suppressWarnings(fitMultiBin(BinRanVar, ActFreq,MultiBin_p,MultiBin_theta,print=F)$exp.freq)

# creating dataset for plotting 

ABD_Data<-tibble(
                  w=BinRanVar,
                  x=ActFreq,
                  y=BinFreq,
                  z=AddBinFreq,
                  a=BetaCorrBinFreq,
                  b=COMPBinFreq,
                  c=CorrBinFreq,
                  d=MultiBinFreq
                )
names(ABD_Data)<-c("Bin_RV","Actual_Freq","EstFreq_BinD","EstFreq_AddBinD","EstFreq_BetaCorrBinD","EstFreq_COMPBinD","EstFreq_CorrBinD","EstFreq_MultiBinD" )

## ----plotting that dataset for ABD,fig.align='center',fig.height=7,fig.width=9,echo=FALSE----

ABD_freq<-ggplot(ABD_Data)+theme_get()+
                     geom_point(aes(x=ABD_Data$Bin_RV,y=ABD_Data$Actual_Freq),color="red",size=2)+
                     geom_line(aes(x=ABD_Data$Bin_RV,y=ABD_Data$Actual_Freq),color="red",size=1)+
                     geom_point(aes(x=ABD_Data$Bin_RV,y=ABD_Data$EstFreq_BinD),color="darkblue",size=2)+
                     geom_line(aes(x=ABD_Data$Bin_RV,y=ABD_Data$EstFreq_BinD),color="darkblue",size=1)+
                     geom_point(aes(x=ABD_Data$Bin_RV,y=ABD_Data$EstFreq_AddBinD),color="forestgreen",size=3.25)+
                     geom_line(aes(x=ABD_Data$Bin_RV,y=ABD_Data$EstFreq_AddBinD),color="forestgreen",size=2.25)+
                     geom_point(aes(x=ABD_Data$Bin_RV,y=ABD_Data$EstFreq_BetaCorrBinD),color="yellow4",size=2)+
                     geom_line(aes(x=ABD_Data$Bin_RV,y=ABD_Data$EstFreq_BetaCorrBinD),color="yellow4",size=1)+
                     geom_point(aes(x=ABD_Data$Bin_RV,y=ABD_Data$EstFreq_COMPBinD),color="purple",size=2)+
                     geom_line(aes(x=ABD_Data$Bin_RV,y=ABD_Data$EstFreq_COMPBinD),color="purple",size=1)+
                     geom_point(aes(x=ABD_Data$Bin_RV,y=ABD_Data$EstFreq_CorrBinD),color="orangered",size=2)+
                     geom_line(aes(x=ABD_Data$Bin_RV,y=ABD_Data$EstFreq_CorrBinD),color="orangered",size=1)+
                     geom_point(aes(x=ABD_Data$Bin_RV,y=ABD_Data$EstFreq_MultiBinD),color="deeppink",size=2)+
                     geom_line(aes(x=ABD_Data$Bin_RV,y=ABD_Data$EstFreq_MultiBinD),color="deeppink",size=1)+
                     ggtitle("Plot of fitted and actual frequencies of Alcohol BOD week2")+
                     xlab("Binomial Random Variables from 0 to 7")+
                     ylab("Frequency Values Estimated and Actual")+
                     scale_y_continuous(breaks=seq(0,120,5))+
                     scale_x_continuous(breaks=seq(0,7,1))+
                     annotate("text",x=0,y=120,label="Actual",color="red")+
                     annotate("text",x=0,y=116,label="Binomial",color="darkblue")+
                     annotate("text",x=0.25,y=112,label="Additive-Binomial",color="forestgreen")+
                     annotate("text",x=0.45,y=108,label="Beta Correlated Binomial",color="yellow4")+
                     annotate("text",x=6.65,y=120,label="COM Poisson Binomial",color="purple")+  
                     annotate("text",x=6.65,y=116,label="Correlated Binomial",color="orangered")+  
                     annotate("text",x=6.65,y=112,label="Multiplicative Binomial",color="deeppink")

  ABD_freq


## ----plotting Expected frequencies with actual frequency for BMD,include=FALSE, warning=FALSE----
Alcohol_data

BinRanVar<-Alcohol_data$Days
ActFreq<-Alcohol_data$week2

# Fitting Binoial Distribution
BinFreq<-suppressWarnings(fitBin(BinRanVar,ActFreq,print=F)$exp.freq)

# Estimating and fitting Triangular Binomial distribution
Para_TriBin<-suppressWarnings(EstMLETriBin(BinRanVar,ActFreq))
TriBin_c<-Para_TriBin$mode


TriBinFreq<-suppressWarnings(fitTriBin(BinRanVar, ActFreq, TriBin_c, print=F)$exp.freq)

# Estimating and fitting Beta Correlated Binoial Distribution
Para_BetaBin<-suppressWarnings(mle2(EstMLEBetaBin, 
                       data=list(x=BinRanVar, freq=ActFreq),
                       start=list(a=10,b=10)))
BetaBin_a<-coef(Para_BetaBin)[1]
BetaBin_b<-coef(Para_BetaBin)[2]

BetaBinFreq<-suppressWarnings(fitBetaBin(BinRanVar, ActFreq,BetaBin_a,BetaBin_b,print=F)$exp.freq)

# Estimating and fitting Kumaraswamy Binomial Distribution

Para_KumBin<-suppressWarnings(mle2(EstMLEKumBin, 
                       data=list(x=BinRanVar, freq=ActFreq),
                       start=list(a=12.1,b=0.9,it=10000)))
KumBin_a<-coef(Para_KumBin)[1]
KumBin_b<-coef(Para_KumBin)[2]
KumBin_it<-coef(Para_KumBin)[3]

KumBinFreq<-suppressWarnings(fitKumBin(BinRanVar, ActFreq,KumBin_a,KumBin_b,KumBin_it*10,print=F)$exp.freq)

# Estimating and fitting GHGBB Distribution

Para_GHGBB<-suppressWarnings(mle2(EstMLEGHGBB, 
                       data=list(x=BinRanVar, freq=ActFreq),
                       start=list(a=0.0021,b=0.19,c=0.3)))
GHGBB_a<-coef(Para_GHGBB)[1]
GHGBB_b<-coef(Para_GHGBB)[2]
GHGBB_c<-coef(Para_GHGBB)[3]

GHGBBFreq<-suppressWarnings(fitGHGBB(BinRanVar, ActFreq,GHGBB_a,GHGBB_b,GHGBB_c,print=F)$exp.freq)

# Estimating and fitting McGBB Distribution

Para_McGBB<-suppressWarnings(mle2(EstMLEMcGBB, 
                       data=list(x=BinRanVar, freq=ActFreq),
                       start=list(a=21,b=0.19,c=0.1)))
McGBB_a<-coef(Para_McGBB)[1]
McGBB_b<-coef(Para_McGBB)[2]
McGBB_c<-coef(Para_McGBB)[3]

McGBBFreq<-suppressWarnings(fitMcGBB(BinRanVar, ActFreq,McGBB_a,McGBB_b,McGBB_c,print=F)$exp.freq)

# creating dataset for plotting 

BMD_Data<-tibble(
                  w=BinRanVar,
                  x=ActFreq,
                  y=BinFreq,
                  z=TriBinFreq,
                  a=BetaBinFreq,
                  b=KumBinFreq,
                  c=GHGBBFreq,
                  d=McGBBFreq
                )
names(BMD_Data)<-c("Bin_RV","Actual_Freq","EstFreq_BinD","EstFreq_TriBinD","EstFreq_BetaBinD","EstFreq_KumBinD","EstFreq_GHGBBD","EstFreq_McGBBD" )

## ----plotting that dataset for BMD,fig.align='center',fig.height=7,fig.width=9,echo=FALSE----

BMD_freq<-ggplot(ABD_Data)+theme_get()+
                     geom_point(aes(x=BMD_Data$Bin_RV,y=BMD_Data$Actual_Freq),color="red",size=2)+
                     geom_line(aes(x=BMD_Data$Bin_RV,y=BMD_Data$Actual_Freq),color="red",size=1)+
                     geom_point(aes(x=BMD_Data$Bin_RV,y=BMD_Data$EstFreq_BinD),color="darkblue",size=2)+
                     geom_line(aes(x=BMD_Data$Bin_RV,y=BMD_Data$EstFreq_BinD),color="darkblue",size=1)+
                     geom_point(aes(x=BMD_Data$Bin_RV,y=BMD_Data$EstFreq_TriBinD),color="forestgreen",size=2)+
                     geom_line(aes(x=BMD_Data$Bin_RV,y=BMD_Data$EstFreq_TriBinD),color="forestgreen",size=1)+
                     geom_point(aes(x=BMD_Data$Bin_RV,y=BMD_Data$EstFreq_BetaBinD),color="yellow4",size=3.5)+
                     geom_line(aes(x=BMD_Data$Bin_RV,y=BMD_Data$EstFreq_BetaBinD),color="yellow4",size=2.5)+
                     geom_point(aes(x=BMD_Data$Bin_RV,y=BMD_Data$EstFreq_KumBinD),color="purple",size=2.75)+
                     geom_line(aes(x=BMD_Data$Bin_RV,y=BMD_Data$EstFreq_KumBinD),color="purple",size=1.75)+
                     geom_point(aes(x=BMD_Data$Bin_RV,y=BMD_Data$EstFreq_GHGBBD),color="orangered",size=2)+
                     geom_line(aes(x=BMD_Data$Bin_RV,y=BMD_Data$EstFreq_GHGBBD),color="orangered",size=1)+
                     geom_point(aes(x=BMD_Data$Bin_RV,y=BMD_Data$EstFreq_McGBBD),color="deeppink",size=2.25)+
                     geom_line(aes(x=BMD_Data$Bin_RV,y=BMD_Data$EstFreq_McGBBD),color="deeppink",size=1.25)+
                     ggtitle("Plot of fitted and actual frequencies of Alcohol BOD week2")+
                     xlab("Binomial Random Variables from 0 to 7")+
                     ylab("Frequency Values Estimated and Actual")+
                     scale_y_continuous(breaks=seq(0,120,5))+
                     scale_x_continuous(breaks=seq(0,7,1))+
                     annotate("text",x=0,y=120,label="Actual",color="red")+
                     annotate("text",x=0,y=116,label="Binomial",color="darkblue")+
                     annotate("text",x=0.25,y=112,label="Triangular Binomial",color="forestgreen")+
                     annotate("text",x=0.15,y=108,label="Beta-Binomial",color="yellow4")+
                     annotate("text",x=6.5,y=120,label="Kumaraswamy Binomial",color="purple")+  
                     annotate("text",x=6.95,y=116,label="GHGBB",color="orangered")+  
                     annotate("text",x=6.95,y=112,label="McGBB",color="deeppink")

BMD_freq


