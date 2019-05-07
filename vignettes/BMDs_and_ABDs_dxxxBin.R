## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(fitODBOD)
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)

## ----Uniform Binomial Distribution,fig.align='center',fig.width=9,fig.height=7----
brv<-0:10
pmfv<-dUniBin(brv,max(brv))$pdf
data<-data.frame(brv,pmfv)
ggplot(data)+
  geom_point(aes(x=data$brv,y=data$pmfv))+
  geom_line(aes(x=data$brv,y=data$pmfv))+
  xlab("Binomial Random Variables")+
  ylab("Probability Mass Function values")+
  ggtitle("Pmf values changing")+
  scale_x_continuous(breaks=seq(0,10,by=1))

## ----Triangular Binomial Distribution,fig.align='center',fig.width=9,fig.height=7----
brv<-seq(0,15,by=1)
mode<-seq(0.02,0.98,by=0.02)
length(mode) #variables
output<-matrix(ncol =length(mode) ,nrow=length(brv))
for (i in 1:length(mode)) 
  {
   output[,i]<-dTriBin(brv,max(brv),mode[i])$pdf
  }
data<-data.frame(brv,output)
data<-melt(data,id.vars ="brv" )
ggplot(data,aes(brv,value,col=variable))+
  geom_line()+guides(fill=FALSE,color=FALSE)+
  xlab("Binomial Random Variable")+
  ylab("Probability Mass values")+
  ggtitle("Triangular Binomial Distribution using dTriBin function")+
  scale_x_continuous(breaks=seq(0,15,by=1))

## ----Beta-Binomial Distribution plot function,include=FALSE--------------
dBetaBinplot<-function(a,b,plot_title,a_seq)
{
  if(a_seq==TRUE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol=length(a),nrow=length(brv))
    for (i in 1:length(a)) 
    {
    output[,i]<-dBetaBin(brv,max(brv),a[i],b)$pdf
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Probability mass values")+
      ggtitle(plot_title)
      return(p1)
  }
  if(a_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol =length(b) ,nrow=length(brv))
    for (i in 1:length(b)) 
    {
    output[,i]<-dBetaBin(brv,max(brv),a,b[i])$pdf
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variables")+
      ylab("Probability mass values")+
      ggtitle(plot_title)
      return(p1)
  }
}

## ----Beta-Binomial Distribution plotting,fig.align='center',fig.width=9,fig.height=7----
b10<-dBetaBinplot(a=seq(1,100,by=1),b=10,plot_title="and when b=10",a_seq= T)
b50<-dBetaBinplot(a=seq(1,100,by=1),b=50,plot_title="and when b=50",a_seq= T)
b100<-dBetaBinplot(a=seq(1,100,by=1),b=100,plot_title="and when b=100",a_seq= T)
b200<-dBetaBinplot(a=seq(1,100,by=1),b=200,plot_title="and when b=200",a_seq= T)

grid.arrange(b10,b50,b100,b200,nrow=2,top="Pmf values changing when a=seq(1,100,by=1)")

a10<-dBetaBinplot(b=seq(1,100,by=1),a=10,plot_title="and when a=10",a_seq= F)
a50<-dBetaBinplot(b=seq(1,100,by=1),a=50,plot_title="and when a=50",a_seq= F)
a100<-dBetaBinplot(b=seq(1,100,by=1),a=100,plot_title="and when a=100",a_seq= F)
a200<-dBetaBinplot(b=seq(1,100,by=1),a=200,plot_title="and when a=200",a_seq= F)

grid.arrange(a10,a50,a100,a200,nrow=2,top="Pmf values changing when b=seq(1,100,by=1)")

## ----Kumaraswamy Binomial Distribution plot function,include=FALSE-------
dKumBinplot<-function(a,b,plot_title,a_seq)
{
  if(a_seq==TRUE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol=length(a),nrow=length(brv))
    for (i in 1:length(a)) 
    {
    output[,i]<-dKumBin(brv,max(brv),a[i],b)$pdf
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Pmf values")+
      ggtitle(plot_title)
      return(p1)
  }
  if(a_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol =length(b) ,nrow=length(brv))
    for (i in 1:length(b)) 
    {
    output[,i]<-dKumBin(brv,max(brv),a,b[i])$pdf
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Pmf values")+
      ggtitle(plot_title)
      return(p1)
  }
}

