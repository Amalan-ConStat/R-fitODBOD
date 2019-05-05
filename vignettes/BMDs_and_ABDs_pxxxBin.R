## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(fitODBOD)
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)

## ----Uniform Binomial Distribution plotting,fig.align='center',fig.width=9,fig.height=7----
brv<-0:10
cpmfv<-pUniBin(brv,max(brv))
data<-data.frame(brv,cpmfv)
ggplot(data)+
  geom_line(aes(x=data$brv,y=data$cpmfv))+
  xlab("Binomial Random Variable")+
  ylab("Cumlative Probability Mass Function values")+
  ggtitle("Cpmf values changing")
  scale_x_continuous(breaks=seq(0,10,by=1))

## ----Triangular Binoimal Distribution plotting,fig.align='center',fig.width=9,fig.height=7----
brv<-seq(0,15,by=1)
mode<-seq(0.02,0.98,by=0.01)
output<-matrix(ncol =length(mode) ,nrow=length(brv))
for (i in 1:length(mode))
  {
   output[,i]<-pTriBin(brv,max(brv),mode[i])
  }
data<-data.frame(brv,output)
data<-melt(data,id.vars ="brv" )
ggplot(data,aes(brv,value,col=variable))+
  geom_line()+guides(fill=FALSE,color=FALSE)+
  xlab("Binomial Random Variable")+
  ylab("Cumulative Probability Mass values")+
  ggtitle("Cpmf values changing for c=seq(0.02,0.98,by=0.01)")
  scale_x_continuous(breaks=seq(0,15,by=1))

## ----Beta Binomial distribution plot function,include=FALSE--------------
pBetaBinplot<-function(a,b,plot_title,a_seq)
{
  if(a_seq==TRUE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol=length(a),nrow=length(brv))
    for (i in 1:length(a))
    {
    output[,i]<-pBetaBin(brv,max(brv),a[i],b)
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
  if(a_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol =length(b) ,nrow=length(brv))
    for (i in 1:length(b))
    {
    output[,i]<-pBetaBin(brv,max(brv),a,b[i])
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
}

## ----Beta-Binomial Distribution plotting,fig.align='center',fig.width=9,fig.height=7----
b10<-pBetaBinplot(a=seq(1,100,by=1),b=10,plot_title="and when b=10",a_seq= T)
b50<-pBetaBinplot(a=seq(1,100,by=1),b=50,plot_title="and when b=50",a_seq= T)
b100<-pBetaBinplot(a=seq(1,100,by=1),b=100,plot_title="and when b=100",a_seq= T)
b200<-pBetaBinplot(a=seq(1,100,by=1),b=200,plot_title="and when b=200",a_seq= T)

grid.arrange(b10,b50,b100,b200,nrow=2,top="Cpmf values changing when a=seq(1,100,by=1)")

a10<-pBetaBinplot(b=seq(1,100,by=1),a=10,plot_title="and when a=10",a_seq= F)
a50<-pBetaBinplot(b=seq(1,100,by=1),a=50,plot_title="and when a=50",a_seq= F)
a100<-pBetaBinplot(b=seq(1,100,by=1),a=100,plot_title="and when a=100",a_seq= F)
a200<-pBetaBinplot(b=seq(1,100,by=1),a=200,plot_title="and when a=200",a_seq= F)

grid.arrange(a10,a50,a100,a200,nrow=2,top="Cpmf values changing when b=seq(1,100,by=1)")

## ----Kumaraswamy Binomial distribution plot function, include=FALSE------
pKumBinplot<-function(a,b,plot_title,a_seq)
{
  if(a_seq==TRUE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol=length(a),nrow=length(brv))
    for (i in 1:length(a))
    {
    output[,i]<-pKumBin(brv,max(brv),a[i],b)
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
  if(a_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol =length(b) ,nrow=length(brv))
    for (i in 1:length(b))
    {
    output[,i]<-pKumBin(brv,max(brv),a,b[i])
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
}

## ----GHGBB distribution plotting,fig.align='center',fig.width=9,fig.height=7----
b10c5<-pGHGBBplot(a=seq(.1,100,by=.1),b=10,c=5,
                  plot_title="and when b=10, c=5",a_seq=T,b_seq=F)
b50c5<-pGHGBBplot(a=seq(.1,100,by=.1),b=50,c=5,
                  plot_title="and when b=50, c=5",a_seq=T,b_seq=F)
b100c5<-pGHGBBplot(a=seq(.1,100,by=.1),b=100,c=5,
                   plot_title="and when b=100, c=5",a_seq=T,b_seq=F)
b150c5<-pGHGBBplot(a=seq(.1,100,by=.1),b=150,c=5,
                   plot_title="and when b=150, c=5",a_seq=T,b_seq=F)

grid.arrange(b10c5,b50c5,b100c5,b150c5,nrow=2,
             top="Cpmf values changing when a=seq(0.1,100,by=0.1)")

b10c10<-pGHGBBplot(a=seq(.1,100,by=.1),b=10,c=10,
                   plot_title="and when b=10, c=10",a_seq=T,b_seq=F)
b50c10<-pGHGBBplot(a=seq(.1,100,by=.1),b=50,c=10,
                   plot_title="and when b=50, c=10",a_seq=T,b_seq=F)
b100c10<-pGHGBBplot(a=seq(.1,100,by=.1),b=100,c=10,
                    plot_title="and when b=100, c=10",a_seq=T,b_seq=F)
b200c10<-pGHGBBplot(a=seq(.1,100,by=.1),b=200,c=10,
                    plot_title="and when b=200, c=10",a_seq=T,b_seq=F)

grid.arrange(b10c10,b50c10,b100c10,b200c10,nrow=2,
             top="Cpmf values changing when a=seq(0.1,100,by=0.1)")

## ----McGBB distribution plot function, include=FALSE---------------------
pMcGBBplot<-function(a,b,c,plot_title,a_seq,b_seq)
{
  if(a_seq==TRUE && b_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol=length(a),nrow=length(brv))
    for (i in 1:length(a))
    {
    output[,i]<-pMcGBB(brv,max(brv),a[i],b,c)
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
  if(b_seq==TRUE && a_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol =length(b) ,nrow=length(brv))
    for (i in 1:length(b))
    {
    output[,i]<-pMcGBB(brv,max(brv),a[i],b,c)
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
  if(a_seq==FALSE && b_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol =length(b) ,nrow=length(brv))
    for (i in 1:length(c))
    {
    output[,i]<-pMcGBB(brv,max(brv),a[i],b,c)
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
}

## ----McGBB distribution plotting,fig.align='center',fig.width=9,fig.height=7----
  b1c5<-pMcGBBplot(a=seq(.5,10,by=.1),b=1,c=5,
                   plot_title="and when b=1, c=5",a_seq=T,b_seq=F)
b1.2c5<-pMcGBBplot(a=seq(.5,10,by=.1),b=1.2,c=5,
                   plot_title="and when b=1.2, c=5",a_seq=T,b_seq=F)
  b3c5<-pMcGBBplot(a=seq(.5,10,by=.1),b=3,c=5,
                   plot_title="and when b=3, c=5",a_seq=T,b_seq=F)
b3.2c5<-pMcGBBplot(a=seq(.5,10,by=.1),b=3.2,c=5,
                   plot_title="and when b=3.2, c=5",a_seq=T,b_seq=F)

grid.arrange(b1c5,b1.2c5,b3c5,b3.2c5,nrow=2,
             top="Cpmf values changing when a=seq(0.5,10,by=0.1)")

  b1c1<-pMcGBBplot(a=seq(.5,100,by=.1),b=1,c=1,
                   plot_title="and when b=1, c=1",a_seq=T,b_seq=F)
b1c1.5<-pMcGBBplot(a=seq(.5,100,by=.1),b=1,c=1.5,
                   plot_title="and when b=1, c=1.5",a_seq=T,b_seq=F)
  b1c2<-pMcGBBplot(a=seq(.5,100,by=.1),b=1,c=2,
                   plot_title="and when b=1, c=2",a_seq=T,b_seq=F)
b1c2.5<-pMcGBBplot(a=seq(.5,100,by=.1),b=1,c=2.5,
                   plot_title="and when b=1, c=2.5",a_seq=T,b_seq=F)

grid.arrange(b1c1,b1c1.5,b1c2,b1c2.5,nrow=2,
             top="Cpmf values changing when a=seq(0.5,100,by=0.1)")

## ----Gamma Binomial Distribution plot function,include=FALSE--------------
pGammaBinplot<-function(a,b,plot_title,a_seq)
{
  if(a_seq==TRUE)
  {
    brv<-seq(0,15,by=1)
    output<-matrix(ncol=length(a),nrow=length(brv))
    for (i in 1:length(a))
    {
      output[,i]<-pGammaBin(brv,max(brv),a[i],b)
    }
    data<-data.frame(brv,output)
    data<-melt(data,id.vars ="brv" )
    p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
    return(p1)
  }
  if(a_seq==FALSE)
  {
    brv<-seq(0,15,by=1)
    output<-matrix(ncol =length(b) ,nrow=length(brv))
    for (i in 1:length(b))
    {
      output[,i]<-pGammaBin(brv,max(brv),a,b[i])
    }
    data<-data.frame(brv,output)
    data<-melt(data,id.vars ="brv" )
    p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variables")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
    return(p1)
  }
}

## ----Gamma Binomial Distribution plotting,fig.align='center',fig.width=9,fig.height=7----
b10<-pGammaBinplot(a=seq(1,100,by=1),b=10,plot_title="and when b=10",a_seq= T)
b50<-pGammaBinplot(a=seq(1,100,by=1),b=50,plot_title="and when b=50",a_seq= T)
b100<-pGammaBinplot(a=seq(1,100,by=1),b=100,plot_title="and when b=100",a_seq= T)
b200<-pGammaBinplot(a=seq(1,100,by=1),b=200,plot_title="and when b=200",a_seq= T)

grid.arrange(b10,b50,b100,b200,nrow=2,top="Cpmf values changing when a=seq(1,100,by=1)")

a10<-pGammaBinplot(b=seq(1,100,by=1),a=10,plot_title="and when a=10",a_seq= F)
a50<-pGammaBinplot(b=seq(1,100,by=1),a=50,plot_title="and when a=50",a_seq= F)
a100<-pGammaBinplot(b=seq(1,100,by=1),a=100,plot_title="and when a=100",a_seq= F)
a200<-pGammaBinplot(b=seq(1,100,by=1),a=200,plot_title="and when a=200",a_seq= F)

grid.arrange(a10,a50,a100,a200,nrow=2,top="Cpmf values changing when b=seq(1,100,by=1)")

## ----Grassia II Binomial II Distribution plot function,include=FALSE--------------
pGrassiaIIBinplot<-function(a,b,plot_title,a_seq)
{
  if(a_seq==TRUE)
  {
    brv<-seq(0,15,by=1)
    output<-matrix(ncol=length(a),nrow=length(brv))
    for (i in 1:length(a))
    {
      output[,i]<-pGrassiaIIBin(brv,max(brv),a[i],b)
    }
    data<-data.frame(brv,output)
    data<-melt(data,id.vars ="brv" )
    p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
    return(p1)
  }
  if(a_seq==FALSE)
  {
    brv<-seq(0,15,by=1)
    output<-matrix(ncol =length(b) ,nrow=length(brv))
    for (i in 1:length(b))
    {
      output[,i]<-pGrassiaIIBin(brv,max(brv),a,b[i])
    }
    data<-data.frame(brv,output)
    data<-melt(data,id.vars ="brv" )
    p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variables")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
    return(p1)
  }
}

## ----Grassia II Binomial Distribution plotting,fig.align='center',fig.width=9,fig.height=7----
b1<-pGrassiaIIBinplot(a=seq(0.1,10,by=0.1),b=0.1,plot_title="and when b=0.1",a_seq= T)
b25<-pGrassiaIIBinplot(a=seq(0.1,10,by=0.1),b=0.25,plot_title="and when b=0.25",a_seq= T)
b35<-pGrassiaIIBinplot(a=seq(0.1,10,by=0.1),b=0.35,plot_title="and when b=0.35",a_seq= T)
b40<-pGrassiaIIBinplot(a=seq(0.1,10,by=0.1),b=0.4,plot_title="and when b=0.4",a_seq= T)

grid.arrange(b1,b25,b35,b40,nrow=2,top="Cpmf values changing when a=seq(0.1,10,by=0.1)")

a1<-pGrassiaIIBinplot(b=seq(0.1,10,by=0.1),a=0.1,plot_title="and when a=0.1",a_seq= F)
a25<-pGrassiaIIBinplot(b=seq(0.1,10,by=0.1),a=0.25,plot_title="and when a=0.25",a_seq= F)
a5<-pGrassiaIIBinplot(b=seq(0.1,10,by=0.1),a=0.5,plot_title="and when a=0.5",a_seq= F)
a10<-pGrassiaIIBinplot(b=seq(0.1,10,by=0.1),a=1,plot_title="and when a=1",a_seq= F)

grid.arrange(a1,a25,a5,a10,nrow=2,top="Pmf values changing when b=seq(0.1,10,by=0.1)")

## ----Additive Binoimal Distribution plot function,include=FALSE----------
pAddBinplot<-function(p,alpha,plot_title,p_seq)
{
  if(p_seq==TRUE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol=length(p),nrow=length(brv))
    for (i in 1:length(p))
    {
    output[,i]<-pAddBin(brv,max(brv),p[i],alpha)
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
  if(p_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol =length(alpha) ,nrow=length(brv))
    for (i in 1:length(alpha))
    {
    output[,i]<-pAddBin(brv,max(brv),p,alpha[i])
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
}

## ----Additive Binomial Distributon plotting, fig.width=9,fig.height=7----
alpha.005<-pAddBinplot(p=seq(0.35,0.65,by=0.0001),alpha=-0.005,
                       plot_title="and when alpha=-0.005",p_seq= T)
alpha.003<-pAddBinplot(p=seq(0.35,0.65,by=0.0001),alpha=-0.003,
                       plot_title="and when alpha=-0.003",p_seq= T)
alpha0.003<-pAddBinplot(p=seq(0.35,0.65,by=0.0001),alpha=0.003,
                        plot_title="and when alpha=0.003",p_seq= T)
alpha0.008<-pAddBinplot(p=seq(0.35,0.65,by=0.0001),alpha=0.008,
                        plot_title="and when alpha=0.008",p_seq= T)

grid.arrange(alpha.005,alpha.003,alpha0.003,alpha0.008,nrow=2,
             top="Cpmf values changing when p=seq(0.35,0.65,by=0.0001)")

p.015<-pAddBinplot(alpha=seq(0.0001,0.05,by=0.0001),p=0.015,
                  plot_title="and when p=0.0.15",p_seq= F)
p.115<-pAddBinplot(alpha=seq(0.0001,0.05,by=0.0001),p=0.115,
                  plot_title="and when p=0.115",p_seq= F)
p.215<-pAddBinplot(alpha=seq(0.0001,0.05,by=0.0001),p=0.215,
                  plot_title="and when p=0.215",p_seq= F)
p.315<-pAddBinplot(alpha=seq(0.0001,0.05,by=0.0001),p=0.315,
                  plot_title="and when p=0.315",p_seq= F)

grid.arrange(p.015,p.115,p.215,p.315,nrow=2,
             top="Cpmf values changing when alpha=seq(0.0001,0.05,by=0.0001)")

## ----Beta-Correlated Binomial Distribution plot function, include=FALSE----
pBetaCorrBinplot<-function(a,b,cov,plot_title,a_seq,b_seq)
{
  if(a_seq==TRUE && b_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol=length(a),nrow=length(brv))
    for (i in 1:length(a))
    {
    output[,i]<-pBetaCorrBin(brv,max(brv),cov,a[i],b)
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
  if(b_seq==TRUE && a_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol =length(b) ,nrow=length(brv))
    for (i in 1:length(b))
    {
    output[,i]<-pBetaCorrBin(brv,max(brv),cov,a,b[i])
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variables")+
      ylab("mf values")+
      ggtitle(plot_title)
      return(p1)
  }
  if(a_seq==FALSE && b_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol =length(b) ,nrow=length(brv))
    for (i in 1:length(cov))
    {
    output[,i]<-pBetaCorrBin(brv,max(brv),cov[i],a,b)
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
}

## ----Beta-Correlated Binomial distribution plotting,fig.align='center',fig.width=9,fig.height=7----
b16cov5<-pBetaCorrBinplot(a=seq(10,100,by=0.1),b=16,cov=0.0005,
                 plot_title="and when b=16, cov=0.0005",a_seq=T,b_seq=F)
b46cov5<-pBetaCorrBinplot(a=seq(10,100,by=0.1),b=46,cov=0.0005,
                   plot_title="and when b=46, cov=0.0005",a_seq=T,b_seq=F)
b76cov5<-pBetaCorrBinplot(a=seq(10,100,by=0.1),b=76,cov=0.0005,
                 plot_title="and when b=76, cov=0.0005",a_seq=T,b_seq=F)
b106cov5<-pBetaCorrBinplot(a=seq(10,100,by=0.1),b=106,cov=0.0005,
                   plot_title="and when b=106 cov=0.0005",a_seq=T,b_seq=F)

grid.arrange(b16cov5,b46cov5,b76cov5,b106cov5,nrow=2,
             top="Cpmf values changing when a=seq(10,100,by=0.1)")

b10cov1<-pBetaCorrBinplot(a=seq(15,100,by=0.1),b=10,cov=0.001,
                 plot_title="and when b=10, cov=0.001",a_seq=T,b_seq=F)
b10cov3<-pBetaCorrBinplot(a=seq(15,100,by=0.1),b=10,cov=0.003,
                   plot_title="and when b=10, cov=0.003",a_seq=T,b_seq=F)
b10cov7<-pBetaCorrBinplot(a=seq(15,100,by=0.1),b=10,cov=0.007,
                 plot_title="and when b=10, cov=0.007",a_seq=T,b_seq=F)
b10cov9<-pBetaCorrBinplot(a=seq(15,100,by=0.1),b=10,cov=0.009,
                   plot_title="and when b=10, cov=0.009",a_seq=T,b_seq=F)

grid.arrange(b10cov1,b10cov3,b10cov7,b10cov9,nrow=2,
             top="Cpmf values changing when a=seq(15,100,by=0.1)")

## ----COM Poisson Binomial plot function,include=FALSE--------------------
pCOMPBinplot<-function(p,v,plot_title,p_seq)
{
  if(p_seq==TRUE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol=length(p),nrow=length(brv))
    for (i in 1:length(p))
    {
    output[,i]<-pCOMPBin(brv,max(brv),p[i],v)
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
  if(p_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol =length(v) ,nrow=length(brv))
    for (i in 1:length(v))
    {
    output[,i]<-pCOMPBin(brv,max(brv),p,v[i])
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
}

## ----COM Poisson Binoial distribution plotting,fig.width=9,fig.height=7----
v.5<-pCOMPBinplot(p=seq(0.25,0.75,by=0.001),v=0.5,
                   plot_title="and when v=0.5",p_seq= T)
 v1<-pCOMPBinplot(p=seq(0.25,0.75,by=0.001),v=1,
                   plot_title="and when v=1",p_seq= T)
 v3<-pCOMPBinplot(p=seq(0.25,0.75,by=0.001),v=3,
                   plot_title="and when v=3",p_seq= T)
 v5<-pCOMPBinplot(p=seq(0.25,0.75,by=0.001),v=5,
                   plot_title="and when v=5",p_seq= T)

grid.arrange(v.5,v1,v3,v5,nrow=2,
             top="Cpmf values changing when p=seq(0.25,0.75,by=0.001)")

p0.40<-pCOMPBinplot(v=seq(-0.5,5.5,by=.01),p=0.40,
                    plot_title="and when p=0.35",p_seq= F)
p0.45<-pCOMPBinplot(v=seq(-0.5,5.5,by=.01),p=0.45,
                    plot_title="and when p=0.45",p_seq= F)
p0.50<-pCOMPBinplot(v=seq(-0.5,5.5,by=.01),p=0.50,
                    plot_title="and when p=0.55",p_seq= F)
p0.55<-pCOMPBinplot(v=seq(-0.5,5.5,by=.01),p=0.55,
                    plot_title="and when p=0.65",p_seq= F)

grid.arrange(p0.40,p0.45,p0.50,p0.55,nrow=2,
             top="Cpmf values changing when v=seq(-0.5,5.5,by=0.01)")

## ----Correlated Binomial distribution plot function----------------------
pCorrBinplot<-function(p,cov,plot_title,p_seq)
{
  if(p_seq==TRUE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol=length(p),nrow=length(brv))
    for (i in 1:length(p))
    {
    output[,i]<-pCorrBin(brv,max(brv),p[i],cov)
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
  if(p_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol =length(cov) ,nrow=length(brv))
    for (i in 1:length(cov))
    {
    output[,i]<-pCorrBin(brv,max(brv),p,cov[i])
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
}

## ----Correlated Binomial distribution plotting,fig.width=9,fig.height=7----
 cov0.001<-pCorrBinplot(p=seq(0.15,0.75,by=0.01),cov=0.001,
                        plot_title="and when cov=0.001",p_seq= T)
 cov0.004<-pCorrBinplot(p=seq(0.15,0.75,by=0.01),cov=0.004,
                        plot_title="and when cov=0.004",p_seq= T)
cov0.005<-pCorrBinplot(p=seq(0.15,0.75,by=0.01),cov=0.005,
                        plot_title="and when cov=0.005",p_seq= T)
 cov0.01<-pCorrBinplot(p=seq(0.15,0.75,by=0.01),cov=0.01,
                        plot_title="and when cov=0.01",p_seq= T)

grid.arrange(cov0.001,cov0.004,cov0.005,cov0.01,nrow=2,
             top="Cpmf values changing when p=seq(0.15,0.75,by=0.01)")

p0.15<-pCorrBinplot(cov=seq(0.002,0.009,by=.0001),p=0.15,
                    plot_title="and when p=0.15",p_seq= F)
p0.25<-pCorrBinplot(cov=seq(0.002,0.009,by=.0001),p=0.25,
                    plot_title="and when p=0.25",p_seq= F)
p0.50<-pCorrBinplot(cov=seq(0.002,0.009,by=.0001),p=0.50,
                    plot_title="and when p=0.50",p_seq= F)
p0.75<-pCorrBinplot(cov=seq(0.002,0.009,by=.0001),p=0.75,
                    plot_title="and when p=0.75",p_seq= F)

grid.arrange(p0.15,p0.25,p0.50,p0.75,nrow=2,
             top="Cpmf values changing when cov=seq(0.002,0.009,by=0.0001)")

## ----Multiplicative Binomial distribution plot function, include=FALSE----
pMultiBinplot<-function(p,theta,plot_title,p_seq)
{
  if(p_seq==TRUE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol=length(p),nrow=length(brv))
    for (i in 1:length(p))
    {
    output[,i]<-pMultiBin(brv,max(brv),p[i],theta)
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
  if(p_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol =length(theta) ,nrow=length(brv))
    for (i in 1:length(theta))
    {
    output[,i]<-pMultiBin(brv,max(brv),p,theta[i])
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
}

## ----Kumarswamy Binomial distribution plotting,fig.align='center',fig.width=9,fig.height=7----
b5<-pKumBinplot(a=seq(1,50,by=1),b=5,plot_title="and when b=5",a_seq=T)
b10<-pKumBinplot(a=seq(1,50,by=1),b=10,plot_title="and when b=10",a_seq=T)
b20<-pKumBinplot(a=seq(1,50,by=1),b=20,plot_title="and when b=20",a_seq=T)
b25<-pKumBinplot(a=seq(1,50,by=1),b=25,plot_title="and when b=25",a_seq=T)

grid.arrange(b5,b10,b10,b25,nrow=2,top="Cpmf values changing when a=seq(1,50,by=1)")

a5<-pKumBinplot(b=seq(1,30,by=1),a=5,plot_title="and when a=5",a_seq=F)
a10<-pKumBinplot(b=seq(1,30,by=1),a=10,plot_title="and when a=10",a_seq=F)
a20<-pKumBinplot(b=seq(1,30,by=1),a=20,plot_title="and when a=20",a_seq=F)
a25<-pKumBinplot(b=seq(1,30,by=1),a=25,plot_title="and when a=25",a_seq=F)

grid.arrange(a5,a10,a20,a25,nrow=2,top="Cpmf values changing when b=seq(1,30,by=1)")

## ----GHGBB distribution plot function, include=FALSE---------------------
pGHGBBplot<-function(a,b,c,plot_title,a_seq,b_seq)
{
  if(a_seq==TRUE && b_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol=length(a),nrow=length(brv))
    for (i in 1:length(a))
    {
    output[,i]<-pGHGBB(brv,max(brv),a[i],b,c)
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
  if(b_seq==TRUE && a_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol =length(b) ,nrow=length(brv))
    for (i in 1:length(b))
    {
    output[,i]<-pGHGBB(brv,max(brv),a,b[i],c)
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
  if(a_seq==FALSE && b_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol =length(b) ,nrow=length(brv))
    for (i in 1:length(c))
    {
    output[,i]<-pGHGBB(brv,max(brv),a,b,c[i])
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
}

## ----Multiplicative Binomial distribution plotting,fig.width=9,fig.height=7----
theta1<-pMultiBinplot(p=seq(0.01,0.99,by=0.001),theta=1,
                       plot_title="and when theta=1",p_seq= T)
theta1.25<-pMultiBinplot(p=seq(0.01,0.99,by=0.001),theta=1.25,
                       plot_title="and when theta=1.25",p_seq= T)
theta1.75<-pMultiBinplot(p=seq(0.01,0.99,by=0.001),theta=1.75,
                      plot_title="and when theta=1.75",p_seq= T)
theta2<-pMultiBinplot(p=seq(0.01,0.99,by=0.001),theta=2,
                      plot_title="and when theta=2",p_seq= T)

grid.arrange(theta1,theta1.25,theta1.75,theta2,nrow=2,
             top="Cpmf values changing when p=seq(0.01,0.99,by=0.001)")

p0.35<-pMultiBinplot(theta=seq(1.001,5,by=0.0001),p=0.35,
                     plot_title="and when p=0.35",p_seq= F)
p0.45<-pMultiBinplot(theta=seq(1.001,5,by=0.0001),p=0.45,
                     plot_title="and when p=0.45",p_seq= F)
p0.55<-pMultiBinplot(theta=seq(1.001,5,by=0.0001),p=0.55,
                     plot_title="and when p=0.55",p_seq= F)
p0.65<-pMultiBinplot(theta=seq(1.001,5,by=0.0001),p=0.65,
                     plot_title="and when p=0.65",p_seq= F)

grid.arrange(p0.35,p0.45,p0.55,p0.65,nrow=2,
             top="Cpmf values changing when theta=seq(1.001,5,by=0.0001)")

## ----GHGBB distribution plotting,fig.align='center',fig.width=9,fig.height=7----
b10c5<-pGHGBBplot(a=seq(.1,100,by=.1),b=10,c=5,
                  plot_title="and when b=10, c=5",a_seq=T,b_seq=F)
b50c5<-pGHGBBplot(a=seq(.1,100,by=.1),b=50,c=5,
                  plot_title="and when b=50, c=5",a_seq=T,b_seq=F)
b100c5<-pGHGBBplot(a=seq(.1,100,by=.1),b=100,c=5,
                   plot_title="and when b=100, c=5",a_seq=T,b_seq=F)
b150c5<-pGHGBBplot(a=seq(.1,100,by=.1),b=150,c=5,
                   plot_title="and when b=150, c=5",a_seq=T,b_seq=F)

grid.arrange(b10c5,b50c5,b100c5,b150c5,nrow=2,
             top="Cpmf values changing when a=seq(0.1,100,by=0.1)")

b10c10<-pGHGBBplot(a=seq(.1,100,by=.1),b=10,c=10,
                   plot_title="and when b=10, c=10",a_seq=T,b_seq=F)
b50c10<-pGHGBBplot(a=seq(.1,100,by=.1),b=50,c=10,
                   plot_title="and when b=50, c=10",a_seq=T,b_seq=F)
b100c10<-pGHGBBplot(a=seq(.1,100,by=.1),b=100,c=10,
                    plot_title="and when b=100, c=10",a_seq=T,b_seq=F)
b200c10<-pGHGBBplot(a=seq(.1,100,by=.1),b=200,c=10,
                    plot_title="and when b=200, c=10",a_seq=T,b_seq=F)

grid.arrange(b10c10,b50c10,b100c10,b200c10,nrow=2,
             top="Cpmf values changing when a=seq(0.1,100,by=0.1)")

## ----McGBB distribution plot function, include=FALSE---------------------
pMcGBBplot<-function(a,b,c,plot_title,a_seq,b_seq)
{
  if(a_seq==TRUE && b_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol=length(a),nrow=length(brv))
    for (i in 1:length(a))
    {
    output[,i]<-pMcGBB(brv,max(brv),a[i],b,c)
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
  if(b_seq==TRUE && a_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol =length(b) ,nrow=length(brv))
    for (i in 1:length(b))
    {
    output[,i]<-pMcGBB(brv,max(brv),a[i],b,c)
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
  if(a_seq==FALSE && b_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol =length(b) ,nrow=length(brv))
    for (i in 1:length(c))
    {
    output[,i]<-pMcGBB(brv,max(brv),a[i],b,c)
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
}

## ----McGBB distribution plotting,fig.align='center',fig.width=9,fig.height=7----
  b1c5<-pMcGBBplot(a=seq(.5,10,by=.1),b=1,c=5,
                   plot_title="and when b=1, c=5",a_seq=T,b_seq=F)
b1.2c5<-pMcGBBplot(a=seq(.5,10,by=.1),b=1.2,c=5,
                   plot_title="and when b=1.2, c=5",a_seq=T,b_seq=F)
  b3c5<-pMcGBBplot(a=seq(.5,10,by=.1),b=3,c=5,
                   plot_title="and when b=3, c=5",a_seq=T,b_seq=F)
b3.2c5<-pMcGBBplot(a=seq(.5,10,by=.1),b=3.2,c=5,
                   plot_title="and when b=3.2, c=5",a_seq=T,b_seq=F)

grid.arrange(b1c5,b1.2c5,b3c5,b3.2c5,nrow=2,
             top="Cpmf values changing when a=seq(0.5,10,by=0.1)")

  b1c1<-pMcGBBplot(a=seq(.5,100,by=.1),b=1,c=1,
                   plot_title="and when b=1, c=1",a_seq=T,b_seq=F)
b1c1.5<-pMcGBBplot(a=seq(.5,100,by=.1),b=1,c=1.5,
                   plot_title="and when b=1, c=1.5",a_seq=T,b_seq=F)
  b1c2<-pMcGBBplot(a=seq(.5,100,by=.1),b=1,c=2,
                   plot_title="and when b=1, c=2",a_seq=T,b_seq=F)
b1c2.5<-pMcGBBplot(a=seq(.5,100,by=.1),b=1,c=2.5,
                   plot_title="and when b=1, c=2.5",a_seq=T,b_seq=F)

grid.arrange(b1c1,b1c1.5,b1c2,b1c2.5,nrow=2,
             top="Cpmf values changing when a=seq(0.5,100,by=0.1)")

## ----Additive Binoimal Distribution plot function,include=FALSE----------
pAddBinplot<-function(p,alpha,plot_title,p_seq)
{
  if(p_seq==TRUE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol=length(p),nrow=length(brv))
    for (i in 1:length(p))
    {
    output[,i]<-pAddBin(brv,max(brv),p[i],alpha)
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
  if(p_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol =length(alpha) ,nrow=length(brv))
    for (i in 1:length(alpha))
    {
    output[,i]<-pAddBin(brv,max(brv),p,alpha[i])
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
}

## ----Additive Binomial Distributon plotting, fig.width=9,fig.height=7----
alpha.005<-pAddBinplot(p=seq(0.35,0.65,by=0.0001),alpha=-0.005,
                       plot_title="and when alpha=-0.005",p_seq= T)
alpha.003<-pAddBinplot(p=seq(0.35,0.65,by=0.0001),alpha=-0.003,
                       plot_title="and when alpha=-0.003",p_seq= T)
alpha0.003<-pAddBinplot(p=seq(0.35,0.65,by=0.0001),alpha=0.003,
                        plot_title="and when alpha=0.003",p_seq= T)
alpha0.008<-pAddBinplot(p=seq(0.35,0.65,by=0.0001),alpha=0.008,
                        plot_title="and when alpha=0.008",p_seq= T)

grid.arrange(alpha.005,alpha.003,alpha0.003,alpha0.008,nrow=2,
             top="Cpmf values changing when p=seq(0.35,0.65,by=0.0001)")

p.015<-pAddBinplot(alpha=seq(0.0001,0.05,by=0.0001),p=0.015,
                  plot_title="and when p=0.0.15",p_seq= F)
p.115<-pAddBinplot(alpha=seq(0.0001,0.05,by=0.0001),p=0.115,
                  plot_title="and when p=0.115",p_seq= F)
p.215<-pAddBinplot(alpha=seq(0.0001,0.05,by=0.0001),p=0.215,
                  plot_title="and when p=0.215",p_seq= F)
p.315<-pAddBinplot(alpha=seq(0.0001,0.05,by=0.0001),p=0.315,
                  plot_title="and when p=0.315",p_seq= F)

grid.arrange(p.015,p.115,p.215,p.315,nrow=2,
             top="Cpmf values changing when alpha=seq(0.0001,0.05,by=0.0001)")

## ----Beta-Correlated Binomial Distribution plot function, include=FALSE----
pBetaCorrBinplot<-function(a,b,cov,plot_title,a_seq,b_seq)
{
  if(a_seq==TRUE && b_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol=length(a),nrow=length(brv))
    for (i in 1:length(a))
    {
    output[,i]<-pBetaCorrBin(brv,max(brv),cov,a[i],b)
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
  if(b_seq==TRUE && a_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol =length(b) ,nrow=length(brv))
    for (i in 1:length(b))
    {
    output[,i]<-pBetaCorrBin(brv,max(brv),cov,a,b[i])
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variables")+
      ylab("mf values")+
      ggtitle(plot_title)
      return(p1)
  }
  if(a_seq==FALSE && b_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol =length(b) ,nrow=length(brv))
    for (i in 1:length(cov))
    {
    output[,i]<-pBetaCorrBin(brv,max(brv),cov[i],a,b)
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
}

## ----Beta-Correlated Binomial distribution plotting,fig.align='center',fig.width=9,fig.height=7----
b16cov5<-pBetaCorrBinplot(a=seq(10,100,by=0.1),b=16,cov=0.0005,
                 plot_title="and when b=16, cov=0.0005",a_seq=T,b_seq=F)
b46cov5<-pBetaCorrBinplot(a=seq(10,100,by=0.1),b=46,cov=0.0005,
                   plot_title="and when b=46, cov=0.0005",a_seq=T,b_seq=F)
b76cov5<-pBetaCorrBinplot(a=seq(10,100,by=0.1),b=76,cov=0.0005,
                 plot_title="and when b=76, cov=0.0005",a_seq=T,b_seq=F)
b106cov5<-pBetaCorrBinplot(a=seq(10,100,by=0.1),b=106,cov=0.0005,
                   plot_title="and when b=106 cov=0.0005",a_seq=T,b_seq=F)

grid.arrange(b16cov5,b46cov5,b76cov5,b106cov5,nrow=2,
             top="Cpmf values changing when a=seq(10,100,by=0.1)")

b10cov1<-pBetaCorrBinplot(a=seq(15,100,by=0.1),b=10,cov=0.001,
                 plot_title="and when b=10, cov=0.001",a_seq=T,b_seq=F)
b10cov3<-pBetaCorrBinplot(a=seq(15,100,by=0.1),b=10,cov=0.003,
                   plot_title="and when b=10, cov=0.003",a_seq=T,b_seq=F)
b10cov7<-pBetaCorrBinplot(a=seq(15,100,by=0.1),b=10,cov=0.007,
                 plot_title="and when b=10, cov=0.007",a_seq=T,b_seq=F)
b10cov9<-pBetaCorrBinplot(a=seq(15,100,by=0.1),b=10,cov=0.009,
                   plot_title="and when b=10, cov=0.009",a_seq=T,b_seq=F)

grid.arrange(b10cov1,b10cov3,b10cov7,b10cov9,nrow=2,
             top="Cpmf values changing when a=seq(15,100,by=0.1)")

## ----COM Poisson Binomial plot function,include=FALSE--------------------
pCOMPBinplot<-function(p,v,plot_title,p_seq)
{
  if(p_seq==TRUE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol=length(p),nrow=length(brv))
    for (i in 1:length(p))
    {
    output[,i]<-pCOMPBin(brv,max(brv),p[i],v)
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
  if(p_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol =length(v) ,nrow=length(brv))
    for (i in 1:length(v))
    {
    output[,i]<-pCOMPBin(brv,max(brv),p,v[i])
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
}

## ----COM Poisson Binoial distribution plotting,fig.width=9,fig.height=7----
v.5<-pCOMPBinplot(p=seq(0.25,0.75,by=0.001),v=0.5,
                   plot_title="and when v=0.5",p_seq= T)
 v1<-pCOMPBinplot(p=seq(0.25,0.75,by=0.001),v=1,
                   plot_title="and when v=1",p_seq= T)
 v3<-pCOMPBinplot(p=seq(0.25,0.75,by=0.001),v=3,
                   plot_title="and when v=3",p_seq= T)
 v5<-pCOMPBinplot(p=seq(0.25,0.75,by=0.001),v=5,
                   plot_title="and when v=5",p_seq= T)

grid.arrange(v.5,v1,v3,v5,nrow=2,
             top="Cpmf values changing when p=seq(0.25,0.75,by=0.001)")

p0.40<-pCOMPBinplot(v=seq(-0.5,5.5,by=.01),p=0.40,
                    plot_title="and when p=0.35",p_seq= F)
p0.45<-pCOMPBinplot(v=seq(-0.5,5.5,by=.01),p=0.45,
                    plot_title="and when p=0.45",p_seq= F)
p0.50<-pCOMPBinplot(v=seq(-0.5,5.5,by=.01),p=0.50,
                    plot_title="and when p=0.55",p_seq= F)
p0.55<-pCOMPBinplot(v=seq(-0.5,5.5,by=.01),p=0.55,
                    plot_title="and when p=0.65",p_seq= F)

grid.arrange(p0.40,p0.45,p0.50,p0.55,nrow=2,
             top="Cpmf values changing when v=seq(-0.5,5.5,by=0.01)")

## ----Correlated Binomial distribution plot function----------------------
pCorrBinplot<-function(p,cov,plot_title,p_seq)
{
  if(p_seq==TRUE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol=length(p),nrow=length(brv))
    for (i in 1:length(p))
    {
    output[,i]<-pCorrBin(brv,max(brv),p[i],cov)
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
  if(p_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol =length(cov) ,nrow=length(brv))
    for (i in 1:length(cov))
    {
    output[,i]<-pCorrBin(brv,max(brv),p,cov[i])
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
}

## ----Correlated Binomial distribution plotting,fig.width=9,fig.height=7----
 cov0.001<-pCorrBinplot(p=seq(0.15,0.75,by=0.01),cov=0.001,
                        plot_title="and when cov=0.001",p_seq= T)
 cov0.004<-pCorrBinplot(p=seq(0.15,0.75,by=0.01),cov=0.004,
                        plot_title="and when cov=0.004",p_seq= T)
cov0.005<-pCorrBinplot(p=seq(0.15,0.75,by=0.01),cov=0.005,
                        plot_title="and when cov=0.005",p_seq= T)
 cov0.01<-pCorrBinplot(p=seq(0.15,0.75,by=0.01),cov=0.01,
                        plot_title="and when cov=0.01",p_seq= T)

grid.arrange(cov0.001,cov0.004,cov0.005,cov0.01,nrow=2,
             top="Cpmf values changing when p=seq(0.15,0.75,by=0.01)")

p0.15<-pCorrBinplot(cov=seq(0.002,0.009,by=.0001),p=0.15,
                    plot_title="and when p=0.15",p_seq= F)
p0.25<-pCorrBinplot(cov=seq(0.002,0.009,by=.0001),p=0.25,
                    plot_title="and when p=0.25",p_seq= F)
p0.50<-pCorrBinplot(cov=seq(0.002,0.009,by=.0001),p=0.50,
                    plot_title="and when p=0.50",p_seq= F)
p0.75<-pCorrBinplot(cov=seq(0.002,0.009,by=.0001),p=0.75,
                    plot_title="and when p=0.75",p_seq= F)

grid.arrange(p0.15,p0.25,p0.50,p0.75,nrow=2,
             top="Cpmf values changing when cov=seq(0.002,0.009,by=0.0001)")

## ----Multiplicative Binomial distribution plot function, include=FALSE----
pMultiBinplot<-function(p,theta,plot_title,p_seq)
{
  if(p_seq==TRUE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol=length(p),nrow=length(brv))
    for (i in 1:length(p))
    {
    output[,i]<-pMultiBin(brv,max(brv),p[i],theta)
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
  if(p_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol =length(theta) ,nrow=length(brv))
    for (i in 1:length(theta))
    {
    output[,i]<-pMultiBin(brv,max(brv),p,theta[i])
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
      return(p1)
  }
}

## ----Multiplicative Binomial distribution plotting,fig.width=9,fig.height=7----
theta1<-pMultiBinplot(p=seq(0.01,0.99,by=0.001),theta=1,
                       plot_title="and when theta=1",p_seq= T)
theta1.25<-pMultiBinplot(p=seq(0.01,0.99,by=0.001),theta=1.25,
                       plot_title="and when theta=1.25",p_seq= T)
theta1.75<-pMultiBinplot(p=seq(0.01,0.99,by=0.001),theta=1.75,
                      plot_title="and when theta=1.75",p_seq= T)
theta2<-pMultiBinplot(p=seq(0.01,0.99,by=0.001),theta=2,
                      plot_title="and when theta=2",p_seq= T)

grid.arrange(theta1,theta1.25,theta1.75,theta2,nrow=2,
             top="Cpmf values changing when p=seq(0.01,0.99,by=0.001)")

p0.35<-pMultiBinplot(theta=seq(1.001,5,by=0.0001),p=0.35,
                     plot_title="and when p=0.35",p_seq= F)
p0.45<-pMultiBinplot(theta=seq(1.001,5,by=0.0001),p=0.45,
                     plot_title="and when p=0.45",p_seq= F)
p0.55<-pMultiBinplot(theta=seq(1.001,5,by=0.0001),p=0.55,
                     plot_title="and when p=0.55",p_seq= F)
p0.65<-pMultiBinplot(theta=seq(1.001,5,by=0.0001),p=0.65,
                     plot_title="and when p=0.65",p_seq= F)

grid.arrange(p0.35,p0.45,p0.55,p0.65,nrow=2,
             top="Cpmf values changing when theta=seq(1.001,5,by=0.0001)")

## ----Lovinson Multiplicative Binomial distribution plot function, include=FALSE----
pLMBinplot<-function(p,phi,plot_title,p_seq)
{
  if(p_seq==TRUE)
  {
    brv<-seq(0,15,by=1)
    output<-matrix(ncol=length(p),nrow=length(brv))
    for (i in 1:length(p))
    {
      output[,i]<-pLMBin(brv,max(brv),p[i],phi)
    }
    data<-data.frame(brv,output)
    data<-melt(data,id.vars ="brv" )
    p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
    return(p1)
  }
  if(p_seq==FALSE)
  {
    brv<-seq(0,15,by=1)
    output<-matrix(ncol =length(phi) ,nrow=length(brv))
    for (i in 1:length(phi))
    {
      output[,i]<-pLMBin(brv,max(brv),p,phi[i])
    }
    data<-data.frame(brv,output)
    data<-melt(data,id.vars ="brv" )
    p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Cpmf values")+
      ggtitle(plot_title)
    return(p1)
  }
}

## ----Lovinson Multiplicative Binomial distribution plotting,fig.width=9,fig.height=7----
phi1<-pLMBinplot(p=seq(0.01,0.99,by=0.001),phi=1,
                      plot_title="and when phi=1",p_seq= T)
phi1.25<-pLMBinplot(p=seq(0.01,0.99,by=0.001),phi=1.25,
                         plot_title="and when phi=1.25",p_seq= T)
phi1.75<-pLMBinplot(p=seq(0.01,0.99,by=0.001),phi=1.75,
                         plot_title="and when phi=1.75",p_seq= T)
phi2<-pLMBinplot(p=seq(0.01,0.99,by=0.001),phi=2,
                      plot_title="and when phi=2",p_seq= T)

grid.arrange(phi1,phi1.25,phi1.75,phi2,nrow=2,
             top="Cpmf values changing when p=seq(0.01,0.99,by=0.001)")

p0.35<-pLMBinplot(phi=seq(1.001,5,by=0.001),p=0.35,
                     plot_title="and when p=0.35",p_seq= F)
p0.45<-pLMBinplot(phi=seq(1.001,5,by=0.001),p=0.45,
                     plot_title="and when p=0.45",p_seq= F)
p0.55<-pLMBinplot(phi=seq(1.001,5,by=0.001),p=0.55,
                     plot_title="and when p=0.55",p_seq= F)
p0.65<-pLMBinplot(phi=seq(1.001,5,by=0.001),p=0.65,
                     plot_title="and when p=0.65",p_seq= F)

grid.arrange(p0.35,p0.45,p0.55,p0.65,nrow=2,
             top="Cpmf values changing when phi=seq(1.001,5,by=0.001)")
