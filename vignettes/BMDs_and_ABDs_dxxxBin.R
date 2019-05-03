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

## ----Kumaraswamy Binomial Distribution plotting,fig.align='center',fig.width=9,fig.height=7----
b5<-dKumBinplot(a=seq(1,50,by=1),b=5,plot_title="and when b=5",a_seq=T)
b10<-dKumBinplot(a=seq(1,50,by=1),b=10,plot_title="and when b=10",a_seq=T)
b20<-dKumBinplot(a=seq(1,50,by=1),b=20,plot_title="and when b=20",a_seq=T)
b25<-dKumBinplot(a=seq(1,50,by=1),b=25,plot_title="and when b=25",a_seq=T)

grid.arrange(b5,b10,b20,b25,nrow=2,top="Pmf values changing when a=seq(1,50,by=1)")

a5<-dKumBinplot(b=seq(1,30,by=1),a=5,plot_title="and when a=5",a_seq=F)
a10<-dKumBinplot(b=seq(1,30,by=1),a=10,plot_title="and when a=10",a_seq=F)
a20<-dKumBinplot(b=seq(1,30,by=1),a=20,plot_title="and when a=20",a_seq=F)
a25<-dKumBinplot(b=seq(1,30,by=1),a=25,plot_title="and when a=25",a_seq=F)

grid.arrange(a5,a10,a20,a25,nrow=2,top="Pmf values changing when b=seq(1,30,by=1)")

## ----GHGBB Distribution plot function, include=FALSE---------------------
dGHGBBplot<-function(a,b,c,plot_title,a_seq,b_seq)
{
  if(a_seq==TRUE && b_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol=length(a),nrow=length(brv))
    for (i in 1:length(a)) 
    {
    output[,i]<-dGHGBB(brv,max(brv),a[i],b,c)$pdf
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
    output[,i]<-dGHGBB(brv,max(brv),a,b[i],c)$pdf
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
  if(a_seq==FALSE && b_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol =length(b) ,nrow=length(brv))
    for (i in 1:length(c)) 
    {
    output[,i]<-dGHGBB(brv,max(brv),a,b,c[i])$pdf
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

## ----GHGBB Distribution plotting,fig.align='center',fig.width=9,fig.height=7----
b10c5<-dGHGBBplot(a=seq(.1,100,by=.1),b=10,c=5,
                  plot_title="and when b=10, c=5",a_seq=T,b_seq=F)
b50c5<-dGHGBBplot(a=seq(.1,100,by=.1),b=50,c=5,
                  plot_title="and when b=50, c=5",a_seq=T,b_seq=F)
b100c5<-dGHGBBplot(a=seq(.1,100,by=.1),b=100,c=5,
                   plot_title="and when b=100, c=5",a_seq=T,b_seq=F)
b200c5<-dGHGBBplot(a=seq(.1,100,by=.1),b=150,c=5,
                   plot_title="and when b=200, c=5",a_seq=T,b_seq=F)

grid.arrange(b10c5,b50c5,b100c5,b200c5,nrow=2,
             top="Pmf values changing when a=seq(0.1,100,by=0.1)")

b20c10<-dGHGBBplot(a=seq(.1,100,by=.1),b=20,c=10,
                   plot_title="and when b=20, c=10",a_seq=T,b_seq=F)
b50c10<-dGHGBBplot(a=seq(.1,100,by=.1),b=50,c=10,
                   plot_title="and when b=50, c=10",a_seq=T,b_seq=F)
b100c10<-dGHGBBplot(a=seq(.1,100,by=.1),b=100,c=10,
                    plot_title="and when b=100, c=10",a_seq=T,b_seq=F)
b200c10<-dGHGBBplot(a=seq(.1,100,by=.1),b=200,c=10,
                    plot_title="and when b=200, c=10",a_seq=T,b_seq=F)

grid.arrange(b20c10,b50c10,b100c10,b200c10,nrow=2,
             top="Pmf values changing when a=seq(0.1,100,by=0.1)")

## ----McGBB Distribution plot function, include=FALSE---------------------
dMcGBBplot<-function(a,b,c,plot_title,a_seq,b_seq)
{
  if(a_seq==TRUE && b_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol=length(a),nrow=length(brv))
    for (i in 1:length(a)) 
    {
    output[,i]<-dMcGBB(brv,max(brv),a[i],b,c)$pdf
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
    output[,i]<-dMcGBB(brv,max(brv),a,b[i],c)$pdf
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
    for (i in 1:length(c)) 
    {
    output[,i]<-dMcGBB(brv,max(brv),a,b,c[i])$pdf
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
b1c5<-dMcGBBplot(a=seq(.5,10,by=.1),b=1,c=5,
                 plot_title="and when b=1, c=5",a_seq=T,b_seq=F)
b1.2c5<-dMcGBBplot(a=seq(.5,10,by=.1),b=1.2,c=5,
                   plot_title="and when b=1.2, c=5",a_seq=T,b_seq=F)
b3c5<-dMcGBBplot(a=seq(.5,10,by=.1),b=3,c=5,
                 plot_title="and when b=3, c=5",a_seq=T,b_seq=F)
b3.2c5<-dMcGBBplot(a=seq(.5,10,by=.1),b=3.2,c=5,
                   plot_title="and when b=3.2 c=5",a_seq=T,b_seq=F)

grid.arrange(b1c5,b1.2c5,b3c5,b3.2c5,nrow=2,
             top="Pmf values changing when a=seq(0.5,10,by=0.1)")

b1c1<-dMcGBBplot(a=seq(.5,100,by=.1),b=1,c=1,
                 plot_title="and when b=1, c=1",a_seq=T,b_seq=F)
b1c1.5<-dMcGBBplot(a=seq(.5,100,by=.1),b=1,c=1.5,
                   plot_title="and when b=1, c=1.5",a_seq=T,b_seq=F)
b1c2<-dMcGBBplot(a=seq(.5,100,by=.1),b=1,c=2,
                 plot_title="and when b=1, c=2",a_seq=T,b_seq=F)
b1c2.5<-dMcGBBplot(a=seq(.5,100,by=.1),b=1,c=2.5,
                   plot_title="and when b=1, c=2.5",a_seq=T,b_seq=F)

grid.arrange(b1c1,b1c1.5,b1c2,b1c2.5,nrow=2,
             top="Pmf values changing when a=seq(0.5,100,by=0.1)")

## ----Additive Binomial Distribution plot function, include=FALSE---------
dAddBinplot<-function(p,alpha,plot_title,p_seq)
{
  if(p_seq==TRUE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol=length(p),nrow=length(brv))
    for (i in 1:length(p)) 
    {
    output[,i]<-dAddBin(brv,max(brv),p[i],alpha)$pdf
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
  if(p_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol =length(alpha) ,nrow=length(brv))
    for (i in 1:length(alpha)) 
    {
    output[,i]<-dAddBin(brv,max(brv),p,alpha[i])$pdf
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

## ----Additie Binomial Distribution plotting,fig.align='center',fig.width=9,fig.height=7----
alpha.005<-dAddBinplot(p=seq(0.35,0.65,by=0.01),alpha=-0.005,
                       plot_title="and when alpha=-0.005",p_seq= T)
alpha.001<-dAddBinplot(p=seq(0.35,0.65,by=0.01),alpha=-0.001,
                       plot_title="and when alpha=-0.001",p_seq= T)
alpha0.001<-dAddBinplot(p=seq(0.35,0.65,by=0.01),alpha=0.001,
                        plot_title="and when alpha=0.001",p_seq= T)
alpha0.005<-dAddBinplot(p=seq(0.35,0.65,by=0.01),alpha=0.005,
                        plot_title="and when alpha=0.005",p_seq= T)

grid.arrange(alpha.005,alpha.001,alpha0.001,alpha0.005,nrow=2,
             top="Pmf values changing when p=seq(0.35,0.65,by=0.01)")

p.10<-dAddBinplot(alpha=seq(.01,.1,by=0.001),p=0.1,
                  plot_title="and when p=0.10",p_seq= F)
p.30<-dAddBinplot(alpha=seq(.01,.1,by=0.001),p=0.3,
                  plot_title="and when p=0.30",p_seq= F)
p.50<-dAddBinplot(alpha=seq(.01,.1,by=0.001),p=0.5,
                  plot_title="and when p=0.50",p_seq= F)
p.85<-dAddBinplot(alpha=seq(.01,.1,by=0.001),p=0.85,
                  plot_title="and when p=0.85",p_seq= F)

grid.arrange(p.10,p.30,p.50,p.85,nrow=2,
             top="Pmf values changing when alpha=seq(0.01,0.1,by=0.001)")

## ----Beta-Correlated Binomial Distribution plot function, include=FALSE----
dBetaCorrBinplot<-function(a,b,cov,plot_title,a_seq,b_seq)
{
  if(a_seq==TRUE && b_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol=length(a),nrow=length(brv))
    for (i in 1:length(a)) 
    {
    output[,i]<-dBetaCorrBin(brv,max(brv),cov,a[i],b)$pdf
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
    output[,i]<-dBetaCorrBin(brv,max(brv),cov,a,b[i])$pdf
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
    for (i in 1:length(c)) 
    {
    output[,i]<-dBetaCorrBin(brv,max(brv),cov[i],a,b)$pdf
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
b16cov5<-dBetaCorrBinplot(a=seq(10,100,by=0.1),b=16,cov=0.0005,
                 plot_title="and when b=16, cov=0.0005",a_seq=T,b_seq=F)
b46cov5<-dBetaCorrBinplot(a=seq(10,100,by=0.1),b=46,cov=0.0005,
                   plot_title="and when b=46, cov=0.0005",a_seq=T,b_seq=F)
b76cov5<-dBetaCorrBinplot(a=seq(10,100,by=0.1),b=76,cov=0.0005,
                 plot_title="and when b=76, cov=0.0005",a_seq=T,b_seq=F)
b106cov5<-dBetaCorrBinplot(a=seq(10,100,by=0.1),b=106,cov=0.0005,
                   plot_title="and when b=106 cov=0.0005",a_seq=T,b_seq=F)

grid.arrange(b16cov5,b46cov5,b76cov5,b106cov5,nrow=2,
             top="Pmf values changing when a=seq(10,100,by=0.1)")

b10cov1<-dBetaCorrBinplot(a=seq(15,100,by=0.1),b=10,cov=0.001,
                 plot_title="and when b=10, cov=0.001",a_seq=T,b_seq=F)
b10cov3<-dBetaCorrBinplot(a=seq(15,100,by=0.1),b=10,cov=0.003,
                   plot_title="and when b=10, cov=0.003",a_seq=T,b_seq=F)
b10cov7<-dBetaCorrBinplot(a=seq(15,100,by=0.1),b=10,cov=0.007,
                 plot_title="and when b=10, cov=0.007",a_seq=T,b_seq=F)
b10cov9<-dBetaCorrBinplot(a=seq(15,100,by=0.1),b=10,cov=0.009,
                   plot_title="and when b=10, cov=0.009",a_seq=T,b_seq=F)

grid.arrange(b10cov1,b10cov3,b10cov7,b10cov9,nrow=2,
             top="Pmf values changing when a=seq(15,100,by=0.1)")

## ----COM Poisson Binomial Distribution, include=FALSE--------------------
dCOMPBinplot<-function(p,v,plot_title,p_seq)
{
  if(p_seq==TRUE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol=length(p),nrow=length(brv))
    for (i in 1:length(p)) 
    {
    output[,i]<-dCOMPBin(brv,max(brv),p[i],v)$pdf
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
  if(p_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol =length(v) ,nrow=length(brv))
    for (i in 1:length(v)) 
    {
    output[,i]<-dCOMPBin(brv,max(brv),p,v[i])$pdf
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

## ----COM Poisson Binomial Distrbution plotting,fig.align='center',fig.width=9,fig.height=7----
v.1<-dCOMPBinplot(p=seq(0.25,0.75,by=0.001),v=-0.1,
                  plot_title="and when v=-0.1",p_seq= T)
v.01<-dCOMPBinplot(p=seq(0.25,0.75,by=0.001),v=-0.01,
                   plot_title="and when v=-0.01",p_seq= T)
v01<-dCOMPBinplot(p=seq(0.25,0.75,by=0.001),v=1,
                  plot_title="and when v=1",p_seq= T)
v05<-dCOMPBinplot(p=seq(0.25,0.75,by=0.001),v=5,
                  plot_title="and when v=5",p_seq= T)

grid.arrange(v.1,v.01,v01,v05,nrow=2,
             top="Pmf values changing when p=seq(0.25,0.75,by=0.01)")

p0.25<-dCOMPBinplot(v=seq(-0.5,0.5,by=.01),p=0.25,
                   plot_title="and when p=0.25",p_seq= F)
p0.45<-dCOMPBinplot(v=seq(-0.5,0.5,by=.01),p=0.45,
                    plot_title="and when p=0.45",p_seq= F)
p0.50<-dCOMPBinplot(v=seq(-0.5,0.5,by=.01),p=0.50,
                    plot_title="and when p=0.50",p_seq= F)
p0.65<-dCOMPBinplot(v=seq(-0.5,0.5,by=.01),p=0.65,
                    plot_title="and when p=0.65",p_seq= F)

grid.arrange(p0.25,p0.45,p0.50,p0.65,nrow=2,
             top="Pmf values changing when v=seq(-0.5,0.5,by=0.01)")

## ----Correlated Binomial Distribution plot function,include=FALSE--------
dCorrBinplot<-function(p,cov,plot_title,p_seq)
{
  if(p_seq==TRUE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol=length(p),nrow=length(brv))
    for (i in 1:length(p)) 
    {
    output[,i]<-dCorrBin(brv,max(brv),p[i],cov)$pdf
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
  if(p_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol =length(cov) ,nrow=length(brv))
    for (i in 1:length(cov)) 
    {
    output[,i]<-dCorrBin(brv,max(brv),p,cov[i])$pdf
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

## ----Correlated Binomial Distribution plotting,fig.align='center',fig.width=9,fig.height=7----
cov.0001<-dCorrBinplot(p=seq(0.35,0.75,by=0.001),cov=-0.0005,
                       plot_title="and when cov=-0.0005",p_seq= T)
cov.0005<-dCorrBinplot(p=seq(0.35,0.75,by=0.001),cov=-0.0001,
                       plot_title="and when cov=-0.0001",p_seq= T)
cov0.01<-dCorrBinplot(p=seq(0.35,0.75,by=0.001),cov=0.01,
                        plot_title="and when cov=0.01",p_seq= T)
cov0.05<-dCorrBinplot(p=seq(0.35,0.75,by=0.001),cov=0.02,
                        plot_title="and when cov=0.02",p_seq= T)

grid.arrange(cov.0001,cov.0005,cov0.01,cov0.05,nrow=2,
             top="Pmf values changing when p=seq(0.35,0.75,by=0.001)")

p0.075<-dCorrBinplot(cov=seq(0.002,0.004,by=.0001),p=0.075,
                    plot_title="and when p=0.075",p_seq= F)
p0.175<-dCorrBinplot(cov=seq(0.002,0.004,by=.0001),p=0.175,
                    plot_title="and when p=0.175",p_seq= F)
p0.275<-dCorrBinplot(cov=seq(0.002,0.004,by=.0001),p=0.275,
                    plot_title="and when p=0.275",p_seq= F)
p0.375<-dCorrBinplot(cov=seq(0.002,0.004,by=.0001),p=0.375,
                    plot_title="and when p=0.375",p_seq= F)

grid.arrange(p0.075,p0.175,p0.275,p0.375,nrow=2,
             top="Pmf values changing when cov=seq(0.002,0.004,by=0.0001)")

## ----Multiplicative Binomial Distribution plot function,include=FALSE----
dMultiBinplot<-function(p,theta,plot_title,p_seq)
{
  if(p_seq==TRUE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol=length(p),nrow=length(brv))
    for (i in 1:length(p)) 
    {
    output[,i]<-dMultiBin(brv,max(brv),p[i],theta)$pdf
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variable")+
      ylab("Probability Mass values")+
      ggtitle(plot_title)
      return(p1)
  }
  if(p_seq==FALSE)
  {
  brv<-seq(0,15,by=1)
  output<-matrix(ncol =length(theta) ,nrow=length(brv))
    for (i in 1:length(theta)) 
    {
    output[,i]<-dMultiBin(brv,max(brv),p,theta[i])$pdf
    }
  data<-data.frame(brv,output)
  data<-melt(data,id.vars ="brv" )
  p1<-ggplot(data,aes(brv,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Binomial Random Variables")+
      ylab("Probability Mass values")+
      ggtitle(plot_title)
      return(p1)
  }
}

## ----Multiplicative Binomial Distribution plotting,fig.align='center',fig.width=9,fig.height=7----
theta1<-dMultiBinplot(p=seq(0.01,0.99,by=0.01),theta=1,
                       plot_title="and when theta=1",p_seq= T)
theta5<-dMultiBinplot(p=seq(0.01,0.99,by=0.01),theta=5,
                       plot_title="and when theta=5",p_seq= T)
theta10<-dMultiBinplot(p=seq(0.01,0.99,by=0.01),theta=10,
                      plot_title="and when theta=10",p_seq= T)
theta50<-dMultiBinplot(p=seq(0.01,0.99,by=0.01),theta=50,
                      plot_title="and when theta=50",p_seq= T)

grid.arrange(theta1,theta5,theta10,theta50,nrow=2,
             top="Pmf values changing when p=seq(0.01,0.99,by=0.01)")

p0.10<-dMultiBinplot(theta=seq(1,20,by=0.01),p=0.01,
                     plot_title="and when p=0.010",p_seq= F)
p0.25<-dMultiBinplot(theta=seq(1,20,by=0.01),p=0.015,
                     plot_title="and when p=0.015",p_seq= F)
p0.50<-dMultiBinplot(theta=seq(1,20,by=0.01),p=0.90,
                     plot_title="and when p=0.900",p_seq= F)
p0.75<-dMultiBinplot(theta=seq(1,20,by=0.01),p=0.925,
                     plot_title="and when p=0.925",p_seq= F)

grid.arrange(p0.10,p0.25,p0.50,p0.75,nrow=2,
             top="Pmf values changing when theta=seq(1,20,by=0.01)")

