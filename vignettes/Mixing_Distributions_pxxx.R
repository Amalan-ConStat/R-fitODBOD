## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(fitODBOD)
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)

## ----Uniform Distribution plotting,fig.align='center',fig.width=9,fig.height=7----
prob<-seq(0,1,by=0.05)
pdfv<-pUNI(prob)
data<-data.frame(prob,pdfv)
ggplot(data)+
  geom_line(aes(x=data$prob,y=data$pdfv))+
  xlab("Vector of Probabilities")+
  ylab("Cumulative Probability density values")+
  ggtitle("Cpdf values changing")+
  scale_x_continuous(breaks=seq(0,1,by=0.1))

## ----Triangular Distribution plotting,fig.align='center',fig.width=9,fig.height=7----
prob<-seq(0.01,0.99,by=0.01)
mode<-seq(0.01,0.99,by=0.01)
output<-matrix(ncol =length(mode) ,nrow=length(prob))
for (i in 1:length(mode))
  {
   output[,i]<-pTRI(prob,mode[i])
  }
data<-data.frame(prob,output)
data<-melt(data,id.vars ="prob" )
ggplot(data,aes(prob,value,col=variable))+
  geom_line()+guides(fill=FALSE,color=FALSE)+
  xlab("Vector of Probabilities")+
  ylab("Cumulative Probability density values")+
  ggtitle("Cpdf values changing when c=seq(0.01,0.09,by0.01)")+
  scale_x_continuous(breaks=seq(0,1,by=0.1))

## ----Beta Distribution plot function, include=FALSE----------------------
pBETAplot<-function(a,b,plot_title,a_seq)
{
  if(a_seq==TRUE)
  {
  prob<-seq(0.01,0.99,by=0.01)
  output<-matrix(ncol=length(a),nrow=length(prob))
    for (i in 1:length(a))
    {
    output[,i]<-pBETA(prob,a[i],b)
    }
  data<-data.frame(prob,output)
  data<-melt(data,id.vars ="prob" )
  p1<-ggplot(data,aes(prob,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Vector of Probabilities")+
      ylab("Probability density values")+
      ggtitle(plot_title)
      return(p1)
  }
  if(a_seq==FALSE)
  {
  prob<-seq(0.01,0.99,by=0.01)
  output<-matrix(ncol =length(b) ,nrow=length(prob))
    for (i in 1:length(b))
    {
    output[,i]<-pBETA(prob,a,b[i])
    }
  data<-data.frame(prob,output)
  data<-melt(data,id.vars ="prob" )
  p1<-ggplot(data,aes(prob,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Vector of Probabilities")+
      ylab("Probability density values")+
      ggtitle(plot_title)
      return(p1)
  }
}

## ----Beta Distribution function plotting,fig.align='center',fig.width=9,fig.height=7----
b3<-pBETAplot(a=seq(1,100,by=1),b=3,plot_title="and when b=3",a_seq= T)
b5<-pBETAplot(a=seq(1,100,by=1),b=5,plot_title="and when b=5",a_seq= T)
b8<-pBETAplot(a=seq(1,100,by=1),b=8,plot_title="and when b=8",a_seq= T)
b10<-pBETAplot(a=seq(1,100,by=1),b=10,plot_title="and when b=10",a_seq= T)

grid.arrange(b3,b5,b8,b10,nrow=2,top="Cpdf values changing when a=seq(1,100,by=1)")

a3<-pBETAplot(b=seq(1,100,by=1),a=3,plot_title="and when a=3",a_seq= F)
a5<-pBETAplot(b=seq(1,100,by=1),a=5,plot_title="and when a=5",a_seq= F)
a8<-pBETAplot(b=seq(1,100,by=1),a=8,plot_title="and when a=8",a_seq= F)
a10<-pBETAplot(b=seq(1,100,by=1),a=10,plot_title="and when a=10",a_seq= F)

grid.arrange(a3,a5,a8,a10,nrow=2,top="Cpdf values changing when b=seq(1,100,by=1)")

## ----Kumaraswamy Distribution plot function, include=FALSE---------------
pKUMplot<-function(a,b,plot_title,a_seq)
{
  if(a_seq==TRUE)
  {
  prob<-seq(0.01,0.99,by=0.01)
  output<-matrix(ncol=length(a),nrow=length(prob))
    for (i in 1:length(a))
    {
    output[,i]<-pKUM(prob,a[i],b)
    }
  data<-data.frame(prob,output)
  data<-melt(data,id.vars ="prob" )
  p1<-ggplot(data,aes(prob,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Vector of Probabilities")+
      ylab("Probability density values")+
      ggtitle(plot_title)
      return(p1)
  }
  if(a_seq==FALSE)
  {
  prob<-seq(0.01,0.99,by=0.01)
  output<-matrix(ncol =length(b) ,nrow=length(prob))
    for (i in 1:length(b))
    {
    output[,i]<-pKUM(prob,a,b[i])
    }
  data<-data.frame(prob,output)
  data<-melt(data,id.vars ="prob" )
  p1<-ggplot(data,aes(prob,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Vector of Probabilities")+
      ylab("Probability density values")+
      ggtitle(plot_title)
      return(p1)
  }
}

## ----Kumaraswamy Distribution plotting,fig.align='center',fig.width=9,fig.height=7----
b1<-pKUMplot(a=seq(1,100,by=1),b=1,plot_title="and when b=1",a_seq=T)
b2<-pKUMplot(a=seq(1,100,by=1),b=2,plot_title="and when b=2",a_seq=T)
b5<-pKUMplot(a=seq(1,100,by=1),b=5,plot_title="and when b=5",a_seq=T)
b10<-pKUMplot(a=seq(1,100,by=1),b=10,plot_title="and when b=10",a_seq=T)

grid.arrange(b1,b2,b5,b10,nrow=2,top="Cpdf values changing when a=seq(1,100,by=1)")

a3<-pKUMplot(b=seq(1,100,by=1),a=3,plot_title="and when a=3",a_seq=F)
a5<-pKUMplot(b=seq(1,100,by=1),a=5,plot_title="and when a=5",a_seq=F)
a10<-pKUMplot(b=seq(1,100,by=1),a=10,plot_title="and when a=10",a_seq=F)
a15<-pKUMplot(b=seq(1,100,by=1),a=13,plot_title="and when a=15",a_seq=F)

grid.arrange(a3,a10,a10,a15,nrow=2,top="Cpdf values changing when b=seq(1,100,by=1)")

## ----Gaussian Hypergeometric Generalized Beta Distribution plot function,include=FALSE----
pGHGBetaplot<-function(n,a,b,c,plot_title,a_seq,b_seq)
{
  if(a_seq==TRUE && b_seq==FALSE)
  {
  prob<-seq(0.01,0.99,by=0.01)
  output<-matrix(ncol=length(a),nrow=length(prob))
    for (i in 1:length(a))
    {
    output[,i]<-pGHGBeta(prob,n,a[i],b,c)
    }
  data<-data.frame(prob,output)
  data<-melt(data,id.vars ="prob" )
  p1<-ggplot(data,aes(prob,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Vector of Probabilities")+
      ylab("Probability density values")+
      ggtitle(plot_title)
      return(p1)
  }
  if(b_seq==TRUE && a_seq==FALSE)
  {
  prob<-seq(0.01,0.99,by=0.01)
  output<-matrix(ncol =length(b) ,nrow=length(prob))
    for (i in 1:length(b))
    {
    output[,i]<-pGHGBeta(prob,n,a,b[i],c)
    }
  data<-data.frame(prob,output)
  data<-melt(data,id.vars ="prob" )
  p1<-ggplot(data,aes(prob,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Vector of Probabilities")+
      ylab("Probability density values")+
      ggtitle(plot_title)
      return(p1)
  }
  if(a_seq==FALSE && b_seq==FALSE)
  {
  prob<-seq(0.01,0.99,by=0.01)
  output<-matrix(ncol =length(b) ,nrow=length(prob))
    for (i in 1:length(c))
    {
    output[,i]<-pGHGBeta(prob,n,a,b,c[i])
    }
  data<-data.frame(prob,output)
  data<-melt(data,id.vars ="prob" )
  p1<-ggplot(data,aes(prob,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Vector of Probabilities")+
      ylab("Probability density values")+
      ggtitle(plot_title)
      return(p1)
  }
}

## ----Gaussian Hypergeometric Generalized Beta Distribution plotting,fig.align='center',fig.width=9,fig.height=7----
b10c5<-pGHGBetaplot(n=10,a=seq(.1,100,by=.1),b=10,c=5,
                    plot_title="and when b=10, c=5",a_seq=T,b_seq=F)
b50c5<-pGHGBetaplot(n=10,a=seq(.1,100,by=.1),b=50,c=5,
                    plot_title="and when b=50, c=5",a_seq=T,b_seq=F)
b100c5<-pGHGBetaplot(n=10,a=seq(.1,100,by=.1),b=100,c=5,
                     plot_title="and when b=100, c=5",a_seq=T,b_seq=F)
b150c5<-pGHGBetaplot(n=10,a=seq(.1,100,by=.1),b=150,c=5,
                     plot_title="and when b=150, c=5",a_seq=T,b_seq=F)

grid.arrange(b10c5,b50c5,b100c5,b150c5,nrow=2,
             top="Cpdf values changing when a=seq(0.1,100,by=0.1)")

b40c10<-pGHGBetaplot(n=10,a=seq(.1,100,by=.1),b=40,c=10,
                     plot_title="and when b=40, c=10",a_seq=T,b_seq=F)
b50c10<-pGHGBetaplot(n=10,a=seq(.1,100,by=.1),b=50,c=10,
                     plot_title="and when b=50, c=10",a_seq=T,b_seq=F)
b100c10<-pGHGBetaplot(n=10,a=seq(.1,100,by=.1),b=100,c=10,
                      plot_title="and when b=100, c=10",a_seq=T,b_seq=F)
b200c10<-pGHGBetaplot(n=10,a=seq(.1,100,by=.1),b=200,c=10,
                      plot_title="and when b=200, c=10",a_seq=T,b_seq=F)

grid.arrange(b40c10,b50c10,b100c10,b200c10,nrow=2,
             top="Cpdf values changing when a=seq(0.1,100,by=0.1)")

## ----Generalized Beta Type 1 Distribution plot function,include=FALSE----
pGBeta1plot<-function(a,b,c,plot_title,a_seq,b_seq)
{
  if(a_seq==TRUE && b_seq==FALSE)
  {
  prob<-seq(0.01,0.99,by=0.01)
  output<-matrix(ncol=length(a),nrow=length(prob))
    for (i in 1:length(a))
    {
    output[,i]<-pGBeta1(prob,a[i],b,c)
    }
  data<-data.frame(prob,output)
  data<-melt(data,id.vars ="prob" )
  p1<-ggplot(data,aes(prob,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Vector of Probabilities")+
      ylab("Probability density values")+
      ggtitle(plot_title)
      return(p1)
  }
  if(b_seq==TRUE && a_seq==FALSE)
  {
  prob<-seq(0.01,0.99,by=0.01)
  output<-matrix(ncol =length(b) ,nrow=length(prob))
    for (i in 1:length(b))
    {
    output[,i]<-pGBeta1(prob,a[i],b,c)
    }
  data<-data.frame(prob,output)
  data<-melt(data,id.vars ="prob" )
  p1<-ggplot(data,aes(prob,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Vector of Probabilities")+
      ylab("Probability density values")+
      ggtitle(plot_title)
      return(p1)
  }
  if(a_seq==FALSE && b_seq==FALSE)
  {
  prob<-seq(0.01,0.99,by=0.01)
  output<-matrix(ncol =length(b) ,nrow=length(prob))
    for (i in 1:length(c))
    {
    output[,i]<-pGBeta1(prob,a[i],b,c)
    }
  data<-data.frame(prob,output)
  data<-melt(data,id.vars ="prob" )
  p1<-ggplot(data,aes(prob,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Vector of Probabilities")+
      ylab("Probability density values")+
      ggtitle(plot_title)
      return(p1)
  }
}

## ----Generalized Beta Type 1 Distribution plotting,fig.align='center',fig.width=9,fig.height=7----
  b1c0.3<-pGBeta1plot(a=seq(1,40,by=0.1),b=1,c=0.3,
                      plot_title="and when b=1, c=0.3",a_seq=T,b_seq=F)
  b1c0.4<-pGBeta1plot(a=seq(1,40,by=0.1),b=1,c=0.4,
                      plot_title="and when b=1, c=0.4",a_seq=T,b_seq=F)
  b1c0.5<-pGBeta1plot(a=seq(1,40,by=0.1),b=1,c=0.5,
                      plot_title="and when b=1, c=0.5",a_seq=T,b_seq=F)
 b1c0.55<-pGBeta1plot(a=seq(1,40,by=0.1),b=1,c=0.55,
                      plot_title="and when b=1, c=0.55",a_seq=T,b_seq=F)

grid.arrange(b1c0.3,b1c0.4,b1c0.5,b1c0.55,nrow=2,
             top="Cpdf values changing when a=seq(1,40,by=0.1)")

  b1c1<-pGBeta1plot(a=seq(.5,100,by=.1),b=1,c=1,
                    plot_title="and when b=1, c=1",a_seq=T,b_seq=F)
b1c1.5<-pGBeta1plot(a=seq(.5,100,by=.1),b=1,c=1.5,
                    plot_title="and when b=1, c=1.5",a_seq=T,b_seq=F)
  b1c2<-pGBeta1plot(a=seq(.5,100,by=.1),b=1,c=2,
                    plot_title="and when b=1, c=2",a_seq=T,b_seq=F)
b1c2.5<-pGBeta1plot(a=seq(.5,100,by=.1),b=1,c=2.5,
                    plot_title="and when b=1, c=2.5",a_seq=T,b_seq=F)

grid.arrange(b1c1,b1c1.5,b1c2,b1c2.5,nrow=2,
             top="Cpdf values changing when a=seq(0.5,100,by=0.1)")

## ----Gamma Distribution plot function, include=FALSE----------------------
pGAMMAplot<-function(a,b,plot_title,a_seq)
{
  if(a_seq==TRUE)
  {
    prob<-seq(0.01,0.99,by=0.01)
    output<-matrix(ncol=length(a),nrow=length(prob))
    for (i in 1:length(a))
    {
      output[,i]<-pGAMMA(prob,a[i],b)
    }
    data<-data.frame(prob,output)
    data<-melt(data,id.vars ="prob" )
    p1<-ggplot(data,aes(prob,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Vector of Probabilities")+
      ylab("Probability density values")+
      ggtitle(plot_title)
    return(p1)
  }
  if(a_seq==FALSE)
  {
    prob<-seq(0.01,0.99,by=0.01)
    output<-matrix(ncol =length(b) ,nrow=length(prob))
    for (i in 1:length(b))
    {
      output[,i]<-pGAMMA(prob,a,b[i])
    }
    data<-data.frame(prob,output)
    data<-melt(data,id.vars ="prob" )
    p1<-ggplot(data,aes(prob,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Vector of Probabilities")+
      ylab("Probability density values")+
      ggtitle(plot_title)
    return(p1)
  }
}

## ----Gamma Distribution function plotting,fig.align='center',fig.width=9,fig.height=7----
b3<-pGAMMAplot(a=seq(1,10,by=0.5),b=3,plot_title="and when b=3",a_seq= T)
b5<-pGAMMAplot(a=seq(1,10,by=0.5),b=5,plot_title="and when b=5",a_seq= T)
b8<-pGAMMAplot(a=seq(1,10,by=0.5),b=8,plot_title="and when b=8",a_seq= T)
b10<-pGAMMAplot(a=seq(1,10,by=0.5),b=10,plot_title="and when b=10",a_seq= T)

grid.arrange(b3,b5,b8,b10,nrow=2,top="Cpdf values changing when a=seq(1,10,by=0.5)")

a3<-pGAMMAplot(b=seq(1,10,by=0.1),a=3,plot_title="and when a=3",a_seq= F)
a5<-pGAMMAplot(b=seq(1,10,by=0.1),a=5,plot_title="and when a=5",a_seq= F)
a8<-pGAMMAplot(b=seq(1,10,by=0.1),a=8,plot_title="and when a=8",a_seq= F)
a10<-pGAMMAplot(b=seq(1,10,by=0.1),a=10,plot_title="and when a=10",a_seq= F)

grid.arrange(a3,a5,a8,a10,nrow=2,top="Cpdf values changing when b=seq(1,10,by=0.1)")
