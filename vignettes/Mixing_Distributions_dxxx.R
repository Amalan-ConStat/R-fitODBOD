## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(fitODBOD)
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)

## ----Uniform Distribution,fig.align='center',fig.width=9,fig.height=7----
prob <- seq(0.01,0.99,by=0.01)
pdfv <- dUNI(prob)$pdf
data <- data.frame(prob,pdfv)
ggplot(data)+
  geom_line(aes(x=data$prob,y=data$pdfv))+
  xlab("Vector of Probabilities")+
  ylab("Probability density values")+
  ggthemes::theme_clean()+
  ggtitle("Pdf values changing")

## ----Triangular Distribution,fig.align='center',fig.width=9,fig.height=7----
prob <- seq(0.01,0.99,by=0.01)
mode <- seq(0.01,0.99,by=0.01)
output <- matrix(ncol =length(mode) ,nrow=length(prob))
for (i in 1:length(mode)) 
  {
   output[,i]<-dTRI(prob,mode[i])$pdf
  }
data <- data.frame(prob,output)
data <- melt(data,id.vars ="prob" )
ggplot(data,aes(prob,value,col=variable))+
  geom_line()+guides(fill=FALSE,color=FALSE)+
  xlab("Vector of Probabilities")+
  ylab("Probability density values")+ggthemes::theme_clean()+
  ggtitle("Pdf values changing when c=seq(0.01,0.09,by0.01)")

## ----Beta Distribution plot function, include=FALSE----------------------
dBETAplot<-function(a,b,plot_title,a_seq)
{
  if(a_seq==TRUE)
  {
  prob<-seq(0.01,0.99,by=0.01)
  output<-matrix(ncol=length(a),nrow=length(prob))
    for (i in 1:length(a)) 
    {
    output[,i]<-dBETA(prob,a[i],b)$pdf
    }
  data<-data.frame(prob,output)
  data<-melt(data,id.vars ="prob" )
  p1<-ggplot(data,aes(prob,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Vector of Probabilities")+
      ylab("Probability density values")+
      ggthemes::theme_clean()+
      ggtitle(plot_title)
      return(p1)
  }
  if(a_seq==FALSE)
  {
  prob<-seq(0.01,0.99,by=0.01)
  output<-matrix(ncol =length(b) ,nrow=length(prob))
    for (i in 1:length(b)) 
    {
    output[,i]<-dBETA(prob,a,b[i])$pdf
    }
  data<-data.frame(prob,output)
  data<-melt(data,id.vars ="prob" )
  p1<-ggplot(data,aes(prob,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Vector of Probabilities")+
      ylab("Probability density values")+
      ggthemes::theme_clean()+
      ggtitle(plot_title)
      return(p1)
  }
}

## ----Beta Distribution plotting, fig.align='center',fig.width=9,fig.height=7----
 b10 <- dBETAplot(a=seq(1,100,by=1),b=10,plot_title="and when b=10",a_seq= T)
 b50 <- dBETAplot(a=seq(1,100,by=1),b=50,plot_title="and when b=50",a_seq= T)
b100 <- dBETAplot(a=seq(1,100,by=1),b=100,plot_title="and when b=100",a_seq= T)
b200 <- dBETAplot(a=seq(1,100,by=1),b=200,plot_title="and when b=200",a_seq= T)

grid.arrange(b10,b50,b100,b200,nrow=2,top="Pdf values changing when a=seq(1:100,by=1)")

 a10 <- dBETAplot(b=seq(1,100,by=1),a=10,plot_title="and when a=10",a_seq= F)
 a50 <- dBETAplot(b=seq(1,100,by=1),a=50,plot_title="and when a=50",a_seq= F)
a100 <- dBETAplot(b=seq(1,100,by=1),a=100,plot_title="and when a=100",a_seq= F)
a200 <- dBETAplot(b=seq(1,100,by=1),a=200,plot_title="and when a=200",a_seq= F)

grid.arrange(a10,a50,a100,a200,nrow=2,top="Pdf values changing when b=seq(1:100,by=1)")

## ----Kumaraswamy Distribution plot function,include=FALSE----------------
dKUMplot<-function(a,b,plot_title,a_seq)
{
  if(a_seq==TRUE)
  {
  prob<-seq(0.01,0.99,by=0.01)
  output<-matrix(ncol=length(a),nrow=length(prob))
    for (i in 1:length(a)) 
    {
    output[,i]<-dKUM(prob,a[i],b)$pdf
    }
  data<-data.frame(prob,output)
  data<-melt(data,id.vars ="prob" )
  p1<-ggplot(data,aes(prob,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Vector of Probabilities")+
      ylab("Probability density values")+
      ggthemes::theme_clean()+
      ggtitle(plot_title)
      return(p1)
  }
  if(a_seq==FALSE)
  {
  prob<-seq(0.01,0.99,by=0.01)
  output<-matrix(ncol =length(b) ,nrow=length(prob))
    for (i in 1:length(b)) 
    {
    output[,i]<-dKUM(prob,a,b[i])$pdf
    }
  data<-data.frame(prob,output)
  data<-melt(data,id.vars ="prob" )
  p1<-ggplot(data,aes(prob,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Vector of Probabilities")+
      ylab("Probability density values")+
      ggthemes::theme_clean()+
      ggtitle(plot_title)
      return(p1)
  }
}

## ----Kumaraswamy Distribution plotting,fig.align='center',fig.width=9,fig.height=7----
 b10 <- dKUMplot(a=seq(1,100,by=1),b=10,plot_title="and when b=10",a_seq=T)
 b50 <- dKUMplot(a=seq(1,100,by=1),b=50,plot_title="and when b=50",a_seq=T)
b100 <- dKUMplot(a=seq(1,100,by=1),b=100,plot_title="and when b=100",a_seq=T)
b200 <- dKUMplot(a=seq(1,100,by=1),b=200,plot_title="and when b=200",a_seq=T)

grid.arrange(b10,b50,b100,b200,nrow=2,top="Pdf values changing when a=seq(1:100,by=1)")

 a3 <- dKUMplot(b=seq(1,100,by=1),a=3,plot_title="and when a=3",a_seq=F)
 a5 <- dKUMplot(b=seq(1,100,by=1),a=5,plot_title="and when a=5",a_seq=F)
a10 <- dKUMplot(b=seq(1,100,by=1),a=10,plot_title="and when a=10",a_seq=F)
a15 <- dKUMplot(b=seq(1,100,by=1),a=15,plot_title="and when a=15",a_seq=F)

grid.arrange(a3,a5,a10,a15,nrow=2,top="Pdf values changing when b=seq(1:100,by=1)")

## ----Gaussian Hypergeometric Generalized Beta Distribution plot function, include=FALSE----
dGHGBetaplot<-function(n,a,b,c,plot_title,a_seq,b_seq)
{
  if(a_seq==TRUE && b_seq==FALSE)
  {
  prob<-seq(0.01,0.99,by=0.01)
  output<-matrix(ncol=length(a),nrow=length(prob))
    for (i in 1:length(a)) 
    {
    output[,i]<-dGHGBeta(prob,n,a[i],b,c)$pdf
    }
  data<-data.frame(prob,output)
  data<-melt(data,id.vars ="prob" )
  p1<-ggplot(data,aes(prob,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Vector of Probabilities")+
      ylab("Probability density values")+
      ggthemes::theme_clean()+
      ggtitle(plot_title)
      return(p1)
  }
  if(b_seq==TRUE && a_seq==FALSE)
  {
  prob<-seq(0.01,0.99,by=0.01)
  output<-matrix(ncol =length(b) ,nrow=length(prob))
    for (i in 1:length(b)) 
    {
    output[,i]<-dGHGBeta(prob,n,a,b[i],c)$pdf
    }
  data<-data.frame(prob,output)
  data<-melt(data,id.vars ="prob" )
  p1<-ggplot(data,aes(prob,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Vector of Probabilities")+
      ylab("Probability density values")+
      ggthemes::theme_clean()+
      ggtitle(plot_title)
      return(p1)
  }
  if(a_seq==FALSE && b_seq==FALSE)
  {
  prob<-seq(0.01,0.99,by=0.01)
  output<-matrix(ncol =length(b) ,nrow=length(prob))
    for (i in 1:length(c)) 
    {
    output[,i]<-dGHGBeta(prob,n,a,b,c[i])$pdf
    }
  data<-data.frame(prob,output)
  data<-melt(data,id.vars ="prob" )
  p1<-ggplot(data,aes(prob,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Vector of Probabilities")+
      ylab("Probability density values")+
      ggthemes::theme_clean()+
      ggtitle(plot_title)
      return(p1)
  }
}

## ----Gaussian Hypergeometric Generalized Beta Distribution plotting,fig.align='center',fig.width=9,fig.height=7----
 b10c.5 <- dGHGBetaplot(n=10,a=seq(.1,100,by=.1),b=10,c=.5,
                     plot_title="and when b=10, c=0.5",a_seq=T,b_seq=F)
 b50c.5 <- dGHGBetaplot(n=10,a=seq(.1,100,by=.1),b=50,c=.5,
                     plot_title="and when b=50, c=0.5",a_seq=T,b_seq=F)
b100c.5 <- dGHGBetaplot(n=10,a=seq(.1,100,by=.1),b=100,c=.5,
                      plot_title="and when b=100, c=0.5",a_seq=T,b_seq=F)
b200c.5 <- dGHGBetaplot(n=10,a=seq(.1,100,by=.1),b=130,c=.5,
                      plot_title="and when b=130, c=0.5",a_seq=T,b_seq=F)

grid.arrange(b10c.5,b50c.5,b100c.5,b200c.5,nrow=2,
             top="Pdf values changing when a=seq(0.1,100,by=0.1)")

 b10c1 <- dGHGBetaplot(n=10,a=seq(.1,100,by=.1),b=10,c=1,
                    plot_title="and when b=10, c=1",a_seq=T,b_seq=F)
 b50c1 <- dGHGBetaplot(n=10,a=seq(.1,100,by=.1),b=50,c=1,
                    plot_title="and when b=50, c=1",a_seq=T,b_seq=F)
b100c1 <- dGHGBetaplot(n=10,a=seq(.1,100,by=.1),b=100,c=1,
                     plot_title="and when b=100, c=1",a_seq=T,b_seq=F)
b200c1 <- dGHGBetaplot(n=10,a=seq(.1,100,by=.1),b=130,c=1,
                     plot_title="and when b=200, c=1",a_seq=T,b_seq=F)

grid.arrange(b10c1,b50c1,b100c1,b200c1,nrow=2,
             top="Pdf values changing when a=seq(0.1,100,by=0.1)")

 b10c5 <- dGHGBetaplot(n=10,a=seq(.1,100,by=.1),b=10,c=5,
                    plot_title="and when b=10, c=5",a_seq=T,b_seq=F)
 b50c5 <- dGHGBetaplot(n=10,a=seq(.1,100,by=.1),b=50,c=5,
                    plot_title="and when b=50, c=5",a_seq=T,b_seq=F)
b100c5 <- dGHGBetaplot(n=10,a=seq(.1,100,by=.1),b=100,c=5,
                     plot_title="and when b=100, c=5",a_seq=T,b_seq=F)
b200c5 <- dGHGBetaplot(n=10,a=seq(.1,100,by=.1),b=150,c=5,
                     plot_title="and when b=150, c=5",a_seq=T,b_seq=F)

grid.arrange(b10c5,b50c5,b100c5,b200c5,nrow=2,
             top="Pdf values changing when a=a=seq(0.1,100,by=0.1)")

 b10c10 <- dGHGBetaplot(n=10,a=seq(.1,100,by=.1),b=10,c=10,
                     plot_title="and when b=10, c=10",a_seq=T,b_seq=F)
 b50c10 <- dGHGBetaplot(n=10,a=seq(.1,100,by=.1),b=50,c=10,
                     plot_title="and when b=50, c=10",a_seq=T,b_seq=F)
b100c10 <- dGHGBetaplot(n=10,a=seq(.1,100,by=.1),b=100,c=10,
                      plot_title="and when b=100, c=10",a_seq=T,b_seq=F)
b200c10 <- dGHGBetaplot(n=10,a=seq(.1,100,by=.1),b=200,c=10,
                      plot_title="and when b=200, c=10",a_seq=T,b_seq=F)

grid.arrange(b10c10,b50c10,b100c10,b200c10,nrow=2,
             top="Pdf values changing when a=seq(0.1,100,by=0.1)")

## ----Generalized Beta Type 1 Distribution plot function, include=FALSE----
dGBeta1plot<-function(a,b,c,plot_title,a_seq,b_seq)
{
  if(a_seq==TRUE && b_seq==FALSE)
  {
  prob<-seq(0.01,0.99,by=0.01)
  output<-matrix(ncol=length(a),nrow=length(prob))
    for (i in 1:length(a)) 
    {
    output[,i]<-dGBeta1(prob,a[i],b,c)$pdf
    }
  data<-data.frame(prob,output)
  data<-melt(data,id.vars ="prob" )
  p1<-ggplot(data,aes(prob,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Vector of Probabilities")+
      ylab("Probability density values")+
      ggthemes::theme_clean()+
      ggtitle(plot_title)
      return(p1)
  }
  if(b_seq==TRUE && a_seq==FALSE)
  {
  prob<-seq(0.01,0.99,by=0.01)
  output<-matrix(ncol =length(b) ,nrow=length(prob))
    for (i in 1:length(b)) 
    {
    output[,i]<-dGBeta1(prob,a[i],b,c)$pdf
    }
  data<-data.frame(prob,output)
  data<-melt(data,id.vars ="prob" )
  p1<-ggplot(data,aes(prob,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Vector of Probabilities")+
      ylab("Probability density values")+
      ggthemes::theme_clean()+
      ggtitle(plot_title)
      return(p1)
  }
  if(a_seq==FALSE && b_seq==FALSE)
  {
  prob<-seq(0.01,0.99,by=0.01)
  output<-matrix(ncol =length(b) ,nrow=length(prob))
    for (i in 1:length(c)) 
    {
    output[,i]<-dGBeta1(prob,a[i],b,c)$pdf
    }
  data<-data.frame(prob,output)
  data<-melt(data,id.vars ="prob" )
  p1<-ggplot(data,aes(prob,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Vector of Probabilities")+
      ylab("Probability density values")+
      ggthemes::theme_clean()+
      ggtitle(plot_title)
      return(p1)
  }
}

## ----Generalized Beta Type 1 Distribution plotting, fig.align='center',fig.width=9,fig.height=7----
 b10c.5 <- dGBeta1plot(a=seq(.1,100,by=.1),b=10,c=.5,
                    plot_title="and when b=10, c=0.5",a_seq=T,b_seq=F)
 b50c.5 <- dGBeta1plot(a=seq(.1,100,by=.1),b=50,c=.5,
                    plot_title="and when b=50, c=0.5",a_seq=T,b_seq=F)
b100c.5 <- dGBeta1plot(a=seq(.1,100,by=.1),b=100,c=.5,
                     plot_title="and when b=100, c=0.5",a_seq=T,b_seq=F)
b130c.5 <- dGBeta1plot(a=seq(.1,100,by=.1),b=130,c=.5,
                     plot_title="and when b=130, c=0.5",a_seq=T,b_seq=F)

grid.arrange(b10c.5,b50c.5,b100c.5,b130c.5,nrow=2,
             top="Pdf values changing when a=seq(0.1,100,by=0.1)")

 b10c1 <- dGBeta1plot(a=seq(.1,100,by=.1),b=10,c=1,
                   plot_title="and when b=10, c=1",a_seq=T,b_seq=F)
 b50c1 <- dGBeta1plot(a=seq(.1,100,by=.1),b=50,c=1,
                   plot_title="and when b=50, c=1",a_seq=T,b_seq=F)
b100c1 <- dGBeta1plot(a=seq(.1,100,by=.1),b=100,c=1,
                    plot_title="and when b=100, c=1",a_seq=T,b_seq=F)
b130c1 <- dGBeta1plot(a=seq(.1,100,by=.1),b=130,c=1,
                    plot_title="and when b=130, c=1",a_seq=T,b_seq=F)

grid.arrange(b10c1,b50c1,b100c1,b130c1,nrow=2,
             top="Pdf values changing when a=seq(0.1,100,by=0.1)")

 b10c5 <- dGBeta1plot(a=seq(.1,100,by=.1),b=10,c=5,
                   plot_title="and when b=10, c=5",a_seq=T,b_seq=F)
 b50c5 <- dGBeta1plot(a=seq(.1,100,by=.1),b=50,c=5,
                   plot_title="and when b=50, c=5",a_seq=T,b_seq=F)
b100c5 <- dGBeta1plot(a=seq(.1,100,by=.1),b=100,c=5,
                    plot_title="and when b=100, c=5",a_seq=T,b_seq=F)
b150c5 <- dGBeta1plot(a=seq(.1,100,by=.1),b=150,c=5,
                    plot_title="and when b=150, c=5",a_seq=T,b_seq=F)

grid.arrange(b10c5,b50c5,b100c5,b150c5,nrow=2,
             top="Pdf values changing when a=a=seq(0.1,100,by=0.1)")

 b10c10 <- dGBeta1plot(a=seq(.1,100,by=.1),b=10,c=10,
                    plot_title="and when b=10, c=10",a_seq=T,b_seq=F)
 b50c10 <- dGBeta1plot(a=seq(.1,100,by=.1),b=50,c=10,
                    plot_title="and when b=50, c=10",a_seq=T,b_seq=F)
b100c10 <- dGBeta1plot(a=seq(.1,100,by=.1),b=100,c=10,
                     plot_title="and when b=100, c=10",a_seq=T,b_seq=F)
b200c10 <- dGBeta1plot(a=seq(.1,100,by=.1),b=200,c=10,
                     plot_title="and when b=200, c=10",a_seq=T,b_seq=F)

grid.arrange(b10c10,b50c10,b100c10,b200c10,nrow=2,
             top="Pdf values changing when a=seq(0.1,100,by=0.1)")

## ----Gamma Distribution plot function, include=FALSE---------------------
dGAMMAplot<-function(a,b,plot_title,a_seq)
{
  if(a_seq==TRUE)
  {
    prob<-seq(0.01,0.99,by=0.01)
    output<-matrix(ncol=length(a),nrow=length(prob))
    for (i in 1:length(a))
    {
      output[,i]<-dGAMMA(prob,a[i],b)$pdf
    }
    data<-data.frame(prob,output)
    data<-melt(data,id.vars ="prob" )
    p1<-ggplot(data,aes(prob,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Vector of Probabilities")+
      ylab("Probability density values")+
      ggthemes::theme_clean()+
      ggtitle(plot_title)
    return(p1)
  }
  if(a_seq==FALSE)
  {
    prob<-seq(0.01,0.99,by=0.01)
    output<-matrix(ncol =length(b) ,nrow=length(prob))
    for (i in 1:length(b))
    {
      output[,i]<-dGAMMA(prob,a,b[i])$pdf
    }
    data<-data.frame(prob,output)
    data<-melt(data,id.vars ="prob" )
    p1<-ggplot(data,aes(prob,value,col=variable))+
      geom_line()+guides(fill=FALSE,color=FALSE)+
      xlab("Vector of Probabilities")+
      ylab("Probability density values")+
      ggthemes::theme_clean()+
      ggtitle(plot_title)
    return(p1)
  }
}

## ----Gamma Distribution plotting, fig.align='center',fig.width=9,fig.height=7----
b10 <- dGAMMAplot(a=seq(1,100,by=1),b=10,plot_title="and when b=10",a_seq= T)
b50 <- dGAMMAplot(a=seq(1,100,by=1),b=50,plot_title="and when b=50",a_seq= T)
b20 <- dGAMMAplot(a=seq(1,100,by=1),b=20,plot_title="and when b=20",a_seq= T)
b30 <- dGAMMAplot(a=seq(1,100,by=1),b=30,plot_title="and when b=30",a_seq= T)

grid.arrange(b10,b20,b30,b50,nrow=2,top="Pdf values changing when a=seq(1:100,by=1)")

a10 <- dGAMMAplot(b=seq(1,100,by=1),a=10,plot_title="and when a=10",a_seq= F)
a50 <- dGAMMAplot(b=seq(1,100,by=1),a=50,plot_title="and when a=50",a_seq= F)
a20 <- dGAMMAplot(b=seq(1,100,by=1),a=20,plot_title="and when a=20",a_seq= F)
a30 <- dGAMMAplot(b=seq(1,100,by=1),a=30,plot_title="and when a=30",a_seq= F)

grid.arrange(a10,a20,a30,a50,nrow=2,top="Pdf values changing when b=seq(1:100,by=1)")

