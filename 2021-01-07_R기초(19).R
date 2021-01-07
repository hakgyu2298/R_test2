library(ggplot2)

ggplot(data.frame(x=c(0,10)), aes(x=x))+
  stat_function(fun=dexp, args=list(rate=1), colour="brown", size=1.5)+
  ggtitle("Exponential Distribution")

ggplot(data.frame(x=c(0,10)),aes(x=x))+
  stat_function(fun=pexp,args=list(rate=1),colour="brown",size=1.5)+
  ggtitle("Cumulative Exponential Distribution")

pexp(q=2,rate=1,lower.tail=TRUE)
qexp(p=0.8646647,rate=1,lower.tail=TRUE)

rexp(100, rate=1)
hist(rexp(100,rate=1),breaks = 10)

dexp <- dexp(c(1:10),rate=1)
dexp_log <- dexp(c(1:10),rate=1,log=TRUE)

exp_df <- data.frame(cbind(1:10),dexp,dexp_log)
exp_df

exp_df <- transform(exp_df,dexp_logarithm=log(dexp))
exp_df

my_par <- par(no.readonly = TRUE)
par(oma=c(0,0,1,0))
par(mfrow=c(1,2))

plot(dexp,main="dexp : log = F")
plot(dexp_log, main = "dexp : log = T")

mtext("denstity function of exponential distribution : log = FALSE vs. log =TRUE",
      outer = TRUE, cex = 1.2)

x <- c(1:30)
y <- c(1:5)

xy <- data.frame(x,y)
str(xy)
xy

for(i in 1:5){
  paste("xy_",i,sep="") <- subset(xy,subset=(y==i))
}
for(i in 1:5){
  assign(paste("xy_",i,sep=""),subset(xy,subset=(y==i)))
}

ggplot(data.frame(x=c(-3,3)),aes(x=x))+
  stat_function(fun=dnorm,colour="blue",size=1)+
  stat_function(fun=dt,args=list(df=3),colour="red",size=2)+
  stat_function(fun=dt,args=list(df=1),colour="yellow",size=3)+
  annotate("segment",x=1.5,xend=2,y=0.4,yend=0.4,colour="blue",size=1)+
  annotate("segment",x=1.5,xend=2,y=0.37,yend=0.37,colour="red",size=2)+
  annotate("segment",x=1.5,xend=2,y=0.34,yend=0.34,colour="yellow",size=3)+
  annotate("text",x=2.4,y=0.4,label="N(0,1)")+
  annotate("text",x=2.4,y=0.37,label="t(3)")+
  annotate("text",x=2.4,y=0.34,label="t(1)")+
  ggtitle("Normal Distribution, t-distribution")

ggplot(data.frame(x=c(-3,3)),aes(x=x))+
  stat_function(fun=pt,args=list(df=1),colour="brown",size=1.5)+
  ggtitle("Cumulative t-Distribution : t(1)")

pt(q=1,df=1,lower.tail=TRUE)
qt(p=0.75,df=1,lower.tail=TRUE)

rt <- rt(50,df=1)
hist(rt,breaks=20)
par(mfrow=c(1,1))
