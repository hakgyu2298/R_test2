library(ggplot2)

ggplot(data.frame(x=c(0,5)),aes(x=x))+
  stat_function(fun=df,args=list(df1=5,df2=10),colour="blue",size=1)+
  stat_function(fun=df,args=list(df1=10,df2=30),colour="red",size=2)+
  stat_function(fun=df,args=list(df1=50,df2=100),colour="yellow",size=3)+
  annotate("segment",x=3,xend=3.5,y=1.4,yend=1.4,colour="blue",size=1)+
  annotate("segment",x=3,xend=3.5,y=1.2,yend=1.2,colour="red",size=2)+
  annotate("segment",x=3,xend=3.5,y=1.0,yend=1.0,colour="yellow",size=3)+
  annotate("text",x=4.3,y=1.4,label="F(df1=5, df2=10")+
  annotate("text",x=4.3,y=1.2,label="F(df1=10, df2=30")+
  annotate("text",x=4.3,y=1.0,label="F(df1=50, df2=100")+
  ggtitle("F Distribution")

ggplot(data.frame(x=c(0,5)),aes(x=x))+
  stat_function(fun=pf,args=list(df1=5, df2=10),colour="blue",size=1)+
  ggtitle("Cumulative F-distribution : F(df1=5,df2=10)")

pf(q=2,df1 = 5,df2=10,lower.tail=TRUE)
qf(p=0.835805,df1=5,df2=10,lower.tail=TRUE)

rf <- rf(10000,df1=5,df2=10)
hist(rf,breaks=1000)

ggplot(data.frame(x=c(0,10)),aes(x=x))+
  stat_function(fun=dchisq,args=list(df=1),colour="black",size=1.2)+
  geom_text(x=0.6,y=1,label="df=1")+
  
  stat_function(fun=dchisq,args=list(df=2),colour="blue",size=1.2)+
  geom_text(x=0,y=0.55,label="df=2")+
  
  stat_function(fun=dchisq,args=list(df=3),colour="red",size=1.2)+
  geom_text(x=0.5,y=0.05,label="df=3")+
  
  ggtitle("Chisq-Distribution")

ggplot(data.frame(x=c(0,10)),aes(x=x))+
  stat_function(fun=pchisq,args=list(df=1),colour="black",size=1.2)+
  geom_text(x=2.5,y=0.93,label="df=1")+
  
  stat_function(fun=pchisq,args=list(df=2),colour="blue",size=1.2)+
  geom_text(x=2.5,y=0.77,label="df=2")+
  
  stat_function(fun=pchisq,args=list(df=3),colour="red",size=1.2)+
  geom_text(x=2.5,y=0.45,label="df=3")+
  
  ggtitle("Cumulative Chisq-Distribution")

pchisq(q=2.5,df=1,lower.tail=TRUE)
pchisq(q=2.5,df=2,lower.tail=TRUE)
pchisq(q=2.5,df=3,lower.tail=TRUE)

qchisq(p=0.8861537,df=1,lower.tail=TRUE)
qchisq(p=0.7134952,df=2,lower.tail=TRUE)
qchisq(p=0.5247089,df=3,lower.tail=TRUE)

rchisq <- rchisq(n=100,df=2)
hist(rchisq,breaks=20)

library(gmodels)
library(vcd)

str(Arthritis)

attach(Arthritis)
CrossTable(Treatment,Improved,
           expected = TRUE,
           chisq=TRUE)
detach(Arthritis)
