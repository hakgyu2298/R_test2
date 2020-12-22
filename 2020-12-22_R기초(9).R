str(anscombe)

options(digits = 2)
sapply(anscombe,mean)
sapply(anscombe,sd)

attach(anscombe)
cor(x1,y1)
cor(x2,y2)
cor(x3,y3)
cor(x4,y4)
detach(anscombe)

attach(anscombe)
lm(y1~x1)
lm(y2~x2)
lm(y3~x3)
lm(y4~x4)
detach(anscombe)

par(mfrow=c(2,2))

attach(anscombe)
plot(x1,y1); abline(lm(y1~x1),col="blue",lty=3)
plot(x2,y2); abline(lm(y2~x2),col="blue",lty=3)
plot(x3,y3); abline(lm(y3~x3),col="blue",lty=3)
plot(x4,y4); abline(lm(y4~x4),col="blue",lty=3)
detach(anscombe)

library(MASS)
str(Cars93)
library(ggplot2)

ggplot(Cars93,aes(x=Price))+geom_histogram()
range(Cars93$Price)
diff(range(Cars93$Price))
diff(range(Cars93$Price))/30

ggplot(Cars93,aes(x=Price))+geom_histogram(binwidth = 1.816)+
  ggtitle("Bindwidth=1.86; Dfault, range/30")

h1 <- ggplot(Cars93,aes(x=Price))+geom_histogram(binwidth = 1.816)+
  ggtitle("Bindwidth=1.86; Dfault, range/30")
h2 <- ggplot(Cars93,aes(x=Price))+geom_histogram(binwidth = 5)+
  ggtitle("Bindwidth=5")
h3 <- ggplot(Cars93,aes(x=Price))+geom_histogram(binwidth = 10)+
  ggtitle("Bindwidth=10")
h4 <- ggplot(Cars93,aes(x=Price))+geom_histogram(binwidth = 20)+
  ggtitle("Bindwidth=20")
h5 <- ggplot(Cars93,aes(x=Price))+geom_histogram(binwidth = 30)+
  ggtitle("Bindwidth=30")
h6 <- ggplot(Cars93,aes(x=Price))+geom_histogram(binwidth = 40)+
  ggtitle("Bindwidth=40")

multiplot <- function(...,plotlist=NULL,file,cols=1,layout=NULL){
  library(grid)
  
  plots = c(list(...),plotlist)
  numPlots = length(plots)
  
  if(is.null(layout)){
    layout=matrix(seq(1,cols*ceiling(numPlots/cols)),
                  ncol=cols,nrow=ceiling(numPlots/cols))
  }
  if(numPlots==1){
    print(plots[[1]])
  }else{
    grid.newpage()
    pushViewport(viewport(layout=grid.layout(nrow(layout),ncol(layout))))
    
    for(i in 1:numPlots){
      matchidx <- as.data.frame(which(layout==i,arr.ind = TRUE))
      print(plots[[i]],vp=viewport(layout.pos.row = matchidx$row,
                                   layout.pos.col = matchidx$col))
    }
  }
}

multiplot(h1,h2,h3,h4,h5,h6,cols = 2)

ggplot(Cars93,aes(x=Price))+
  geom_histogram(binwidth=5,fill="blue",colour="black")+
  ggtitle("Binwidth = 5, fill = blue, colour = black")

class(Cars93$Type);levels(Cars93$Type)

ggplot(Cars93,aes(x=Price))+
  geom_histogram(binwidth=5,fill="blue",colour="black")+
  ggtitle("Binwidth = 5, fill = blue, colour = black, group by Type")+
  facet_grid(Type~.)

ggplot(Cars93,aes(x=Price))+
  geom_density(fill="yellow",colour=NA,alpha=.5)+
  geom_line(stat = "density")+
  expand_limits(y=0)+
  ggtitle("Kernel Density Curve")

ggplot(Cars93,aes(x=Price,colour=Type))+
  geom_density(fill=NA)+
  geom_line(stat="density")+
  expand_limits(y=0)+
  ggtitle("Kernel Density Curve by Car Type overlap")

ggplot(Cars93,aes(x=Price))+
  geom_density(fill="yellow",colour=NA,alpha=.5)+
  geom_line(stat="density")+
  expand_limits(y=0)+
  ggtitle("Kernel Density Curve by Car Type")+
  facet_grid(Type~.)+
  xlim(10,40)

ggplot(Cars93,aes(x=Price,y=..density..))+
  geom_histogram(binwidth=5,fill="blue",colour="white",alpha=0.5)+
  geom_density(fill=NA,colour=NA,alpha=0.8)+
  geom_line(stat="density")+
  expand_limits(y=0)+
  ggtitle("Histogram + Kernel Density Curve")

mydf <- data.frame(var=c(1100,10000,100000,190000,110000,220000,550000,701000,790000))
options(scipe=30)
mydf$group <- ifelse(mydf$var<10000,1,
                     ifelse(mydf$var<100000,2,
                            ifelse(mydf$var<200000,3,
                                   ifelse(mydf$var<500000,4,5))))
bins <- c(1000,10000,100000,200000,500000,800000)

ggplot(mydf,aes(x=var))+
  geom_histogram(data=subset(mydf,group==1),breaks=c(1000,10000),fill="black")+
  geom_histogram(data=subset(mydf,group==2),breaks=c(10000,100000),fill="yellow")+
  geom_histogram(data=subset(mydf,group==3),breaks=c(100000,200000),fill="green")+
  geom_histogram(data=subset(mydf,group==4),breaks=c(200000,500000),fill="blue")+
  geom_histogram(data=subset(mydf,group==5),breaks=c(500000,800000),fill="red")+
  scale_x_continuous(breaks = bins,limits=c(1000,800000))+
  xlab("variable 1")+
  ylab("count")+
  ggtitle("Histogram with different size of bin width and colors")+
  theme(plot.title=element_text(hjust=0.5,size=14))
