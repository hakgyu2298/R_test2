plot(dpois(x=c(0,1,2,3,4,5,6,7,8,9,10),lambda = 3),
     type='h',
     main="Poisson distribution, lamda=3")

dpois(x=15,lambda = 20)
ppois(q=15,lambda = 20,lower.tail=TRUE)

sum(dpois(x=c(0:15),lambda=20))
qpois(p=0.1565131,lambda = 20,lower.tail=TRUE)

rpois(n=1000,lambda = 20)
table(rpois(n=1000,lambda = 20))

plot(table(rpois(n=1000,lambda = 20)))

x <- seq(-3,3,length=200)

plot(x,dnorm(x,mean=0,sd=1),type='l',main="Normal distribution, X~N(0,1)")
plot(x,pnorm(x,mean=0,sd=1),type='l',main="Cumulative normal distribution, X~N(0,1)")

pnorm(q=c(1),mean=0,sd=1)
pnorm(q=c(-1),mean=0,sd=1)
pnorm(q=c(1),mean=0,sd=1)-pnorm(q=c(-1),mean=0,sd=1)

pnorm(q=c(2),mean=0,sd=1)
pnorm(q=c(-2),mean=0,sd=1)
pnorm(q=c(2),mean=0,sd=1)-pnorm(q=c(-2),mean=0,sd=1)

pnorm(q=c(1),mean=0,lower.tail = TRUE)
pnorm(q=c(1),mean=0,lower.tail = FALSE)

pnorm(q=c(1),mean=0,sd=1)
qnorm(p=0.8413447,mean=0,sd=1,lower.tail = TRUE)
qnorm(pnorm(1))

random_norm_100 <- rnorm(100,mean=0,sd=1)
random_norm_100

hist(random_norm_100)

library(MASS)
library(ggplot2)

ggplot(Cars93,aes(x=Weight,y=MPG.highway))+
  geom_point(shape=19,size=3,colour="blue")+
  ggtitle("Scatter Plot of Weight & MPG.highway")

ggsave(file="C:/Users/user/Desktop/밀화부리/R/scatter_plot.jpg",
       width=20,height=15,units=c("cm"))

fit_1 <- lm(MPG.highway~Weight,Cars93)
summary(fit_1)

fit_2 <- lm(MPG.highway~Weight+EngineSize+Horsepower+Length+Width,Cars93)
fit_3 <- stepAIC(fit_2,direction="both")

summary(fit_3)

capture.output(summary(fit_1),
    file="C:/Users/user/Desktop/밀화부리/R/lm_MPG.highway.txt",
    append=TRUE)
capture.output(summary(fit_3),
    file="C:/Users/user/Desktop/밀화부리/R/lm_MPG.highway.txt",
    append=TRUE)

ggplot(data.frame(x=c(-2,20)),aes(x=x))+
  stat_function(fun=dunif,args = list(min=0,max=10),colour="black",size=1)+
  ggtitle("Uniform Distribution of (min=1,max=10)")

ggplot(data.frame(x=c(-2,20)),aes(x=x))+
  stat_function(fun=punif,args=list(min=0,max=10),colour="black",size=1)+
  ggtitle("Cumulative Uniform Distribution of (min=0,max=10)")
  
punif(3,min=0,max=10,lower.tail = TRUE)

ggplot(data.frame(x=c(-2,20)),aes(x=x))+
  stat_function(fun=dunif,args=list(min=0,max=10),colour="black",size=1)+
  annotate("rect",xmin=0,xmax=3,ymin=0,ymax=0.1,alpha=0.2,fill="red")+
  ggtitle("Uniform Distribution of (min=1, max=10), x from 0 to 3")

qunif(0.3,min=0,max=10,lower.tail=TRUE)

ru_100 <- runif(n=100,min=0,max=10)
ru_100

hist(ru_100,freq = F,breaks = 10,col="yellow")
abline(h=0.1,lty=3,lwd=3,col="red")
