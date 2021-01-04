library(MASS)

stat_function <- function(x){
  x_mean = mean(x)
  x_sd = sd(x)
  x_min = min(x)
  x_max = max(x)
  x_summary = list(x_min=x_min,
                   x_sd=x_sd,
                   x_min=x_min,
                   x_max=x_max)
  return(x_summary)
}
stat_function(x=Cars93$MPG.highway)

summary(Cars93$MPG.highway)

plot_function <- function(dataset,x,y,title){
  attach(dataset)
  plot(y~x,dataset,type="p",
       main=title)
  detach(dataset)
}

plot_function(dataset = Cars93,x=MPG.highway,y=Weight,
              title="Scatter Plot of MPG.highway & Weight")
plot_function(dataset = Cars93,x=Price,y=Horsepower,
              title = "Scatter Plot of Price & Horsepower")

library(manipulate)

manipulate(
  hist(Cars93$Price,breaks=bin_slider),
  bin_slider=slider(3,100,step=5,initial=20)
)

manipulate(
  hist(Cars93[, continuous_variable],
       freq = FALSE,main=continuous_variable),
  continuous_variable=picker("MPG.highway","Weight","Price")
)

manipulate(
  plot(MPG.highway ~ Weight, data=Cars93[Cars93$Type == Type,]),
  
  Type=picker("Compact","Large","Midsize","Small","Sporty","Van")
)

manipulate(
  boxplot(Price~Type,data=Cars93,outline=outline),
  outline=checkbox(FALSE,"show outliers")
)

manipulate(
  hist(Cars93[,continuous_variable],
       breaks=bin_slider,
       freq=F,main=continuous_variable),
  continuous_variable=picker("MPG.highway","Weight","Price"),
  bin_slider=slider(5,50,step=5,initial=10)
)

y <- dbinom(0:20,size=20,prob=0.5)
plot(0:20,y,type='h',lwd=5,col="grey",ylab="Probability",xlab="확률변수 X",
     main=c("X~B(20,0.5)"))
#P(X=12)확률 계산
dbinom(12,size=20,prob=0.5)
#P(X<=12)확률계산
pbinom(12,size=20,prob=0.5,lower.tail=TRUE)
#P(x>12)확률 계산
pbinom(12,size=20,prob=0.5,lower.tail=FALSE)
1-pbinom(12,size=20,prob=0.5,lower.tail=TRUE)

rbinom(12,size = 20,prob=0.5)
rbinom(12,size = 20,prob=0.5)
rbinom(12,size = 20,prob=0.5)

plot(pbinom(0:20,size=20,prob=0.5),type='h')

plot(dhyper(x=c(0:20),m=5,n=20,k=5),
     type='h',
     main="Hypergeometric distribution, with m=5,n=20,k=5")

#x= 원하는 갯수,m=하나의 그룹,n=다른 그룹,k=뽑은 수
dhyper(x=4,m=5,n=20,k=5)

#P(x<4)확률 값 계산
phyper(q=4,m=5,n=20,k=5,lower.tail=TRUE)
sum(dhyper(x=c(0:4),m=5,n=20,k=5))

#특정 활률에 해당하는 분위수 구하기
dhyper(x=3,m=5,n=20,k=5)
qhyper(p=0.03576134,m=5,n=20,k=5,lower.tail = F)

phyper(q=3,m=5,n=20,k=5,lower.tail = T)
qhyper(p=0.998099,m=5,n=20,k=5,lower.tail = T)

random_hyper <- rhyper(1000,m=5,n=20,k=5)
table(random_hyper)
