library(MASS)
str(Cars93)

hist(Cars93$MPG.highway,freq = F,breaks=20)

hist(Cars93$Price,freq=F,breaks = 20)
lines(density(Cars93$Price),col="red",lty=3)

mean(Cars93$Price)
median(Cars93$Price)

Cars93 <- within(Cars93,{
  Price_cd <- character()
  Price_cd[Price<10]="5_10"
  Price_cd[Price>=10 & Price <15]="10_15"
  Price_cd[Price>=15 & Price <20]="15_20"
  Price_cd[Price>=20 & Price <25]="20_25"
  Price_cd[Price>=25 & Price <30]="25_30"
  Price_cd[Price>=30 & Price <35]="30_35"
  Price_cd[Price>=35 & Price <40]="35_40"
  Price_cd[Price>=40 & Price <45]="40_45"
  Price_cd[Price>=45 & Price <50]="45_50"
  Price_cd[Price>=50 & Price <55]="50_55"
  Price_cd[Price>=55 & Price <60]="55_60"
  Price_cd[Price>=60]="60_65"
  Price_cd <- factor(Price_cd,levels = c("5_10","10_15","15_20",
                                         "20_25","25_30","30_35",
                                         "40_45","45_50",
                                         "50_55","55_60","60_65"))
})
table(Cars93$Price_cd)
which.max(table(Cars93$Price_cd))

x <- c(1.01,1.05)
prod(x)
n <- length(x)
prod(x)^(1/n)

IV <- c(100)
FV <- c(350)
n <- c(5)
CAGR_rev <- (FV/IV)^(1/n)-1
CAGR_rev

#조화평균
km_per_hour <- c(30,90)
arithmetic_mean <- mean(km_per_hour)
arithmetic_mean #wrong answer

harmonic_mean <- 1/mean(1/km_per_hour)
harmonic_mean #correct answer

weighted_earning_rate_1 <- (0.7*0.15+0.2*0.09+0.1*0.05)/(0.7+0.2+0.1)
weighted_earning_rate_1

investment <- data.frame(weight=c(0.7,0.2,0.1),earning_rate=c(0.15,0.09,0.05))
weighted_earning_rate_2 <- weighted.mean(investment$earning_rate,investment$weight)
weighted_earning_rate_2

weighted_alchhol_mean_1 <- (200*0.09+1000*0.21)/(200+1000)
weighted_alchhol_mean_1

alcohol <- data.frame(volume=c(200,1000),alcohol_rate=c(0.09,0.21))
weighted_alchhol_mean_2 <- weighted.mean(alcohol$alcohol_rate,alcohol$volume)
weighted_alchhol_mean_2

library(ggplot2)
ggplot(Cars93,aes(x=Price))+
  geom_histogram(binwidth=5,fill="blue",colour="black")+
  ggtitle("Histogram, Price by Type")+
  facet_grid(Type~.)

var(Cars93$Price)
with(Cars93,tapply(Price,Type,var))

sd(Cars93$Price)
with(Cars93,tapply(Price,Type,sd))

#coefficient of variation : sd()/mean()

with(Cars93,100*sd(Price)/mean(Price))
attach(Cars93)
with(Cars93[Type == c("Compact"),],100*sd(Price)/mean(Price))
with(Cars93[Type == c("Large"),],100*sd(Price)/mean(Price))
with(Cars93[Type == c("Midsize"),],100*sd(Price)/mean(Price))
with(Cars93[Type == c("Small"),],100*sd(Price)/mean(Price))
with(Cars93[Type == c("Sporty"),],100*sd(Price)/mean(Price))
with(Cars93[Type == c("Van"),],100*sd(Price)/mean(Price))
detach(Cars93)

company_A_mean <- c(10000)
company_A_sd <- c(1000)

company_B_mean <- c(50000)
company_B_sd <- c(2000)

coe_var_A <- 100*company_A_sd/company_A_mean
coe_var_A

coe_var_B <- 100*company_B_sd/company_B_mean
coe_var_B

attach(Cars93)
min(Price)
tapply(Price,Type,min)

max(Price)
tapply(Price,Type,max)

diff(range(Price))
diff(range(Cars93[Type==c("Compact"),]$Price)) #Max-Min
diff(range(Cars93[Type==c("Large"),]$Price))
diff(range(Cars93[Type==c("Midsize"),]$Price))
diff(range(Cars93[Type==c("Small"),]$Price))
diff(range(Cars93[Type==c("Sporty"),]$Price))
diff(range(Cars93[Type==c("Van"),]$Price))

quantile(Price,c(0.25,0.75))
quantile(Cars93[Type==c("Compact"),]$Price,c(0.25,0.75))
quantile(Cars93[Type==c("Large"),]$Price,c(0.25,0.75))
quantile(Cars93[Type==c("Midsize"),]$Price,c(0.25,0.75))
quantile(Cars93[Type==c("Small"),]$Price,c(0.25,0.75))
quantile(Cars93[Type==c("Sporty"),]$Price,c(0.25,0.75))
quantile(Cars93[Type==c("Van"),]$Price,c(0.25,0.75))

IQR(Price)
IQR(Cars93[Type==c("Compact"),]$Price) #Q3-Q1
IQR(Cars93[Type==c("Large"),]$Price)
IQR(Cars93[Type==c("Midsize"),]$Price)
IQR(Cars93[Type==c("Small"),]$Price)
IQR(Cars93[Type==c("Sporty"),]$Price)
IQR(Cars93[Type==c("Van"),]$Price)

detach(Cars93)

ggplot(Cars93,aes(x=Type,y=Price))+
  geom_boxplot(width=0.8,outlier.size = 3,outlier.shape = 16,outlier.colour="red")+
  stat_summary(fun.y="mean",geom="point",shape=21,size=3,fill="blue")+
  ggtitle("Box Plot by Car Type, adding mean")

library(fBasics)

skewness(Cars93$Price)
with(Cars93,tapply(Price, Type, skewness))

kurtosis(Cars93$Price)
with(Cars93,tapply(Price, Type, kurtosis))
