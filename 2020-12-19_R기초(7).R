height_korean <- rnorm(n=1000,mean=170,sd=10)
height_bushman <- rnorm(n=1000,mean=150,sd=8)
height <- data.frame(height_korean,height_bushman)
rm(height_korean,height_bushman)
head(height)

attach(height)
par(mfrow=c(1,2))
hist(height_korean,freq = TRUE,main="한국인 키 빈도 히스토그램")
hist(height_korean,freq = FALSE,main="한국인 키 확률밀도함수 그래프")

hist(height_bushman,freq = TRUE,main="부시맨 키 빈도 히스토그램")
hist(height_bushman,freq = FALSE,main="부시맨 키 확률밀도함수 그래프")

detach(height)

height <- transform(height,
                    z.height_korean=scale(height_korean),
                    z.height_bushman=scale(height_bushman))
head(height)
height <- transform(height,
                    z2.height_korean=(height_korean-mean(height_korean))/sd(height_korean),
                    z2.height_bushman=(height_bushman)-mean(height_bushman)/sd(height_bushman))
head(height)
hist(height$z.height_korean,freq=TRUE,main="standized freq. of Korean H")
hist(height$z.height_bushman,freq=TRUE,main="standized freq. of Bushman H")

height <- height[,c(1:2)]
library(reshape)
height <- rename(height,c(height_korean="h_kor",height_bushman="h_bush"))
head(height)
height <- transform(height,
                    h_kor_01=(h_kor-min(h_kor))/(max(h_kor)-min(h_kor)),
                    h_bush_01=(h_bush-min(h_bush))/(max(h_bush)-min(h_bush)))
head(height)
hist(height$h_kor_01)
hist(height$h_bush_01)

install.packages("UsingR")
library(UsingR)

data(cfb)
head(cfb)
summary(cfb$INCOME)
par(mfrow=c(1,1))
hist(cfb$INCOME,breaks=500,freq=TRUE)

cfb <- transform(cfb,INCOME_log=log(INCOME+1))
hist(cfb$INCOME_log,breaks=500,freq = TRUE)

cfb <- transform(cfb,INCOME_sqrt=sqrt(INCOME+1))
hist(cfb$INCOME_sqrt,breaks=500,freq = TRUE)

par(mfrow=c(1,3))
qqnorm(cfb$INCOME,main="Q-Q plot of INCOME")
qqline(cfb$INCOME)

qqnorm(cfb$INCOME_log,main="Q-Q plot of INCOME_log")
qqline(cfb$INCOME_log)

qqnorm(cfb$INCOME_sqrt,main="Q-Q plot of INCOME_sqrt")
qqline(cfb$INCOME_sqrt)
par(mfrow=c(1,1))

library(MASS)
str(Cars93)
hist(Cars93$MPG.highway)
disc_1 <- Cars93[,c("Model","MPG.highway")]
head(disc_1)
disc_1 <- within(disc_1,{
  MPG.highway_cd=character(0)
  MPG.highway_cd[MPG.highway>=20 & MPG.highway<25]="20~25"
  MPG.highway_cd[MPG.highway>=25 & MPG.highway<30]="25~30"
  MPG.highway_cd[MPG.highway>=30 & MPG.highway<35]="30~35"
  MPG.highway_cd[MPG.highway>=35 & MPG.highway<40]="35~40"
  MPG.highway_cd[MPG.highway>=40 & MPG.highway<45]="40~45"
  MPG.highway_cd[MPG.highway>=45 & MPG.highway<50]="45~50"
  MPG.highway_cd=factor(MPG.highway_cd,
                        level=c("20~25","25~30","30~35",
                                "35~40","40~45","45~50"))
})
head(disc_1)
attributes(disc_1$MPG.highway_cd)
table(disc_1$MPG.highway_cd)
summary(disc_1$MPG.highway)
disc_1 <- within(disc_1,{
  MPG.highway_cd2=character(0)
  MPG.highway_cd2[MPG.highway<quantile(MPG.highway,0.25)]="1Q"
  MPG.highway_cd2[MPG.highway>=quantile(MPG.highway,0.25)
                  & MPG.highway<quantile(MPG.highway,0.5)]="2Q"
  MPG.highway_cd2[MPG.highway>=quantile(MPG.highway,0.5)
                  & MPG.highway<quantile(MPG.highway,0.75)]="3Q"
  MPG.highway_cd2[MPG.highway>=quantile(MPG.highway,0.75)]="4Q"
  MPG.highway_cd2=factor(MPG.highway_cd2,
                         level=c("1Q","2Q","3Q","4Q"))
})
head(disc_1)
tail(disc_1)
table(disc_1$MPG.highway_cd2)

disc_1 <- disc_1[order(disc_1$MPG.highway),]
dim(disc_1)
dim(disc_1)[1]
disc_1$N <- seq(1:length(disc_1$MPG.highway))
disc_1 <- within(disc_1,{
  MPG.highway_cd3=character(0)
  MPG.highway_cd3[N<=23]="1st_Freq"
  MPG.highway_cd3[N>=24 & N <=46]="2nd_Freq"
  MPG.highway_cd3[N>=47 & N <=69]="3rd_Freq"
  MPG.highway_cd3[N>=70]="4th_Freq"
  MPG.highway_cd3=factor(MPG.highway_cd3,
                         level=c("1st_Freq","2nd_Freq","3rd_Freq","4rd_Freq"))
})
head(disc_1)
table(disc_1$MPG.highway_cd3)


cust_id <- c("c01","c02","c03","c04","c05","c06","c07")
age <- c(25,45,31,30,49,53,27)
cust_profile <- data.frame(cust_id,age,stringsAsFactors = F)
cust_profile
sapply(cust_profile,class)
cust_profile <- transform(cust_profile,
                          age_20=ifelse(age>=20 & age<30,1,0),
                          age_30=ifelse(age>=30 & age<40,1,0),
                          age_40=ifelse(age>=40 & age<50,1,0),
                          age_50=ifelse(age>50 & age<50,1,0))
cust_profile
Season <- c("S1","S2","S3","S4","S1","S2","S3","S4")
SalesAmt <- c(300,800,400,100,280,750,390,60)
TS <- data.frame(Season,SalesAmt,stringsAsFactors=F)
TS
TS <- transform(TS,
                Season1=ifelse(Season=="S1",1,0),
                Season2=ifelse(Season=="S2",1,0),
                Season3=ifelse(Season=="S3",1,0))
TS

sample(1:10,5,replace=FALSE)
sample(1:10,5,replace=TRUE)
library(MASS)
dim(Cars93)

sim_ran_sam <- sample(1:nrow(Cars93),5)
Cars93_srs <- Cars93[sim_ran_sam,]
dim(Cars93_srs)

require(data.table)
require(sampling)

set.seed(1)
n <- 1000
d.t <- data.table(gender=rbinom(n,1,.5),
                  age=sample(1:5,replace=TRUE),
                  rebuy_yn=rbinom(n,1,.2))
setkey(d.t,gender,age)
d.t[,.N,keyby=list(gender,age)]

set.seed(2)
samp <- data.table(strata(d.t,c("gender","age"),rep(20,10),"srswor"))
samp[,.N,keyby=list(gender,age)]
#method to select units; the following methods are implemented: 
#simple random sampling without replacement (srswor), 
#simple random sampling with replacement (srswr), 
#Poisson sampling (poisson), systematic sampling (systematic); 
#if "method" is missing, the default method is "srswor".



