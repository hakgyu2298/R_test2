alpha <- 0.05
N <- 100
n <- 20
mu <- 10
sigma <- 3

#layout(matrix(1:2),heights=c(1,2))

x <- seq(0,20,length=100)
plot(x, dnorm(x, mean=10, sd=3), type='l', main="Normal distribution, X~N(10,3^2)")


abline(v=mu,col="red",lty=2)
abline(v=mu-1.96*3,col="red",lty=2)
abline(v=mu+1.96*3,col="red",lty=2)

plot(c(0,20),c(1,N),
     xlab="confidence interval",
     ylab="simulation frequency",
     type='n')

within_yn <- 0

for(i in 1:N){
  x=rnorm(n,mu,sigma)
  conf_limit_lower=mean(x)-qnorm(1-alpha/2)*sd(x)/sqrt(n)
  conf_limit_upper=mean(x)+qnorm(1-alpha/2)*sd(x)/sqrt(n)
  within_yn_eval = mu <= conf_limit_upper & mu >= conf_limit_lower
  if(within_yn_eval) within_yn = within_yn+1
  segments(conf_limit_lower, i, conf_limit_upper, i, col=(! within_yn_eval)+1,
          lty=(!within_yn_eval)+1)
  #segments(x1,y1,x2,y2) : line from (x1,y1) to (x2,y2)
}
mtext(paste("confidence coefficient(1-alpha) = ", within_yn/n), side=3, line = 0, outer= F,cex = 1.3)


x1 <- c(33:93)
plot(x1,dnorm(x1,mean=63,sd=8),type='l',
     main="Normal distribution, X~N(63,8^2), right-sided test")
abline(v=63,col="blue",lty=3)
abline(v=63+1.96*8,col="red",lty=2)
text(82,0.003,labels="---------->")

x <- c(70.2,54.9,67.0,60.5,63.4,61.9,71.8,66.1,72.6,73.0,68.7,
       70.3,66.2,55.6,65.9)
mean(x)
var(x);sd(x)

stem(x,scale=2)

t.test(x,alternative = c("greater"), #alternative(c("less","greater","two.sided"))
       mu=63.0,
       conf.level=0.95)
t.test(x,alternative = c("two.sided"), #alternative(c("less","greater","two.sided"))
       mu=63.0,
       conf.level=0.95)

t.test_confi_95 <- t.test(x,alternative = c("two.sided"), #alternative(c("less","greater","two.sided"))
                          mu=63.0,
                          conf.level=0.95)
names(t.test_confi_95)
t.test_confi_95$conf.int
t.test_confi_95$conf.int[1]
t.test_confi_95$conf.int[2]

var_test_TwoSided <- function(n,sigma0Squared,SSquared,alpha){
  df <- n-1
  v <- df*(SSquared)/sigma0Squared
  upper.critical <- qchisq((1-alpha),df,lower.tail = F) #right-sided test
 # lower.critical <- qchisq((alpha), df, lower.tail = FALSE) # left-sided test
 # upper.critical <- qchisq((1-alpha)/2, df, lower.tail = FALSE) # two-sided test
 # lower.critical <- qchisq(alpha/2, df, lower.tail = FALSE) # two-sided test
 
  print(paste("degrees of freedom = ", df), quote = FALSE)
  print(paste("population variance = ", sigma0Squared), quote = FALSE)
  print(paste("sample variance = ", SSquared), quote = FALSE) 
  print(paste("significance level = ", alpha), quote = FALSE)
  print(paste("confidence level = ", 1-alpha), quote = FALSE)
  print("  ")
  print(paste((1-alpha)*100, "% confidence interval for variance"), quote=FALSE)
  print(paste("   upper critical limit = ", round(upper.critical, 2)), quote = FALSE)
  print(paste("   chisq statistic      = ", round(v, 2)), quote = FALSE)
  print("  ")
  print(paste("P-value =", round(pchisq(v, df, lower.tail=FALSE),4)), quote = FALSE)
}

var_test_TwoSided(n=100,sigma0Squared = 30,SSquared = 38,alpha=0.05)

prop.test_confi_95<- prop.test(x=485,
          n=1000,
          p=0.5,
          alternative = c("two.sided"),
          conf.level = 0.95)
names(prop.test_confi_95)
prop.test_confi_95$conf.int

x <- c(70.2, 54.9, 67.0, 60.5, 63.4, 61.9, 71.8, 66.1, 72.6, 73.0, 68.7, 70.3, 66.2, 55.6, 65.9)
mean(x)
var(x);sd(x)
stem(x,scale=2)

wilcox.test(x,
            alternative=c("greater"),
            mu=63.0,
            conf.int=TRUE)
t.test(x,
       alternative=c("greater"),
       mu=63.0,
       conf.level=0.95)
wilcox.test_confi_95 <- wilcox.test(x,alternative = c("greater"),
                                    mu=63.0,
                                    conf.int = T)
names(wilcox.test_confi_95)

library(UsingR)
str(cfb)

shapiro.test(cfb$INCOME)
hist(cfb$INCOME,breaks=100)

hist(cfb$INCOME,freq=F,breaks=100,main="Kernel Density Plot of cfb$INCOME")
lines(density(cfb$INCOME),col="blue",lwd=3)

qqnorm(cfb$INCOME)
qqline(cfb$INCOME)
