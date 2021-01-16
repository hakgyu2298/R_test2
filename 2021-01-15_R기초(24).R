library(MASS)

summary(Cars93$Price)
summary(Cars93[c("Price","MPG.highway")])

library(pastecs)

round(
  stat.desc(Cars93[c("Price","MPG.highway")], #dataframe[c(variable name)]
            basic=TRUE, #nbr.val, nbr.null, nbr.na, min, max, range, sum
            desc=TRUE, #median, mean, var, std.dev, coef.var
            norm=TRUE, # skewness, kurtosis, normtest.W, normtest.p
            p=0.9 # SE.mean, CI.mean.p
 ),2
)

library("psych")

describe(Cars93[c("Price","MPG.highway")],
         na.rm=TRUE, #missing value is not included
         interp=TRUE, #method of median calculation
         skew=TRUE, #skewness,kurtosis
         ranges=TRUE, #range
         trim=0.1 #trimmed mean
         )

with(Cars93,tapply(Price,Type,summary))
with(Cars93,tapply(MPG.highway,Type,summary))

fun_summary <- function(x,...){
  c(n=sum(!is.na(x)),mean=mean(x,...),sd=sd(x,...))
}
by(Cars93[c("Price","MPG.highway")],#dataset$variable
   Cars93$Type, #by Group(Factor)
   function(x) sapply(x,fun_summary,na.rm=T)) #function
#function(x)를 쓴 이유는 sapply가 x를 받아야 되서


library(stats)

fun_summary_2 <- function(x,...) {
     c(n=sum(!is.na(x)), mean=mean(x,...), sd=sd(x,...))
}
aggregate(Cars93[c("Price","MPG.highway")],#dataset$variable
          by=list(Car_Type=Cars93$Type), #by Group(Factor)
          fun_summary_2,na.rm=T)

library(doBy)
summaryBy(Price+MPG.highway~Type,data = Cars93,FUN=c(mean,sd)) 

library(psych)
describeBy(Cars93[c("Price","MPG.highway")],Cars93$Type,mat=FALSE) #list
describeBy(Cars93[c("Price","MPG.highway")],Cars93$Type,mat=TRUE) #matrix

plot(MPG.highway~Weight,data=Cars93,
     main="Scatter Plot of MPG.highway~Weight",
     xlab="Weight",ylab="MPG.highway")
abline(lm(MPG.highway~Weight,data=Cars93),col="blue",lty=2)

with(Cars93,cov(x=MPG.highway,
                y=Weight,
                use="complete.obs",
                method=c("pearson")))

x <- Cars93[c("MPG.highway","Weight")]
with(Cars93,cov(x,
                y=NULL,
                use="complete.obs",
                method=c("pearson")))

with(Cars93,cor(x=MPG.highway,
                y=Weight,
                use="complete.obs",
                method=c("pearson")))

cor(Cars93$MPG.highway,Cars93$Weight)

x <- Cars93[c("MPG.highway","Weight")]
with(Cars93,cor(x,
                y=NULL,
                use="complete.obs",
                method=c("pearson")))

a <- c(10,9,8,6,NA)
b <- c(9,6,4,3,2)
c <- c(NA,13,18,20,25)

x <- data.frame(a,b,c)
cor(x,
    y=NULL,
    use="everything",
    method=c("pearson"))
cor(x,
    y=NULL,
    use="all.obs",
    method=c("pearson"))
cor(x,
    y=NULL,
    use="complete.obs",
    method=c("pearson"))

a <- c(10,9,8,6,NA)
b <- c(9,6,4,3,2)
c <- c(NA,13,18,20,25)

a_complete.obs <- c(9,8,6)
b_complete.obs <- c(6,4,3)
c_complete.obs <- c(13,18,20)
x_complete.obs <- data.frame(a_complete.obs,b_complete.obs,c_complete.obs)

cor(x_complete.obs,
    y=NULL,
    use="all.obs",
    method=c("pearson"))

cor(x, 
    y = NULL, 
    use = "pairwise.complete.obs", 
    method = c("pearson"))

a_ab_pairwise.complete.obs <- c(10, 9, 8, 6)
b_ab_pairwise.complete.obs <- c(9, 6, 4, 3)

cor(x = a_ab_pairwise.complete.obs, 
    y = b_ab_pairwise.complete.obs, 
    use = "all.obs", # because it is calculated after deleting 5th element
    method = c("pearson"))

a_ac_pairwise.complete.obs <- c(9, 8, 6)
c_ac_pairwise.complete.obs <- c(13, 18, 20)

cor(x = a_ac_pairwise.complete.obs, 
    y = c_ac_pairwise.complete.obs, 
    use = "all.obs", 
    method = c("pearson"))

b_bc_pairwise.complete.obs <- c(6, 4, 3, 2)
c_bc_pairwise.complete.obs <- c(13, 18, 20, 25)

cor(x = b_bc_pairwise.complete.obs, 
    y = c_bc_pairwise.complete.obs, 
    use = "all.obs", 
    method = c("pearson"))













