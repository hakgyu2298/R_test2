attach(mtcars)
plot(hp,mpg)

mpg <- c(20.0,21.0,19.2,18.4,19.9)
plot(mpg,hp)
rm(mpg)
detach(mtcars)


hp <- c(120,160,110,220)
attach(mtcars)
hp
mtcars$hp
rm(hp)
hp

cust_id <- c(1,2,3)
last_nm <- c("Kim","Lee","Park")
age <- c(20,25,29)

profile <- data.frame(cust_id,last_nm,age)
profile

rm(cust_id,last_nm,age)

attach(profile)
age

profile$age <- c(30,35,39)
profile
age
detach(profile)
attach(profile)
age
detach(profile)

data_fwf <- read.fwf("C:/Users/user/Desktop/밀화부리/R/data_fwf.txt",
                     widths=c(2,3,3),
                     col.names=c("Var_1","Var_2","Var_3"))
data_fwf

rep("a",times=10)
rep(1,time=15)
rep(c("a",1),5)
rep(c("a",1),c(5,10))
rep(1:3,each=10)

x <- c(1:20)
y <- rep(1,times=20)
z <- rep(c(1,2),c(10,10))
xyz <- data.frame(cbind(x,y,z))
xyz

xyz$seq_no_1 <- rep(c(1:5),len=nrow(xyz))
xyz
seq_no_2 <- rep(c(1:5),len=nrow(xyz))
xyz <- cbind(xyz,seq_no_2)
xyz

c(1:10)
seq(from=1,to=10)
seq_len(10)
seq(from=1,to=10,by=2)
seq(1,10,length=5)
seq(from=1,by=2,length.out = 10)

abs(10)
abs(-10)
sqrt(16)
sqrt(-16)
ceiling(5.88)
ceiling(6.00)
floor(5.88)
floor(5.00)
trunc(5.88)
trunc(5.10)
round(5.88,digits=1)
round(5.88,digits=0)
round(5.10,digits=0)
log(10,base=2)
log(10,base=exp(1))
log(10)
log(10,base=10)
log10(10)
exp(log(10,base=exp(1)))
exp(1)
exp(10)
factorial(2)
factorial(3)
factorial(4)
factorial(5)

x <- c(1,2,3,4,5,6,7,8,9,10)
mean(x)
median(x)
y <- c(1,2,3,4,5,6,7,8,9)
median(y)
min(x)
min(y)

my_vec <- c(-5,3,10,3,-12,NA)
my_vec
min(my_vec)
which.min(my_vec)
my_vec[which.min(my_vec)]
max(x)
my_vec <- c(-5,3,10,3,-12,NA)
max(my_vec)
which.max(my_vec)
my_vec[which.max(my_vec)]
range(x)
range(y)
IQR(x)
IQR(y)
summary(x)
var(x)
sd(x)

install.packages("fBasics")
library(fBasics)
hist(mtcars$mpg)
skewness(mtcars$mpg)
kurtosis(mtcars$mpg)
diff(x,lag=1)
diff(x,lag=2)
diff(x,lag=3)
length(x)
length(y)
length(mtcars)
length(mtcars$mpg)

rank(x)
rank(-x)
mtcars$mpg
rank(mtcars$mpg,na.last = TRUE,ties.method = c("max"))
x <- c(1,5,8,7)
rank(x)
y <- c(1,1,1,5,9,7)
rank(y)
rank(y,ties.method = c("average"))
rank(y,ties.method = c("first"))
rank(y,ties.method = c("random"))
rank(y,ties.method = c("max"))
rank(y,ties.method = c("min"))
