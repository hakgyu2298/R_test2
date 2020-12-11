x <- c(1,2,3,4,5)
y <- c("Kim","Lee","Lee","Choi","Park")
z <- c(TRUE,TRUE,FALSE,FALSE,TRUE)

x <- c(1,2,3,4,5)
X <- c(1,0,1,0,1)
x
X
mean(x)
mean(X)
Mean(x)

3 <- c(1,2,3)
3x <- <- c(1,2,3)
x3 <- c(1,2,3)
x3
한글 <- c("김씨","이씨","이씨","최씨","박씨")
한글

x <- c(1,2,3,4,5)
x2 <- c(x,6:10)
x2

z <- c(TRUE,TRUE,FALSE,FALSE,TRUE)
x_plus_z <- c(x+z)
x_plus_z

x2_plus_z <- c(x2+z)
x2_plus_z

x3 <- c(1,"Kim",TRUE)
x3

s1 <- c(1)
s2 <- c("Kim")

v1 <- c(1,2,3)
v2 <- c("Kim","Lee","Choi")
v3 <- c(TRUE,TRUE,FALSE)

f1 <- c("Middle","Low","High")
f1

f2 <- factor(f1)
f2

f3 <- factor(f2,order=TRUE,level=c("Low","Middel","High"))
f3

m1 <- matrix(1:12,nrow=4)
m1
m2 <- matrix(1:12,nrow=4,byrow=TRUE)
m2

a1 <- array(1:24,c(2,3,4))
a1

a2 <- array(1:23,c(3,4,2))
a2

d1 <- c(1,2,3,4)
d2 <- c("Kim","Lee","Choi","Park")
d3 <- data.frame(cust_id=d1,last_name=d2)
d3

L1 <- c(1,2,3,4)
L2 <- matrix(1:6,3,byrow = TRUE)
L3 <- array(1:24,c(3,4,2))
L4 <- data.frame(cust_id=c(1,2,3,4),last_name=c("Kim","Lee","Choi","Park"))
L5 <- list(L1,L2,L3,L4)
L5



x <- c(1,2,3,4,5,6,7,8,9,10)
x[1]
x[3]
x[c(3:7)]
x[c(3,5,7)]
x[c(TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)]

m1 <- matrix(1:12,nrow=4)
m1
m1[4,2]
m1[,2]
m1[c(3,4),2]

a1 <- array(1:24,c(2,3,4))
a1
a1[2,3,4]
a1[c(1,2),3,4]
a1[,3,4]

d1 <- c(1,2,3,4)
d2 <- c("Kim","Lee","Choi","Park")
d3 <- data.frame(cust_id=d1,last_name=d2)
d3
d3[1,] #데이터 프레임으로 출력됨
d3[,2]
d3[1,2]
d3[1] #데이터 프레임으로 출력됨
d3["cust_id"] #데이터 프레임으로 출력됨
d3[[1]]
d3$cust_id
d3[c(3,4),2]


L1 <- c(1,2,3,4)
L2 <- matrix(1:6,3,byrow = TRUE)
L3 <- array(1:24,c(3,4,2))
L4 <- data.frame(cust_id=c(1,2,3,4),last_name=c("Kim","Lee","Choi","Park"))
L5 <- list(L1,L2,L3,L4)
L5
L5[[2]]
L5[[2]][3,1]
