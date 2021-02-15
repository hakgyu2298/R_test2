A_zero <- matrix(c(0,0,0,0,0,0,0,0,0),byrow=TRUE,nrow=3)
A_zero

B_zero <- matrix(rep(0,9),byrow=TRUE,nrow=3)
B_zero

A <- matrix(c(1,2,3,4,5,6,7,8,9),byrow = TRUE,nrow=3)
A

A_t <- t(A)
A_t

B <- matrix(c(4,5,6,7,8,9),nc=3)
B <- matrix(c(4,5,6,7,8,9),nrow =2)
B

B_t <- t(B)
B_t

s <- matrix(c(1:25),5)
s

lower.tri(s,diag=FALSE)

s[lower.tri(s,diag=FALSE)]
t(s)[lower.tri(s,diag=FALSE)]

s[lower.tri(s,diag=FALSE)]=t(s)[lower.tri(s,diag=FALSE)]
s


(m_upper <- matrix(1:20,4,4))
lower.tri(m_upper,diag=F)
m_upper[lower.tri(m_upper,diag=F)] <- c(0)
m_upper

(m_lower <- matrix(1:20,4,4))
upper.tri(m_lower,diag=F)
m_lower[upper.tri(m_lower,diag=F)] <- c(0)
m_lower

A <- c(1,2,3,4)
diag(A)

diag(4)
diag(6)

library(MASS)

raw_data <- c(1,2,3,4)
A_1 <- matrix(raw_data,byrow=TRUE,nrow=2)
A_1
ginv(A_1)
A_1%*%ginv(A_1)

solve(A_1)


M11 <- matrix(c(1,2,0,4),byrow=TRUE,nrow=2)
det(M11)

A <- matrix(c(1,0,1,0,1,2,-1,0,4),byrow=TRUE,nrow=3)
A
options(digits = 2)
ginv(A)
solve(A)
