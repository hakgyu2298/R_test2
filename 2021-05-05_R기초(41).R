A <- matrix(c(3, 2, 0, 0, 6, 3, 0, 0), nc=2, byrow = FALSE)
A

t(A)

##---(1) calculation of U
# A%*%t(A)
W_1 <- A%*%t(A)
W_1

# eigenvalue, eigenvector of W
eigen(W_1)

# U
U <- eigen(W_1)[[2]] # eigenvectors
U

##----(2) calculation of V^T
# t(A)#*#A
W_2 <- t(A)%*%A
W_2

# eigenvalue of W
eigen(W_2)

# V
V <- eigen(W_2)[[2]] # eigenvectors
V

##---(3) calculation of Σ
# square root of eigenvalues
W_2_eigenvalue_sqrt <- sqrt(eigen(W_2)[[1]])
W_2_eigenvalue_sqrt

S <- matrix(rep(0,8),nc=2,byrow=F) # all zeros, temp matrix
S

S[1,1] <- W_2_eigenvalue_sqrt[1]
S[2,2] <- W_2_eigenvalue_sqrt[2]
S

# overall (aggregation)

A # 4by 2 rectangular matrix

U # eigenvectors of A*t(A)
S # square root of eigenvalues of t(A)*A
V # eigenvectors of t(A)*A

SVD_of_A <- U %*% S %*% t(V)
SVD_of_A
