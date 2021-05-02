A <- matrix(
  c(1:12), # the data elements
  nrow = 3, # number of rows
  ncol = 4, # number of columns
  byrow = TRUE # fill matrix by rows
)
A

dim(A)

# making a matrix A
A <- matrix(
  c(2,1,0,4,
    4,3,-1,12,
    0,-3,3,-12),
  nrow = 3,
  ncol = 4,
  byrow = TRUE
)
A

# rank() : sequence, order, <= be cautions. it's not rank of Matrix A
rank(A)

#rnakMatrix() : rank of matrix A, <= this is what we are looking for

library(Matrix)
rankMatrix(A)

# making square matrix A
A <- matrix(c(4,3,2,5), nc = 2, byrow = FALSE)
A

# eigenvalue & eigenvector of matrix A
lambda_A <- eigen(A)
lambda_A

# indexing of eigenvalue 1
lambda_A$values[[1]]

# indexing of eigenvector 1
lambda_A$vectors[,1]

# indexing of eigenvalue 2
lambda_A$values[[2]]

#indexing of eigenvector 2
lambda_A$vectors[,2]
