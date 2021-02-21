a <- c(4,5)
b <- c(3,1)

a
b

a+b
a-b

a <- c(2,4)
scala_1 <- c(2)
scala_2 <- c(1/2)

a*scala_1
a*scala_2

a <- c(-5,6)
b <- c(3,9)

a %*%b

dot_prod_fun_v2 <- function(a,b){
  if(length(a)!=2 |length(b) !=2) stop('number of vector component is not 2')
  d <- a[1]*b[1]+a[2]*b[2]
  return(d)
}

dot_prod_fun_v2(a=a,b=b)

c <- c(5,3,6)
d <- c(2,7,4)

c %*% d

dot_prod_fun_v3 <- function(c,d){
  if(length(c) !=3 | length(d) !=3) stop('number of vector component is not 3')
  dot_prd <- c[1]*d[1]+c[2]*d[2]+c[3]*d[3]
  return(dot_prd)
}

dot_prod_fun_v3(c=c,d=d)

a <- c(-5,6)
b <- c(3,9)

angle_theta <- function(a,b){
  dot.prod <- a%*%b
  norm.a <- norm(a,type = "2")
  norm.b <- norm(b,type = "2")
  theta_radian <- acos(dot.prod/(norm.a*norm.b))
  angle_theta <- 57.3*theta_radian
  as.numeric(angle_theta)
  
  return(angle_theta)
}
angle_theta(a,b)
