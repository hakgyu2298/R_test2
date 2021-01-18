set.seed(1004)
x <- rnorm(50,mean=0,sd=1)
df <- data.frame(matrix(x,nrow = 5,ncol=10,byrow=TRUE))
df

cor_long <- data.frame()
col_nm <- colnames(df)
n <- 1

for(i in 1:(ncol(df)-1)){
  for(j in (i+1):ncol(df)){
    cor_long[n,1] <- col_nm[i]
    cor_long[n,2] <- col_nm[j]
    cor_long[n,3] <- cor(df[,i],df[,j])
    n <- n+1
  }
}
colnames(cor_long) <- c("col_1","col_2","corr_coef")
cor_long

library(MASS)

table(Cars93$Origin)
with(Cars93,tapply(Price,Origin,summary))

boxplot(Price~Origin,
        data=Cars93,
        main="Boxplot of Price by Origin",
        xlab="Origin",
        ylab="Price")

library(ggplot2)

ggplot(Cars93,aes(x=Price))+
  geom_histogram(binwidth = 5)+
  facet_grid(Origin~.)+
  ggtitle("Histogram of Price by Origin")

var.test(Price~Origin,data=Cars93)

t.test(Price~Origin,
       data=Cars93,
       alternative=c("two.sided"),
       var.equal=FALSE, #var.test에서 두 분포의 분산이 다르다는 대립가설을 채택하였으므로
       conf.level=0.95
       )
Price_USA <- Cars93[which(Cars93$Origin ==c("USA")),c("Price")]
Price_nonUSA <- Cars93[which(Cars93$Origin ==c("non-USA")),c("Price")]

t.test(Price_USA,Price_nonUSA,
       alternative = c("two.sided"),
       var.equal = FALSE,
       conf.level = 0.95)

x1 <- c(51.4,52.0,45.5,54.5,52.3,50.9,52.7,50.3,53.8,53.1)
x2 <- c(50.1,51.5,45.9,53.1,51.8,50.3,52.0,49.9,52.5,53.0)

diff_x <- x1-x2
diff_x

mean_diff_x <- mean(diff_x)
mean_diff_x

sd_diff_x <- sd(diff_x)
sd_diff_x

t_x <- mean_diff_x/(sd_diff_x/sqrt(length(diff_x)))
t_x

t.test(x1,x2,
       alternative=c("greater"),
       paired = TRUE,
       conf.level = 0.95)

str(shoes)
diff_shoes <- shoes$A-shoes$B
diff_shoes

mean_diff_shoes <- mean(diff_shoes)
mean_diff_shoes

sd_diff_shoes <- sd(diff_shoes)
sd_diff_shoes

t_shoes <- mean_diff_shoes/(sd_diff_shoes/sqrt(length(diff_shoes)))
t_shoes

t.test(shoes$A,shoes$B,
       alternative=c("two.sided"),
       paired = TRUE,
       conf.level=0.95)
