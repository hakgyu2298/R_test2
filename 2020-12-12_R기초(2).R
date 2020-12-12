d.f <- data.frame()
d.f <- edit(d.f)

dataset_1 <- read.table("C:/Users/user/Desktop/밀화부리/R/dataset_1.txt",
                        header=TRUE,
                        sep=",",
                        stringsAsFactors=FALSE,
                        na.strings="")
dataset_1

write.table(dataset_1,"C:/Users/user/Desktop/밀화부리/R/cust_profile.txt",
            sep=",",
            row.names=FALSE,
            quote=FALSE,
            append=TRUE,
            na="NA")

x <- c(1:10)
mean_x <- mean(x)
sd_x <- sd(x)
z_x <- ((x-mean_x)/sd_x)

cat("Data is as follows:,\n",
    x,"\n",
    file="C:/Users/user/Desktop/밀화부리/R/data_x.txt",
    append=TRUE)
cat("Mean of x is",mean_x,"\n",
    file="C:/Users/user/Desktop/밀화부리/R/data_x.txt",
    append=TRUE)
cat("Standard Deviation of x is",sd_x,"\n",
    "\n",
    "\n",
    "Z score of x is",z_x,"\n",
    file="C:/Users/user/Desktop/밀화부리/R/data_x.txt",
    append = TRUE)

height <- c(175,159,166,189,171,173,179,167,182,170)
weight <- c(62,55,59,75,61,64,63,65,70,60)
d.f_h_w <- data.frame(height,weight)

lm_fit_h_w <- lm(weight~height,d.f_h_w)
summary(lm_fit_h_w)

cat("Dataset is as follows;",
    "\n",
    "\n",
    file="C:/Users/user/Desktop/밀화부리/R/lm_fit_h_w.txt")
write.table(d.f_h_w,"C:/Users/user/Desktop/밀화부리/R/lm_fit_h_w.txt",
            sep=",",
            row.names=FALSE,
            quote=FALSE,
            append=TRUE)

cat("Summary of linear regression model is",
    "\n",
    "\n",
    file="C:/Users/user/Desktop/밀화부리/R/lm_fit_h_w.txt",
    append=TRUE)

capture.output(summary(lm_fit_h_w),
               file="C:/Users/user/Desktop/밀화부리/R/lm_fit_h_w.txt",
               append=TRUE)

mtcars
str(mtcars)
head(mtcars)
tail(mtcars)
dim(mtcars)
length(mtcars)
length(mtcars$mpg)
names(mtcars)
class(mtcars)
sapply(mtcars,class)

mean(mtcars$mpg)
tf_0_1 <- (max(mtcars$mpg)-mtcars$mpg)/(max(mtcars$mpg)-min(mtcars$mpg))
tf_0_1

tf_0_2 <- with(mtcars,(max(mpg)-mpg)/(max(mpg)-min(mpg)))
tf_0_2

attach(mtcars)
tf_0_1_3 <- (max(mpg)-mpg)/(max(mpg)-min(mpg))
tf_0_1_3
hist(mpg)
summary(mpg)
detach(mtcars)
