# a X b = |a|*|b|*sin(theta)*n

cross_prod_fun_v3 <- function(a,b){
  if(length(a) !=3 | length(b) !=3) stop('number of vector component is not 3')
  c_x <- a[2]*b[3]-a[3]*b[2]
  c_y <- a[3]*b[1]-a[1]*b[3]
  c_z <- a[1]*b[2]-a[2]*b[1]
  cross_prd <- c(c_x,c_y,c_z)
  return(cross_prd)
}

a <- c(2,3,4)
b <- c(5,6,7)

cross_prod_fun_v3(a=a,b=b)

library(MASS)
attach(Cars93)

plot(MPG.highway~Weight,type="p",pch=19,col="black")

abline(lm(MPG.highway~Weight))

text(Weight,MPG.highway,labels=abbreviate(Manufacturer,minlength=5),
     cex=0.6,pos=2,col="blue")
detach(Cars93)

hist(Cars93$MPG.highway,main="histogram : hist()")
boxplot(Cars93$MPG.highway,main="box-and-whisker plot : boxplot()")
stem(Cars93$MPG.highway)

table_cyl <- table(Cars93$Cylinders)
barplot(table_cyl,main="bar plot : barplot()")

table_cyl <- table(Cars93$cylinders)
Cylinders <- names(table_cyl)
dotchart(as.numeric(table_cyl),labels=Cylinders,main="cleveland dot plot")

table_cyl <- table(Cars93$Cylinders)
Cylinders <- names(table_cyl)
pie(table_cyl,labels=Cylinders,main="pie chart")

with(Cars93,plot(Weight,MPG.highway,main="scatter plot : plot(x,y)"))


Cars93_subset <- Cars93[,c("Weight","Horsepower","MPG.highway","MPG.city")]
plot(Cars93_subset,main="scatter plot matrix : plot(dataframe)")

library(car)
scatterplotMatrix(Cars93_subset,main="scatter plot matrix : scatterplotMatrx(dataframe)")

Cars93_1 <- Cars93[order(Cars93$Weight),] #weight가 내림차순으로 정렬됨
par(mfrow=c(1,1))

attach(Cars93_1)

plot(MPG.highway~Weight,type="l",main="type = l")
plot(MPG.highway~Weight,type="h",main="type = h")
plot(MPG.highway~Weight,type="b",main="type = b")
plot(MPG.highway~Weight,type="o",main="type = o")
plot(MPG.highway~Weight,type="s",main="type = s")
plot(MPG.highway~Weight,type="n",main="type = n")

detach(Cars93_1)

