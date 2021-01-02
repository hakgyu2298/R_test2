library(MASS)
str(Cars93)
library(ggplot2)

ggplot(Cars93,aes(x=Weight,y=MPG.highway))+
  geom_point(shape=21,size=6)

ggplot(Cars93,aes(x=Weight,y=MPG.highway))+
  geom_point(shape=21,size=6,colour="blue")

ggplot(Cars93,aes(x=Weight,y=MPG.highway))+
  geom_point(shape=21,size=6,fill="blue")

ggplot(Cars93,aes(x=Weight,y=MPG.highway,fill=Price))+
  geom_point(colour="grey",shape=21,size=6)

ggplot(Cars93,aes(x=Weight,y=MPG.highway,fill=Cylinders))+
  geom_point(colour="grey",shape=21,size=6)

ggplot(Cars93,aes(x=Weight,y=MPG.highway,fill=Cylinders))+
  geom_point(colour="grey",shape=21,size=6)+
  scale_fill_brewer(palette="Oranges")
#palette 설정은 범주형 데이터에만 해당
ggplot(Cars93,aes(x=Weight,y=MPG.highway,fill=Cylinders))+
  geom_point(colour="grey",shape=21,size=6)+
  scale_fill_brewer(palette="Reds")

ggplot(Cars93,aes(x=Weight,y=MPG.highway,fill=Cylinders))+
  geom_point(colour="grey",shape=21,size=6)+
  scale_fill_brewer(palette="Blues")

str(iris)

a1 <- ggplot(iris,aes(x=Petal.Width,y=Petal.Length,fill=Species))+
  geom_point(colour="grey",shape=21,size=6)+
  scale_fill_brewer(palette="Reds")
a1

a2 <- a1+
  annotate("text",x=0.25,y=2.4,label="Setosa",size=7)+
  annotate("text",x=1.3,y=3.3,label="Versicolor",size=7)+
  annotate("text",x=1.7,y=6.8,label="Virginica",size=7)
a2

a3 <- a2+
  geom_hline(yintercept = 2.6,colour="grey",lty="dashed",size=1)+ #horizontal line
  geom_hline(yintercept = 4.9,colour="grey",lty="dashed",size=1)+
  
  geom_vline(xintercept=0.8,colour="grey",lty="dashed",size=1)+ #vertical line
  geom_vline(xintercept=1.75,colour="grey",lty="dashed",size=1)+
  
  geom_abline(intercept = 8,slope=-2.1,colour="red",lty="dotted",size=1.5) #abline
a3

library(grid)
a4 <- a3+
  annotate("segment",x=2,xend=2.1,y=2,yend=3.5,size=1.5,colour="red",
           arrow=arrow())
a4

a5 <- a4+
  annotate("text",x=2,y=1.8,size=6,colour="red",label="y=8-2.1x")
a5

a6 <- a5+
  annotate("rect",xmin=0,xmax=0.8,ymin=0,ymax=2.6,alpha=0.1,fill="red")+
  annotate("rect",xmin=0.8,xmax=1.75,ymin=2.6,ymax=4.9,alpha=0.2,fill="red")+
  annotate("rect",xmin=1.3,xmax=2.7,ymin=4.3,ymax=7.2,alpha=0.3,fill="red")
a6  

a7 <- a6+
  ggtitle("Annotation of Text, Line, Arrow, Shadowed Box, Title")
a7

for (x in 1:10){
  y=10+5*x
  print(y)
}

y <- 0
for(i in 1:10){
  y=y+i
  cat("cummulative summation from 0 to ",i," is ",y,"\n",sep="")
}

z <- 0
i <- 1
while(i<=10){
  z=z+i
  cat("cummulative summation from 0 to ",i," is ",z,"\n",sep="")
  i=i+1
}

i <- 1
while(i<=5){
  i <- i+3
}
i

j <- 1
while(TRUE){
  j <- j+3
  if(j>5) break
}
j

k <- 1
repeat{
  k <- k+3
  if(k>5) break
}
k

i <- 1
repeat{
  factorial_value <- factorial(i)
  cat("factorial(", i,") = ",factorial_value, "\n",sep="")
  if(factorial_value>1000000000000) break
  i <- i+1
}

i <- 1
repeat{
  factorial_value <- factorial(i)
  cat("factorial(", i,")=",factorial_value,"\n",sep="")
  if(factorial_value>1e+100) break
  i <- i+1
}
