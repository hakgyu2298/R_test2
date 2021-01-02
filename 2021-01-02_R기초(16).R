x1 <- c(4)
if(x1%%2 == 0){
  y1=c("Even Number")
  print(y1)
}else{
  y1=c("Odd Number")
  print(y1)
}

x2 <- c(5)
if(x2%%2 == 0){
  y2="Even Number"
  print(y2)
}else{
  y2="Odd Number"
  print(y2)
}

x3 <- c(1,2,3,4,5)
if(x3%%2 == 0){
  y3="Even Number"
  print(y3)
}else{
  y3="Odd Number"
  print(y3)
}
#하나의 논리값에 대한 판단의 경우:if()
x <- c(1,2,3,4,5)
z <- ifelse(x%%2 == 0,"Even Number","Odd Number")
xz <- data.frame(x,z)
xz
#두개 이상의 논리값 벡터에 대한 판단인 경우:ifelse()
x <- c(-2,-1,0,1,2)
y <- ifelse(x>0,"Positive",
            ifelse(x==0,"Zero","Negative")
            )
xy <- data.frame(x,y)
xy

str(iris)
library(ggplot2)

a1 <- ggplot(iris,aes(x=Petal.Width,y=Petal.Length,fill=Species))+
  geom_point(colour="grey",shape=21,size=6)+
  scale_fill_brewer(palette = "Reds")+
  annotate('text',x=0.25,y=2.4,label="Setosa",size=7)+ #text annotation
  annotate("text",x=1.3,y=3.3,label="Versicolor",size=7)+
  annotate("text",x=1.7,y=6.8,label="Virginica",size=7)
a1

g1 <- ggplot(iris,aes(x=Petal.Width,y=Petal.Length))+
  geom_point(colour="blue",shape=19,size=4)+
  facet_grid(.~Species)
g1

g1+annotate("text",x=1,y=7,label=c("Setosa","Versicolor","Virginica"))
#분할 면마다 각각 텍스트 주석 넣기:geom_text()
iris_species_labels <- data.frame(Species=c("setosa","versicolor","virginica"),
                                  label=c("Species=Setosa","Species=Versicolor","Species=Virginica"))
g2 <- g1+
  geom_text(x=1,y=6.5,aes(label=label),data=iris_species_labels)
g2

library(sqldf)
mean_iris <- sqldf('SELECT "Species",
                   AVG("Petal.Width") AS "mean_Petal.Width",
                   AVG("Petal.Length") AS "mean_Petal.Length"
                   FROM iris
                   GROUP BY Species
                   ORDER BY Species')
mean_iris
label_mean_iris <- transform(mean_iris,
                             centroid=paste(c("centroid("),
                                              round(mean_Petal.Width,2),
                                              c(","),
                                              round(mean_Petal.Length,2),
                                              c(")"),
                                              sep=""))
label_mean_iris

g3 <- g2+
  geom_text(x=1,y=6,aes(label=centroid),data=label_mean_iris)
g3

library(reshape)
iris_melt_petal.length <- melt(data=iris,
                               id.vars=c("Species"),
                               measure.vars=c("Petal.Length"))
iris_melt_petal.length

f1 <- ggplot(iris_melt_petal.length,aes(x=Species,y=value))+
  geom_boxplot()+
  ggtitle(("Boxplot of Petal.Length"))
f1

f2 <- f1+
  scale_x_discrete(limits=c("virginica","versicolor","setosa"))+
  ggtitle("Changed Order of x axis by scale_x_discrete(limit...)")
f2

library(MASS)

a1 <- ggplot(Cars93,aes(x=Type,y=Cylinders,fill=MPG.highway))+
  geom_tile()
a1

a2 <- a1+
  scale_x_discrete(limits=c("Small","Compact","Midsize","Sporty",
                            "Large","Van"))
a2

ggplot(Cars93,aes(x=Type,y=Cylinders,fill=MPG.highway))+
  geom_raster()+
  scale_x_discrete(limits=c("Small","Compact","Midsize","Sporty","Large","Van"))

ggplot(Cars93,aes(x=Type,y=Cylinders,fill=MPG.highway))+ #filling with numeric value
  geom_tile()+
  scale_x_discrete(limits=c("Small","Compact","Midsize","Sporty","Large","Van"))+
  scale_fill_gradient(low="yellow",high="red")+
  ggtitle("Heatmap of MPG.highway by Type & Cylinders")

my.df <- data.frame(XCoord=c(1,1,2,3,3,4,4,5,5,5),
                    YCoord=c(1,4,3,1,3,1,5,2,3,5),
                    Seg=c("A","A","A","A","B","A","B","B","B","B"))

ggplot(my.df,aes(x=XCoord,y=YCoord,fill=Seg))+ #filling with categorical value
  geom_tile(colour="white")+
  scale_fill_manual(values=c("blue","red"))+
  ggtitle("heatmap with 2 categories with scale_fill_manual()")

ggplot(data.frame(x=c(-3,3)),aes(x=x))+
  stat_function(fun=dnorm,colour="blue",size=1)+
  ggtitle("Normal Distribution")

dnorm_range <- function(x){
  y <- dnorm(x)
  y[x < -1|x > 2] <-  NA # x<-1로 붙여쓰면 R에서 x에 1를 대입하시오로 인식함
  return(y)
}
Sys.setenv(LANG="en")

ggplot(data.frame(x=c(-3,3)),aes(x=x))+
  stat_function(fun=dnorm,colour="blue",size=1)+
  stat_function(fun=dnorm_range,geom="area",fill="red",alpha=0.5)+
  ggtitle("Normal Distribution of x~N(0,1) with colour from -1 to 2")

ggplot(data.frame(x=c(-3,3)),aes(x=x))+
  stat_function(fun=pnorm,colour="black",size=1.5)+
  ggtitle("Cumulative Normal Distribution of x~N(0,1)")

ggplot(data.frame(x=c(-5,5)),aes(x=x))+
  stat_function(fun=dnorm,args = list(mean=2,sd=1),
                colour="black",size=1.5)+
  geom_vline(xintercept=2,colour="grey",linetype="dashed",size=1)+#평균에 세로 직선 추가
  geom_text(x=0,y=0.3,label="x = N(2,1)")+
  ggtitle("Normal Distribution of x~N(2,1)")

ggplot(data.frame(x=c(-3,3)),aes(x=x))+
  stat_function(fun=dt,args=list(df=2),colour="red",size=2)+
  ggtitle("t-Distribution of df=2")

ggplot(data.frame(x=c(0,10)),aes(x=x))+
  stat_function(fun=dchisq,args=list(df=1),colour="black",size=1.2)+
  geom_text(x=0.6,y=1,label="df=1")+
  
  stat_function(fun=dchisq,args=list(df=2),colour="blue",size=1.2)+
  geom_text(x=0.6,y=55,label="df=2")+
  
  stat_function(fun=dchisq,args=list(df=3),colour="red",size=1.2)+
  geom_text(x=0.5,y=0.05,label="df=3")+
  
  ggtitle("Chisq-Distribution")

ggplot(data.frame(x=c(0,10)),aes(x=x))+
  stat_function(fun=dexp,colour="brown",size=1.5)+
  ggtitle("Exponential Distribution")

ggplot(data.frame(x=c(0,5)),aes(x=x))+
  stat_function(fun=df,args=list(df1=5,df2=10),colour="purple",size=1)+
  ggtitle("F Distribution of (df1=5,df2=10)")

ggplot(data.frame(x=c(0,400)),aes(x=x))+
  stat_function(fun=dgamma,args=list(shape=5,rate=0.05),colour="green")+
  ggtitle("Gamma Distribution of (shape=5,rate=0.05)")

ggplot(data.frame(x=c(-2,20)),aes(x=x))+
  stat_function(fun=dunif,args=list(min=0,max=10),colour="black",size=1)+
  ggtitle("Uniform Distribution of (min=0,max=10)")

ggplot(data.frame(x=c(0,100)), aes(x=x)) +
  stat_function(fun=log10, colour="black", size=1.5) +
  geom_vline(xintercept=10, colour="grey", linetype="dashed", size=1) +
  geom_vline(xintercept=100, colour="grey", linetype="dashed", size=1) +
  ggtitle("Common Logarithm Distribution")

ggplot(data.frame(x=c(0,6.28)), aes(x=x)) +
  stat_function(fun=sin, colour="blue", size=1) +
  geom_text(x=0.2, y=0, label="sine curve") +
     
  stat_function(fun=cos, colour="yellow", size=1) + 
  geom_text(x=0.2, y=1, label="cosine curve") +
     
  geom_vline(xintercept=3.14, colour="grey", linetype="dashed", size=1) + # pi값에 세로 직선 추가  
  geom_vline(xintercept=6.28, colour="grey", linetype="dashed", size=1) + # 2pi값에 세로 직선 추가  
  ggtitle("Sine(blue curve), Cosine(yellow curve) Function")



