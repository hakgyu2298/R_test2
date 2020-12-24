library(MASS)
str(Cars93)

Cars93_MPG <- Cars93[,c("MPG.highway","EngineSize","Weight","Length")]
cor(Cars93_MPG)

plot(Cars93_MPG,
     main="Scatter Plot Matrix")

library(ggplot2)
ggplot(data=Cars93,aes(x=EngineSize,y=MPG.highway))+
  geom_point(shape=15,size=3,colour="blue")+ #shape 15: solid square
  ggtitle("Scatter Plot: MPG.highway vs. EngineSize")

ggplot(data=Cars93,aes(x=Weight,y=MPG.highway))+
  geom_point(shape=19,size=3,colour="red")+ #shape 19: solid circle
  ggtitle("Scatter Plot: MPG.highway vs. Weight")

ggplot(data=Cars93,aes(x=Length,y=MPG.highway))+
  geom_point(shape=24,size=3,colour="black")+ #shape 24: filled triangel point-up
  ggtitle("Scatter Plot: MPG.highway vs. Length")

help(pch)

ggplot(data=Cars93,aes(x=Weight,y=MPG.highway))+
  geom_point(shape=19,size=3,colour="red")+ #shpae 19: solid circle
  ggtitle("Scatter Plot: MPG.highway vs. Weight with Model Label")+
  geom_text(aes(label=Model,size=2,vjust=-1,hjust=0)) #vjust=-1,hujst=1

ggplot(data = Cars93,aes(x = Weight,y =  MPG.highway, colour = Type))+
  geom_point(shape=19, size=3)+
  ggtitle("Scatter Plot by Type, using different Colours")

ggplot(data=Cars93,aes(x = Weight, y = MPG.highway, shape= Type))+
  geom_point(size = 3)+
  ggtitle("Scatter Plot by Type, different Shapes")

ggplot(data=Cars93,aes(x=Weight,y=MPG.highway))+
  geom_point(size=3,shape=19)+
  facet_grid(Type~.)+
  ggtitle("Scatter Plot by Type, using facet_grid")

ggplot(data=Cars93,aes(x=Weight,y=MPG.highway))+
  geom_point(shape=19,size=3,colour="red")+ #shape 19 solid circle
  stat_smooth(method=lm,level=0.95)+
  ggtitle("Scatter Plot: Linear Regression Line with Confidence Level 95%")

ggplot(data=Cars93,aes(x=Weight,y=MPG.highway))+
  geom_point(shape=19,size=3,colour="red")+ #shape 19 solid circle
  stat_smooth(method=lm,se=FALSE)+
  ggtitle("Scatter Plot: Linear Regression Line without Confidence Level")

ggplot(data=Cars93, aes(x=Weight,y=MPG.highway))+
  geom_point(shape=19,size=3,colour="red")+
  stat_smooth(method=loess,level=0.95)+
  ggtitle("Scatter Plot: loess Line with Confidence Level 95%")

str(airquality)
head(airquality)

sum(is.na(airquality$Ozone))
sum(is.na(airquality$Solar.R))
sum(is.na(airquality$Wind))
sum(is.na(airquality$Temp))

airquality <- transform(airquality,
                        Month.ch_temp=as.character(Month),
                        Day.ch_temp=as.character(Day))
airquality <- transform(airquality,
                        Month.ch=paste("0",Month.ch_temp,sep=""),
                        Day.ch = ifelse(Day.ch_temp == "1","01",
                                 ifelse(Day.ch_temp=="2","02",
                                 ifelse(Day.ch_temp=="3","03",
                                 ifelse(Day.ch_temp=="4","04",
                                 ifelse(Day.ch_temp=="5","05",
                                 ifelse(Day.ch_temp=="6","06",
                                 ifelse(Day.ch_temp=="7","07",
                                 ifelse(Day.ch_temp=="8","08",
                                 ifelse(Day.ch_temp=="9","09",
                                        Day))))))))))
airquality <- transform(airquality,
                        Time = paste(Month.ch, Day.ch, sep=""))                                 
airquality_May <- airquality[c(1:31),c(1:6,11)]
head(airquality_May)

ggplot(airquality_May,aes(x=Time,y=Wind,group=1))+
  geom_line()+
  ggtitle("Line Graph, Wind from May.01 to May.31")

ggplot(airquality_May,aes(x=Time,y=Temp,group=1))+
  geom_line()+
  ylim(0,max(airquality_May$Temp))+
  ggtitle("Time Series Graph, Temp from May.01 to May.31, y axis from 0 to max")

ggplot(airquality_May,aes(x=Time,y=Temp,group=1))+
  geom_line(linetype="dotted",size=1,colour="blue")+
  geom_point(size=3,shape=19,colour="blue")+
  ggtitle("Time Series Graph, Temp from May.01 to May.31, y axis from 0 to max")

ggplot(airquality,aes(x=Day,y=Temp,colour=Month,group=Month))+
  geom_line()+
  geom_point(size=3)+
  ggtitle("Time Series Graph, Temp from May to Sep.")

