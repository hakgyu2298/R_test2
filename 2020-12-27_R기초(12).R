str(airquality)
airquality_May_July <- subset(airquality,
                              select = c(Month,Day,Wind,Temp),
                              subset = (Month %in% c(5,7)))
#airquality$Month %in% c(5,7)
head(airquality_May_July)
tail(airquality_May_July)

library(ggplot2)
#aes()에 shape나 colour에는 범주형변수만 들어가야됨
ggplot(data=airquality_May_July,aes(x=Wind,y=Temp,shape=Month))+
  geom_point()+
  stat_density2d()+
  ggtitle("2D desity plot of Wind and Temp, at May and July")

airquality_May_July <- transform(airquality_May_July,Month.ch=as.character(Month))
sapply(airquality_May_July,class)
head(airquality_May_July)

ggplot(data=airquality_May_July,aes(x=Wind,y=Temp,shape=Month.ch))+
  geom_point(size=4)+
  stat_density2d()+
  ggtitle("2D desity plot of Wind and Temp, at May/July by Shape")

ggplot(data=airquality_May_July,aes(x=Wind,y=Temp,colour=Month.ch))+
  geom_point(size=4)+
  stat_density2d()+
  ggtitle("2D desity plot of Wind and Temp, at May/July by Colour")

ggplot(data=airquality_May_July,aes(x=Wind,y=Temp,colour=Month.ch))+
  geom_point(size=4)+
  stat_density2d()+
  ggtitle("2D desity plot of Wind and Temp, at May/July by Colour")+
  annotate("text",x=11,y=65,label="1973.May",alpha=0.5)+
  annotate("text",x=9,y=83,label="1973.July",alpha=0.5)

library(MASS)
str(Cars93)
table(Cars93$Type)
Cars93_P <- subset(Cars93,
                 select=c(Model,Type,Min.Price,Max.Price),
                 subset=(Type %in% c("Large","Midsize","Small")))
str(Cars93_P)
head(Cars93_P)

ggplot(Cars93_P,aes(x=Max.Price,y=reorder(Model,Max.Price),shape=Type))+
  geom_point(size=3,colour="blue")+
  theme_bw()+ #background 색 없애기
  theme(panel.grid.major.x = element_blank(),
       panel.grid.minor = element_blank(),
       panel.grid.major.y = element_line(colour="grey90",linetype="dashed"))+
  ggtitle("Cleveland dot plot of Max.Price of Models with different shape by Type")

Model_Order <- Cars93_P$Model[order(Cars93_P$Type,
                                    -Cars93_P$Max.Price,
                                    decreasing = TRUE)]
#decreasing=TRUE하면 전부 내림차순으로 변함 그래서 Cars93_P$Max.Price에
#-를 곱하여 일단 내림차순으로 만들고 전체를 내림차순으로 만들어 
#Cars93_P$Type은 내림차순,-Cars93_P$Max.Price를 오름차순으로 만듬
Cars93_P$Model <- factor(Cars93_P$Model,levels = Model_Order)

ggplot(Cars93_P,aes(x=Max.Price,y=Model))+
  geom_point(size=3,aes(colour=Type))+
  theme_bw()+
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank())+
  facet_grid(Type~.,scales="free_y",space="free_y")+
  ggtitle("Cleveland dot plot of Max.Price of Models with Facets of Type")

library(reshape)
Cars93_P_melt <- melt(Cars93_P,idvars=c("Type","Model"))
head(Cars93_P_melt)
Cars93_P_melt <- rename(Cars93_P_melt,c(variable="Price_cd",value="Price"))
head(Cars93_P_melt)

ggplot(Cars93_P_melt,aes(x=Price,y=Model))+
  geom_segment(aes(yend=Model,xend=0))+ #점까지만 선 그리기
  #geom_segment함수는 점을 기준으로 그리기 때문에
  #시작점은 Price와 Model이므로 y값은 그대로 고정 x값만 0으로 둔다
  geom_point(size=3,aes(shape=Price_cd))+ #Price_cd로모양 구분
  theme_bw()+ # background 색 없애기
  theme(panel.grid.major.y = element_blank(), #y축 없애기
        panel.grid.minor.y = element_blank())+ #y축 없애기
  facet_grid(Type~.,scales = "free_y",space = "free_y")+ #Type별로 면 분할
  ggtitle("Cleveland dot plot of Max, Min Price of Models with Facets of Type")

trade_stat <- read.csv("C:/Users/user/Desktop/밀화부리/R/trade_stat_07_14.csv",
                       header=TRUE)
trade_stat <- transform(trade_stat,Year=substr(Time,1,4))
sapply(trade_stat,class)

library(sqldf)
trade_stat_Year <- sqldf("SELECT Year,
                         SUM(export_amt)/100000 AS exp_amt_Year,
                         SUM(import_amt)/100000 AS imp_amt_Year
                         FROM trade_stat
                         GROUP BY Year
                         ORDER BY YEAR")
trade_stat_Year
trade_stat_Year_melt <- melt(trade_stat_Year,idvars=c("Year"))
trade_stat_Year_melt
trade_stat_Year_melt <- rename(trade_stat_Year_melt,c(variable="trade_cd",value="amount_B"))
trade_stat_Year_melt

ggplot(trade_stat_Year_melt,aes(x=Year,y=amount_B,fill=trade_cd,group=trade_cd))+
  geom_area(colour=NA,alpha=0.5)+ #alpha 투명도
  scale_fill_brewer(palette="Blues")+
  geom_line(position="stack",size=0.3)+
  ggtitle("Stacked Area Plot of Trade (Import,Export) from 2007 to 2014")

library(plyr)
ggplot(trade_stat_Year_melt,aes(x=Year,y=amount_B,fill=trade_cd,group=trade_cd,
                                order=desc(trade_cd)))+
  geom_area(colour=NA,alpha=0.5)+ #alpha 투명도
  scale_fill_brewer(palette="Blues")+
  geom_line(position="stack",size=0.3)+
  ggtitle("Stacked Area Plot of Trade (Import,Export) from 2007 to 2014")

trade_stat_Year_melt_prop <- ddply(trade_stat_Year_melt,
                                   "Year",transform,
                                   trade_prop=round(100*amount_B/sum(amount_B),1))
trade_stat_Year_melt_prop

ggplot(trade_stat_Year_melt_prop,aes(x=Year,y=trade_prop,fill=trade_cd,group=trade_cd,
                                     order=desc(trade_cd)))+
  geom_area(colour=NA,alpha=0.5)+
  scale_fill_brewer(palette="Blues")+
  geom_line(position="stack",size=0.3)+
  ggtitle("Stacked Area Plot of Trade Proportion (Import, Export) from 2007 to 2014")
