library(MASS)
str(Cars93)
library(ggplot2)

ggplot(Cars93,aes(x=1,y=Price))+#x=1(임의의 값)
  geom_boxplot(width=0.8,outlier.size=3,outlier.shape=16,outlier.colour="red")+
  scale_x_continuous(breaks=NULL)+#x축 이름 생력
  theme(axis.title.x=element_blank())+#x축 구분자 생략
  ggtitle("Box Plot")

ggplot(Cars93,aes(x=Type,y=Price))+
  geom_boxplot(width=0.8,outlier.size = 3,outlier.shape = 16,outlier.colour = "red")+
  ggtitle("Box Plot by Car Type")

ggplot(Cars93,aes(x=Type,y=Price))+
  geom_boxplot(width=0.8,outlier.size = 3,outlier.shape = 16,outlier.colour = "red")+
  stat_summary(fun = "mean",geom="point",shape=21,size=3,fill="blue")+
  ggtitle("Box Plot by Car Type, adding mean")

ggplot(Cars93,aes(x=Type,y=Price,fill=Origin))+
  geom_boxplot(width=0.8,outlier.size = 3,outlier.shape = 16,outlier.colour="red")+
  ggtitle("BoxPlot by Car Type and Origin")

ggplot(Cars93,aes(x=Type,y=Price))+
  geom_violin()+
  geom_boxplot(width=0.1,fill="white",outlier.colour=NA)+
  stat_summary(fun="median",geom = "point",shape=21,size=2,fill="black")+
  ggtitle("Violin Plot by Car Type with Box Plot")

ggplot(Cars93,aes(x=Type))+
  geom_bar(fill="white",colour="black")+
  ggtitle("Bar Chart of Frequency by Car Type")

library(sqldf)

Car_Type_cnt <- sqldf("select Type, count(*) as Type_cnt
                      from Cars93
                      group by Type
                      order by Type")
Car_Type_cnt
sapply(Car_Type_cnt,class)

ggplot(Car_Type_cnt,aes(x=Type,y=Type_cnt))+
  geom_bar(stat = "identity",fill="white",colour="black")+
  ggtitle("Bar Chart of Frequency by Car Type")

ggplot(Cars93,aes(x=Type,fill=Origin))+
  geom_bar(position = "dodge",colour="black")+
  scale_fill_brewer(palette=1)+
  ggtitle("Bar Chart of Frequency by Car Type & Origin")

Car_Type_Origin_cnt <- sqldf("select Type, Origin, count(*) as Type_Origin_cnt
                             from Cars93
                             group by Type,Origin
                             order by Type,Origin")
Car_Type_Origin_cnt

ggplot(Car_Type_Origin_cnt,aes(x=Type,y = Type_Origin_cnt,fill=Origin))+
  geom_bar(stat="identity",position="dodge",colour="black")+
  scale_fill_brewer(palette=1)+
  ggtitle("Bar Chart of Frequency by Car Type & Origin_1")

ggplot(Car_Type_Origin_cnt,aes(x=Type,y = Type_Origin_cnt,fill=Origin))+
  geom_bar(stat="identity",colour="black")+ #position="dodge" 미지정
  scale_fill_brewer(palette=1)+
  ggtitle("Bar Chart of Frequency by Car Type & Origin_1")


df <- read.table('C:/Users/user/Desktop/밀화부리/R/parsed.txt',sep=',',header=T)
df <- transform(df,bin_val=bin_end-bin_start)
df
ggplot(df,aes(x=id,y=bin_val,fill=color,group=id))+
  geom_bar(stat="identity")+
  scale_fill_manual("legend",values=c("red"="red","blue"="blue"))

Car_Type_Origin_cnt <- sqldf(("SELECT Type, Origin, count(*) AS Type_Origin_cnt
                              FROM Cars93
                              GROUP BY Type,Origin
                              ORDER BY Type,Origin"))
Car_Type_Origin_cnt
sapply(Car_Type_Origin_cnt,class)

ggplot(data = Car_Type_Origin_cnt,aes(x="",y=Type_Origin_cnt,fill=Type))+
  facet_grid(facets = .~Origin)+
  geom_bar(stat="identity",width=1)+
  ggtitle("Bar Chart of Frequency by Car Type & Origin")

ggplot(data = Car_Type_Origin_cnt,aes(x="",y=Type_Origin_cnt,fill=Type))+
  facet_grid(facets = .~Origin)+
  geom_bar(stat="identity",width=1)+
  coord_polar(theta="y")+
  ggtitle("Bar Chart of Frequency by Car Type & Origin")

ggplot(data = Car_Type_Origin_cnt,aes(x=Type,y=Type_Origin_cnt))+
  facet_grid(facets = .~Origin)+
  geom_bar(stat="identity",width=1,fill="white",colour="black")+
  ggtitle("Bar Chart of Frequency by Car Type & Origin")

library(vcd)
table_1 <- with(Cars93,table(Type,Origin))
table_1
mosaic(table_1,
       gp=gpar(fill=c("yellow","blue")),
       direction = "v",#세로
       main="Mosaic Chart by Car Type and Origin, using vcd package")

mosaic(table_1,
       gp=gpar(fill=c("yellow","blue")),
       direction = "h",#세로
       main="Mosaic Chart by Car Type and Origin, using vcd package")

levels(Cars93$Type)
levels(Cars93$Origin)
levels(Cars93$DriveTrain)
table_2 <- with(Cars93,table(Type,Origin,DriveTrain))
table_2

mosaic(table_2,
       gp=gpar(fill=c("yellow","blue","red")),
       direction="v",
       main = "Mosaic Chart by Car Type, Origin and DeriveTrain, direction=v")

table_1 <- with(Cars93,table(Type,Origin))
proportions <- round(prop.table(table_1)*100,1)
proportions
values <- c(table_1)
rowvarcat <- c("USA","non_USA")
columnvarcat <- c("Compact","Large","Midsize","Small","Sporty","Van")
names=c("Origin","Type")
dims <- c(2,6)

TABS <- structure(c(values),
                  .Dim=as.integer(dims),
                  .Dimnames=structure(list(rowvarcat,columnvarcat),
                  .Names=c(names)),class="table")
PROPORTIONS <- structure(c(proportions),
                  .Dim=as.integer(dims),
                  .Dimnames=structure(list(rowvarcat,columnvarcat),
                                      .Names=c(names)),class="table")
TABSPROPORTIONS <- structure(c(paste(proportions,"%","\n","(",values,")",sep="")),
                  .Dim=as.integer(dims),
                  .Dimnames=structure(list(rowvarcat,columnvarcat),
                                      .Names=c(names)),class="table")
mosaic(TABS,
       pop=FALSE,
       main="Mosaic Chart by Car Type and Origin, with Percentage Labels")
labeling_cells(text=TABSPROPORTIONS,clip_cells = FALSE)(TABS)
