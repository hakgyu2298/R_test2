library(MASS)
str(Cars93)

R_aggregate_mean <- aggregate(Cars93[,c(7,8)],
                              by = list(Car_Type=Cars93$Type), #list
                              FUN = mean, #function
                              na.rm = TRUE)
R_aggregate_mean

library(sqldf)

R_sqldf_1 <- sqldf('
                   SELECT "Type" AS "Car_Type",
                   AVG("MPG.city") AS "mean_MPG.city",
                   AVG("MPG.highway") AS "mean_MPG.highway"
                   FROM Cars93
                   GROUP BY Type
                   ORDER BY Type
                   ')
R_sqldf_1

Type_mean <- merge(R_aggregate_mean,R_sqldf_1, by='Car_Type')
Type_mean <- transform(Type_mean,
                       gap_MPG.city = MPG.city - mean_MPG.city,
                       gap_MPG.highway = MPG.highway - mean_MPG.highway)
Type_mean

R_sqldf_2 <- sqldf('
  SELECT "TYpe" AS "Car_Type",
  COUNT("MPG.city") AS "count_MPG.city",
  SUM("MPG.city") AS "SUM_MPG.city",
  
  AVG("MPG.city") AS "mean_MPG.city",
  VARIANCE("MPG.city") AS "variance_MPG.city",
  STDEV("MPG.city") AS "stdev_MPG.city",
  
  MIN("MPG.city") AS "min_MPG.city",
  MAX("MPG.city") AS "max_MPG.city"
  FROM Cars93
  GROUP BY Type
  ORDER BY Type DESC
')
R_sqldf_2
R_aggregate_median <- aggregate(Cars93[,c(7,8)],
                                by = list(Car_Type=Cars93$Type),
                                FUN = median)
R_aggregate_median
quantile_MPG.city <- quantile(Cars93[,c("MPG.city")],
                              c(0,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1))
quantile_MPG.city

table(Cars93$Type)
Cars93_sample <- subset(Cars93,
                           select = c(Type,Origin,MPG.city,MPG.highway),
                           subset = (Type %in% c("Compact","Van")))
Cars93_sample

library(reshape)
Cars93_sample_melt <- melt(data = Cars93_sample,
                           id.vars = c("Type","Origin"),
                           measure.vars = c("MPG.city","MPG.highway"))
Cars93_sample_melt
options(digits = 3)
cast(data = Cars93_sample_melt, Type~variable, fun =mean)
cast(data = Cars93_sample_melt, Origin~variable, fun =mean)
cast(data = Cars93_sample_melt, Type+Origin~variable, fun =mean)
cast(data = Cars93_sample_melt, Type~Origin+variable, fun =mean)
cast(data = Cars93_sample_melt, Origin~Type+variable, fun =mean)
cast(data = Cars93_sample_melt, Type+variable~Origin, fun =mean)
cast(data = Cars93_sample_melt, Origin+variable~Type, fun =mean)
# 앞 세로~뒤 가로

library(gcookbook)
data(cabbage_exp)
str(cabbage_exp)
library(reshape2)
cabbage_exp_cast <- acast(data=cabbage_exp,
                          Cultivar~Date,
                          value.var = "Weight",
                          fun.aggregate = mean,
                          #fill = 0 #if you want to fill the missing value with'0'
                          drop = TRUE)
cabbage_exp_cast

str(airquality)
sum(is.na(airquality$Temp))
May <- subset(airquality,
              select = c(Month,Day,Temp),
              subset = (Month == 5))
May_Temp_Diff<- diff(May$Temp,lag=1) # 차분=후자의 값-전자의 값
May_Temp_Diff

May_Day <- May[c(2:31),c("Day")]
May_Temp_Diff.df <- data.frame(May_Day,May_Temp_Diff)
May_Temp_Diff.df

attach(May_Temp_Diff.df)
May_Temp_Diff.df$plus_minus <- ifelse(May_Temp_Diff>0,"Plus","Minus")
May_Temp_Diff.df
detach(May_Temp_Diff.df)

library(ggplot2)
ggplot(data=May_Temp_Diff.df,
       aes(x=May_Day,y=May_Temp_Diff,fill=plus_minus))+
  geom_bar(stat = "identity",position="identity",colour="white",width=0.2)+
  #width 막대폭 좁게
  scale_fill_manual(values=c("blue","red"),guide=FALSE)+ #guide=F 범례 생략
  ggtitle("1st order differenced Temp of May")

ggplot(data=May_Temp_Diff.df,
       aes(x=May_Day,y=May_Temp_Diff,fill=plus_minus))+
  geom_bar(stat = "identity",position="identity",colour="white",width=1)+
  scale_fill_manual(values=c("blue","red"),guide=FALSE)+ #guide=F 범례 생략
  ggtitle("1st order differenced Temp of May")

ggplot(data=May_Temp_Diff.df,
       aes(x=May_Day,y=May_Temp_Diff,fill=plus_minus))+
  geom_bar(stat = "identity",position="identity",colour="white",width=0.5)+
  scale_fill_manual(values=c("blue","red"),guide=FALSE)+
  ggtitle("1st order differenced Temp of May")+
  scale_x_continuous(breaks=c(5,10,15,20,25,30)) #x축 세분화화

ggplot(data=May_Temp_Diff.df,
       aes(x=May_Day,y=May_Temp_Diff,fill=plus_minus))+
  geom_bar(stat = "identity",position="identity",colour="white",width=0.5)+
  scale_fill_manual(values=c("blue","red"),guide=FALSE)+
  ggtitle("1st order differenced Temp of May")+
  scale_x_continuous(breaks=c(5,10,15,20,25,30))+
  xlab("Day of May, 1973")+
  ylab("Temp difference from previous day")
