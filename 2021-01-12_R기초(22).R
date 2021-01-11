library(MASS)
str(Cars93)

Car_table_1 <- with(Cars93,table(Type))
Car_table_1

Car_table_2 <- xtabs(~Type,data=Cars93)
Car_table_2

options("digits"=2) #소수점 자리수 설정
Sys.setenv(LANG = "en") #R 언어 영어로
prop.table(Car_table_1)

barplot(Car_table_1,
        main = "Bar Plot of Car Type",
        xlab = "Car Type", ylab = "counts",
        col = "yellow")

Car_table_3 <- with(Cars93,table(Type,Cylinders))
Car_table_3

Car_table_4 <- xtabs(~Type+Cylinders,data=Cars93) #~왼쪽에 빈도를 나타내는 변수
Car_table_4

data(UCBAdmissions)
str(UCBAdmissions)
UCBAdmissions
class(UCBAdmissions)
UCBAdmissions.df<- as.data.frame(UCBAdmissions)
UCBAdmissions.df

xtabs(Freq~Gender+Admit,data=UCBAdmissions.df)

options('digit'=3)
Prop_Car_table_3 <- prop.table(Car_table_3)
Prop_Car_table_3

margin.table(Car_table_3,margin=1)
margin.table(Prop_Car_table_3,margin=1)

margin.table(Car_table_3,margin=2)
margin.table(Prop_Car_table_3,margin=2)

addmargins(Car_table_3)
addmargins(Prop_Car_table_3)

addmargins(Car_table_3,margin=1)
addmargins(Prop_Car_table_3,margin=1)
addmargins(Car_table_3,margin=2)
addmargins(Prop_Car_table_3,margin=2)

dim(Cars93) #행과 열 개수
sum(is.na(Cars93$Type))
sum(is.na(Cars93$Cylinders))
sum(is.na(Cars93$Luggage.room))

summary(Cars93$Luggage.room)

table((Luggage.room_big=Cars93$Luggage.room>=14),Cars93$Type)
addmargins(with(Cars93, table((Luggage.room_big = Luggage.room >= 14), Type)))
addmargins(with(Cars93, table((Luggage.room_big = Luggage.room >= 14), Type,
                              useNA = "ifany")))
library(gmodels)
with(Cars93,CrossTable(Type,Cylinders,expected = TRUE,chisq = TRUE))

library(vcd)
Car_table_3 <- with(Cars93,table(Type,Cylinders))
Car_table_3

mosaic(Car_table_3,
       gp=gpar(fill=c("red","blue")),
       direction="v",
       main="Mosaic plot of Car Type & Cylinders")
#gpar는 Handling Grid Graphical Parameters이다



