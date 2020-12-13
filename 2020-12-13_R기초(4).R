aa <- c(1,2,3,4,5,6,7)
bb <- c(7,6,5,4,3,2,1)
cc <- c(4)
aa>bb
aa>cc
aa>=bb
aa>=cc
aa<bb
aa<cc
aa<=bb
aa<=cc
aa==bb
aa==cc

aa>3 & aa<6
aa>=3 & aa<=6
aa<3 | aa>6
aa<=3 | aa>=6

sum(aa>3 & aa<6)
sum(aa<3 | aa>6)
mean(aa>3 & aa<6)
mean(aa<3 | aa>6)

any(aa>3 & aa<6)
all(aa>3 & aa<6)
any(aa>8)
any(aa==1)
all(aa==1)

library(MASS)
str(Cars93)
head(Cars93)
with(Cars93,tapply(MPG.highway,Type,mean))
install.packages("ggplot2")
library(ggplot2)
qplot(MPG.highway, data=Cars93, facets = Type~. , binwidth=2)

cust_profile_1 <- read.table("C:/Users/user/Desktop/밀화부리/R/cust_profile_2.txt",
                             header=TRUE,
                             sep=",",
                             stringsAsFactor=FALSE,
                             na.strings="")
is.character(cust_profile_1$cust_id)
is.character(cust_profile_1$last_name)
is.numeric(cust_profile_1$age)
is.factor(cust_profile_1$gender)
sapply(cust_profile_1,class)
cust_profile_1$gender <- as.factor(cust_profile_1$gender)
levels(cust_profile_1$gender)
levels(cust_profile_1$gender) <- c("FEMALE","MALE")
levels(cust_profile_1$gender)
cust_profile_1$gender <- factor(cust_profile_1$gender,labels = c("FEMALE","MALE"))

cust_profile_2 <- read.table("C:/Users/user/Desktop/밀화부리/R/cust_profile_2.txt",
                             header=TRUE,
                             sep=",",
                             stringsAsFactor=TRUE,
                             na.strings="")
sapply(cust_profile_2,class)
cust_profile_2$cust_id <- as.character(cust_profile_2$cust_id)
cust_profile_2$age <- as.numeric(cust_profile_2$age)
cust_profile_2$last_name <- as.character(cust_profile_2$last_name)
sapply(cust_profile_2,class)

with(Cars93,tapply(MPG.highway,Type,mean))
with(Cars93,tapply(MPG.highway,Type,sd))
sapply(Cars93,class)
lapply(Cars93,class)

var_1 <- c(1:3)
var_2 <- c(4:6)
var_3 <- c(7:9)
df <- data.frame(var_1,var_2,var_3)
df
colnames(df) <- lapply(colnames(df),function(x){gsub("var_","x_",x)})
df

x <- c(1,2,3,4,NA,6,7,8,9,NA)
is.na(x)
sum(is.na(x))
sum(is.na(Cars93))
sum(is.na(Cars93$Manufacturer))
sum(is.na(Cars93$Price))
sum(is.na(Cars93$Rear.seat.room))
sum(is.na(Cars93$Luggage.room))
colSums(is.na(Cars93))

sum(x)
mean(x)
sum(x,na.rm = TRUE)
mean(x,na.rm=TRUE)
sum(Cars93$Luggage.room,na.rm=TRUE)
Cars93_1 <- na.omit(Cars93)
str(Cars93_1)

sum(is.na(Cars93))
Cars93_2 <- Cars93[complete.cases(Cars93[,c("Rear.seat.room")]),]
sum(is.na(Cars93_2))
#k1 <- c(1,2,3,4,5)
#k2 <- c(3,4,5,2,1)
#k <- data.frame(k1,k2)
#k
#k[c(TRUE,TRUE,FALSE,TRUE,TRUE),]
#k
Cars93_3 <- Cars93[complete.cases(Cars93[,c(23:24)]),]
sum(is.na(Cars93_3))
dim(Cars93_3)
dim(Cars93)

Cars93$Luggage.room
sum(is.na(Cars93$Luggage.room))
Cars93_4 <- Cars93
Cars93_4$Luggage.room[is.na(Cars93_4$Luggage.room)] <- 0
Cars93_4$Luggage.room
Cars93_5 <- Cars93
Cars93_5$Luggage.room[is.na(Cars93_5$Luggage.room)] <- mean(Cars93$Luggage.room,
                                                            na.rm =TRUE )
sum(is.na(Cars93_5$Luggage.room))
Cars93_6 <- Cars93
sum(is.na(Cars93_6))
Cars93_6[is.na(Cars93_6)] <- 0
sum(is.na(Cars93_6))
Cars93_7 <- Cars93[1:20,c("Rear.seat.room","Luggage.room")]
colSums((is.na(Cars93_7)))
Cars93_7
sapply(Cars93_7, function(x) mean(x, na.rm=T))
Cars93_7 <- data.frame(sapply(Cars93_7,
                              function(x) ifelse(is.na(x),
                                                 mean(x,na.rm=TRUE),x)))
Cars93_7
grp <- c(rep('a',5),rep('b',5))
val <- c(1,2,3,NaN,6,2,4,NaN,10,8)
df <- data.frame(grp,val)
df
library(dplyr)
df%>%group_by(grp)%>%summarise(grp_mean=mean(val,na.rm = TRUE))
df%>%group_by(grp)%>%mutate(val=ifelse(is.na(val),mean(val,na.rm = TRUE),val))

my_data <- c(-1,0,10,NA,NaN,Inf)
my_data
is.finite((my_data))
is.na(my_data)
is.nan(my_data)
is.infinite(my_data)
