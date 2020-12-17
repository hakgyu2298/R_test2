library(MASS)
names(Cars93)

Cars93_subset <- Cars93[,c(1:5)]
names(Cars93_subset)
names(Cars93_subset) <- c("V1","V2","V3","V4","V5")
names(Cars93_subset)

library(reshape)
Cars93_subset <- rename(Cars93_subset,
                        c(V1="V1_Manufacturer",
                          V2="V2_Model",
                          V3="V3_Type",
                          V4="V4_Min.Price",
                          V5="V5_Price"))
library(plyr)
Cars93_subset <- rename(Cars93_subset,
                        c("V1_Manufacturer"="Manufacturer",
                          "V2_Model"="Model",
                          "V3_Type"="Type",
                          "V4_Min.Price"="Min.Price",
                          "V5_Price"="Price"))
library(dplyr)

Cars93_2 <- Cars93[,c(1:3)]
names(Cars93_2)
Cars93_3 <- rename(Cars93_2,
                   New_manufacturer=Manufacturer,
                   New_Model=Model,
                   New_Type=Type)
names(Cars93_3)

height <- c(175,159,166,189,171,173,179,167,182,170)
weight <- c(62,55,59,75,61,64,63,65,70,60)
h_w_d.f <- data.frame(height,weight)
h_w_d.f

options(digits=4)
h_w_d.f$bmi_1 <- (h_w_d.f$weight)/(h_w_d.f$height/100)^2
h_w_d.f
h_w_d.f <- transform(h_w_d.f,
                     bmi_2=weight/(height/100)^2)
h_w_d.f

options(digits = 3)
h_w_d.f <- transform(h_w_d.f,
                     bmi_sqrt=sqrt(bmi_2),
                     bmi_log10=log10(bmi_2))
View(h_w_d.f)

help(mtcars)


attach(mtcars)
#"변속기가 자동(am == 0)이고 & 실린더가 4개 또는 6개 (cyl == 4 or cyl == 6) 인
#자동차들의 연비(mpg) 평균(mean())는?"
mtcars_mart_0 <- mtcars[which(am==0 & cyl %in% c(4,6)),c("mpg","cyl","am")]
mtcars_mart_0
mean(mtcars_mart_0$mpg)
mtcars_mart_1 <- mtcars[which(am==1 & cyl %in% c(4,6)),c("mpg","cyl","am")]
mean(mtcars_mart_1$mpg)

detach(mtcars)

mtcars_subset_0 <- subset(mtcars,
                          select=c(mpg,cyl,am),
                          subset=(am==0 & cyl %in% c(4,6)))
mtcars_mart_0
mean(mtcars_subset_0$mpg)
subset(mtcars,
       select=c(mpg,cyl,am),
       subset=((am == 0 & cyl == 4) | (am == 0 & cyl == 6)))
mtcars_subset_1 <- subset(mtcars,
                          select=c(mpg,cyl,am),
                          subset=(am==1&cyl %in% c(4,6)))
mtcars_subset_1
mtcars_subset_1_5 <- subset(mtcars,
                            select=c(1:5))
head(mtcars_subset_1_5)
mtcars_subset_6_11 <- subset(mtcars,
                             select=-c(1:5))
head(mtcars_subset_6_11)

library(dplyr)
mtcars %>%select(mpg,cyl,am)%>%filter(am==0 & cyl %in% c(4,6))
mtcars %>%select(mpg,cyl,am)%>%filter(am==0 & cyl %in% c(4,6))%>%summarise(mean(mpg))
mtcars %>%select(mpg,cyl,am)%>%filter(am==1 & cyl %in% c(4,6))

v1 <- c(40,30,50,50,90,40,50)
v2 <- c(5100,6500,2000,2000,9000,4500,3000)
v3 <- c("A","B","A","B","A","A","B")
v123 <- data.frame(v1,v2,v3)
v123
v1

sort(v1)
sort(v1,decreasing=TRUE)
order(v1)
v1[order(v1)]
rm(v1,v2,v3)
attach(v123)
v123_order <- v123[order(v1,-v2,v3),]
v123
v123_order

order(v1,-v2,v3)
row.names(v123_order)
detach(v123)

library(plyr)
arrange(v123,v1,desc(v2),v3)

cust_id <- c("c01","c02","c03","c04")
last_name <- c("Kim","Lee","Choi","Park")
cust_mart_1 <- data.frame(cust_id,last_name)
cust_mart_2 <- data.frame(cust_id=c("c05","c06","c07"),
                          last_name=c("Bae","Kim","Lim"))
cust_mart_12 <- rbind(cust_mart_1,cust_mart_2)
cust_mart_3 <- data.frame(cust_id=c("c08","c09"),
                          last_name=c("Lee","Park"),
                          gender=c("F","M"))
rbind(cust_mart_12,cust_mart_3)
cust_mart_4 <- data.frame(cust_id=c("c10","c11"),
                          first_name=c("kildong","Yongpal"))
rbind(cust_mart_12,cust_mart_4)
cust_mart_5 <- data.frame(age=c(20,25,19,40,32,39,28),
                          income=c(2500,2700,0,7000,3400,3600,2900))
cust_mart_125 <- cbind(cust_mart_12,cust_mart_5)
cust_mart_125
cust_mart_6 <- data.frame(age=c(34,50),
                          income=c(3600,5100))
cbind(cust_mart_125,cust_mart_6)

cust_mart_12
cust_mart_7 <- data.frame(cust_id=c("c03","c04","c05","c06","c07","c08","c09"),
                          buy_cnt=c(3,1,0,7,3,4,1))
cust_mart_7
cust_mart_127_cbind <- cbind(cust_mart_12,cust_mart_7)
cust_mart_127_cbind

cust_mart_127_innerjoin <- merge(x=cust_mart_12,
                                 y=cust_mart_7,
                                 by="cust_id")
cust_mart_127_innerjoin
cust_mart_127_outerjoin <- merge(x=cust_mart_12,
                                 y=cust_mart_7,
                                 by="cust_id",
                                 all=TRUE)
cust_mart_127_outerjoin
cust_mart_127_leftouter <- merge(x=cust_mart_12,
                                 y=cust_mart_7,
                                 by="cust_id",
                                 all.x=TRUE)
cust_mart_127_leftouter
cust_mart_127_rightouter <- merge(x=cust_mart_12,
                                  y=cust_mart_7,
                                  by="cust_id",
                                  all.y = TRUE)
cust_mart_127_rightouter


