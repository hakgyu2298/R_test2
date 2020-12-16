x <- c(1,2,3,4,5,6,7,8,9,10)
y <- c(5)
x%%y
x[x%%y==c(0)]

x%/%y

x%*%y
c(1,2,3) %*% c(4,5,6)
x%in%y
sum(x%in%y)

x <- c("Seoul","New York","London","1234")
nchar(x)

time_stamp <- c("201507251040","201507251041","2015072514042","201507251043","201507251044")
t_yyyymm <- substr(time_stamp,1,6)
t_yyyymm
gas_temp <- c(145.0,145.1,145.5,150.1,150.6)
ts_gas_temp <- data.frame(time_stamp,gas_temp)
ts_gas_temp
ts_gas_temp <- transform(ts_gas_temp,mmdd=substr(time_stamp,5,8))
ts_gas_temp <- transform(ts_gas_temp,hhmm=substr(time_stamp,9,12))
ts_gas_temp

paste("I","Love","New York",sep="")
paste("I","Love","New York",sep=" ")
paste("I","Love","New York",sep="_")
ts_gas_temp <- transform(ts_gas_temp,mmddhhmm=paste(mmdd,"일_",hhmm,"분",sep=""))
ts_gas_temp

name <- c("Chulsu,Kim","Younghei,Lee","Dongho,Choi")
name_split <- strsplit(name,split=",")
name_split

last_name <- c(name_split[[1]][2],name_split[[2]][2],name_split[[3]][2])
last_name

first_name <- c(name_split[[1]][1],name_split[[2]][1],name_split[[3]][1])
first_name
name_d.f <- data.frame(last_name,first_name,name)
name_d.f

name_df <- data.frame(ID=c(1:3),name=c("Chulsu/KIM","Younghei/Lee","Dongho/Choi"))
name_df
name_strsplit <- data.frame(do.call('rbind',
                                    strsplit(as.character(name_df$name),
                                             split='/',
                                             fixed=TRUE)))
name_strsplit
library(reshape)
name_strsplit <- rename(name_strsplit,
                        c(X1="First Name",X2="Last_Name"))
name_strsplit
z <- c("My name is Chulsu. What's your name?")
sub("name","first name",z)
gsub("name","first name",z)
sub("My name is Chulsu.","",z)

cust_id <- c("c1","c2","c3","c4","c5","c6")
size <- c("XS","L","M","XS","XL","S")
cust_db <- data.frame(cust_id,size)
cust_db
cust_db <- transform(cust_db,size_1=sub("XS","S",size))
cust_db

id <- c("a","b","c","c")
col <- c("11.23","64.12","931.01","3.3.0.4.1.2")
df <- data.frame(id,col)
df
df <- transform(df,
                col_2=gsub(".","",col,fixed=TRUE))
df
df <- transform(df,
                col_3=gsub("[.]","",col))
df
grep("1010",c("1001","1010","1110","101000"))
grep("1010",c("1001","1009","1110","100000"))

regexpr("NY","I love NY and I'm from NY")
gregexpr("NY","I love NY and I'm from NY")

student_id <- c("s01","s02","s03","s04","s05","s06","s07","s08","s09","s10")
stat_score <- c(56,94,82,70,64,82,78,80,76,78)
mean(stat_score)
hist(stat_score)
score_d.f <- data.frame(student_id,stat_score)
score_d.f
rm(student_id,stat_score)
score_d.f
score_d.f <- transform(score_d.f,
                       stat_score_1=cut(stat_score,breaks=c(0,60,70,80,90,100),
                                        include.lowest=TRUE,
                                        right=FALSE,
                                        labels=c("가","양","미","우","수")),
                       stat_score_2=cut(stat_score,breaks=c(0,60,70,80,90,100),
                                        include.lowest=FALSE,
                                        right=FALSE,
                                        labels=c("가","양","미","우","수")),
                       stat_score_3=cut(stat_score,breaks=c(0,60,70,80,90,100),
                                        include.lowest=FALSE,
                                        right=TRUE,
                                        labels=c("가","양","미","우","수")),
                       stat_score_4=cut(stat_score,breaks=c(0,60,70,80,90,100),
                                        include.lowest=TRUE,
                                        right=TRUE,
                                        labels=c("가","양","미","우","수"))
                       )
score_d.f
attach(score_d.f)
score_d.f <- transform(score_d.f,
                       stat_score_5=ifelse(stat_score<60, "가",
                                           ifelse(stat_score>=60 & stat_score<70, "양",
                                                  ifelse(stat_score>=70 & stat_score<80, "미",
                                                         ifelse(stat_score>=80 & stat_score<90, "우","수")))))
detach(score_d.f)
score_d.f

score_d.f <-within(score_d.f,{
  stat_score_6 = character(0)
  stat_score_6[stat_score<60]="가"
  stat_score_6[stat_score>=60 & stat_score<70]="양"
  stat_score_6[stat_score>=70 & stat_score<80]="미"
  stat_score_6[stat_score>=80 & stat_score<90]="우"
  stat_score_6[stat_score>=90]="수"
  stat_score_6=factor(stat_score_6,level=c("수","우","미","양","가"))
})
score_d.f$stat_score_6
score_d.f

X <- matrix(1:4,nrow=2,ncol=2,byrow =FALSE,dimnames=NULL)
X
Y <- matrix(5:8,nrow=2,ncol=2,byrow=TRUE,dimnames=NULL)
Y
X+Y
X-Y
X*Y
X/Y
X^Y
X%*%Y
cbind(X,Y)
rbind(X,Y)
colMeans(X)
rowMeans(X)
colMeans(Y)
rowMeans(Y)

colSums(X)
rowSums(X)
colSums(Y,na.rm=TRUE)
rowSUms(Y,na.rm=TRUE)

library(MASS)
str(Cars93)
sum(is.na(Cars93))
Cars93_na.omit <- na.omit(Cars93)
sum(is.na(Cars93_na.omit))
Cars93_rowSums <- Cars93[rowSums(is.na(Cars93))==0,]
sum(is.na(Cars93_rowSums))

X
t(X)
Y
t(Y)
Z <- matrix(1:6,nrow=2,ncol=3)
Z
t(Z)
