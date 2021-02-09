#daily outcome by temp condition (group 1/2/3)

y1 <- c(50.5,52.1,51.9,52.4,50.6,51.4,51.2,52.2,51.5,50.8)
y2 <- c(47.5,47.7,46.6,47.1,47.2,47.8,45.2,47.4,45.0,47.9)
y3 <- c(46.0,47.1,45.6,47.1,47.2,46.4,45.9,47.1,44.9,46.2)

y <- c(y1,y2,y3)
y

n <- rep(10,3)
n

group <- rep(1:3,n)
group

group_df <- data.frame(y,group)
group_df

sapply(group_df,class)

group_df <- transform(group_df,group=factor(group))
sapply(group_df,class)

aov_model <- aov(y~group,data=group_df)
summary(aov_model)

library(agricolae)
duncan.test(aov_model,"group",alpha=0.05,console=TRUE)

library(laercio)
aov_model <- aov(y~group,data=group_df)
LDuncan(aov_model,"group")

data(sweetpotato)
str(sweetpotato)

attach(sweetpotato)
boxplot(yield~virus,
  main="Yield of sweetpotato, Dealt with different virus",
  xlab="Virus",
  ylab="Yield")
detach(sweetpotato)

library(doBy)

summaryBy(yield~virus,data=sweetpotato,FUN=c(mean,sd,min,max))

aov_sweetpotato <- aov(yield~virus,data=sweetpotato)
summary(aov_sweetpotato)

comparison <- scheffe.test(aov_sweetpotato, #ANOVA model
                           "virus", #vector treatment applied to each experimental unit
                           alpha=0.05,#significant level
                           group=TRUE,
                           console=TRUE, #print out
                           main="Yield of sweetpotato\nDealt with different virus")

car_type <- rep(c('1000','1500','1800'),2)
car_type <- as.factor(car_type)
car_type

insurance <- as.factor(c(rep('K',3),rep('M',3)))
insurance

y <- c(140,210,220,100,180,200)

two_way_aov_model_1 <- aov(y~car_type+insurance)
summary(two_way_aov_model_1)
