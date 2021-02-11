gender.fac <- as.factor(c(rep("M",9),rep("F",9)))
gender.fac

class <- c("Class_1","class_1","class_1","class_2","class_2","class_2",
           "class_3","class_3","class_3")
class.fac <- as.factor(c(rep(class,2)))
class.fac

score_stats <- c(71,77,78,76,77,78,71,70,69,80,76,80,79,78,77,73,71,70)

score.df <- data.frame(gender.fac,class.fac,score_stats)
score.df

library(doBy)

summaryBy(score_stats~gender.fac,data=score.df,FUN=c(mean,sd,min,max))
summaryBy(score_stats~class.fac,data=score.df,FUN=c(mean,sd,min,max))
summary(score_stats,data=score.df)

par(mfrow=c(2,2))
plot(score_stats~gender.fac,main="box plot by gender")
plot(score_stats~class.fac,main="box plot by class")
interaction.plot(gender.fac,class.fac,score_stats,bty='l',main="interaction effect plot")
interaction.plot(class.fac,gender.fac,score_stats,bty='l',main="interaction effect plot")

aov_model <- aov(score_stats~gender.fac+class.fac+gender.fac:class.fac)
summary(aov_model)

obs <- c(19,41,40)
null.probs <- c(2/10,3/10,5/10)
chisq.test(obs,p=null.probs)

chisq.test_output_1 <- chisq.test(obs,p=null.probs)

obs_2 <- c(5,5)
null.probs_2 <- c(0.3,0.7)
chisq.test(obs_2,p=null.probs_2)#도수가 너무 작으면 경고메시지

str(HairEyeColor)
HairEyeColor
dimnames(HairEyeColor)

margin.table(HairEyeColor,1) #Hair
margin.table(HairEyeColor,2) #Eye
margin.table(HairEyeColor,3) #Sex

Hair_Freq <- c(margin.table(HairEyeColor,1))
Hair_Freq

Hair_Prob <- c(0.2,0.5,0.1,0.2)
chisq.test(x=Hair_Freq,p=Hair_Prob)

Hair_Freq <- c(margin.table(HairEyeColor,1))
Hair_Freq

Hair_Prob <- c(0.2,0.5,0.1,0.2)
chisq.test(x=Hair_Freq,p=Hair_Prob)

data(Cars93,package="MASS")
head(Cars93)
str(Cars93)

Car_Type <- table(Cars93$Type)
Car_Type
Car_Type_Prob <- c(0.2,0.1,0.2,0.2,0.2,0.1)
chisq.test(x=Car_Type,p=Car_Type_Prob)
