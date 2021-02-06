prop <- c(0.33,0.41)
n <- c(500,600)
x <- prop*n
x

prop.test(x=x, # number of events
          n=n, # number of trials
          alternative=c("two.sided"), # alternative=c("two.sided","less","greater")
          conf.level=0.95)
library(MASS)
# way1 : y~Factor
wilcox.test(Price~Origin,
            data=Cars93,
            alternative = c("two.sided"),
            mu = 0,
            conf.int = FALSE,
            conf.level = 0.95)

# x,y numeric vector indexing
Price_USA <- Cars93[which(Cars93$Origin==c("USA")),c("Price")]
Price_nonUSA <- Cars93[which(Cars93$Origin==c("non-USA")),c("Price")]
wilcox.test(Price_USA,Price_nonUSA,
            alternative = c("two.sided"),
            mu=0,
            conf.int=FALSE,
            conf.level=0.95)

# paired 10 sample of patient's blood sugar
x1 <- c(51.4,52.0,45.5,54.5,52.3,50.9,52.7,50.3,53.8,53.1)
x2 <- c(50.1,51.5,45.9,53.1,51.8,50.3,52.0,49.9,52.5,53.0)

wilcox.test(x1,x2,
            alternative=c("greater"),
            paired=TRUE,
            conf.level=0.95)

y1 <- c(50.5,52.1,51.9,52.4,50.6,51.4,51.2,52.2,51.5,50.8)
y2 <- c(47.5,47.7,46.6,47.1,47.2,47.8,45.2,47.4,45.0,47.9)
y3 <- c(46.0,47.1,45.6,47.1,47.2,46.4,45.9,47.1,44.9,46.2)

y <- c(y1,y2,y3)
n <- rep(10,3)

group <- rep(1:3,n)

group_df <- data.frame(y,group)
group_df

sapply(group_df,class)

group_df <- transform(group_df,group=factor(group))
sapply(group_df,class)

attach(group_df)

boxplot(y~group,
        main="Boxplot of Daily Outcome by Temperature condition 1/2/3/",
        xlab="Factor Levels : Temperature condition 1/2/3",
        ylab="Daily Outcome")

tapply(y,group,summary)

detach(group_df)

aov(y~group,data=group_df)
summary(aov(y~group,data=group_df))

bartlett.test(y~group,data=group_df)
