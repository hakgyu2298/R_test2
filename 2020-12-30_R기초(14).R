str(airquality)
airquality_1 <- airquality[,c(1:4)]
str(airquality_1)
cor(airquality_1)

airquality_2 <- na.omit(airquality_1)
airquality_cor <- cor(airquality_2)

library(corrplot)
plot(airquality_2)

corrplot(airquality_cor,method="circle")
corrplot(airquality_cor,method="square")
corrplot(airquality_cor,method="ellipse")
corrplot(airquality_cor,method="number")
corrplot(airquality_cor,method="shade")
corrplot(airquality_cor,method="color")
corrplot(airquality_cor,method="pie")

corrplot(airquality_cor,
         method ="shade", #색 입힌 사각형
         addshade = "all", #상관관계 방향선 제시
         #shade.col=NA, #상관관계 방향선 미제시
         tl.col = "red", #라벨 색 지정
         tl.srt = 30, #위쪽 라벨 회전 각도
         diag=FALSE, #대각선 값 미제시
         addCoef.col="black", #상관계수 숫자 색
         order = "FPC" #"FPC": First Principle Component
                       #"hclust" : hierarchical clustering
                       #"AOE" : Angular Order of Eigenvectors
         )
help(pairs)
## put histograms on the diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
example(pairs)
## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
## put linear regression line on the scatter plot
panel.lm <- function(x, y, col=par("col"), bg=NA, pch=par("pch"),
                     cex=1, col.smooth="black", ...) {
  points(x, y, pch=pch, col=col, bg=bg, cex=cex)
  abline(stats::lm(y~x), col=col.smooth, ...)
} 

pairs(airquality_2,
      lower.panel = panel.lm, # 아래쪽 산점도에 선형 직선 추가
      upper.panel = panel.cor, #위쪽에는 상관계수 숫자
      diag.panel = panel.hist, # 대각선에는 히스토그램
      pch="*", # 점 모양은 *로
      main = "scatter-plot matrix, correlation coef.,histogram")

pairs(iris[1:4], main = "Anderson's Iris Data -- 3 species",
      pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])

library(MASS)
str(Cars93)
levels(Cars93$Type)
table(Cars93$Type)

Cars93_sample <- subset(Cars93,
                        select = c("Model","Type","Weight","MPG.highway",
                                   "Price"),
                        subset = (Type %in% c("Compact","Large")))
Cars93_sample
library(ggplot2)
ggplot(Cars93_sample,aes(x=Weight,y=MPG.highway))+
  geom_point(aes(size=Price),shape=21,colour="grey90",fill="yellow",alpha=0.5)+
  scale_size_area(max_size = 15)+ # 범례 없애려면 guide=FALSE
  #geom_text(aes(y=as.numeric(MPG.highway)-sqrt(Price)/10,label=Model),
  #          vjust=1,colour="grey40",size=3)+
  geom_text(aes(label=Model),check_overlap = TRUE,
            vjust=1,colour="grey40",size=3)+
  ggtitle("Bubble chart with scale_size_area and label")

ggplot(Cars93,aes(x=Weight,y=MPG.highway))+
  geom_point(shape=21,colour="black")+
  facet_grid(Type~.)
  #facet_grid -> 세로~가로
ggplot(Cars93,aes(x=Weight,y=MPG.highway))+
  geom_point(shape=21,colour="black")+
  facet_grid(.~Type)

ggplot(Cars93,aes(x=Weight,y=MPG.highway))+
  geom_point(shape=21,colour="black")+
  facet_grid(Origin~Type)

ggplot(Cars93,aes(x=Weight,y=MPG.highway))+
  geom_point(shape=21,colour="black")+
  facet_wrap(~Type,ncol=3)

ggplot(Cars93,aes(x=Weight,y=MPG.highway))+
  geom_point(shape=21,colour="black")+
  facet_wrap(Origin~Type,ncol=3)

ggplot(Cars93,aes(x=Weight,y=MPG.highway))+
  geom_point(shape=21,colour="black")+
  facet_wrap(Origin~Type,ncol=2)

ggplot(Cars93,aes(x=MPG.city,y=MPG.highway))+
  geom_point(shape=21,colour="black",size=3)+
  ggtitle("default setting of x and y axis")

ggplot(Cars93,aes(x=MPG.city,y=MPG.highway))+
  geom_point(shape=21,colour="black",size=3)+
  coord_fixed()+
  ggtitle("1:1 proportion of x and y axis : coord_fixed()")

ggplot(Cars93,aes(x=MPG.city,y=MPG.highway))+
  geom_point(shape=21,colour="black",size=3)+
  coord_fixed()+
  scale_x_continuous(breaks=seq(0,80,5))+
  scale_y_continuous(breaks=seq(0,80,5))+
  ggtitle("manual setting with fixed interval of x and y axis : scale_x_continuous(breaks=seq())")

ggplot(Cars93,aes(x=MPG.city,y=MPG.highway))+
  geom_point(shape=21,colour="black",size=3)+
  coord_fixed()+
  scale_x_continuous(breaks=c(10,15,20,25,30,40))+
  scale_y_continuous(breaks=c(20,25,30,40,50))+
  ggtitle("manual setting with fixed interval of x and y axis : scale_x_continuous(breaks=c())")
