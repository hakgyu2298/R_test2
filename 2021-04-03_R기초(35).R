library(MASS)

plot(MPG.highway~Weight, Cars93, type="p",
     xlab = "Inner Margin Area 1",
     ylab = "Inner Margin Area 2",
     main = "Inner Margin Area 3")

mtext("Inner Margin Area 4", side = 4)

text(3000,35,cex=3,labels="Plot Area",pos=3)

#Save defalut par values, for resetting later
op <- par(no.readonly = TRUE)

par(mfrow=c(1,2), #make frame by 1 row, 2columns
    mar=c(4,3,3,1), #inner margin
    oma=c(0.5,0.5,2,0.5)) #outer margin

#plot area, inner margin area, outer margin area
plot(MPG.highway~Weight, Cars93, type="p",
     xlab ="Inner Margin Area",
     main = "Inner Margin Area")

plot(MPG.highway~Horsepower, Cars93, type="p",
     xlab = "Inner Margin Area",
     main = "Inner Margin Area")

mtext("Outer Margin Area", outer = TRUE, cex = 2, col = "blue") #outer = TRUE :outer margin area

#Reset par to the default values at startup
par(op)

par(mfrow=c(4,2),
    mar=c(4,3,3,1), #inner margin
    oma=c(0.5,0.5,2,0.5) # outer margin)

plot(MPG.highway~Weight, Cars93, type="p", main="plot 1")
plot(MPG.highway~Weight, Cars93, type="p", main="plot 2")
plot(MPG.highway~Weight, Cars93, type="p", main="plot 3")
plot(MPG.highway~Weight, Cars93, type="p", main="plot 4")
plot(MPG.highway~Weight, Cars93, type="p", main="plot 5")
plot(MPG.highway~Weight, Cars93, type="p", main="plot 6")
plot(MPG.highway~Weight, Cars93, type="p", main="plot 7")
plot(MPG.highway~Weight, Cars93, type="p", main="plot 8")

mtext("par(mfrow = c(4,2)",outer=TRUE, cex = 2, col= "blue")

par(mfcol=c(4,2),
    mar=c(4,3,3,1), #inner margin
    oma=c(0.5,0.5,2,0.5) # outer margin)
    
plot(MPG.highway~Weight, Cars93, type="p", main="plot 1")
plot(MPG.highway~Weight, Cars93, type="p", main="plot 2")
plot(MPG.highway~Weight, Cars93, type="p", main="plot 3")
plot(MPG.highway~Weight, Cars93, type="p", main="plot 4")
plot(MPG.highway~Weight, Cars93, type="p", main="plot 5")
plot(MPG.highway~Weight, Cars93, type="p", main="plot 6")
plot(MPG.highway~Weight, Cars93, type="p", main="plot 7")
plot(MPG.highway~Weight, Cars93, type="p", main="plot 8")

mtext("par(mfcol = c(4,2)", outer = TRUE, cex = 2, col = "blue")

par(mfrow=c(1,1))
op <- par(no.readonly = TRUE)

layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))

layout.show(4)

layout(matrix(c(1,0,2,2),2,2, byrow=TRUE))
# 0은 비어있는 그래프 영역, 같은 숫자는 합치라는 뜻
layout.show(2)

layout(matrix(c(1,1,0,2),2,2,byrow=TRUE))
layout.show(2)

layout_1 <- layout(matrix(1),widths=lcm(10),heights=lcm(10))
layout.show(layout_1)

dev.off()
layout_2 <- layout(matrix(c(2,0,1,3),2,2, byrow =TRUE),
                   widths = lcm(c(8,4)),
                   heights = lcm(c(4,8)),
                   respect = TRUE)
layout.show(layout_2)

par(op)

par(mar=c(1,1,1,1))


plot(MPG.highway~Weight, Cars93,
     main = "main title : scatter plot of Weight, Mpg.highway",
     sub = "sub title : plotting with high level graphic functions",
     xlab = "x label : Weight",
     ylab = "y label : MPG.highway")
par(mfrow=c(1,1))
plot(MPG.highway~Weight, Cars93, ann=FALSE)

title(main = "main title : scatter plot of Weight, MPG.highway",
      sub = "sub title : plotting with low level graphic functions",
      xlab = "x label : Weight",
      ylab = "y label : MPG.highway")

plot(MPG.highway~Weight, Cars93, ann = FALSE)
title(main = "main title with cex 2.2",
      sub = "sub title with cex 1.5",
      xlab = "x label with cex 1",
      ylab = "y label with cex 1",
      cex.main = 2.2, #main title size
      cex.sub = 1.5, #sub title size
      cex.lab = 1) # x and y label size
dev.off()
