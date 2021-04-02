length(colors())
colors()

par(mfrow=c(1,2))
pie(rep(1,8),col=1:8)
pie(rep(1,16),col=1:16)

library(MASS)
par(mfrow=c(2,2))

plot(MPG.highway~Weight,Cars93,cex=2,pch=19,
     col= 4,main="col = 4 (index)")

plot(MPG.highway~Weight,Cars93,cex=2,pch=19,
     col="blue",main="col = blue (name)")

plot(MPG.highway~Weight,Cars93,cex=2,pch=19,
     col="#0000FF",main="col = #0000FF (hexadecimal)")

rgb_1 <- rgb(0,0,255,maxColorValue = 255)
plot(MPG.highway~Weight,Cars93,cex=2,pch=19,
     col=rgb_1,main="col = RGB(0, 0, 255)(RGB triple)")

example(rainbow)
#Error in plot.new() : plot region too large가 나올때는 
par("mar")#로 margin크기를 확인한 후
par(mar=c(0.5,0.5,0.5,0.5))
#사용하여 margin 크기를 증가시킨다.
par(mfrow=c(2,2))

plot(MPG.highway~Weight, Cars93, cex=1, pch =21,
     col.axis = "blue", main = "col.axis = blue")

plot(MPG.highway~Weight, Cars93, cex=1, pch =21,
     col.axis = "red", main = "col.axis = red")

plot(MPG.highway~Weight, Cars93, cex=1, pch =21,
     col.axis = "yellow", main = "col.axis = yellow")

plot(MPG.highway~Weight, Cars93, cex=1, pch =21,
     col.axis = "gray", main = "col.axis = gray")

par(mfrow=c(2,2))

plot(MPG.highway~Weight, Cars93, cex=1, pch =21,
     col.lab = "blue", main = "col.lab = blue")

plot(MPG.highway~Weight, Cars93, cex=1, pch =21,
     col.lab = "red", main = "col.lab = red")

plot(MPG.highway~Weight, Cars93, cex=1, pch =21,
     col.lab = "yellow", main = "col.lab = yellow")

plot(MPG.highway~Weight, Cars93, cex=1, pch =21,
     col.lab = "gray", main = "col.lab = gray")

par(mfrow=c(2,2))

plot(MPG.highway~Weight, Cars93, cex=1, pch=21,
     col.main="blue", main = "col.main = blue")

plot(MPG.highway~Weight, Cars93, cex=1, pch=21,
     col.main="red", main = "col.main = red")

plot(MPG.highway~Weight, Cars93, cex=1, pch=21,
     col.main="yellow", main = "col.main = yellow")

plot(MPG.highway~Weight, Cars93, cex=1, pch=21,
     col.main="gray", main = "col.main = gray")

par(mfrow=c(1,1))

plot(MPG.highway~Weight, Cars93, cex=1, pch=21,
     col.sub ="blue", sub = "col.sub = blue")

plot(MPG.highway~Weight, Cars93, cex=1, pch=21,
     col.sub ="red", sub = "col.sub = red")

plot(MPG.highway~Weight, Cars93, cex=1, pch=21,
     col.sub ="yellow", sub = "col.sub = yellow")

plot(MPG.highway~Weight, Cars93, cex=1, pch=21,
     col.sub ="gray", sub = "col.sub = gray")

plot(MPG.highway~Weight,Cars93,cex = 1, pch =21,
     fg = "blue", main = "fg (foreground) = blue")

plot(MPG.highway~Weight,Cars93,cex = 1, pch =21,
     fg = "red", main = "fg (foreground) = red")

plot(MPG.highway~Weight,Cars93,cex = 1, pch =21,
     fg = "yellow", main = "fg (foreground) = yellow")

plot(MPG.highway~Weight,Cars93,cex = 1, pch =21,
     fg = "gray", main = "fg (foreground) = gray")

plot(MPG.highway~Weight, Cars93, cex =2, pch =21,
     bg = "blue", main = "bg (background) = blue")

plot(MPG.highway~Weight, Cars93, cex =2, pch =21,
     bg = "red", main = "bg (background) = red")

plot(MPG.highway~Weight, Cars93, cex =2, pch =21,
     bg = "yellow", main = "bg (background) = yellow")

plot(MPG.highway~Weight, Cars93, cex =2, pch =21,
     bg = "gray", main = "bg (background) = gray")

par(mfrow=c(1,1))

plot(MPG.highway~Weight, Cars93, cex=2, pch=1,
     bg = "blue", main = "pch =1, bg is not working")

plot(MPG.highway~Weight, Cars93, cex=2, pch=21,
     bg = "blue", main = "pch =21, bg is not working")

plot(MPG.highway~Weight, Cars93, cex=2, pch=22,
     bg = "blue", main = "pch =22, bg is not working")

plot(MPG.highway~Weight, Cars93, cex=2, pch=23,
     bg = "blue", main = "pch =23, bg is not working")

plot(MPG.highway~Weight, Cars93, cex=2, pch=24,
     bg = "blue", main = "pch =24, bg is not working")

plot(MPG.highway~Weight, Cars93, cex=2, pch=25,
     bg = "blue", main = "pch =25, bg is not working")
