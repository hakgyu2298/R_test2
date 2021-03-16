library(MASS)
# method 1
par_origin <- par(no.readonly = TRUE)
par(pch = 15, col = "blue")

plot(MPG.highway~Weight, type = "p", Cars93)

plot(MPG.highway~Horsepower, type = "p", Cars93)

par(par_origin)
plot(MPG.highway~Weight, type = "p", Cars93)

# method 2
plot(MPG.highway~Horsepower, type = "p", # scatter plot with point
     pch = 15, # point character
     col = "blue", # color
     data = Cars93)
plot(MPG.highway~Weight, type = "p",
     pch = 21,
     col = "black",
     data = Cars93)

par(mfrow = c(1,1))

plot(MPG.highway~Weight, data = Cars93, pch = 1, main = "pch = 1" )
plot(MPG.highway~Weight, data = Cars93, pch = 2, main = "pch = 2" )
plot(MPG.highway~Weight, data = Cars93, pch = 3, main = "pch = 3" )
plot(MPG.highway~Weight, data = Cars93, pch = 4, main = "pch = 4" )
plot(MPG.highway~Weight, data = Cars93, pch = 5, main = "pch = 5" )
plot(MPG.highway~Weight, data = Cars93, pch = 6, main = "pch = 6" )

par(mfrow = c(1,3))
plot(MPG.highway~Weight, data = Cars93, pch = '$', main = "pch = '$' ")
plot(MPG.highway~Weight, data = Cars93, pch = '%', main = "pch = '%' ")
plot(MPG.highway~Weight, data = Cars93, pch = '*', main = "pch = '*' ")

## symbol size : cex
par(mfrow = c(2,3))
plot(MPG.highway~Weight, data = Cars93, pch = 19, cex = 0.5, main = "cex = 0.5")
plot(MPG.highway~Weight, data = Cars93, pch = 19, cex = 1, main = "cex = 1 (default)")
plot(MPG.highway~Weight, data = Cars93, pch = 19, cex = 1.5, main = "cex = 1.5" )
plot(MPG.highway~Weight, data = Cars93, pch = 19, cex = 2, main = "cex = 2" )
plot(MPG.highway~Weight, data = Cars93, pch = 19, cex = 3, main = "cex = 3" )
plot(MPG.highway~Weight, data = Cars93, pch = 19, cex = 4, main = "cex = 4" )

## line type : lty
Cars93_order <- Cars93[order(Cars93$Weight),]
par(mfrow = c(2,3))
plot(MPG.highway~Weight, data = Cars93_order, type = "l", lty = 1, main = "lty = 1")
plot(MPG.highway~Weight, data = Cars93_order, type = "l", lty = 2, main = "lty = 2")
plot(MPG.highway~Weight, data = Cars93_order, type = "l", lty = 3, main = "lty = 3")
plot(MPG.highway~Weight, data = Cars93_order, type = "l", lty = 4, main = "lty = 4")
plot(MPG.highway~Weight, data = Cars93_order, type = "l", lty = 5, main = "lty = 5")
plot(MPG.highway~Weight, data = Cars93_order, type = "l", lty = 6, main = "lty = 6")

## line width : lwd
Cars93_order <- Cars93[order(Cars93$Weight),]
par(mfrow = c(2,3))
plot(MPG.highway~Weight, data = Cars93_order, type = "l", lwd = 0.5, main = "lwd = 0")
plot(MPG.highway~Weight, data = Cars93_order, type = "l", lwd = 1, main = "lwd = 1 (default)")
plot(MPG.highway~Weight, data = Cars93_order, type = "l", lwd = 2, main = "lwd = 2")
plot(MPG.highway~Weight, data = Cars93_order, type = "l", lwd = 3, main = "lwd = 3")
plot(MPG.highway~Weight, data = Cars93_order, type = "l", lwd = 4, main = "lwd = 4")
plot(MPG.highway~Weight, data = Cars93_order, type = "l", lwd = 5, main = "lwd = 5")

par()
