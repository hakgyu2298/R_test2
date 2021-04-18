# to use Cars93 dataframe
library(MASS)

#scatter plot
attach(Cars93)

# adding points with different characters by condition
plot(Weight, MPG.highway, type = 'n') #blank plot

#Type = Compact
points(Weight[Type == "Compact"], MPG.highway[Type == "Compact"], pch = 0)

#Type = Large
points(Weight[Type == "Large"], MPG.highway[Type == "Large"], pch = 1)

#Type = Midsize
points(Weight[Type == "Midsize"], MPG.highway[Type == "Midsize"], pch = 17, col = "yellow")

#Type = Small
points(Weight[Type == "Small"], MPG.highway[Type == "Small"], pch = 3)

#Type = Sporty
points(Weight[Type == "Sporty"], MPG.highway[Type == "Sporty"], pch = 9)

#Type = Van
points(Weight[Type == "Van"], MPG.highway[Type == "Van"], pch = 15, col = "blue")

title("adding legend to the plot")

# adding legend to topright side
legend(x = 3500, y = 50,
       c("Compact","Large","Midsize","Small","Sporty","Van"),
       col = c("black","black","yellow","black","black","blue"),
       pch = c(0,1,17,3,9,15)
)
detach(Cars93)

#scatter plot
attach(Cars93)

# adding points with different characters by condition
plot(Weight, MPG.highway, type = 'n') #blank plot

#Type = Compact
points(Weight[Type == "Compact"], MPG.highway[Type == "Compact"], pch = 0)

#Type = Large
points(Weight[Type == "Large"], MPG.highway[Type == "Large"], pch = 1)

#Type = Midsize
points(Weight[Type == "Midsize"], MPG.highway[Type == "Midsize"], pch = 17, col = "yellow")

#Type = Small
points(Weight[Type == "Small"], MPG.highway[Type == "Small"], pch = 3)

#Type = Sporty
points(Weight[Type == "Sporty"], MPG.highway[Type == "Sporty"], pch = 9)

#Type = Van
points(Weight[Type == "Van"], MPG.highway[Type == "Van"], pch = 15, col = "blue")

title("adding legend to the plot")

# adding legend to topright side
legend("topright",
       c("Compact","Large","Midsize","Small","Sporty","Van"),
       col = c("black","black","yellow","black","black","blue"),
       pch = c(0,1,17,3,9,15)
)
detach(Cars93)

#Multiple polygons from NA values

plot(c(1,6),c(-3.5,3.5),type="n")
x <- c(1,2,3,NA,4,4,6)
y <- c(1,-3,2,NA,-3,3,-3)
polygon(x,y,
        col=c("yellow","blue"),
        border=c("black","red"),
        lwd=2,
        lty=c("dotted","solid"))
title("Multiple polygons from NA values")

##what if no NA value
plot(c(1,6),c(-3.5,3.5),type="n")
x <- c(1,2,3,4,4,6)
y <- c(1,-3,2,-3,3,-3)
polygon(x,y,
        col = c("yellow","blue"),
        border = c("black","red"),
        lwd=2,
        lty=c("dotted","solid"))
title("Multiple polygons without NA value")

#Line-shaded polygons
plot(c(1,6),c(-3.5,3.5),type="n")
x <- c(1,2,3,NA,4,4,6)
y <- c(1,-3,2,NA,-3,3,-3)
polygon(x,y,
        col=c("yellow","blue"),
        border=c("black","red"),
        lwd=2,
        lty=c("dotted","solid"),
        density=c(10,20),
        angle=c(45,-45))
title("Multiple polygons with Line-shaded density")

##Color-shaded polygon
# example source : http://www.math.ucla.edu/~anderson/rw1001/library/base/html/polygon.html
n <- 100
xx <- c(0:n,n:0)
yy <- c(c(0,cumsum(rnorm(n))),rev(c(0,cumsum(rnorm(n)))))
plot(xx,yy,type="n",xlab="Time",ylab="Distance")
polygon(xx,yy,col="gray",border="red")
title("Distance Between Brownian Motions")
