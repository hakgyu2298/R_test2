library(MASS)

#Save defalut par values
op <- par(no.readonly = TRUE)

#Change par() function options
par(mfrow=c(2,2)) #make frame by 2 row, 2 columns

#plot with X and Y axis
plot(MPG.highway ~ Weight, Cars93,
     main = "plot with X and Y axis")

#deleting X and Y axes : axes = FALSE
plot(MPG.highway ~ Weight, Cars93, axes = FALSE,
     main = "axes = FALSE")

#delteting X axis : xaxt = "n"
plot(MPG.highway~Weight, Cars93, xaxt = "n",
     main = "xaxt = n")

#deleting Y axis : yaxt = "n"
plot(MPG.highway~ Weight, Cars93, yaxt = "n",
     main = "yaxt = n")

#Reset par to the defalut values at startup
par(op)

#summary statistics

summary(Cars93$Weight) # X axis

summary(Cars93$MPG.highway) # Y axis

# X axis(1600~4200, by 100)
# Y axis(18~52 range, by 2)
plot(MPG.highway~Weight, Cars93, axes = FALSE,
     xlim = c(1600,4200),
     ylim = c(18,52),
     main = "scatter plot of Weight and MPG.highway")

x <- seq(1600,4200,by=100)
y <- seq(18,52,by=2)

axis(side = 1, # bottom side
     at = x,
     labels = TRUE,
     pos = 18, # coordinate of X axis starting point
     tck = 0.02) #tick marks at vertical direction with 0.02 length

axis(side = 2, #left side
     at = y,
     labels = TRUE,
     pos = 1600, # coordinate of Y axis starting point
     tck = -0.02) # tick marks at horizontal direction with 0.02 length

#fitting regression model
fit_1 <- lm(MPG.highway~Weight, data = Cars93)
fit_1

names(fit_1)

## adding regression line to the current plot
# (1) lines ()
with(Cars93, plot(MPG.highway ~ Weight)) #scatter plot of MPG.highway~Weight
lines(Cars93$Weight, fit_1$fitted.values, col = "blue")
title("adding regression line : lines()")

# (2) abline(reg = regression_model)
with(Cars93, plot(MPG.highway ~ Weight))
abline(reg=fit_1, col = "red", lwd = 3)
title("adding regression line : abline(reg = )")

# (3) abline(a = fit_1$coef[1], b = fit_1$coef[2])
with(Cars93, plot(MPG.highway ~Weight))
abline(a = fit_1$coef[1], b = fit_1$coef[2], col = "black", lwd =3)
title("adding regression line : abline(a = coef[1], b = coef[2])")

fit_1$coef #coefficients of regression model
fit_1$coef[1] #intercept
fit_1$coef[2] #slope

##adding smoothed regression line to the current plot : lines(), loess.smooth()
# (1) fitting non-parametric regression model
fit_2 <- loess.smooth(x = Cars93$Weight, y = Cars93$MPG.highway)
names(fit_2)
fit_2

#(2) scatter plot, adding smoothed regression line
with(Cars93, plot(MPG.highway ~ Weight))
lines(fit_2$x, fit_2$y, col = "blue", lwd = 3)
title("adding smoothed regression line: lines(), loess.smooth()")

##drawing horizontal or vertical straight lines : abline()
with(Cars93, plot(MPG.highway ~ Weight))
# (1) vertical line
abline(v = mean(Cars93$Weight), col = "black", lty = 3, lwd = 2)

# (2) horizontal line
abline(h = mean(Cars93$MPG.highway), col = "blue", lty =3, lwd = 2)

# scatter plot
attach(Cars93)

plot(Weight, MPG.highway, type = 'p') # points plot

#adding points to the current plot
plot(Weight, MPG.highway, type = 'n') # blank plot
points(Weight, MPG.highway) # exactly the same with the upper points plot

#adding points to the current plot with pch, col, cex parameters
plot(Weight, MPG.highway, type = 'n') #blank plot
points(Weight, MPG.highway, pch = 15, col = "blue", cex =1.5)

#adding points with different characters by condition
plot(Weight, MPG.highway, type = 'n') #blank plot

table(Cars93$Type)

#Type = Compact
points(Weight[Type == "Compact"], MPG.highway[Type == "Compact"], pch = 0)

#Type = Large
points(Weight[Type == 'Large'], MPG.highway[Type == "Large"], pch = 1)

#Type = Midsize
points(Weight[Type == "Midsize"], MPG.highway[Type == "Midsize"], pch = 17, col = "yellow")

#Type = Small
points(Weight[Type == "Small"], MPG.highway[Type == "Small"], pch = 3)

#Type = Sporty
points(Weight[Type == "Sporty"], MPG.highway[Type == "Sporty"], pch = 9)

#Type = Van
points(Weight[Type == "Van"], MPG.highway[Type == "Van"], pch = 15, col = "blue")
title("adding points with different characters by Car Types")

# adding legend to topright side
legend("topright",
       c("Compact","Large","Mdisize","Small","Sporty","Van"),
       col = c("black","black","yellow","black","black","blue"),
       pch = c(0,1,17,3,9,15)
       )
detach(Cars93)

attach(Cars93)

plot(Weight, MPG.highway, main = "scatter plot of MPG.highway ~ Weight")
text(x = Weight, y = MPG.highway, labels = Model, pos =3, cex = 0.5)

#placing text at the point of cursor : locator(1)
text(locator(1), labels = "Low Mileage Per Gallon")

detach(Cars93)

##places text in one of the four margins : mtext()

#Save defalut par values
op <- par(no.readonly = TRUE)

#combining 2 graphs in 1 row
par(mfrow = c(1,2), # 1 row, 2 windows
    OMA = c(2,2,4,1)) #outer margin

attach(Cars93)

plot(Weight, MPG.highway, main = "MPG.highway ~ Weight") #plot 1
plot(Horsepower, MPG.highway, main = "MPG.highway ~ Horsepower") #plot 2

mtext("MPG.highway by Weight, Horsepower",
      side = 3, # which margin to place text. 1=bottom, 2=left, 3=top, 4=right
      line = 1, # to indicate the line in the margin starting with - and moving out
      adj = 2, # adj=0 for left/bottom alignment or adj=1 for top/right alignment
      cex = 2, #font size
      outer = TRUE) # outer + TRUE : to place text at outer margin

detach(Cars93)

# Reset par to the defalut values at startup
par(op)
