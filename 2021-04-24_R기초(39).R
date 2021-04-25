library(MASS)

#making Type_number variable to put line type and color by Car Type
Cars93_1 <- transform(Cars93,
                      Type_no = ifelse(Type == "Compact", 1,
                                       ifelse(Type == "Large", 2,
                                              ifelse(Type == "Midsize",3,
                                                     ifelse(Type == "Small",4,
                                                            ifelse(Type == "Sporty",5,6))))))
# checking top 10 observations
head(Cars93_1[,c("Type","Type_no")], n = 10)

#parallel coordinate plot
parcoord(Cars93_1[,c("MPG.highway","RPM","Horsepower","Weight","Length","Price")],
         lty = Cars93_1$Type_no,
         col = Cars93_1$Type_no,
         var.label = TRUE,
         main = "parallel coordinate plot of Cars93 by TYpe")
# putting legend
legend("topright",
       legend = c("Compact","Large","Midsize","Small","Sporty","Van"),
       lty = c(1:6),
       col = c(1:6),
       lwd = 2, # line width
       cex = 0.7) # character size

library(scatterplot3d)

x <- Cars93$Weight
y <- Cars93$Horsepower
z <- Cars93$MPG.highway

# 3 dimensional scatter plotting
Cars93_3d <- scatterplot3d(x,y,z,
                           type = "h", # "p" for point, "l" for line, "h" for vertical lines to x-y-plane
                           pch = 16, # symbol, character
                           scale.y = 0.7, # scale of y axis related to x- and z axis
                           angle = 50, # angle between x and y axis
                           highlight.3d = TRUE, # points will be drawn in different colors related to y coordinates
                           box = TRUE, # a logical value indicating whether a box should be drawn around the plot
                           col.axis = "blue", # the color to be used for axis
                           grid = TRUE, # a logical value indicating whether a grid should be drawn on the plot
                           col.grid = "gray", # the color to be used for grid
                           mar = c(3,4,4,3), # margin : c(bottom, left, top, right)
                           xlab = "x_Weight", # label for the x
                           ylab = "y_Horsepower", # label for the y
                           zlab = "z_MPG.highway", # label for the z
                           main = "3 dimensional scatter plot of Cars93"# main title
)

#using rainbow color
dim(Cars93)
rainbowcolor <- rainbow(93) # number 93 indicate 93 observations of Cars93 dataframe
Cars93_3d <- scatterplot3d(x,y,z,
                           type = "h", #"p" for point, "l" for line, "h" for vertical lines to x-y-plane
                           pch =16, # symbol, character
                           scale.y = 0.7, # scale of y axis related to x- and z axis
                           angle = 50, #angle between x and y axis
                           color = rainbowcolor, # colors of points in the the plot
                           box = TRUE, # a logical value indicating whether a box be drawn around the plot
                           col.grid = "gray", # the color to be used for grid
                           mar = c(3,4,4,3), #margin : c(bottom, left, top, right)
                           xlab = "x_Weight", # label for the x
                           ylab = 'y_Horsepower', # lab el for the y
                           zlab = "z_MPG.highway", # label for the z
                           main = '3 dimensional scatter plot of Cars93 - using rainbow color' # main title
)

# Adding a regression plane to the "scatterplot3d"
attach(Cars93)
Cars93_lm <- lm(MPG.highway~Weight + Horsepower)
Cars93_3d$plane3d(Cars93_lm, lty.box = "solid")
detach(Cars93)

# samploing observations from 1st to 20th, selecting 5 variables
Cars93_1 <- Cars93[c(1:20),c("Price","MPG.highway","Horsepower","RPM","Length","Weight")]
Cars93_1

library(aplpack)

# face.type = 0 : line drawing faces
faces(Cars93_1, face.type = 0, main = "Chernoff faces: face.type = 0")

# face.type = 1 : the elements of the faces are painted
faces(Cars93_1, face.type = 1, main = "Chernoff faces: face.type = 1")

# face.type = 2 : Santa Claus faces are drawn
faces(Cars93_1, face.type = 2, main = "Chernoff faces: face.type = 2")

# overlapping chernoff faces over scatter plot (MPG.highway*Weight)
plot(Cars93_1[,c("MPG.highway","Weight")],
     bty = "n", # To make a plot with no box around the plot area
     main = "Chercoff faces of Cars93")

Cars93_1_faces <- faces(Cars93_1, scale = TRUE, plot = FALSE)

plot.faces(Cars93_1_faces,
           Cars93_1[,c("MPG.highway")],
           Cars93_1[,c("Weight")],
           width = 2,
           height = 250)
