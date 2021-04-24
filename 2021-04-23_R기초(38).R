a1 <- rep(1:10, each = 2)
a1

a2 <- rep(c(1,3,5,7,9),each = 4)
a2

a3 <- c(1,1,1,1,3,3,3,3,5,5,6,6,7,7,8,8,9,10,11,12)
a3

a1a2a3 <- data.frame(cbind(a1,a2,a3))
a1a2a3

str(a1a2a3)

# extracting unique elements by 2 variables
aqa2a3_uniq_var2 <- unique(a1a2a3[,c("a1","a2")])
aqa2a3_uniq_var2

# extracting unique elements by 3 variables
aqa2a3_uniq_var3 <- unique(a1a2a3[,c("a1","a2","a3")])
aqa2a3_uniq_var3

# identical element will be kept from the last : fromLast = TRUE
# default : fromLast = FALSE
aqa2a3_uniq_var3_fromLast <- unique(a1a2a3[,c("a1","a2","a3")], fromLast = TRUE)
aqa2a3_uniq_var3_fromLast # different order

## duplicated() function

# original dataset
a1a2a3

# returning TRUE for duplicated value
duplicated(a1a2a3$a1)

# indexing unduplicated rows using dataframe[!duplicated(dataframe$var1),]
a1a2a3_not_duplicated_var1 <- a1a2a3[!duplicated(a1a2a3$a1),]
a1a2a3_not_duplicated_var1

# another example
a1a2a3[!duplicated(a1a2a3$a2),]

library(MASS)

# cross tabulation by car type
table(Cars93$Type)

# mean of multivariates by Car Type
install.packages("doBy")

library(doBy)

mean_by_Type <- summaryBy(MPG.highway+RPM+Horsepower+Weight+Length+Price~Type,
                          data = Cars93,
                          FUN = c(mean))
mean_by_Type

library(fmsb)

# manipulating dataset for radar chart
# data frame includes possible maximum values as row 1
# and possible minimum values as row 2
df_radarchart <- function(df){
  df <- data.frame(df)
  dfmax <- apply(df,2,max)
  dfmin <- apply(df,2,min)
  as.data.frame(rbind(dfmax,dfmin,df))
}
# maximum value as row 1, minimum value as row 2 : user-defined function df_radarchart
# standardization : scale()
mean_by_Type_scale <- df_radarchart((scale(mean_by_Type[,c(2:7)])))
mean_by_Type_scale

# radar chart (or spider plot)
radarchart(df = mean_by_Type_scale, #The data frame to be used to draw radarchart
           seg = 6, # The number of segments for each axis
           pty = 16, # A vector to specify point symbol : Default 16 (closed circle)
           pcol = 1:6, # A vector of color codes for plot data
           plty = 1:6, # A vector of line types for plot data
           plwd = 2, # A vecotr of line widths for plot data
           title = c("radar chart by Car Types") # putting title at the top-middle
           )
#adding legend
legend("topleft",
       legend = mean_by_Type$Type, 
       col = c(1:6), 
       lty = c(1:6), 
       lwd = 2,
       cex = 0.7
       )
# cross tabulation by Car Type
table(Cars93$Type)

# mean of multivariates by Car Type
library(doBy)

mean_by_Type <- summaryBy(MPG.highway+RPM+Horsepower+Weight+Length+Price~Type,
                          data=Cars93,
                          FUN=c(mean))
mean_by_Type

# creating row names with Type
rownames(mean_by_Type) <- mean_by_Type$Type

mean_by_Type

# renaming of variables
library(reshape)
mean_by_Type <- rename(mean_by_Type,
                       c(MPG.highway.mean = "MPG.highway",
                         RPM.mean = "RPM",
                         Horsepower.mean = "Horsepower",
                         Weight.mean = "Weight",
                         Length.mean = "Length",
                         Price.mean = "Price"
                         )
                       )
mean_by_Type

#star plot
stars(mean_by_Type[,2:7], # dataframe or matrix
      locations = NULL, # locations = NULL, the segment plots will be placed in a rectangular grid
      nrow = 2, # number of rows at a square layout (w/locations = NULL)
      ncol = 4, # number of columns at a square layout (w/locations - NULL)
      scale = TRUE, # the columns are scaled independently (max in each column: 1, min: 0)
      full = TRUE, # the radii corresponding to each variable in the data will be drawn
      radius = TRUE, # if TRUE, the plot region is framed
      main = "Star plot - means of multivariate by Car Type", #a main title for the plot
      cex = 1, # size of label character (by default, cex = 1)
      #labels = NULL # if NULL, no attempt is made to construct labels
      lwd = 1, # line width (by default, lwd = 1)
      key.loc = c(7.5,1.5) # vector with x and y coordinates of the unit key
      )

# radar chart (or spider chart)
stars(mean_by_Type[,2:7],
      locations = c(0,0),
      key.loc = c(0,0),
      scale = TRUE,
      radius = FALSE,
      cex = 1, 
      lty = 2,
      col.lines = c(1:6),
      lwd = 2,
      main = "radar chart - means of multivariate by Car Type"
      )
legend(x=1, y=1, legend = mean_by_Type$Type, lty =2, col = c(1:6),lwd = 2)
