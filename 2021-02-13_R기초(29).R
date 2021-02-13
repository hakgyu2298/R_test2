row_1 <- c(7,13,9,12)
row_2 <- c(13,21,10,19)
row_3 <- c(11,18,12,13)

data_rbind <- rbind(row_1,row_2,row_3)
data_rbind

raw_data <- c(7,13,9,12,13,21,10,19,11,18,12,13)
data_matrix <- matrix(raw_data,byrow=TRUE,nrow=3)
data_matrix

dimnames(data_matrix) <- list("Class"=c("Class_1","Class_2","Class_3"),
                              "Score"=c("Score_H","Score_M","Score_L","Fail"))
data_matrix

addmargins(data_matrix)
prop.table(data_matrix)
addmargins((prop.table(data_matrix)))

barplot(t(data_matrix),beside=TRUE,legend=TRUE,
        ylim=c(0,30),
        ylab="Observed frequencies in sample",
        main="Frequency of math score by class")

chisq.test(data_matrix)
chisq.test_output_2 <- chisq.test(data_matrix)

chisq.test_output_2$observed
chisq.test_output_2$expected
chisq.test_output_2$residuals
chisq.test_output_2$statistic
chisq.test_output_2$parameter
chisq.test_output_2$p.value

row_1 <- c(50,30,20)
row_2 <- c(50,80,70)

data_rbind <- rbind(row_1,row_2)
data_rbind

raw_data <- c(50,30,20,50,80,70)
data_matrix <- matrix(raw_data,byrow=TRUE,nrow=2)
data_matrix

dimnames(data_matrix) <- list("Gender"=c("Boys","Girls"),
                              "TV_Preferences"=c("Pororo","JJangGu","RobotCar"))
data_matrix

addmargins(data_matrix)
prop.table(data_matrix)
addmargins(prop.table(data_matrix))

barplot(t(data_matrix),beside=TRUE,legend=TRUE,
        ylim=c(0,120),
        ylab="Observed frequencies in sample",
        main="TV viewing preferences by gender")

chisq.test(data_matrix)

row_1 <- c(1,2,3,4)
row_2 <- c(5,6,7,8)

data_rbind <- rbind(row_1,row_2)
data_rbind

colnames(data_rbind) <- paste("col_",1:4,sep="")
data_rbind

column_1 <- c(1,5)
column_2 <- c(2,6)
column_3 <- c(3,7)
column_4 <- c(4,8)

data_cbind <- cbind(column_1,column_2,column_3,column_4)
data_cbind

raw_data <- c(1,2,3,4,5,6,7,8)
data_matrix <- matrix(raw_data,byrow=TRUE,nrow=2)
data_matrix

dimnames(data_matrix) <- list("row"=c("row_1","row_2"),
                              "column"=c("col_1","col_2","col_3","col_4"))
data_matrix

data_index <- data_matrix[,2]
data_index

class(data_matrix)
class(data_index)

data_drop_false <- data_matrix[,2,drop=FALSE]
data_drop_false

class(data_drop_false)
