library(arules)

data("Epub")
summary(Epub)

## check itemsets in sparse matrix
inspect(Epub[1:10])

## support per item: itemFrequency()
itemFrequency(Epub[,1:10])

## item frequency plot : itemFrequentPlot()
itemFrequencyPlot(Epub, support = 0.01, main = "item frequency plot above support 1%")

## item frequency plot top 30: itemFrequencyPlot(,topN)
itemFrequencyPlot(Epub, topN = 30, main = "support top 30 items")

# matrix diagram : image()
image(sample(Epub, 500, replace = FALSE), main = "matrix diagram")

## association rule analysis : apriori()
Epub_rule <- apriori(data = Epub,
                     parameter = list(support = 0.01,
                                      confidence = 0.20,
                                      minlen = 2))
# re-setting minimum support from 0.01 to 0.001
Epub_rule_2 <- apriori(data = Epub,
                       parameter = list(support = 0.001,
                                        confidence = 0.20,
                                        minlen = 2))
Epub_rule_2

summary(Epub_rule_2)

# inspection of 1~20 association rules : inspect()
inspect(Epub_rule_2[1:20])

# sorting association rules by lift : sort(, by = "lift")
inspect(sort(Epub_rule_2, by = "lift")[1:20])

# sorting association rules by support : sort(, by = "support")
inspect(sort(Epub_rule_2, by = "support")[1:20])

# subset of association rules : subset()
rule_interest <- subset(Epub_rule_2, items %in% c("doc_72f","doc_4ac"))
inspect(rule_interest)

# subset with left-hand side item : subset(lhs %in% "item")
rule_interest_lhs <- subset(Epub_rule_2,lhs %in% c("doc_72f", "doc_4ac"))
inspect(rule_interest_lhs)

# partial subset : %pin%
rule_interest_pin <- subset(Epub_rule_2, items %pin% c("60e"))
inspect(rule_interest_pin)

rule_interest_lhs_ain <- subset(Epub_rule_2,lhs %ain% c("doc_6e8","doc_6e9"))
inspect(rule_interest_lhs_ain)

# partial subset with confidence condition : %pin%, confidence
rule_interest_pin_conf <- subset(Epub_rule_2, items %pin% c("60e") & confidence > 0.25)
inspect(rule_interest_pin_conf)

library(arulesViz)

#scatter plot of association rules
plot(Epub_rule_2)

# grouped matrix for association rules
plot(sort(Epub_rule_2, by = "support")[1:20], method = "grouped")

# Graph for association rules
plot(Epub_rule_2,method = "graph", control = list(type = "items"))

# changing font size(vertex.label.cex), arrow
plot(Epub_rule_2, method = "graph",
     contorl = list(type = "items"),
     vertex.label.cex = 0.7,
     edge.arrow.size = 0.3,
     edge.arrow.width = 2)
plot(Epub_rule_2[55:65], method = "graph", control = list(type="items"))
plot(Epub_rule_2[55:65], method = "graph", control = list(type="itemsets"))

# saving in CSV format : write()
write(Epub_rule_2,
      file = "C:/Users/user/Desktop/밀화부리/R/Epub_rule.csv",
      sep = ",",
      quote = TRUE,
      row.names = FALSE)

# transforming into data.frame
Epub_rule_df <- as(Epub_rule_2, "data.frame")
str(Epub_rule_df)

# IS(Interest-Support) measure = sqrt(lift(A,B)*support(A,B))
Epub_rule_df <- transform(Epub_rule_df,IS = sqrt(lift*support))
Epub_rule_df[order(-Epub_rule_df$IS),][1:10,]

## List -> transactions 자료로 변환
tr_list <- list(c("a","b"),
                   c("a","c"),
                   c("b","c","d"),
                   c("a","e"),
                   c("c","d","e")
)
# set transaction names
names(tr_list) <- paste("tr",c(1:5),sep = "_")
tr_list

tr <- as(tr_list,"transactions")
tr
summary(tr)

## Matrix ->  transactions 자료로 변환
tr_matrix <- matrix(c(1,1,0,0,0,
                      1,0,1,0,0,
                      0,1,1,1,0,
                      1,0,0,0,1,
                      0,0,1,1,1),
                    ncol = 5)
# set dim names
dimnames(tr_matrix) <- list(c("a","b","c","d","e"),
                            paste("tr",c(1:5),sep = "_"))
tr_matrix

# coerce into transactions
tr2 <- as(tr_matrix,"transactions")
tr2
summary(tr2)

## data.frame -> transactions 자료로 변환
tr_dataframe <- data.frame(
  age = as.factor(c("30대","20대","30대","40대","10대")),
                  grade = as.factor(c("A","B","A","A","C")))

# coerce into transactions
tr3 <- as(tr_dataframe,"transactions")
tr3

summary(tr3)

##-- making data.frame
transactionID <- c(rep("tr1",3),rep("tr2",4))
itemID <- c("item1","item2","item3","item1","item2","item4","item5")
tr_df <- data.frame(transactionID,itemID)
str(tr_df)

## converting data.frame to transactions format
tr <- as(split(tr_df[,"itemID"],tr_df[,"transactionID"]),"transactions")
inspect(tr)





