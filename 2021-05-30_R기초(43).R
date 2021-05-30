# categorical data -> binarization -> association rule analysis

# vector -> cbind -> data.frame
cust_id <- c(1,2,3,4,5,6)
gender <- c("FEMALE","MALE","FEMALE","FEMALE","MALE","FEMALE")
age <- c(23,28,42,34,45,36)
child_prd_yn <- c("NO","NO","NO","YES","NO","YES")
mobile_app_use <- c("YES","YES","NO","YES","NO","YES")
re_order <- c("YES","NO","NO","YES","NO","YES")

cust_mart <- cbind(cust_id,gender,age,child_prd_yn,mobile_app_use,re_order)
cust_mart <- as.data.frame(cust_mart)
sapply(cust_mart,class)

# cust_id : factor -> character
# age : factor -> numeric
cust_mart <- transform(cust_mart,
                       cust_id = as.character(cust_id),
                       age = as.numeric(age))
sapply(cust_mart,class)

# age : custinuous data -> discretization
cust_mart <- within(cust_mart, {
   age_cd = character(0)
   age_cd[ age <= 29 ] = "age_20"
   age_cd[ age > 29 & age <= 39 ] = "age_30"
   age_cd[ age > 39 ] = "age_40"
   age_cd = factor(age_cd, level = c("age_20", "age_30", "age_40"))
})

# dataset for association rule : (1) deleting 'cust_id','age'
cust_mart_ar <- subset(cust_mart, select = -c(cust_id,age))
str(cust_mart_ar)

# dataset for association rule : (2) transaction data format
library(arules)


cust_mart_ar_tr <- as(cust_mart_ar,"transactions")
str(cust_mart_ar_tr)

# association rule generation
cust_mart_ar_tr_rule <- apriori(cust_mart_ar_tr,
                                parameter = list(support=0.3,confidence = 0.5))

# association rule inspection
inspect(head(cust_mart_ar_tr_rule,20)) # head 20 rules

# subset : right-hand sided in "re_order = YES" & lift >=2
cust_mart_ar_tr_rule_reorder <- subset(cust_mart_ar_tr_rule,
                                       subset = rhs %in% "re_order=YES" & lift >=2)

inspect(cust_mart_ar_tr_rule_reorder)
