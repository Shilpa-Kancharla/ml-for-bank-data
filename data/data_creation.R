library(caret)

# This line will need to be changed to the appropriate directory"
bank.full <- read.csv("~/Documents/STOR_565/Group project/565project/data/bank-additional-full.csv", sep=";")

# Remove duration column
bank.full <- bank.full[bank.full$default != 'yes',-11]

# Create pdays
pdays <- bank.full$pdays

#Create height categories of A = 0-7, B=7-14, C=14-998, D=998+
cutpdays <- cut(pdays, breaks=c(-1,7,14,998,999), labels=c("A", "B", "C", "D"), right=TRUE)
bank.full$cutpdays <- cutpdays

# Split data into training and test set
set.seed(565)
train.index <- createDataPartition(bank.full$y, p = .75, list = FALSE)
train <- bank.full[ train.index,]
test  <- bank.full[-train.index,]

# Check to make sure class proportions have been maintained
mean(bank.full$y == 'yes')
mean(train$y == 'yes')
mean(test$y == 'yes')


Principal_Component <- function(dataframe){
  "Subset of raw data with only the economic features"
  dataframe_features_subset <- data.frame("emp.var.rate"=dataframe$emp.var.rate,"cons.price.idx"=dataframe$cons.conf.idx,"cons.conf.idx"=dataframe$cons.conf.idx,"euribor3m"=dataframe$euribor3m,"nr.employed"=dataframe$nr.employed)
  
  "Create Principal Components of the economic features"
  dataframe_subset_princomp <- prcomp(dataframe_features_subset,center=TRUE,scale.=TRUE)
  
  "Create a new dataframe by removing the original economic features and replacing them with the principal components  "
  dataframe_new <- dataframe[, !(colnames(dataframe) %in% c("emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed"))]
  dataframe_new$economic1 <- dataframe_subset_princomp$x[,1]
  dataframe_new$economic2 <- dataframe_subset_princomp$x[,2]
  
  "Return the new dataframe"
  return (dataframe_new)
}

train.pca <- Principal_Component(train)
test.pca <- Principal_Component(test)

# Output basic train/test
write.csv(train[,!(colnames(train) %in% c("cutpdays"))], "~/Documents/STOR_565/Group project/565project/data/basic_train.csv", row.names = FALSE)
write.csv(test[,!(colnames(test) %in% c("cutpdays"))], "~/Documents/STOR_565/Group project/565project/data/basic_test.csv", row.names = FALSE)

# Output changed train/test
write.csv(train.pca[,!(colnames(train.pca) %in% c("pdays"))], "~/Documents/STOR_565/Group project/565project/data/modified_train.csv", row.names = FALSE)
write.csv(test.pca[,!(colnames(test.pca) %in% c("pdays"))], "~/Documents/STOR_565/Group project/565project/data/modified_test.csv", row.names = FALSE)
