library(caret)

# This line will need to be changed to the appropriate directory"
bank.full <- read.csv("~/Documents/STOR_565/Group project/565project/data/bank-additional-full.csv", sep=";")

# Remove duration column
bank.full <- bank.full[,-11]

# Split data into training and test set
set.seed(565)
train.index <- createDataPartition(bank.full$y, p = .75, list = FALSE)
train <- bank.full[ train.index,]
test  <- bank.full[-train.index,]

# Check to make sure class proportions have been maintained
mean(bank.full$y == 'yes')
mean(train$y == 'yes')
mean(test$y == 'yes')

write.csv(train, "~/Documents/STOR_565/Group project/565project/data/basic_train.csv", row.names = FALSE)
write.csv(test, "~/Documents/STOR_565/Group project/565project/data/basic_test.csv", row.names = FALSE)
