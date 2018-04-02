library(gbm)

bank.train <- read.csv("~/Documents/STOR_565/Group project/565project/data/basic_train.csv")

# Make 0/1 factor for gbm package
bank.train$y <- as.numeric((bank.train$y == 'y')*1)

boost.bank = gbm(y~.,data=bank.train, distribution= "bernoulli", n.trees=100,
             shrinkage=0.01, verbose =F, cv.folds = 5)

