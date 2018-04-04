#library(gbm)
bank.train <- read.csv("C:\\Users\\tbian\\Documents\\GitHub\\565project\\combolog.train.csv", sep ="")
head(bank.train)
summary(bank.train)
# Make 0/1 factor 
bank.train$y <- as.factor(ifelse(bank.train$y =="yes",1,0))
summary(bank.train)

logistic.bank = glm(y~.,data=bank.train, family= "binomial")

#   Warning message:
#   glm.fit: algorithm did not converge

summary(logistic.bank)
