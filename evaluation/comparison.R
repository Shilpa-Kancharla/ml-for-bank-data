library(pROC)

model.evaluate <- function(pred.probs, test.y){
  ##### For testing ####
  #pred.probs <- svm.probs
  #test.y <- test.sample$y
  ######################
  
  preds <- factor(pred.probs < 0.5)
  levels(preds) <- c("no","yes")
  (error <- mean(preds != test.y))
  
  roc.perf <- roc(test.y, pred.probs)
  print(roc.perf$auc)
  plot(roc.perf)
  
}

# Read in Test data
bank.test <- read.csv("~/Documents/STOR_565/Group project/565project/data/basic_test.csv")

### SVM Evaluation ###
library("e1071")
load("~/Documents/STOR_565/Group project/565project/modeling/best.svm.R")
svm.predicts <- predict(best.svm$model, newdata = bank.test, probability = TRUE)
svm.probs <- attr(svm.predicts, "probabilities")[,1]
(svm.err <- mean(svm.predicts != bank.test$y))
svm.roc <- roc(bank.test$y, attr(svm.predicts, "probabilities")[,1])
plot(svm.roc)
svm.roc$auc



