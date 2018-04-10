# SVM Model

library("e1071")
library(pROC)
bank.train <- read.csv("~/Documents/STOR_565/Group project/565project/data/basic_train.csv")
mod.train <- read.csv("~/Documents/STOR_565/Group project/565project/data/modified_train.csv")

set.seed(5)
sample.index <- sample(1:nrow(bank.train),floor(0.3*nrow(bank.train)),replace=FALSE)
train.sample <- bank.train[sample.index,]
test.sample <- bank.train[-sample.index,]

grid <- c(0.1,0.2,0.3)
set.seed(565)
start.time <- Sys.time()
auto.svm.linear.tune <- tune(svm, y ~ ., data = train.sample, 
                             kernel = "linear", ranges = list(cost = grid),
                             tunecontrol = tune.control(cross = 5))
stop.time <- Sys.time()
stop.time - start.time
plot(auto.svm.linear.tune)

svm.linear <- svm(y ~ ., data = train.sample, kernel = "linear", cost = 0.1, probability = TRUE)
svm.predicts <- predict(svm.linear, newdata = test.sample, probability = TRUE)
svm.err <- mean(svm.predicts != test.sample$y)

svm.roc <- roc(test.sample$y, attr(svm.predicts, "probabilities")[,1])
plot(svm.roc)
svm.roc$auc


set.seed(5)
sample.index <- sample(1:nrow(mod.train),floor(0.3*nrow(mod.train)),replace=FALSE)
train.sample <- mod.train[sample.index,]
test.sample <- mod.train[-sample.index,]

grid <- c(0.1,0.5,1)
set.seed(565)
start.time <- Sys.time()
auto.svm.linear.tune <- tune(svm, y ~ ., data = train.sample, 
                             kernel = "linear", ranges = list(cost = grid),
                             tunecontrol = tune.control(cross = 5))
stop.time <- Sys.time()
stop.time - start.time
plot(auto.svm.linear.tune)

svm.linear <- svm(y ~ ., data = train.sample, kernel = "linear", cost = 0.1, probability = TRUE)
svm.predicts <- predict(svm.linear, newdata = test.sample, probability = TRUE)
svm.err <- mean(svm.predicts != test.sample$y)

svm.roc <- roc(test.sample$y, attr(svm.predicts, "probabilities")[,1])
plot(svm.roc)
svm.roc$auc

