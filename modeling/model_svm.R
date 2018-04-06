# SVM Model

library("e1071")
library(pROC)
bank.train <- read.csv("~/Documents/STOR_565/Group project/565project/data/basic_train.csv")
combo.train <- read.csv("~/Documents/STOR_565/Group project/565project/data/combolog.train.csv", sep=" ")
set.seed(5)
sample.index <- sample(1:nrow(combo.train),floor(0.3*nrow(combo.train)),replace=FALSE)
train.sample <- combo.train[sample.index,]
test.sample <- combo.train[-sample.index,]
  
svm.linear <- svm(y ~ ., data = train.sample, kernel = "linear", cost = 0.1, probability = TRUE)


grid <- c(0.1,0.2,0.3)
set.seed(565)
start.time <- Sys.time()
auto.svm.linear.tune <- tune(svm, y ~ ., data = train.sample, 
                             kernel = "linear", ranges = list(cost = grid),
                             tunecontrol = tune.control(cross = 5))
stop.time <- Sys.time()
stop.time - start.time
plot(auto.svm.linear.tune)

svm.predicts <- predict(svm.linear, newdata = test.sample, probability = TRUE)
svm.err <- mean(svm.predicts != test.sample$y)

svm.roc <- roc(test.sample$y, attr(svm.predicts, "probabilities")[,1])
plot(svm.roc)
svm.roc$auc

save(svm.linear, 
     )