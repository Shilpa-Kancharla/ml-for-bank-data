# SVM Model

library("e1071")


bank.train <- read.csv("~/Documents/STOR_565/Group project/565project/data/basic_train.csv")

svm.linear <- svm(y ~ ., data = bank.train, kernel = "linear", cost = 0.1)


grid <- c(0.01,0.1,1)
set.seed(565)
start.time <- Sys.time()
auto.svm.linear.tune <- tune(svm, y ~ ., data = bank.train, 
                             kernel = "linear", ranges = list(cost = grid),
                             tunecontrol = tune.control(cross = 10))
stop.time <- Sys.time()
plot(auto.svm.linear.tune)

