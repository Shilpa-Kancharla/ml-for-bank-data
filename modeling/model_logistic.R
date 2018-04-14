library(glmnet)
library(pROC)
bank.train.mod <- read.csv("C:\\Users\\tbian\\Documents\\GitHub\\565project\\data\\modified_train.csv")
bank.test.mod <- read.csv("C:\\Users\\tbian\\Documents\\GitHub\\565project\\data\\modified_test.csv")
bank.train <- read.csv("C:\\Users\\tbian\\Documents\\GitHub\\565project\\data\\basic_train.csv")
bank.test <- read.csv("C:\\Users\\tbian\\Documents\\GitHub\\565project\\data\\basic_test.csv")




######################################  train basic_ works good  ##########################################################################
str(bank.train)
str(bank.test)
dim(bank.train)

x.matrix.b<-model.matrix(~.,bank.train[,-20])[,-1]
x.test.b<-model.matrix(~.,bank.test[,-20])[,-1]
y.test.b=bank.test$y

foldid=sample(1:2,size=length(bank.train$y),replace=TRUE)

start=Sys.time()
bank.lasso.b<-cv.glmnet(x.matrix.b,bank.train$y, family="binomial", type.measure="class", alpha=1)
stop=Sys.time()

stop-start
plot(bank.lasso.b)
fit.b<-glmnet(x.matrix.b,bank.train$y, family="binomial", alpha=1,lambda = bank.lasso.b$lambda.1se)

logistic.predict.b<-predict (fit.b, newx = x.test.b , type="response")
log.pre.b<-ifelse(logistic.predict.b<0.5,0,1)
y.test.b<-ifelse(y.test.b=='no',0,1)

table(y.test.b, log.pre.b)
1-mean(y.test.b==log.pre.b)  #####[1] 0.1012044

log.roc.b <- roc(y.test.b, as.numeric(logistic.predict.b))
plot(log.roc.b)
log.roc.b$auc


########################################## modified_ works good ########################################################################

str(bank.train.mod)
str(bank.test.mod)
dim(bank.train.mod)

x.matrix.m<-model.matrix(~.,bank.train.mod[,-14])[,-1]
x.test.m<-model.matrix(~.,bank.test.mod[,-14])[,-1]
y.test.m=bank.test.mod$y
dim(x.matrix.m)

foldid.m=sample(1:2,size=length(bank.train.mod$y),replace=TRUE)

start=Sys.time()
bank.lasso.m<-cv.glmnet(x.matrix.m,bank.train.mod$y, family="binomial", type.measure="class", alpha=1)
stop=Sys.time()

stop-start
plot(bank.lasso.m)
fit.m<-glmnet(x.matrix.m,bank.train.mod$y, family="binomial", alpha=1,lambda = bank.lasso.m$lambda.1se)


min(bank.lasso.m$cvm)    ######[1] 0.1004241

logistic.predict.m<-predict (fit.m, newx = x.test.m , type="response")
log.pre.m<-ifelse(logistic.predict.m<0.5,0,1)
y.test.m<-ifelse(y.test.m=="no",0,1)
table(y.test.m, log.pre.m)
1-mean(y.test.m==log.pre.m)   #####[1] 0.1007187



log.roc.m <- roc(y.test.m, as.numeric(logistic.predict.m))
plot(log.roc.m)
log.roc.m$auc



