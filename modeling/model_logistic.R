library(glmnet)
bank.train.mod <- read.csv("C:\\Users\\tbian\\Documents\\GitHub\\565project\\data\\modified_train.csv")
bank.test.mod <- read.csv("C:\\Users\\tbian\\Documents\\GitHub\\565project\\data\\modified_test.csv")
bank.train <- read.csv("C:\\Users\\tbian\\Documents\\GitHub\\565project\\data\\basic_train.csv")
bank.test <- read.csv("C:\\Users\\tbian\\Documents\\GitHub\\565project\\data\\basic_test.csv")

dim(bank.train)
dim(bank.test)

###################################### data ##########################################################
source("565_proj_func.R")

bank.dummy=augmented_dataframe(bank.train)
dim(bank.dummy)
#bank.smote=syn_data_borderline(bank.PCA, k=5)
set.seed(1005)
bank.smote <- ADAS(bank.dummy[,2:ncol(bank.dummy)],bank.dummy[,1],K=5)
dim(bank.smote$data)
str(bank.smote$data)
#bank.PCA<-Principal_Component(bank.dummy)
#dim(bank.PCA)

#################### generate test dataset###################
bank.dummy.t=augmented_dataframe(bank.test)
dim(bank.dummy.t)
str(bank.dummy.t)
#bank.smote.t=syn_data_borderline(bank.dummy.t, k=5)
#set.seed(1005)
#bank.smote.t <- ADAS(bank.dummy.t[,2:ncol(bank.dummy.t)],bank.dummy.t[,1],K=5)
#dim(bank.smote.t$data)
#str(bank.smote.t$data)
#bank.PCA.t<-Principal_Component(bank.dummy.t)
#dim(bank.PCA.t)


################################## logistic #####################################################################


x.matrix=model.matrix(~.,bank.smote$data[,-53])[,-1]
x.test=model.matrix(~.,bank.dummy.t[,-1])[,-1]
y.test=bank.dummy.t$y

dim(x.matrix)

foldid=sample(1:2,size=length(bank.smote$class),replace=TRUE)

start=Sys.time()
bank.lasso<-cv.glmnet(x.matrix,bank.smote$data$class, family="binomial", type.measure="class", alpha=1)
stop=Sys.time()

stop-start

summary(bank.lasso)
plot(bank.lasso)

fit<-glmnet(x.matrix,bank.smote$data$class, family="binomial", alpha=1,lambda = bank.lasso$lambda.1se)

logistic.predict<-predict (fit, newx = x.test , type="response")
log.pre<-ifelse(logistic.predict>=0.5,1,0)

dim(log.pre)
dim(y.test)
table(y.test, log.pre)


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
log.pre.b<-ifelse(logistic.predict.b<0.5,1,2)

table(y.test.b, log.pre.b)
1-mean(as.numeric(y.test.b)==log.pre.b)  #####[1] 0.1012044
log.roc <- roc(y.test.b, attr(logistic.predict.b, "probabilities")[,1])
plot(log.roc)
log.roc$auc


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

logistic.predict.m<-predict (fit.m, newx = x.test.m , type="response")
log.pre.m<-ifelse(logistic.predict.m<0.5,"no","yes")

table(y.test.m, log.pre.m)
1-mean(y.test.m==log.pre.m)   #####[1] 0.1007187
log.roc <- roc(y.test.b, attr(logistic.predict.b, "probabilities")[,1])
plot(log.roc)
log.roc$auc




##################################################################################################################
# Make 0/1 factor 
#bank.train$y <- ifelse(bank.train$y =="yes",1,0)
summary(bank.train)
bank.name=names(bank.train)
p=0
for (i in 1:(length(bank.train)-1)){
logistic.bank = glm(bank.train$y~bank.train[,i], data= bank.train, family= binomial)
p[i]=coef(summary(logistic.bank))[2,4]
#print(bank.name[i])
print(paste("*******************",i,bank.name[i],"**********************************"))
print(coef(summary(logistic.bank)))
}




log.all=glm(y~.-bank.train[,2] , data= bank.train, family= binomial)

summary(log.all)

dim(bank.test)

bank.probs=predict(log.all, newdata = bank.test, type="response")
bank.probs[1:10]
glm.pred=rep("yes",length(bank.test$y))
glm.pred[glm.probs>.5]="no"
table(glm.pred,bank.test$y)

mean(glm.pred==bank.test$y)


svm.predicts <- predict(svm.linear, newdata = test.sample, probability = TRUE)
svm.err <- mean(svm.predicts != test.sample$y)

svm.roc <- roc(test.sample$y, attr(svm.predicts, "probabilities")[,1])
plot(svm.roc)
svm.roc$auc

