#library(gbm)
library(glmnet)
bank.train <- read.csv("C:\\Users\\tbian\\Documents\\GitHub\\565project\\data\\combolog.train.csv", sep ="")
bank.test <- read.csv("C:\\Users\\tbian\\Documents\\GitHub\\565project\\data\\combolog.test.csv", sep ="")
bank.train <- read.csv("C:\\Users\\tbian\\Documents\\GitHub\\565project\\data\\basic_train.csv")
# bank.test<-read.csv("C:\\Users\\tbian\\Documents\\GitHub\\565project\\data\\basic_test.csv")
dim(bank.train)
dim(bank.test)

###################################### data ##########################################################
source("565_proj_func.R")

bank.dummy=augmented_dataframe(bank.train)
head(bank.dummy)
bank.smote=syn_data_borderline(bank.dummy, k=5)
set.seed(1005)
bank.smote <- ADAS(bank.dummy[,2:ncol(bank.dummy)],bank.dummy[,1],K=5)
dim(bank.smote$data)
str(bank.smote$data)
bank.PCA<-Principal_Component(bank.dummy)

#################### generate test dataset###################
bank.dummy.t=augmented_dataframe(bank.test)
head(bank.dummy.t)
bank.smote.t=syn_data_borderline(bank.dummy.t, k=5)
set.seed(1005)
bank.smote.t <- ADAS(bank.dummy.t[,2:ncol(bank.dummy.t)],bank.dummy.t[,1],K=5)
dim(bank.smote.t$data)
str(bank.smote.t$data)
bank.PCA<-Principal_Component(bank.dummy.t)



################################## logistic #####################################################################


x.matrix=model.matrix(~.,bank.smote$data[,-52])[,-1]
dim(x.matrix)

foldid=sample(1:2,size=length(bank.smote$class),replace=TRUE)

start=Sys.time()
bank.lasso<-cv.glmnet(x.matrix,bank.smote$data$class, family="binomial", type.measure="class", alpha=1)
stop=Sys.time()

stop-start

summary(bank.lasso)
plot(bank.lasso[-10])

log(bank.lasso$lambda.1se)


fit<-glmnet(x.matrix,bank.smote$data$class, family="binomial", alpha=1,lambda = bank.lasso$lambda.1se)

logistic.pridict<-predict (fit, newdata = bank.test, type="response")


#foldid=sample(1:10,size = length(bank.train$y) )

#cv.glmnet(x.matrix,bank.full$y, family=binomial, type.measure = "class",alpha=1)





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
