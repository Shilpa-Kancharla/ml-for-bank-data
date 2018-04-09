#library(gbm)
library(glmnet)
bank.train <- read.csv("C:\\Users\\tbian\\Documents\\GitHub\\565project\\data\\combolog.train.csv", sep ="")
bank.test <- read.csv("C:\\Users\\tbian\\Documents\\GitHub\\565project\\data\\combolog.test.csv", sep ="")
bank.train <- read.csv("C:\\Users\\tbian\\Documents\\GitHub\\565project\\data\\basic_train.csv")
# bank.test<-read.csv("C:\\Users\\tbian\\Documents\\GitHub\\565project\\data\\basic_test.csv")
dim(bank.train)
dim(bank.test)

###################################### data ##########################################################
str(bank.train)
source("565_proj_func.R")

bank.dummy=augmented_dataframe(bank.train)
head(bank.dummy)
bank.smote=syn_data_borderline(bank.dummy, k=5)
set.seed(1005)
bank.smote <- ADAS(bank.dummy[,2:ncol(bank.dummy)],bank.dummy[,1],K=5)
dim(bank.smote$data)
str(bank.smote$data)
bank.PCA<-Principal_Component(bank.dummy)


# opar<-par(lwd=2,cex=0.5,mfrow=c(5,4))
# for (i in 1:(length(bank.train)-1)){
# boxplot(as.factor(bank.train[,19])~bank.train[,i], ylab=colnames(bank.train)[2])
# }
################################## logistic #####################################################################


x.matrix=model.matrix(~.,bank.smote$data[,-52])[,-1]
dim(x.matrix)

foldid=sample(1:3,size=length(bank.smote$class),replace=TRUE)
bank.lasso<-cv.glmnet(x.matrix,bank.smote$data$class, family="binomial", type.measure="class", alpha=1)

summary(bank.lasso)
plot(bank.lasso)
text(bank.lasso)


#foldid=sample(1:10,size = length(bank.train$y) )

#cv.glmnet(x.matrix,bank.full$y, family=binomial, type.measure = "class",alpha=1)




bank.x <- bank.full[,!colnames(bank.full) %in% c("duration","y")]
library(glmnet)
x.matrix <- model.matrix(~., bank.x)[,-1]
set.seed(1)
foldid=sample(1:10,size=length(bank.full$y),replace=TRUE)
glm.cv =cv.glmnet(x.matrix,bank.full$y,family="binomial",type.measure = "class",foldid=foldid,alpha=1)
plot(glm.cv)
text(glm.cv)




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
