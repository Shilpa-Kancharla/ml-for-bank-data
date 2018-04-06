#library(gbm)
bank.train <- read.csv("C:\\Users\\tbian\\Documents\\GitHub\\565project\\data\\combolog.train.csv", sep ="")
bank.test <- read.csv("C:\\Users\\tbian\\Documents\\GitHub\\565project\\data\\combolog.test.csv", sep ="")
# bank.train <- read.csv("C:\\Users\\tbian\\Documents\\GitHub\\565project\\data\\basic_train.csv")
# bank.test<-read.csv("C:\\Users\\tbian\\Documents\\GitHub\\565project\\data\\basic_test.csv")
dim(bank.train)
dim(bank.test)


# opar<-par(lwd=2,cex=0.5,mfrow=c(5,4))
# for (i in 1:(length(bank.train)-1)){
# boxplot(as.factor(bank.train[,19])~bank.train[,i], ylab=colnames(bank.train)[2])
# }



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




log.all=glm(y~.[,-2] , data= bank.train, family= binomial)

summary(log.all)

dim(bank.test)

bank.probs=predict(log.all, newdata = bank.test, type="response")
bank.probs[1:10]
glm.pred=rep("yes",length(bank.test$y))
glm.pred[glm.probs>.5]="no"
table(glm.pred,bank.test$y)

mean(glm.pred==bank.test$y)
