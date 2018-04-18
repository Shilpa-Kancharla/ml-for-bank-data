---
title: "Decision Forest"
output: html_notebook
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Cmd+Shift+Enter*. 

```{r}
#modified_train.csv is the training dataset
#modified_test.csv is the test dataset
library(tree)
bank.train <- tree(y ~., data=modified_train)
summary(bank.train)
plot(bank.train)
text(bank.train, pretty=0)
```
```{r}
bank.train
bank.pred <- predict(bank.train, modified_test, type="class")
table(bank.pred, modified_test$y)
```
```{r}
1 - (9058 + 191)/10296
```
The test error rate is about 10.17%. 

```{r}
cv.bank <- cv.tree(bank.train, FUN = prune.misclass)
cv.bank
```
```{r}
plot(cv.bank$size, cv.bank$dev, type="b", xlab="Tree size", ylab="Deviance")
```
We see that node 3 is the smallest tree with the lowest classification error rate.

```{r}
prune.bank <- prune.misclass(bank.train, best=2)
plot(prune.bank)
text(prune.bank, pretty=0)
```
```{r}
summary(bank.train)
summary(prune.bank) #compare this to summary(tree.train)
```
The misclassification error rate is for the classification tree with and without pruning are the same.
```{r}
prune.pred <- predict(prune.bank, modified_test, type="class")
table(prune.pred, modified_test$y)
```
```{r}
1 - (9058 + 191)/10296
mode(modified_train)
mode(lambdas)
mode(bank.train.err)
```
The pruning process did not increase the test error rate, and it produced a more interpretable tree. (But the misclassification error rate is increased)
```{r}
library(gbm)
powers <- seq(-5, -0.2, by=0.1)
lambdas <- factor(10^powers)
bank.train.err <- rep(NA, length(lambdas))
train.matrix <- model.matrix(~ ., data=modified_train)[,-1]
for (i in 1:length(lambdas)) {
  bank.boost.train <- gbm(y ~ ., data = modified_train, distribution = "gaussian", n.trees = 1000, shrinkage = lambdas[i])
  bank.pred.train <- predict(bank.boost.train, modified_train, n.trees = 1000)
  diff <- as.numeric(bank.pred.train) - as.numeric(modified_train$y)
  bank.train.err[i] <- mean((diff)^2)
}
plot(lambdas, bank.train.err, type="b", xlab="Shrinkage Values", ylab="Training MSE")
```
```{r}
bank.test.err <- rep(NA, length(lambdas))
for (i in 1:length(lambdas)) {
  bank.boost.test <- gbm(y ~ ., data=modified_train, distribution = "gaussian", n.trees = 1000, 
                    shrinkage = lambdas[i])
  yhat <- predict(bank.boost.test, modified_test, n.trees = 1000)
  diff2 <- as.numeric(yhat) - as.numeric(modified_test$y)
  bank.test.err[i] <- mean((diff2)^2)
}
plot(lambdas, bank.test.err, type="b", xlab="Shrinkage Values", ylab="Test MSE")
```
```{r}
min(bank.test.err)
lambdas[which.min(bank.test.err)]
```
```{r}
library(gbm)
library(randomForest)
boost.bank <- gbm(y ~ ., data = modified_train, distribution = "gaussian", n.trees=1000,
                  shrinkage = lambdas[which.min(bank.test.err)])
summary(boost.bank)
```
```{r}
library(randomForest)
library(pROC)
bank.bag <- randomForest(y ~ ., data = modified_train, mtry = 10, ntree = 500)
yhat.bag <- predict(bank.bag, newdata = modified_test, type= "response")
yhat.bag[1:5]
mean((as.numeric(yhat.bag) - as.numeric(modified_test$y))^2)
pred_table <- table(yhat.bag, modified_test$y)
pred_table
bag.roc_basic <- roc(modified_test$y, ifelse(yhat.bag==1,1,0))
auc_basic <- bag.roc_basic
auc_basic
plot(bag.roc_basic)
bag.roc_basic$auc
```
The test MSE for bagging is 10.2%.
```{r}
bag.roc_basic.yes <- roc(modified_test$y, ifelse(yhat.bag=="yes",1,0))
bag.roc_basic.yes
plot(bag.roc_basic.yes)
bag.roc_basic.yes$auc
```

```{r}
varImpPlot(bank.bag)
```
```{r}
set.seed(1)
bank.bag1 <- randomForest(y ~ ., data=modified_train, mtry = sqrt(20), ntree=500)
bank.bag1
bank.bag2 <- randomForest(y ~ ., data=modified_train, mtry = 20/3, ntree= 500)
bank.bag2
varImpPlot(bank.bag1)
varImpPlot(bank.bag2)
```
In the first model, we see economic2 is the most important predictor. In the second model, we see age is the most important predictor.


```{r}
library(randomForest)
library(pROC)
yhat.bag1 <- predict(bank.bag1, newdata = modified_test) #prediction
mean((as.numeric(yhat.bag1) - as.numeric(modified_test$y))^2)
pred_table1 <- table(yhat.bag1, modified_test$y)
bag1.roc_basic <- roc(modified_test$y, ifelse(yhat.bag1=="yes",1,0))
auc_basic1 <- bag1.roc_basic
auc_basic1
plot(bag1.roc_basic)
bag1.roc_basic$auc


yhat.bag2 <- predict(bank.bag2, newdata = modified_test) #prediction
mean((as.numeric(yhat.bag2) - as.numeric(modified_test$y))^2)
pred_table2 <- table(yhat.bag2, modified_test$y)
bag2.roc_basic <- roc(modified_test$y, ifelse(yhat.bag2=="yes",1,0))
auc_basic2 <- bag2.roc_basic
auc_basic2
plot(bag2.roc_basic)
bag2.roc_basic$auc
```

Both models have very similar test MSE, but bank.bag1 has a slightly lower test error than bank.bag2.

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Cmd+Option+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Cmd+Shift+K* to preview the HTML file). 

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.

