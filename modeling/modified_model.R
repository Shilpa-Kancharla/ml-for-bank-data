
# running training and test on modified data
library(gbm)
#setwd("C:/Users/sunhwa/Documents/Spring2018/STOR565ML/group project/565project")
setwd("~/Documents/STOR_565/Group project/565project")

source("modeling/565_proj_func.R")
#PREPROCESSING MODIFIED TRAINING DATA
modified_test <-read.csv("data/modified_test.csv")
modified_train<-read.csv("data/modified_test.csv")

modified_test <- augmented_dataframe_combodata(modified_test)
names(modified_test)[25] <- "default_no"
names(modified_test)[26] <- "default_unknown"
names(modified_test)[27] <- "housing_no"
names(modified_test)[28] <- "housing_unknown"

#modified.train.aug<-augmented_dataframe_combodata(modified.train)

modified_train <- augmented_dataframe_combodata(modified_train)
names(modified_train)[25] <- "default_no"
names(modified_train)[26] <- "default_unknown"
names(modified_train)[27] <- "housing_no"
names(modified_train)[28] <- "housing_unknown"
#Applying smote function
#modified_train <- syn_data_borderline(modified_train,5)### for applying smote, run on
#USING MODIFIED TRAINING DATA TO PICK OPTIMAL TREE SIZE WITH CROSS VALIDATION

idx2 = sample(1:nrow(modified_train), size=0.75*nrow(modified_train))
#Split data
subtrain.mod<-modified_train[idx2,]
validate.mod<-modified_train[-idx2,]
#####################smote
#USING CV TO BUILD A RF MODEL ON ENTIRE TRAINING SET AND PREDICT ON TEST SET 
#source("565_proj_func.r")
#subtrain.mod<- syn_data_borderline(subtrain.mod,5)### for applying smote, run on
#modified_test <- augmented_dataframe_combodata(modified_test)

#????????????????????????????ABORT ?????????????????????
boost.modified = gbm(y~.,data=modified_train, distribution= "bernoulli", n.trees=1000,
                     shrinkage=0.01, verbose =F, cv.folds = 5)
summary(boost.modified)
#????????????????????????????????????????????????????????????
modified.boot<-boost(subtrain.mod,validate.mod,1000,0.01)
#smote.test<-syn_data_borderline(bank.test.aug,k=5)
#bank.boot.test<-boost(subtrain,validate,1000,0.1)
model <- modified.boot[[3]]
save(modified.boot,file="model.smoter")
###### to predict ###########
prediction<-predict(boost.modified,modified_test,n.trees=1000,type='response')
#save(prediction,file="prediction_basic_test.csv")
prediction_class <- ifelse(prediction >0.5,1,0)
save(prediction,file="prediction_mod.smotecsv")

table <- table(prediction_class,modified_test$y)
save (table, file=("table_testerror.smote.mod.r"))
test_error = (mean(prediction_class!=modified_test$y))

#   prediction errors without smote             
#prediction_class    0    1
#0 9022  830
#1  114  330
# test_error
# 0.09294872
# Area under the curve: 0.8155
# prediction errors with smote
#### AUC graph##################
#install.packages("pROC")
#### without smote
library(pROC)
roc_obj <- roc(prediction_class, modified_test$y)
auc(roc_obj)
plot(roc_obj)
save(roc_obj,file="evaluation/best.boost.R" )


smote.mod<-syn_data_borderline(subtrain.mod,k=5)

boost.modified.smote = gbm(y~.,data=smote.mode, distribution= "bernoulli", n.trees=100,
                     shrinkage=0.01, verbose =F, cv.folds = 5)

modified.boot<-boost(subtrain.mod,validate.mod,1000,0.01)
#smote.test<-syn_data_borderline(bank.test.aug,k=5)
#bank.boot.test<-boost(subtrain,validate,1000,0.1)
model <- modified.boot[[3]]
save(modified.boot,file="model.r")
###### to predict ###########
prediction<-predict(model,modified_test,n.trees=1000,type='response')
#save(prediction,file="prediction_basic_test.csv")
prediction_class <- ifelse(prediction>0.5,1,0)
save(prediction,file="prediction_mod.csv")

table <- table(prediction_class,modified_test$y)
save (table, file=("table_testerror.mod.r"))
test_error = (mean(prediction_class!=bank.test.aug$y))


summary(modified_train)
summary(modified_test)
summary(modified.boot)

importance(modified.boot)



###AUC get this??????????????????????****************

dataframe <-rbind(modified_train,modified_test)
modified_model <- randomForest(factor(y)~.,data=dataframe[1:55093,],mtry=5,importance=TRUE)
prediction_modified <- predict(modified_model,modified_test,type='response')
pred_table_modified <- table(prediction_modified,modified_test$y)
rf.roc_bestmodel_modified <- roc(modified_test$y,ifelse(prediction_modified==1,1,0))
#area_under_curve_bestmodel_modified <- rf.roc_bestmodel_modified 

"Prediction accuracy is 0.891 and AUC is 0.6697"