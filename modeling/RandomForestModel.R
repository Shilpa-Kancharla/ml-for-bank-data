library(randomForest)

######################################################################################################################################################
#Basic Model - preprocessing, class balancing , cross validation and prediction########################

#PREPROCESSING DATA
source("565_proj_func.R")
train <- read.csv("~/Documents/565Project/Data/basic_train.csv")
test <- read.csv("~/Documents/565Project/Data/basic_test.csv")

train <- augmented_dataframe(train)


#Grid search and cv on basic train and test set
#Cross Validation TO IDENTIFY OPTIMAL TREESIZE
treesize = c(4,5,6,7)
validation <- c(rep(0,length(treesize)))
auc_tree <- list(rep(0,5))

for (i in 1:4){
  results <- cross_validation_trees(train,treesize[i])
  validation[i] <- sum(results[[1]])/nrow(train)
  auc_tree[[i]] <- results[[3]]
}
print (validation)
print (auc_tree)
write.csv(auc_tree,file="basicmodelauc.csv")
write.csv(validation, file = "basicmodelvalidation.csv")

#USING CV RESULTS TO TRAIN MODEL ON ENTIRE TRAINING SET AND PREDICT ON TEST SET

train <- syn_data_borderline(train,5)

test <- augmented_dataframe(test)

modeldf <- rbind(train,test)

model <- randomForest(factor(y)~.,data=modeldf[1:nrow(train),],mtry=5,importance=TRUE)
library(pROC)
prediction_basic <- predict(model,test,type='response')
pred_table_basic <- table(prediction_basic,test$y)
rf.roc_bestmodel_basic <- roc(test$y,ifelse(prediction_basic==1,1,0))
area_under_curve_bestmodel_basic <- rf.roc_bestmodel_basic
save(rf.roc_bestmodel_basic,file="~/Documents/565Project/evaluation/comparison.R")
"The prediction accuracy is 0.896 and area under the curve is 0.663"
#########################################################################################################################################################

#Advanced Model - preprocessing, class balancing, cross-validation and prediction

#PREPROCESSING MODIFIED TRAINING DATA
modified_train <- read.csv("~/Documents/565Project/Data/modified_train.csv")
modified_test <- read.csv("~/Documents/565Project/Data/modified_test.csv")
modified_train <- augmented_dataframe_combodata(modified_train)
names(modified_train)[25] <- "default_no"
names(modified_train)[26] <- "default_unknown"
names(modified_train)[27] <- "housing_no"
names(modified_train)[28] <- "housing_unknown"

#USING MODIFIED TRAINING DATA TO PICK OPTIMAL TREE SIZE WITH CROSS VALIDATION
treesize = c(4,5,6,7)
validation_complexmodel <- c(rep(0,length(treesize)))
auc_tree_complexmodel <- list(rep(0,5))

z=0
for (i in 1:4){
  results <- cross_validation_trees(modified_train,treesize[i])
  validation_complexmodel[i] <- sum(results[[1]])/nrow(modified_train)
  auc_tree_complexmodel[[i]] <- results[[3]]
  
}
print (validation_complexmodel)
print (auc_tree_complexmodel)
  
write.csv(validation_complexmodel, file = "complexmodelvalidation.csv")

#USING CV TO BUILD A RF MODEL ON ENTIRE TRAINING SET AND PREDICT ON TEST SET 
modified_train <- syn_data_borderline(modified_train,5)
modified_test <- augmented_dataframe_combodata(modified_test)

names(modified_test)[25] <- "default_no"
names(modified_test)[26] <- "default_unknown"
names(modified_test)[27] <- "housing_no"
names(modified_test)[28] <- "housing_unknown"

dataframe <-rbind(modified_train,modified_test)
modified_model <- randomForest(factor(y)~.,data=dataframe[1:55093,],mtry=5,importance=TRUE)
prediction_modified <- predict(modified_model,modified_test,type='response')
pred_table_modified <- table(prediction_modified,modified_test$y)
rf.roc_bestmodel_modified <- roc(modified_test$y,ifelse(prediction_modified==1,1,0))
area_under_curve_bestmodel_modified <- rf.roc_bestmodel_modified 
save(rf.roc_bestmodel_modified,file="~/Documents/565Project/evaluation/comparison.R")

"Prediction accuracy is 0.891 and AUC is 0.6697"
##########################################################################################################################################################




