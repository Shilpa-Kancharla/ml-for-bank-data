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
treesize = c(5,6,7,8)
validation <- c(rep(0,length(treesize)))
for (i in 1:4){
  results <- cross_validation_trees(train,treesize[i])
  validation[i] <- sum(results[[1]])/nrow(train)
}
print (validation)

write.csv(validation, file = "basicmodelvalidation.csv")

#USING CV RESULTS TO TRAIN MODEL ON ENTIRE TRAINING SET AND PREDICT ON TEST SET

train <- syn_data_borderline(train,5)

test <- augmented_dataframe(test)

modeldf <- rbind(train,test)

model <- randomForest(factor(y)~.,data=modeldf[1:nrow(train),],mtry=5,importance=TRUE)

prediction <- predict(model,test,type='response')
pred_table <- table(prediction,test$y)

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
treesize = c(5,6,7,8)
validation_complexmodel <- c(rep(0,length(treesize)))
z=0
for (i in 1:4){
  results <- cross_validation_trees(modified_train,treesize[i])
  validation_complexmodel[i] <- sum(results[[1]])/nrow(modified_train)
}
print (validation_complexmodel)
  
write.csv(validation_complexmodel, file = "complexmodelvalidation.csv")

#USING CV TO BUILD A RF MODEL ON ENTIRE TRAINING SET AND PREDICT ON TEST SET 
modified_train <- syn_data_borderline(modified_train,5)
modified_test <- augmented_dataframe_combodata(modified_test)

names(modified_test)[25] <- "default_no"
names(modified_test)[26] <- "default_unknown"
names(modified_test)[27] <- "housing_no"
names(modified_test)[28] <- "housing_unknown"

dataframe <-rbind(modified_train,modified_test)
model <- randomForest(factor(y)~.,data=dataframe[1:55093,],mtry=5,importance=TRUE)
prediction <- predict(model,modified_test,type='response')

##########################################################################################################################################################




