library(gbm)
boost <- function(train,test,trees,maxshrinkage){
    train_error <- c(rep(1,30))
    test_error <- c(rep(1,30))
    grid=c(seq(0.01,maxshrinkage,0.01))
    j=0
    for (i in grid){
        j=j+1
        boost.tree.model <- gbm(y~.,data=train,distribution="bernoulli",n.trees=trees,interaction.depth=4,shrinkage=i)
        prediction_train <- predict(boost.tree.model,train,n.trees=trees,type='response')
        prediction_class <- ifelse(prediction_train>0.5,1,0)
        table <- table(prediction_class,train$y)
        train_error[j]= (mean(prediction_class!=train$y))
        
        prediction <- predict(boost.tree.model,test,n.trees=trees,type='response')
        prediction_class <- ifelse(prediction>0.5,1,0)
        
        table <- table(prediction_class,test$y)
        test_error[j] = (mean(prediction_class!=test$y))
        
    }
    return(list(train_error,test_error))
    
}
dummy_list <- c('job','marital','education','default','housing','loan','contact','month','day_of_week','poutcome')


augmented_dataframe <- function(dataframe){
    library(mlr)
    
    'Initilize the new dataframe'
    bank_aug <- data.frame(y=ifelse(dataframe$y=="yes",1,0))
    bank_aug$age <- dataframe$age
    
    'Create the dummy variables'
    job_dummy <- createDummyFeatures(dataframe$job)
    marital_dummy <- createDummyFeatures(dataframe$marital)
    education_dummy <- createDummyFeatures(dataframe$education)
    default_dummy <- createDummyFeatures(dataframe$default)
    housing_dummy <- createDummyFeatures(dataframe$housing)
    loan_dummy <- createDummyFeatures(dataframe$loan)
    contact_dummy <- createDummyFeatures(dataframe$contact)
    month_dummy <- createDummyFeatures(dataframe$month)
    day_of_week_dummy <- createDummyFeatures(dataframe$day_of_week)
    poutcome_dummy <- createDummyFeatures(dataframe$poutcome)
    
    'Adding dummy columns to the main dataframe'
    bank_aug <- cbind(bank_aug,job_dummy[,1:length(job_dummy)-1])
    bank_aug <- cbind(bank_aug,marital_dummy[,1:length(marital_dummy)-1])
    bank_aug <- cbind(bank_aug,education_dummy[,1:length(education_dummy)-1])
    bank_aug <- cbind(bank_aug,default_dummy[,1:length(default_dummy)])
    bank_aug <- cbind(bank_aug,housing_dummy[,1:length(housing_dummy)-1])
    bank_aug <- cbind(bank_aug,loan_dummy[,1:length(loan_dummy)-1])
    bank_aug <- cbind(bank_aug,contact_dummy[,1:length(contact_dummy)-1])
    bank_aug <- cbind(bank_aug,month_dummy[,1:length(month_dummy)-1])
    bank_aug <- cbind(bank_aug,day_of_week_dummy[,1:length(day_of_week_dummy)-1])
    bank_aug <- cbind(bank_aug,dataframe$campaign)
    bank_aug <- cbind(bank_aug,dataframe$pdays)
    bank_aug <- cbind(bank_aug,dataframe$previous)
    bank_aug <- cbind(bank_aug,poutcome_dummy[,1:length(poutcome_dummy)-1])
    bank_aug <- cbind(bank_aug,dataframe$emp.var.rate)
    bank_aug <- cbind(bank_aug,dataframe$cons.price.idx)
    bank_aug <- cbind(bank_aug,dataframe$cons.conf.idx)
    bank_aug <- cbind(bank_aug,dataframe$euribor3m)
    bank_aug <- cbind(bank_aug,dataframe$nr.employed)

   names(bank_aug)[4] <- "blue.collar"
   names(bank_aug)[9] <- "self.employed"
   
   names(bank_aug)[24] <- "default_no"
   
   names(bank_aug)[26] <- "housing_no"
   names(bank_aug)[27] <- "housing_unknown"
   names(bank_aug)[28] <- "loan_no"
   names(bank_aug)[29] <- "loan_unknown"
   names(bank_aug)[30] <- "contact_cellular"
   names(bank_aug)[44] <- "campaign"
   names(bank_aug)[45] <- "pdays"
   names(bank_aug)[46] <- "previous"
    
   names(bank_aug)[49] <- "emp.var.rate"
   names(bank_aug)[50] <- "cons.price.idx"
   names(bank_aug)[51] <- "cons.conf.idx"
   names(bank_aug)[52] <- "euribor3m" 
   names(bank_aug)[53] <- "nr.employed" 
   

   return (bank_aug)
}

#Dummy variable creation for combolog dataset
augmented_dataframe_combodata <- function(dataframe){
  library(mlr)
  
  'Initilize the new dataframe'
  bank_aug <- data.frame(y=ifelse(dataframe$y=="yes",1,0))
  bank_aug$age <- dataframe$age
  'Create the dummy variables'
  job_dummy <- createDummyFeatures(dataframe$job)
  marital_dummy <- createDummyFeatures(dataframe$marital)
  education_dummy <- createDummyFeatures(dataframe$education)
  default_dummy <- createDummyFeatures(dataframe$default)
  housing_dummy <- createDummyFeatures(dataframe$housing)
  loan_dummy <- createDummyFeatures(dataframe$loan)
  contact_dummy <- createDummyFeatures(dataframe$contact)
  month_dummy <- createDummyFeatures(dataframe$month)
  day_of_week_dummy <- createDummyFeatures(dataframe$day_of_week)
  poutcome_dummy <- createDummyFeatures(dataframe$poutcome)
  pdays_dummy <- createDummyFeatures(dataframe$cutpdays)
  'Adding dummy columns to the main dataframe'
  bank_aug <- cbind(bank_aug,job_dummy[,1:length(job_dummy)-1])
  bank_aug <- cbind(bank_aug,marital_dummy[,1:length(marital_dummy)-1])
  bank_aug <- cbind(bank_aug,education_dummy[,1:length(education_dummy)-1])
  bank_aug <- cbind(bank_aug,default_dummy[,1:length(default_dummy)])
  bank_aug <- cbind(bank_aug,housing_dummy[,1:length(housing_dummy)-1])
  bank_aug <- cbind(bank_aug,loan_dummy[,1:length(loan_dummy)-1])
  bank_aug <- cbind(bank_aug,contact_dummy[,1:length(contact_dummy)-1])
  bank_aug <- cbind(bank_aug,month_dummy[,1:length(month_dummy)-1])
  bank_aug <- cbind(bank_aug,day_of_week_dummy[,1:length(day_of_week_dummy)-1])
  bank_aug <- cbind(bank_aug,dataframe$campaign)
  bank_aug <- cbind(bank_aug,pdays_dummy[,1:length(pdays_dummy)-1])
  bank_aug <- cbind(bank_aug,dataframe$previous)
  bank_aug <- cbind(bank_aug,poutcome_dummy[,1:length(poutcome_dummy)-1])
  bank_aug <- cbind(bank_aug,dataframe$economic1)
  bank_aug <- cbind(bank_aug,dataframe$economic2)

  
  names(bank_aug)[4] <- "blue.collar"
  names(bank_aug)[9] <- "self.employed"
  names(bank_aug)[30] <- "contact_cellular"
  names(bank_aug)[44] <- "campaign"
  names(bank_aug)[48] <- "previous"
  names(bank_aug)[51] <- "economic1"
  names(bank_aug)[52] <- "economic2"
  
  return (bank_aug)
}

Principal_Component <- function(dataframe){
  "Subset of raw data with only the economic features"
  dataframe_features_subset <- data.frame("emp.var.rate"=dataframe$emp.var.rate,"cons.price.idx"=dataframe$cons.conf.idx,"cons.conf.idx"=dataframe$cons.conf.idx,"euribor3m"=dataframe$euribor3m,"nr.employed"=dataframe$nr.employed)
  
  "Create Principal Components of the economic features"
  dataframe_subset_princomp <- prcomp(dataframe_features_subset,center=TRUE,scale.=TRUE)
  
  "Create a new dataframe by removing the original economic features and replacing them with the principal components  "
  dataframe_new <- dataframe[, !(colnames(dataframe) %in% c("emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed"))]
  dataframe_new$PC1 <- dataframe_subset_princomp$x[,1]
  dataframe_new$PC2 <- dataframe_subset_princomp$x[,2]
  dataframe_new$PC3 <- dataframe_subset_princomp$x[,3]
  dataframe_new$PC4 <- dataframe_subset_princomp$x[,4]
  dataframe_new$PC5 <- dataframe_subset_princomp$x[,5]
  
  "Return the new dataframe"
  return (dataframe_new)
}


syn_data_borderline <- function(dataframe,k){
    library(smotefamily)
    set.seed(1005)
    data <- ADAS(dataframe[,2:ncol(dataframe)],dataframe[,1],K=k)
    newdf <- data.frame("y"=data$data$class)
    newdf <- cbind(newdf,data$data[,1:length(data$data)-1])
    return (newdf)
}





cross_validation_trees <- function(trainingdata,treesize){
  library(caret)
  library(randomForest)
  library(pROC)
  #Divide the data into 5 folds
  data_folds <- createFolds(trainingdata$y,k=5,list=FALSE)
  
  #Create different training sets
  data_train_fold1<- trainingdata[data_folds!=5,] 
  data_train_fold2<- trainingdata[data_folds!=1,] 
  data_train_fold3<- trainingdata[data_folds!=2,] 
  data_train_fold4<- trainingdata[data_folds!=3,] 
  data_train_fold5<- trainingdata[data_folds!=4,] 
  
  #Create different test sets
  test_data_fold1 <- trainingdata[data_folds==5,]
  test_data_fold2 <- trainingdata[data_folds==1,]
  test_data_fold3 <- trainingdata[data_folds==2,]
  test_data_fold4 <- trainingdata[data_folds==3,]
  test_data_fold5 <- trainingdata[data_folds==4,]
  
  #Create a vector to store number of correctly classified points at every step. 
  result_list <- c(rep(0,5))
  area_under_curve <- c(rep(0,5))
  trainlist <- c(rep(0,5))
  testlist <- c(rep(0,5))
  #Create list to store each training and test fold. The lists will be used inside a for loop to build different models.
  trainlist <- list(data_train_fold1,data_train_fold2,data_train_fold3,data_train_fold4,data_train_fold5)
  testlist <- list(test_data_fold1,test_data_fold2,test_data_fold3,test_data_fold4,test_data_fold5)
  for (i in 1:5){
    trainlist[[i]] <- syn_data_borderline(trainlist[[i]],5)
    randomforest_model <- randomForest(factor(y)~.,data=trainlist[[i]],mtry=treesize,importance=TRUE)
    prediction <- predict(randomforest_model,newdata=testlist[[i]],type='response')
    table <- table(prediction,testlist[[i]]$y)
    result_list[i] <- (table[1]+table[4])
    rf.roc <- roc(testlist[[i]]$y,ifelse(prediction==1,1,0))
    area_under_curve[i] <- rf.roc$auc[1]

  }
  return (list(result_list,randomforest_model,area_under_curve))
}


