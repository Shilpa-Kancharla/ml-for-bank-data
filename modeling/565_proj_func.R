library(gbm)
boost <- function(train,test,trees,maxshrinkage){
    train_error <- c(rep(1,30))
    test_error <- c(rep(1,30))
    grid=c(seq(0.01,maxshrinkage,0.01))
    j=0
    for (i in grid){
        j=j+1
        boost.tree.model <- gbm(y~.-default_no,data=train,distribution="bernoulli",n.trees=trees,interaction.depth=4,shrinkage=i)
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
    bank_aug <- cbind(bank_aug,default_dummy[,1:length(default_dummy)-1])
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
    
    names(bank_aug)[23] <- "default_no"
    names(bank_aug)[24] <- "default_unknown"
    names(bank_aug)[25] <- "housing_no"
    names(bank_aug)[26] <- "housing_unknown"
    names(bank_aug)[27] <- "loan_no"
    names(bank_aug)[28] <- "loan_unknown"
    names(bank_aug)[29] <- "contact_cellular"
    names(bank_aug)[43] <- "campaign"
    names(bank_aug)[44] <- "pdays"
    names(bank_aug)[45] <- "previous"
    names(bank_aug)[48] <- "emp.var.rate"
    names(bank_aug)[49] <- "cons.price.idx"
    names(bank_aug)[50] <- "cons.conf.idx"
    names(bank_aug)[51] <- "euribor3m"
    names(bank_aug)[52] <- "nr.employed"
    
    return (bank_aug)
}

syn_data_borderline <- function(dataframe,k){
    library(smotefamily)
    set.seed(1005)
    data <- ADAS(dataframe[,2:ncol(dataframe)],dataframe[,1],K=k)
    return (data$data)
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

