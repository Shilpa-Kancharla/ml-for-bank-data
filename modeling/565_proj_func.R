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

augmented <- function(dataframe){
  bank_aug <- data.frame(y=ifelse(dataframe$y=="yes",1,0))
  bank_aug$single <- ifelse(dataframe$marital=='single',1,0)
  bank_aug$married <- ifelse(dataframe$marital=='married',1,0)
  bank_aug$default_no<- ifelse(dataframe$default=='no',1,0)
  bank_aug$education_unidegree<- ifelse(dataframe$education=='university.degree',1,0)
  bank_aug$education_prof_cert<- ifelse(dataframe$education=='professional.course',1,0)
  bank_aug$hous_loan_yes<- ifelse(dataframe$housing=='yes',1,0)
  bank_aug$empvar <- scale(dataframe$emp.var.rate,center=TRUE,scale=TRUE)
  bank_aug$price <- scale(dataframe$cons.price.idx,center=TRUE,scale=TRUE)
  bank_aug$conf <- scale(dataframe$cons.conf.idx,center=TRUE,scale=TRUE)
  bank_aug$euribor <- scale(dataframe$euribor3m,center=TRUE,scale=TRUE)
  bank_aug$nremployed <- scale(dataframe$nr.employed,center=TRUE,scale=TRUE)
  return (bank_aug)
}

syn_data_borderline <- function(dataframe,k){
  library(smotefamily)
  set.seed(1005)
  data <- ADAS(dataframe[,2:ncol(dataframe)],dataframe[,1],K=k)
  return (data$data)
}