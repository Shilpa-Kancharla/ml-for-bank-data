# Evaluation on Test set

## SVM
* Error rate: 14.5%, AUC: 0.7563 (Basic, SMOTE)

## RandomForest
* Error rate: 11%, AUC: 0.67 (Modified, SMOTE)
* Error rate: 10.2%, AUC: TBA (Modified, without SMOTE)

## Boosting
Running on basic data set
* Error rate: 9.8%, AUC: .80(Modified, SMOTE)
* Error rate: 9.8%, AUC: .80 (Modified, without SMOTE)
Running on modified data set
* Error rate: 9.3%, AUC: 0.82 (Modified, without SMOTE)

## Bagging
* Error rate: 10.2%, AUC: TBA (Modified, without SMOTE)

  **Bagging Model I: using square root; Test Error: 10.13%; AUC: TBA
  
  **Bagging Model II: divide by 3; Test Error: 10.18%; AUC: TBA

## Logistic Regression
* Error rate: 10.14%, AUC: 0.764 (basic, No SMOTE)
* Error rate: 10.16%, AUC: 0.785 (Modified, No SMOTE)
