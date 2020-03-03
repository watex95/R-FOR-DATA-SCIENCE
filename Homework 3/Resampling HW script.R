# RESAMPLING SCRIPT


# RESAMPLING

suppressWarnings(library(tidyverse))
suppressWarnings(library(caret))
suppressWarnings(library(rpart.plot))
suppressWarnings(library(randomForest))
suppressWarnings(library(e1071))
suppressWarnings(library(ROCR))
suppressWarnings(library(pROC))
suppressWarnings(library(RCurl))
suppressWarnings(library(gbm))
library(rsample)   
library(purrr)
library(dplyr)
library(ggplot2)
library(scales)
library(mlbench)
library(kernlab)
library(sessioninfo)
library(RCurl)

# Get the German Credit dataset from the UCI machine learning repository

UCI_german<-getURL("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")

names<-c('checking_account','month','Credit_history','Purpose',
         'Credit_amount','Savings','employment','Installment_rate',
         'status','Other_debtors','Present_residence_since','Property',
         'Age','installment_plans','Housing','#credits','Job','#people',
         'Telephone','foreign','Cost_Matrix')

credit_data<-read.table(textConnection(UCI_german),sep=" ",
                        col.names=names)
head(credit_data)
str(credit_data)
credit_data$Cost_Matrix=as.factor(credit_data$Cost_Matrix)





# Fit a SVM model to predict the type of credit (good or bad) with the following resampling techniques
# ------------------------------------------------------------------------------
# 5 fold cross validation
# ----------------------------------------------------------------------------
# First split the dataset
set.seed(123)
## 85% of the sample size
smp_size <- floor(0.85 * nrow(credit_data))
train_ind <- sample(seq_len(nrow(credit_data)), size = smp_size)
train.credit <- credit_data[train_ind, ]
test.credit <- credit_data[-train_ind, ]


#Define training control and modelling
train_control <- trainControl(method="cv", number=5)
model <- train(Cost_Matrix~., data=train.credit, trControl=train_control, method="svmLinear")
# summarize results
print(model)

# prediction
pred=predict(model,newdata = test.credit)
pred

# Show the accuracy for the training dataset
model$results

# Show the accuracy results for each resample
model$resample

# Show the accuracy for the test dataset
mean(pred == test.credit$Cost_Matrix)



# ----------------------------------------------------------------------------------
# 10 fold cross-validation with 3 repeats
# ----------------------------------------------------------------------------------

# define training control
train_control2 <- trainControl(method="repeatedcv", number=10, repeats=3)
model2<-train(Cost_Matrix~.,data=train.credit,trControl=train_control2, method="svmLinear")
# summarize results
print(model2)
pred2=predict(model2,newdata = test.credit)
pred2

# Show the accuracy for the training dataset
model2$results

# Show the accuracy results for each resample
model2$resample

# Show the accuracy for the test dataset
mean(pred2 == test.credit$Cost_Matrix)


# --------------------------------------------------------------------------------------
#Leave group out with 5 iterations and 85% data used for training define training control
# --------------------------------------------------------------------------------------

# Modelling
train_control3 <- trainControl(method="LOOCV",number = 5)
# train the model
model3<-train(Cost_Matrix~.,data=train.credit,trControl=train_control3, method="svmLinear")
# summarize results
print(model3)


# Prediction
pred3=predict(model3,newdata=test.credit)
pred3


# Show the accuracy for the training dataset
model3$results

# Show the accuracy results for each resample
model3$resample

# Show the accuracy for the test dataset
mean(pred3 == test.credit$Cost_Matrix)


# ---------------------------------------------------------------------------------------
# Bootstrap with 25 iterations
# -------------------------------------------------------------------------------------
# define training control
train_control4 <- trainControl(method="boot", number=25)
# train the model
model4 <- train(Cost_Matrix~.,data=train.credit,trControl=train_control4,method="svmLinear")
# summarize results
print(model4)

# Prediction
pred4=predict(model4,newdata=test.credit)
pred4

# Show the accuracy for the training dataset
model4$results

# Show the accuracy results for each resample
model4$resample

# Show the accuracy for the test dataset
mean(pred4 == test.credit$Cost_Matrix)




# Which resampling method gives the best estimate of the test error?

- Clearly the method with the highest test accuracy was the best 
#5 Fold cross-validation
mean(pred == test.credit$Cost_Matrix)

# 10 fold cross-validation
mean(pred2 == test.credit$Cost_Matrix)


# Leave group out with 5 iterations and 85% data used for training define training control
mean(pred4 == test.credit$Cost_Matrix)


# Bootstrap with 25 iterations
mean(pred4 == test.credit$Cost_Matrix)

