# IMPUTATIONS and IMBALANCED DATA, ROCR, AUC, SMOTE

# Performance measurement, Imbalance correction and Imputing missing values
# 1.	Load the Wisconsin breast cancer dataset from blackboard

suppressWarnings(library(tidyverse))
suppressWarnings(library(caret))
suppressWarnings(library(rpart.plot))
suppressWarnings(library(randomForest))
suppressWarnings(library(e1071))
suppressWarnings(library(ROCR))
suppressWarnings(library(pROC))
suppressWarnings(library(RCurl))
suppressWarnings(library(gbm))

breast_cancer<-read.csv('WisconsinBreastCancer.csv',header = T)

str(breast_cancer) #check the variables
dim(breast_cancer) #check the size of dataframe

# 2. How many missing data points are there? 
sum(is.na(breast_cancer)) #no missing values

#In which columns are they missing?
colnames(breast_cancer)[colSums(is.na(breast_cancer)) > 0]


### 3. Impute for the first column with missing values using the mean and round to an integer

breast_cancer[,1][is.na(breast_cancer[,1])]= round(mean(breast_cancer[,1], na.rm=TRUE),digits = 0)

sum(is.na(breast_cancer[,1])) #check again for number of missing values


# 4. Impute for the second column with missing values using KNN with three nearest
# neighbors and round to integer

library(VIM)
breast_cancer<-kNN(data = breast_cancer, variable = 'uniformity_of_cell_size',
                   k=3, imp_var=FALSE)
summary(breast_cancer)



# 5. Impute for the third column with missing values using a regression to predict the third column based on uniformity_of_cell_shape, marginal_adhesion and normal_nucleoli.  Round to integers.

#Start by creating an indicator variable
Ind<-function(t)
{
  x=dim(length(t))
  x[which(!is.na(t))]=1
  x[which(is.na(t))]=0
  return(x)
  
}
breast_cancer$I<-Ind(breast_cancer[,3])
colnames(breast_cancer) #check to see if it was created


#Fit a simple linear between third column and the mentioned columns
attach(breast_cancer)
lm(breast_cancer[,3]~uniformity_of_cell_shape+marginal_adhesion+normal_nucleoli,data=breast_cancer)


#Impute missing value with the model output in a for loop
for(i in 1:nrow(breast_cancer))
{
  if(breast_cancer$I==0)
  {
    breast_cancer[,3][i]=mymodel$coefficients[1]+ 
      mymodel$coefficients[2]*breast_cancer$uniformity_of_cell_shape[i]+
      mymodel$coefficients[3]*breast_cancer$marginal_adhesion[i]+
      mymodel$coefficients[4]*breast_cancer$normal_nucleoli[i]
    
  }
}

#check for the number of missing values
sum(is.na(breast_cancer[,3])) 



# 6. Build a decision tree model to predict class using 80% training data and five
# fold cross validation with three repeats.  Hint: Use  method =" rpart" in caret

# Split the data into training and test set
library(tidyverse)
library(rpart)
library(dplyr)

# First split the dataset
set.seed(123)
## 85% of the sample size
smp_size <- floor(0.80 * nrow(breast_cancer))
train_ind <- sample(seq_len(nrow(breast_cancer)), size = smp_size)
train.cancer <- breast_cancer[train_ind, ]
test.cancer <- breast_cancer[-train_ind, ]

colnames(train.cancer)


# Fit the model on the training set
set.seed(123)
model_tree <- train(classes ~., data = train.cancer, method = "rpart",
                    trControl = trainControl("cv", number = 5),
                    tuneLength = 3
)
model_tree

#visulize the decision tree
prp(model_tree$finalModel, box.palette = "Reds", tweak = 1.2)



# 7. Show the accuracy results for each resample (fold) in the training data
model_tree$results
# Plot model accuracy vs different values of cp (complexity parameter)
plot(model_tree)




# 8. Do the following performance measures for the test dataset

# (a).Compute all the accuracy measures (Accuracy, sensitivity, specificity etc)

# Make predictions on the test data
predicted.classes=model_tree%>% predict(test.cancer)
# Compute model accuracy rate on test data
mean(predicted.classes == test.cancer$classes)

#calculate sensitivity

sensitivity(predicted.classes,test.cancer$classes)


#calculate specificity
specificity(predicted.classes,test.cancer$classes)



# (b).Plot the ROC curve

library(ROCR)
pred <- prediction(predict(model_tree, type = "prob")[, 2],train.cancer$classes)
plot(performance(pred, "tpr", "fpr"))
abline(0, 1, lty = 2)

# (c).Plot the lift curve

perf <- performance(pred,"lift","rpp")
plot(perf, main="lift curve", colorize=T)

# (d).Compute the AUC

auc = performance(pred, 'auc')
slot(auc, 'y.values')


# 9. Is the cancer data imbalanced by the class feature? What is the percentage of the majority class and the percentage of the minority class?

table(breast_cancer$classes) #calculates the class frequencies
tablature <- as.matrix(prop.table(table(breast_cancer$classes)) * 100)
tablature #gets the percentage: hence class "B" is the majority while class "M" is the minority




### 10.	Now re-build the decision tree model above after correcting for imbalance using SMOTE.
# initialize imbalanced data
imbal_train=train.cancer
imbal_test=test.cancer

# Set up control function for training
ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 3,
                     classProbs = TRUE)


#Build smote model

ctrl$sampling <- "smote"

smote_fit <- train(classes ~ .,
                   data = imbal_train,
                   method = "gbm",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = ctrl)
smote_fit


### 11.	Did any of the accuracy measures improve? If so, which ones?
model_list <- list(original = model_tree,
                   SMOTE = smote_fit)

#First Build custom AUC function to extract AUC from the caret model object
test_roc <- function(model, data) {
  roc(data$classes,
      predict(model, data, type = "prob")[, "malignant"])
}

# Get the AUC for the original model with the original dataset.
model_tree %>%
  test_roc(data = test.cancer)%>%auc()

# Get the AUC for the SMOTE model with the Imbalanced dataset.
smote_fit %>%
  test_roc(data = imbal_test)%>%auc()















