# Homework 3

# Performance measurement, Imbalance correction and Imputing missing values

getwd()

# 1.	Load the Wisconsin breast cancer dataset from blackboard
suppressWarnings(library(tidyverse))
suppressWarnings(library(caret))
suppressWarnings(library(rpart.plot))
suppressWarnings(library(ggcorrplot))
suppressWarnings(library(GGally))
suppressWarnings(library(randomForest))
suppressWarnings(library(e1071))
suppressWarnings(library(ROCR))
suppressWarnings(library(pROC))

breast_cancer=read.csv("WisconsinBreastCancer.csv",header = T)

head(breast_cancer)



# 2.How many missing data points are there? 
sum(is.na(breast_cancer))

#In which columns are they missing?
colnames(breast_cancer)[colSums(is.na(breast_cancer)) > 0]

# 3.Impute for the first column with missing values using
# the mean and round to an integer
breast_cancer[,2][is.na(breast_cancer[,2])]=round(mean(breast_cancer[,2],na.rm=TRUE),digits = 0)
sum(is.na(breast_cancer[,2]))






# 4.Impute for the second column with missing values using KNN with
# three nearest neighbors and round to integer
library(DMwR)
sec_col=as.data.frame(breast_cancer[,2])

breast_cancer<-knnImputation(sec_col, k=3, scale=T ,meth="weighAvg",
                             distData = breast_cancer)
sum(is.na(breast_cancer[,2])) #There was no missing values



# Alternative method

# check the number of missing in the column
sum(is.na(breast_cancer[,3]))

library(VIM)
attach(breast_cancer)

breast_cancer<-kNN(breast_cancer,variable=uniformity_of_cell_size,k=3)

summary(breast_cancer)





### 4. Impute for the second column with missing values using KNN with three nearest neighbors and round to integer
```{r ,warning=FALSE}
library(DMwR)
sec_col=as.data.frame(breast_cancer[,2])

breast_cancer<-knnImputation(sec_col, k=3, scale=T ,meth="weighAvg",distData = breast_cancer)
sum(is.na(breast_cancer[,2])) #There was no missing values
```











#5.	Impute for the third column with missing values using a regression
# to predict the third column based on uniformity_of_cell_shape,
# marginal_adhesion and normal_nucleoli.  Round to integers.

#create a function when called it creates an indicator variable

Ind<-function(t){
  x=dim(length(t))
  x[which(!is.na(t))]=1
  x[which(is.na(t))]=0
  return(x)
}

# call the function
breast_cancer$I<-Ind(breast_cancer[,3])

# check the indicator variable to see the count of missing values
table(breast_cancer$I)

#fit a linear regression on the third column with uniformity_of_cell_shape,
# marginal_adhesion and normal_nucleoli as predictors

mymodel=lm(breast_cancer[,3]~uniformity_of_cell_shape+marginal_adhesion
           +normal_nucleoli,data=breast_cancer)

mymodel$coefficients


#Impute missing values with the model coefficients in a for loop

for(i in 1:nrow(breast_cancer))
{
      if(breast_cancer$I==0)
      {
        breast_cancer[,3][i]= mymodel$coefficients[1]+ 
          mymodel$coefficients[2]*breast_cancer$uniformity_of_cell_shape[i]+
          mymodel$coefficients[3]*breast_cancer$marginal_adhesion[i]+
          mymodel$coefficients[4]*breast_cancer$normal_nucleoli[i]
      }
}

sum(is.na(breast_cancer[,3]))




# 6.	Build a decision tree model to predict class using 80% training
# data and five fold cross validation with three repeats.

# Split the data into training and test set
set.seed(123)
training.samples <- breast_cancer$classes %>% 
  createDataPartition(p = 0.8, list = FALSE)

train.data<- breast_cancer[training.samples, ]
test.data <- breast_cancer[-training.samples, ]

# Fit the model on the training set
set.seed(123)
model_tree<-train(classes ~., data = train.data, method = "rpart",
  trControl = trainControl("cv", number = 5),tuneLength = 3)


prp(model_tree$finalModel, box.palette = "Reds", tweak = 1.2)




# 7.Show the accuracy results for each resample (fold) in the training data
model_tree$results
# Plot model accuracy vs different values of cp (complexity parameter)
plot(model_tree)





# 8.Do the following performance measures for the test dataset

# (a).Compute all the accuracy measures (Accuracy, sensitivity, specificity etc)

# Make predictions on the test data
predicted.classes=model_tree %>% predict(test.data)

# Compute model accuracy rate on test data
mean(predicted.classes == test.data$classes)

#calculate sensitivity
sensitivity(predicted.classes,test.data$classes)


#calculate specificity
specificity(predicted.classes,test.data$classes)


# b.	Plot the ROC curve

library(ROCR)
pred<-prediction(predict(model_tree,type="prob")[,2],train.data$classes)

plot(performance(pred, "tpr", "fpr"))
abline(0, 1, lty = 2)

# c.	Plot the lift curve
perf <- performance(pred,"lift","rpp")
plot(perf, main="lift curve", colorize=T)


# d.	Compute the AUC
auc = performance(pred, 'auc')
slot(auc, 'y.values')












# 9.	Is the cancer data imbalanced by the class feature?
# What is the percentage of the majority class and the percentage
# of the minority class?

table(breast_cancer$classes) #calculates the class frequencies
tablature <- as.matrix(prop.table(table(breast_cancer$classes)) * 100)
tablature #gets the percentage: hence class "B" is the majority while class "M" is the minority




# 10.	Now re-build the decision tree model above after correcting
# for imbalance using SMOTE.

# initialize imbalanced data
imbal_train=train.data
imbal_test=test.data


# Set up control function for training
ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 3,
                     classProbs = TRUE)


# Build smote model
ctrl$sampling <- "smote"

smote_fit <-train(classes ~ .,
                   data = imbal_train,
                   method = "rpart",
                   metric = "ROC",
                   trControl = ctrl)








# 11.	Did any of the accuracy measures improve? If so, which ones?
model_list <- list(original = model_tree,
                      SMOTE = smote_fit)

#First Build custom AUC function to extract AUC from the caret model object
test_roc <- function(model, data) {
  roc(data$classes,
      predict(model, data, type = "prob")[, "M"])
}

# Get the AUC for the original model with the original dataset.
model_tree %>%
  test_roc(data = test.data)%>%auc()

# Get the AUC for the SMOTE model with the Imbalanced dataset.
smote_fit %>%
  test_roc(data = imbal_test)%>%auc()




# DATA WRANGLING
# 1.	Install and load the library nycflights13
library(tidyverse)
library(nycflights13)
ls("package:nycflights13")

# 2.	Load the datasets/tables flights and airlines

airlines_data <- airlines
airports_data <-airports
flights_data <- flights
planes_data <- planes
weather_data <- weather

head(airlines_data)
head(flights_data)
head(airports_data)
head(planes_data)
head(weather_data)

# 3.	Add full airline name from the airlines table to the flights table that
# keeps all the records in the flights table by using the appropriate join


merge1=merge(x=flights_data,y=airlines_data,by='carrier',all.x=TRUE)[c("airline_name",
        "year","month","day","dep_time","sched_dep_time","dep_delay", "arr_time",
        "sched_arr_time","arr_delay","carrier","flight","tailnum","origin",
        "dest","air_time","distance","hour","minute" ,"time_hour")]

head(merge1)

# 4.	Now add the destination latitude and longitude to the flights table from 
# the airports table by using the appropriate join


#Rename the joining column
names(airports_data)[names(airports_data)=="faa"] <- "dest"


merge2=merge(x=flights_data,y=airports_data,by='dest',all.x=TRUE)[c("dest",
      "year","month","day","dep_time","sched_dep_time","dep_delay",
      "arr_time","sched_arr_time","arr_delay","carrier","flight",
        "tailnum","origin","air_time","distance","hour","minute" ,
                        "time_hour","lat","lon")]

dim(flights_data)
dim(merge2)






# RESAMPLING
library(rsample)   
library(purrr)
library(dplyr)
library(ggplot2)
library(scales)
library(mlbench)
library(kernlab)
library(sessioninfo)
theme_set(theme_bw())
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

# Fit a SVM model to predict the type of credit (good or bad) with the following resampling techniques

# 5 fold cross validation
# First split the dataset
set.seed(123)
training.samples <-credit_data$Cost_Matrix %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.credit<-credit_data[training.samples, ]
test.credit<-credit_data[-training.samples, ]


test.credit


attach(credit_data)
tuned = tune.svm(Cost_Matrix~., data = train.credit,
    gamma = 10^-2, cost = 10^2, tunecontrol=tune.control(cross=5))

# Next, you can obtain the summary information of the model, tuned:
summary(tuned)

# Then, you can access the performance details of the tuned model:
tuned$performances
# Lastly, you can use the optimum model to generate a classification table:
svmfit = tuned$best.model
svmfit
test_pred=predict(svmfit,newdata=test.credit)

# Show the accuracy for the training dataset
mean(test_pred == test.credit$Cost_Matrix)

# Show the accuracy results for each resample


# Show the accuracy for the test dataset


# Which resampling method gives the best estimate of the test error?



# ///////////////////////////////////////////////////////////////////////////////

# 10 fold cross validation with 3 repeats
tuned2 = tune.svm(Cost_Matrix~.,data=train.credit,gamma = 10^-2,
  cost =10^2, tunecontrol=tune.control(cross=10),tuneLength=3)

# Next, you can obtain the summary information of the model, tuned:
summary(tuned2)

# Then, you can access the performance details of the tuned model:
tuned2$performances
# Lastly, you can use the optimum model to generate a classification table:
svmfit2 = tuned2$best.model
svmfit2

test_pred2=predict(svmfit2,newdata=test.credit)


# Show the accuracy for the training dataset
mean(test_pred == test.credit2$Cost_Matrix)


# Show the accuracy results for each resample


# Show the accuracy for the test dataset


# Which resampling method gives the best estimate of the test error?


# //////////////////////////////////////////////////////////////////////////////

#  Leave group out with 5 iterations and 85% data used for training

# Split the dataset into 85% for training and 15% testing
set.seed(123)
training.samples <-credit_data$Cost_Matrix %>% 
  createDataPartition(p = 0.85, list = FALSE)
train.credit2<-credit_data[training.samples, ]
test.credit2<-credit_data[-training.samples, ]

#convert class variable to factor
train.credit2$Cost_Matrix=as.factor(train.credit2$Cost_Matrix)
test.credit2$Cost_Matrix=as.factor(test.credit2$Cost_Matrix)

# Re-sampling with 5 iterations ("number"=5)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)

svm_class <- train(Cost_Matrix ~., data = train.credit2,
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

# predicting the results
test_pred <- predict(svm_Linear, newdata =test.credit2)
test_pred3

# Show the accuracy for the training dataset
mean(test_pred == test.credit2$Cost_Matrix)
 
# Show the accuracy results for each resample


# Show the accuracy for the test dataset


# Which resampling method gives the best estimate of the test error?


# ///////////////////////////////////////////////////////////////////////////////

# Bootstrap with 25 iterations
library(e1071)
library(boot)
class(train.credit$Cost_Matrix)

train.credit$Cost_Matrix=as.factor(train.credit$Cost_Matrix)
test.credit$Cost_Matrix=as.factor(test.credit$Cost_Matrix)

model2<-best.svm(Cost_Matrix~.,data=train.credit, gamma = 10^(-6:-1),
      cost = 10^(-1:1),type="C-classification",kernel = "radial",
      tunecontrol=tune.control(sampling="bootstrap", nboot=25))


pred.b<-predict(model2, newdata=test.credit, decision.values=FALSE,
                probability=TRUE)
pred.b

# Show the accuracy for the training dataset
mean(pred.b == test.credit$Cost_Matrix)

# Show the accuracy results for each resample


# Show the accuracy for the test dataset


# Which resampling method gives the best estimate of the test error?
  
























