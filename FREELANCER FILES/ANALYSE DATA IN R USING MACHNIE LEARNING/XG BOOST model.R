
# Classification of users based on satisfaction with XGBoost Model in R
# The process shall entail
# 1.Preparing data
# 2.Defining the model
# 3.Predicting test data

# We'll start by loading the required packages.

library(xgboost)
library(caret) 

# Loading and Preparing data
df=read.csv("Gvault_survey_encoded.csv",header = T)
 
#Drop irrelevant columns
df=subset(df, select=-c(Respondent.ID,Collector.ID,Start.Date,
         End.Date,explain_training_effectiveness,explain_support,
         explain_complete_without_help,explain_easy_access_documents,
                             explain_satisfaction,explain_Gvault_efficiency,explain_Gvault_improved,
                             explain_office_location,explain_job_level) )
 
 
 
# In this tutorial, use Gvault survey dataset as a given classification problem.
# First, we'll split the dataset into the train and test parts.
# Here, I'll use 10 percent of the dataset as test data.

indexes = sample(2,nrow(df),replace = T,prob = c(0.9,0.1))
train=df[indexes==1,]
test=df[indexes==2,]

dim(train)
dim(test)

# Next, we'll separate x - feature and y - label parts. Note, the training 
# x data should be matrix type to use in xgboost model. Thus, we'll convert x
# parts into the matrix type.

train_x = data.matrix(train.Hitters[,-19])
train_y =train.Hitters[,19]
train_x[1:5,]
train_y[1:5]

test_x = data.matrix(test.Hitters[,-19])
test_y = test.Hitters[,19]

# Here, you may know that 4 is the number of levels in the satisfaction column
# in the iris data frame. Next, we need to convert the train and test data into xgb matrix type.

xgb_train= xgb.DMatrix(data=train_x, label=train_y)
xgb_train
xgb_test = xgb.DMatrix(data=test_x, label=test_y)
xgb_test

# Defining the model

# We can define the xgboost model with xgboost function with changing some of the parameters. Note that xgboost is a training function, thus we need to include the train data too. Once we run the function, it fits the model with training data.

xgbc = xgboost(data=xgb_train, max.depth=3, nrounds=50)

print(xgbc)

# Predicting test data
# The model is ready and we can predict our test data.

pred = predict(xgbc, xgb_test)
print(pred)



# Now, we'll convert the result into factor level type.

pred[(pred>4)] = 4
pred_y = as.factor((levels(test_y))[round(pred)])
print(pred_y)

# We'll check the prediction accuracy with a confusion matrix.
table(test_y)
cm = confusionMatrix(test_y, pred_y)
print(cm)

# We can compare the test with original values.

result = cbind(orig=as.character(test_y),
               factor=as.factor(test_y),
               pred=pred,
               rounded=round(pred),
               pred=as.character(levels(test_y))[round(pred)])

print(data.frame(result))


