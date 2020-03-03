
install.packages("ISLR")
library(ISLR)

attach(College)

dim(College)

# Train test split
indexes = sample(2,nrow(College),replace = T,prob = c(0.9,0.1))
train_cole=College[indexes==1,]
test_cole=College[indexes==2,]

OLS_model=lm(Apps~., data = train_cole)
summary(OLS_model)
RSS <- c(crossprod(OLS_model$residuals))
MSE <- RSS / length(OLS_model$residuals)
RMSE=sqrt(MSE)
RMSE


library(ISLR)
attach(College)
x <- model.matrix(Apps~., College)[,-1]
y <- College$Apps

lambda <- 10^seq(10, -2, length = 100)

# Train test split
#create test and training sets
library(glmnet)
set.seed(489)
train = sample(1:nrow(x), nrow(x)/2)
class(train)
test = (-train)
ytest = y[test]



#ridge
ridge.mod <- glmnet(x, y, alpha = 0, lambda = lambda)
predict(ridge.mod, s = 0, exact = T, type = 'coefficients')[1:18,]






library(leaps)
library(caret)

models_new=regsubsets(Apps~.,data=train_cole,nvmax = 18)

get_model_formula <- function(id, object, outcome){
  # get models data
models_new <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models_new == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}

str(College)

# For example to have the best 10-variable model formula, type this:

get_model_formula(10, models_new, "Apps")


# To get the cross-validation (CV) error for a given model:
get_cv_error <- function(model.formula, data){
  set.seed(1)
  train.control <- trainControl(method = "cv", number = 10)
  cv <- train(model.formula, data = train_cole, method = "lm",
              trControl = train.control)
  cv$results$RMSE
}

# Finally, use the above defined helper functions to compute the prediction
# error of the different best models returned by the regsubsets() function:

library(tidyverse)
# Compute cross-validation error
model.ids<-1:10
cv.errors<-map(model.ids, get_model_formula, models_new, "Apps")%>%map(get_cv_error, data = train_cole)%>%unlist()
cv.errors


# Select the model that minimize the CV error
which.min(cv.errors)


# It can be seen that the model with 3 variables is the best model. 
# It has the lower prediction error. The regression coefficients of this
# model can be extracted as follow:

coef(models_new, 7)


# Question three

library(tidyverse)
library(broom)
install.packages("glmnet")
library(glmnet)

str(train_cole)

y <-train_cole$Apps
attach(train_cole)
x <-train_cole%>%select(Private,Accept,Enroll,Top10perc,Top25perc,
              F.Undergrad,P.Undergrad,Outstate,Room.Board,Books,
              Personal,PhD,Terminal,S.F.Ratio,perc.alumni,
              Expend,Grad.Rate)%>% data.matrix()

lambdas <- 10^seq(3, -2, by = -.1)

fit <- glmnet(x, y, alpha = 0, lambda = lambdas)
summary(fit)
 
# Because, unlike OLS regression done with lm(), ridge regression 
# involves tuning a hyperparameter, lambda, glmnet() runs the model
# many times for different values of lambda. We can automatically find
# a value for lambda that is optimal by using cv.glmnet() as follows:
   
cv_fit <- cv.glmnet(x, y, alpha = 0, lambda = lambdas)

# cv.glmnet() uses cross-validation to work out how well each model
# generalises, which we can visualise as:
  
plot(cv_fit)

# The lowest point in the curve indicates the optimal lambda: the log
# value of lambda that best minimised the error in cross-validation.
# We can extract this values as:
  
opt_lambda <- cv_fit$lambda.min
opt_lambda

# And we can extract all of the fitted models (like the object returned
# by glmnet()) via:
  
fit <- cv_fit$glmnet.fit
summary(fit)


# These are the two things we need to predict new data. For example,
# predicting values and computing an R2 value for the data we trained on:
  
  y_predicted <- predict(fit, s = opt_lambda, newx = x)

# Sum of Squares Total and Error
sst <- sum(y^2)
sse <- sum((y_predicted - y)^2)

# R squared
rsq <- 1 - sse / sst
rsq


# QUESTION FOUR

attach(College)

x <- model.matrix(Apps~., College)[,-1]
y <- College$Apps
lambda <- 10^seq(10, -2, length = 100)

set.seed(489)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
ytest = y[test]

lasso.mod <- glmnet(x[train,],y[train], alpha = 1, lambda = lambda)
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred-ytest)^2)




































