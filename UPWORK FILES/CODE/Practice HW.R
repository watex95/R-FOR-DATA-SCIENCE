
# PRACTIC HW
# 1. Load the cpus dataset from the MASS package

library(MASS)
cpus
install.packages("leaps")

library(tidyverse)
library(caret)
library(leaps)

# inspect data
sample_n(cpus,5)

# 2. Use syct, mmin , mmax , cach , chmin, chmax as the predictors (independent variables) to predict performance (perf)

# The function summary() reports the best set of variables for each model
# size. From the output, an asterisk specifies that a given variable
# is included in the corresponding model.
 
# For example, it can be seen that the best 2-variables model contains only
# mmax and cach variables (perf ~ mmax + cach). The best three-variable model
# is (perf ~ mmax + cach + mmin), and so forth.

# A natural question is: which of these best models should we finally choose
# for our predictive analytics?

models <- regsubsets(perf~syct + mmin + mmax + cach + chmin + chmax, data = cpus,
                     nvmax = 6)
summary(models)

#3. What is the best model obtained according to Cp, BIC, and adjusted R2?

#Perform best subset selection in order to choose the best predictors from the above predictors

# Model selection criteria: Adjusted R2, Cp and BIC
res.sum <- summary(models)
res.sum$cp
res.sum$bic

data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)

# Here, adjusted R2 tells us that the best model is the one with all the 5 predictor
# variables. However, using the BIC and Cp criteria, we should go for the model 
# with 5 variables.

# 5.Show some plots to provide evidence for your answer, and report the coefficients of the best model obtained for each criteria
# Graphical evidence
plot(res.sum$bic, xlab="Parameter", ylab="BIC")
plot(res.sum$cp, xlab="Parameter", ylab="CP")




# 6. Repeat using forward stepwise selection and also using backwards stepwise selection.
#How does your answer compare to the best subset results?

library(MASS)
# Fit the full model 
full.model <- lm(perf ~syct + mmin + mmax + cach + chmin + chmax, data = cpus)
full.model
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both",trace = FALSE)
summary(step.model)  

models <- regsubsets(perf ~syct + mmin + mmax + cach + chmin + chmax, data = cpus,
                     nvmax = 6,
                     method = "seqrep")
summary(models)

# Set seed for reproducibility

#This indicates that the best model is the one with nvmax = 4 variables. 
#The function summary() reports the best set of variables for each model
#size, up to the best 4-variables model.

set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)

# Train the model
step.model <- train(perf ~syct + mmin + mmax + cach + chmin + chmax, data = cpus,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:6),
                    trControl = train.control)

step.model$results

step.model$bestTune

summary(step.model$finalModel)

# The model with 5 variables (nvmax = 5) is the one that has the lowest RMSE.

step1.model <- train(perf ~syct + mmin + mmax + cach + chmin + chmax, data = cpus,
                    method = "leapForward", 
                    tuneGrid = data.frame(nvmax = 1:6),
                    trControl = train.control
)
step1.model$results
step1.model$bestTune
summary(step1.model$finalModel)






















# K-FOLD CROSS VALIDATION
#---------------------------
# A more rigorous approach is to select a models based on the prediction
# error computed on a new test data using k-fold cross-validation
# techniques

# id: model id
# object: regsubsets object
# data: data used to fit regsubsets
# outcome: outcome variable

get_model_formula <- function(id, object, outcome){
  # get models data
  models <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}


# For example to have the best 3-variable model formula, type this:
  
get_model_formula(3, models, "perf")


# To get the cross-validation (CV) error for a given model:
get_cv_error <- function(model.formula, data){
      set.seed(1)
      train.control <- trainControl(method = "cv", number = 5)
      cv <- train(model.formula, data = data, method = "lm",
                  trControl = train.control)
      cv$results$RMSE
    }

# Finally, use the above defined helper functions to compute the prediction
# error of the different best models returned by the regsubsets() function:

# Compute cross-validation error
model.ids <- 1:5
cv.errors <-  map(model.ids, get_model_formula, models, "perf") %>%
    map(get_cv_error, data = cpus) %>%
    unlist()
cv.errors


# Select the model that minimize the CV error
which.min(cv.errors)


# It can be seen that the model with 3 variables is the best model. 
# It has the lower prediction error. The regression coefficients of this
# model can be extracted as follow:
   
coef(models, 3)

# This script describes the best subsets regression approach for choosing
# the best linear regression model that explains our data.
# 
# Note that, this method is computationally expensive and becomes unfeasible
# for a large data set with many variables. A better alternative is
# provided by the stepwise regression method



# STEPWISE SELECTION

# Fit the full model 
full.model <- lm(perf~syct + mmin + mmax + cach + chmin + chmax, data = cpus)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)

# The regsubsets() function , which has the tuning parameter nvmax
# specifying the maximal number of predictors to incorporate in the model
# It returns multiple models with different size up to nvmax. You need
# to compare the performance of the different models for choosing the
# best one. regsubsets() has the option method, which can take the values
# "backward", "forward" and "seqrep" (seqrep = sequential replacement,
# combination of forward and backward selections).

models<-regsubsets(perf~syct + mmin + mmax + cach + chmin + chmax,
        data = cpus, nvmax = 5, method = "seqrep")
summary(models)


# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(perf~syct + mmin + mmax + cach + chmin + chmax,
                    data = cpus,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:6),
                    trControl = train.control
)
step.model$results


step.model$bestTune


# This indicates that the best model is the one with nvmax = 4 variables.
# The function summary() reports the best set of variables for each model
# size, up to the best 4-variables model.

summary(step.model$finalModel)

str(cpus)

# An asterisk specifies that a given variable is included in the corresponding
# model. For example, it can be seen that the best 4-variables model
# contains  mmin,chmax,mmax,cach(perf ~ mmin+chmax+mmax+cach).
# The regression coefficients of the final model (id = 4) can be
# accessed as follow:
  
coef(step.model$finalModel, 4)

# Or, by computing the linear model using only the selected predictors:
  
lm(perf ~ mmin+chmax+mmax+cach, data =cpus)

























