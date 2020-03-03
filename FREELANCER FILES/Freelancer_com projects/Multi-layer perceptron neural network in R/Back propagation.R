
# Load dataset
data("iris")
iris

# update.packages(checkBuilt=TRUE)
# Install and load packages
# install.packages('neuralnet')
# install.packages(ggplot2)
# install.packages('nnet')
# install.packages('dplyr')
# install.packages('reshape2')
# install.packages('caTools')

library(neuralnet)
library(ggplot2)
library(nnet)
library(dplyr)
library(reshape2)
library(caTools)

# Split the dataset
set.seed(123)
split = sample.split(iris$Species, SplitRatio = 0.75)
split
training_data = subset(iris, split == TRUE)
test_data = subset(iris, split == FALSE)

labels <- class.ind(as.factor(iris$Species))

standardiser <- function(x){
  (x-min(x))/(max(x)-min(x))}

iris[, 1:4] <- lapply(iris[, 1:4], standardiser)

pre_process_iris <- cbind(iris[,1:4], labels)

f <- as.formula("setosa + versicolor + virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width")
iris_net <- neuralnet(f,data=pre_process_iris, hidden=c(16,12),act.fct="tanh",linear.output=FALSE)
plot(iris_net)

iris_preds<-compute(iris_net, pre_process_iris[, 1:4])

origi_vals <- max.col(pre_process_iris[, 5:7])
pr.nn_2 <- max.col(iris_preds$net.result)
print(paste("Model Accuracy: ", round(mean(pr.nn_2==origi_vals)*100, 2), "%.", sep = ""))


in_n<-length(training_data[[1]][[1]])
out_n<-length(training_data[[1]][[-1]])
in_n
neuralnetwork<-function(sizes, training_data, epochs, 
                          mini_batch_size, lr, C, verbose=FALSE, 
                          validation_data=training_data)

trained_net <- neuralnetwork(
  c(in_n, 40, out_n),
  training_data=training_data,
  epochs=30, 
  mini_batch_size=10,
  lr=0.5,
  C='ce',
  verbose=TRUE,
  validation_data=testing_data
)


# Trained matricies:
biases <- trained_net[[1]]
weights <- trained_net[[-1]]

# Accuracy (train)
evaluate(training_data, biases, weights)
# Accuracy (test)
evaluate(testing_data, biases, weights)






