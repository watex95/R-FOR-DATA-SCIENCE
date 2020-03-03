#LOAD DATA AND PACKAGES

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

df=iris
#STANDARDIZE CONTINUOUS VARIABLES
# Function to standarise inputs to range(0, 1)
scalemax <- function(df)
{
  numeric_columns <- which(sapply(df, is.numeric))
  if (length(numeric_columns)){df[numeric_columns] <- lapply(df[numeric_columns], function(x){
    denom <- ifelse(max(x)==0, 1, max(x))
    x/denom
  })}
  df
}

df=scalemax(df)

#NEURAL NETWORK MODEL 
neuralnetwork <- function(sizes, training_data, epochs, mini_batch_size, lr, C,
                          verbose=FALSE, validation_data=training_data)
{
  num_layers <- length(sizes)
  listw <- sizes[1:length(sizes)-1] # Skip last (weights from 1st to 2nd-to-last)
  listb <-  sizes[-1]  # Skip first element (biases from 2nd to last)
  
  # Initialise with gaussian distribution for biases and weights
  biases <- lapply(seq_along(listb), function(idx){
    r <- listb[[idx]]
    matrix(rnorm(n=r), nrow=r, ncol=1)
  })
  
  weights <- lapply(seq_along(listb), function(idx){
    c <- listw[[idx]]
    r <- listb[[idx]]
    matrix(rnorm(n=r*c), nrow=r, ncol=c)
  })
  
  SGD(training_data, epochs, mini_batch_size, lr, C, 
      sizes, num_layers, biases, weights, verbose, validation_data)
}



cost_delta <- function(method, z, a, y) {if (method=='ce'){return (a-y)}}


#STOCHASTIC GRADIENT DESCENT
SGD <- function(training_data, epochs, mini_batch_size, lr, C, sizes, num_layers, biases, weights,
                verbose=FALSE, validation_data)
{
  start.time <- Sys.time()
  # Every epoch
  for (j in 1:epochs){
    # Stochastic mini-batch (shuffle data)
    training_data <- sample(training_data)
    # Partition set into mini-batches
    mini_batches <- split(training_data, 
                          ceiling(seq_along(training_data)/mini_batch_size))
    # Feed forward (and back) all mini-batches
    for (k in 1:length(mini_batches)) {
      # Update biases and weights
      res <- update_mini_batch(mini_batches[[k]], lr, C, sizes, num_layers, biases, weights)
      biases <- res[[1]]
      weights <- res[[-1]]
    }
    # Logging
    if(verbose){if(j %% 1 == 0){
      cat("Epoch: ", j, " complete")
      # Print acc and hide confusion matrix
      confusion <- evaluate(validation_data, biases, weights)
    }}
  }
  time.taken <- Sys.time() - start.time
  if(verbose){cat("Training complete in: ", time.taken)}
  cat("Training complete")
  # Return trained biases and weights
  list(biases, weights)
}





#UPDATE THE BIASE AND WEIGHTS MATRICIES FOR EACH MINI-BATCH
update_mini_batch <- function(mini_batch, lr, C, sizes, num_layers, biases, weights)
{
  nmb <- length(mini_batch)
  listw <- sizes[1:length(sizes)-1] 
  listb <-  sizes[-1]  
  
  # Initialise updates with zero vectors (for EACH mini-batch)
  nabla_b <- lapply(seq_along(listb), function(idx){
    r <- listb[[idx]]
    matrix(0, nrow=r, ncol=1)
  })
  nabla_w <- lapply(seq_along(listb), function(idx){
    c <- listw[[idx]]
    r <- listb[[idx]]
    matrix(0, nrow=r, ncol=c)
  })  
  
  # Go through mini_batch
  for (i in 1:nmb){
    x <- mini_batch[[i]][[1]]
    y <- mini_batch[[i]][[-1]]
    # Back propogation will return delta
    # Backprop for each observation in mini-batch
    delta_nablas <- backprop(x, y, C, sizes, num_layers, biases, weights)
    delta_nabla_b <- delta_nablas[[1]]
    delta_nabla_w <- delta_nablas[[-1]]
    # Add on deltas to nabla
    nabla_b <- lapply(seq_along(biases),function(j)
      unlist(nabla_b[[j]])+unlist(delta_nabla_b[[j]]))
    nabla_w <- lapply(seq_along(weights),function(j)
      unlist(nabla_w[[j]])+unlist(delta_nabla_w[[j]]))
  }
  # After mini-batch has finished update biases and weights:
  # i.e. weights = weights - (learning-rate/numbr in batch)*nabla_weights
  # Opposite direction of gradient
  weights <- lapply(seq_along(weights), function(j)
    unlist(weights[[j]])-(lr/nmb)*unlist(nabla_w[[j]]))
  biases <- lapply(seq_along(biases), function(j)
    unlist(biases[[j]])-(lr/nmb)*unlist(nabla_b[[j]]))
  # Return
  list(biases, weights)
}



#BACK PROPAGATION 

backprop <- function(x, y, C, sizes, num_layers, biases, weights)
{
  # Initialise updates with zero vectors
  listw <- sizes[1:length(sizes)-1] 
  listb <-  sizes[-1]  
  
  # Initialise updates with zero vectors (for EACH mini-batch)
  nabla_b_backprop <- lapply(seq_along(listb), function(idx){
    r <- listb[[idx]]
    matrix(0, nrow=r, ncol=1)
  })
  nabla_w_backprop <- lapply(seq_along(listb), function(idx){
    c <- listw[[idx]]
    r <- listb[[idx]]
    matrix(0, nrow=r, ncol=c)
  })  
  
  # First:
  # Feed-forward (get predictions)
  activation <- matrix(x, nrow=length(x), ncol=1)
  activations <- list(matrix(x, nrow=length(x), ncol=1))
  # z = f(w.x + b)
  # So need zs to store all z-vectors
  zs <- list()
  for (f in 1:length(biases)){
    b <- biases[[f]]
    w <- weights[[f]]
    w_a <- w%*%activation
    b_broadcast <- matrix(b, nrow=dim(w_a)[1], ncol=dim(w_a)[-1])
    z <- w_a + b
    zs[[f]] <- z
    activation <- sigmoid(z)
    activations[[f+1]] <- activation  # Activations already contain one element
  }
  # Second:
  # Backwards (update gradient using errors)
  # Last layer
  delta <- cost_delta(method=C, z=zs[[length(zs)]], a=activations[[length(activations)]], y=y)
  nabla_b_backprop[[length(nabla_b_backprop)]] <- delta
  nabla_w_backprop[[length(nabla_w_backprop)]] <- delta %*% t(activations[[length(activations)-1]])
  # Second to second-to-last-layer
  # If no hidden-layer reduces to multinomial logit
  if (num_layers > 2) {
    for (k in 2:(num_layers-1)) {
      sp <- sigmoid_prime(zs[[length(zs)-(k-1)]])
      delta <- (t(weights[[length(weights)-(k-2)]]) %*% delta) * sp
      nabla_b_backprop[[length(nabla_b_backprop)-(k-1)]] <- delta
      testyy <- t(activations[[length(activations)-k]])
      nabla_w_backprop[[length(nabla_w_backprop)-(k-1)]] <- delta %*% testyy
    }
  }
  return_nabla <- list(nabla_b_backprop, nabla_w_backprop)
  return_nabla
}

#SPLIT DATA INTO TRAIN AND TEST DATASETS

train_test_from_df <- function(df, predict_col_index, train_ratio, 
                               shuffle_input = TRUE, scale_input=TRUE)
train_test_split <- train_test_from_df(df = iris, predict_col_index = 5, train_ratio = 0.7,shuffle_input = TRUE, scale_input=TRUE)
training_data <- train_test_split[[1]]
testing_data <- train_test_split[[2]]



in_n <- length(training_data[[1]][[1]])
out_n <- length(training_data[[1]][[-1]])
in_n
out_n

trained_net <- neuralnetwork(c(in_n,out_n),training_data=training_data,
  epochs=30,mini_batch_size=10, lr=0.5, C='ce', verbose=TRUE,
  validation_data=testing_data
)


# Trained matricies:
biases <- trained_net[[1]]
weights <- trained_net[[-1]]

# Accuracy (train)
evaluate(training_data, biases, weights)
# Accuracy (test)
evaluate(testing_data, biases, weights)


