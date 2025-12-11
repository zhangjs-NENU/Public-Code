#' Data : 2024.8.19
#' Title : Machine-Learning-Based-Prediction-and-Analysis-of-Chinese-Youth-Marriage-Decision
#' Part : 6 SHAP calculation & visualisation

#' Package Versions : 
#  "doParallel 1.0.17"
#  "shapviz 0.9.3"
#  "kernelshap 0.7.0"
#  "dplyr 1.1.4"

#--------CODE--------#

library(doParallel)
library(shapviz)
library(kernelshap)
library(dplyr)

rm(list = ls())
set.seed(111)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read data
learner_catboost <- readRDS("data/learners.RDS")[[7]]
train_task <- readRDS("data/train_task.RDS")
data <- rbind(readRDS("data/test_task.RDS")$data(),readRDS("data/train_task.RDS")$data())

# Detect the number of cores available for parallel processing
numCores <- detectCores()
print(paste("Number of cores detected:", numCores))

# Create a parallel cluster to utilize all available cores
cl <- makeCluster(numCores, outfile="")
registerDoParallel(cl)
getDoParWorkers()

# Define a function to calculate SHAP values in parallel
parallel_kernelshap <- function(learner, X, bg_X, pred_fun,numCores) {
  registerDoParallel(numCores)
  shap_values <- foreach(i = 1:nrow(X), .combine = rbind, .packages = 'kernelshap',.verbose=TRUE) %dopar% {
    message(paste("Processing row", i, "of", nrow(X)))
    kernelshap::kernelshap(learner,
                           X = X[i, , drop = FALSE],
                           bg_X = bg_X,
                           pred_fun = pred_fun,
                           verbose = FALSE)
  }
  return(shap_values)
}

# Create a background dataset by randomly sampling from the test data
bg_X <- data[sample(nrow(data),200),]

# Define a function to predict probabilities using the trained model
pred_fun <- function(m, X) m$predict_newdata(X)$prob

# Calculate SHAP values in parallel using the defined function
shap_values <- parallel_kernelshap(learner_catboost$train(train_task), 
                                   X = data[,-1], 
                                   bg_X = bg_X, 
                                   pred_fun = pred_fun,
                                   numCores = cl)

# Stop the parallel cluster
stopCluster(cl)

# Populate the matrix with SHAP values
S <- matrix(nrow = nrow(shap_values),ncol = ncol(data)-1)
for(i in 1:nrow(shap_values)){
  part <- shap_values[i,1][[1]][2] %>% unlist()
  S[i,] <- part
}
colnames(S) <- colnames(data[,2:ncol(data)])

# Create a SHAP visualization object
shap <- shapviz(S,X = data[1:nrow(shap_values),-1],baseline = shap_values[1,3][[1]][2])

# Save the SHAP visualization object to a file
saveRDS(shap,"data/shap.RDS")
