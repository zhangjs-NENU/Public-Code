#' Data : 2025.11.7
#' Title : Machine-Learning-Based-Prediction-and-Analysis-of-Chinese-Youth-Marriage-Decision
#' Part : 8 Revision Round 1

#--------CODE--------#

library(ROSE)
library(caret)
library(mlr3)
library(mlr3misc)
library(mlr3learners)
library(mlr3extralearners)
library(mlr3tuningspaces)
library(mlr3hyperband)
library(mlr3pipelines)

###For Reviewer 1 Comment 2###

rm(list = ls())
set.seed(111)
setwd(dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path))))

# Read data
data <- readRDS(data,file = "data/data_post_FE.RDS")

# # Sample Balance
# data <- ovun.sample(treat~.,data = data,
#                     p=0.5,seed=1,method = "both")$data

# Factor Encoding
raw_task <- as_task_classif(data, target = "treat")

factor_encoding <-  
  po("encode", method = "one-hot", 
     affect_columns = selector_cardinality_greater_than(2),
     id = "low_card_enc") %>>%  
  po("encode", method = "treatment", 
     affect_columns = selector_type("factor"), id = "binary_enc")

raw_task <- factor_encoding$train(raw_task)$binary_enc.output

# Dataset Splitting
trainIndex <- createDataPartition(data$treat, p = 0.7, list = FALSE)
trainData <- raw_task$data()[trainIndex, ]
testData <- raw_task$data()[-trainIndex, ]

train_task <- as_task_classif(trainData, target = "treat")
test_task <- as_task_classif(testData, target = "treat")

#Build tasks/learners & Configure benchmarks
learner_rf <- lrn("classif.ranger", predict_type="prob")
learner_rf$id <- "randomForest"

learner_log <- lrn("classif.log_reg", predict_type="prob")
learner_log$id <- "logistic"

learner_xgboost <- lrn("classif.xgboost", predict_type="prob")
learner_xgboost$id <- "xgboost"

learner_lightgbm <-  lrn("classif.lightgbm", predict_type="prob") 
learner_lightgbm$id <- "lightgbm"

learner_catboost <- lrn("classif.catboost",predict_type = "prob")
learner_catboost$id <- "catboost"

learner_knn <- lrn("classif.kknn",predict_type = "prob")
learner_knn$id <- "knn"

learner_svm <- lrn("classif.svm",predict_type = "prob",
                   id = "svm",type = "C-classification",kernel = "radial",
                   scale = T)

learners_raw = list(learner_log,learner_knn,
                    learner_svm,learner_rf,learner_xgboost,
                    learner_lightgbm,learner_catboost)

saveRDS(learners_raw,"data/learners_raw.RDS")

# Set search space
search_space <- list()
result_learner_param_vals <- list()


search_space[[learner_rf$id]] <- ps(  
  num.trees = p_int(lower = 200, upper = 1500,tags = "budget"),
  max.depth = p_int(lower = 1, upper = 30),
  alpha = p_dbl(lower = 1e-1, upper = 1),
  num.threads = p_int(lower = 1, upper = 20)
)  

search_space[[learner_log$id]] <- ps(
  epsilon = p_dbl(lower = 1e-12, upper = 1e-6, logscale = TRUE),  
  maxit = p_dbl(lower = 10, upper = 1000,tags = "budget")
)

search_space[[learner_xgboost$id]] <- ps(
  nrounds           = p_int(lower = 16, upper = 2048, tags = "budget"),
  eta               = p_dbl(lower = 1e-4, upper = 1, logscale = TRUE),
  max_depth         = p_int(lower = 1, upper = 30),
  colsample_bytree  = p_dbl(lower = 1e-1, upper = 1),
  colsample_bylevel = p_dbl(lower = 1e-1, upper = 1),
  lambda            = p_dbl(lower = 1e-3, upper = 1e3, logscale = TRUE),
  alpha             = p_dbl(lower = 1e-3, upper = 1e3, logscale = TRUE),
  subsample         = p_dbl(lower = 1e-1, upper = 1),
  min_child_weight  = p_dbl(1, 10)
)

search_space[[learner_knn$id]] <- ps(  
  k = p_int(lower = 1, upper = 10, tags = "budget"),
  distance = p_dbl(lower = 0,upper = 10)
)

search_space[[learner_catboost$id]] <- ps(  
  iterations = p_int(lower = 1000,upper = 5000),
  learning_rate = p_dbl(lower = 0.01, upper = 0.1,tags = "budget")
)

search_space[[learner_lightgbm$id]] <- ps(  
  num_leaves = p_int(lower = 5, upper = 50),   
  learning_rate = p_dbl(0.01, 0.1, tags = "budget"),
  num_iterations = p_int(lower = 20, upper = 500)
)

search_space[[learner_svm$id]] <- ps(
  cost = p_dbl(0.1, 10, tags = "budget"),
  gamma = p_dbl(0, 5)
)

learners = list(learner_log,learner_knn,
                learner_svm,learner_rf,learner_xgboost,
                learner_lightgbm,learner_catboost)

future::plan("multisession")

# Hyperparameter parameter tuning
set.seed(111)
for(i in learners){
  instance <- mlr3tuning::tune(
    tuner = tnr("hyperband", eta = 2),
    task = train_task,
    learner = i,
    resampling = rsmp("holdout",ratio =0.7),
    measure = msr("classif.auc"),
    search_space = search_space[[i$id]],
    store_models = T,
  )
  i$param_set$values <- instance$result_learner_param_vals
  result_learner_param_vals[[i$id]] <- instance$archive$data
}

future::plan("sequential")

# Updata the learners
learners = list(learner_log,learner_knn,
                learner_svm,learner_rf,learner_xgboost,
                learner_lightgbm,learner_catboost)

#Performance_tune
pre_res <- data.frame(matrix(ncol = 3, nrow = 7))
colnames(pre_res) <- c("learner","Auc_before","Auc_after")
learners_raw <- readRDS("data/learners_raw.RDS")

for (i in 1:7) {pre_res[i,1] <- learners[[i]]$id}

design_raw <- benchmark_grid(
  tasks = train_task,
  learners = learners_raw,
  resamplings =rsmp("cv", folds = 10) 
)
design <- benchmark_grid(
  tasks = train_task,
  learners = learners,
  resamplings =rsmp("cv", folds = 10) 
)

bmr_raw <- benchmark(design_raw)
bmr <- benchmark(design)

pre_res[,2] <- bmr_raw$aggregate(msr("classif.auc"))$classif.auc
pre_res[,3] <- bmr$aggregate(msr("classif.auc"))$classif.auc

# Create a benchmark grid for model evaluation
design <- benchmark_grid(
  tasks = train_task,
  learners = learners,
  resamplings =rsmp("cv", folds = 10) 
)

# Perform the benchmarking
future::plan("multisession")
bmr <- benchmark(design)

# Define the performance measures to evaluate
measures <- msrs(c("classif.auc","classif.acc",
                   "classif.precision", "classif.recall","classif.specificity"))

# Aggregate the results based on the defined measures
bmr_res <- bmr$aggregate(measures)

# Prepare the performance table for visualization
table <- bmr_res[,c(4,7:11)]
table$F1 <- 2*(table$classif.precision*table$classif.recall/
                 (table$classif.precision+table$classif.recall))
colnames(table) <- capitalize(gsub("classif.", "", colnames(table)))

writexl::write_xlsx(table,"Revision/Round1/Performance_without_balance.xlsx")

