#' Data : 2025.11.7
#' Title : Machine-Learning-Based-Prediction-and-Analysis-of-Chinese-Youth-Marriage-Decision
#' Part : 8 Revision Round 1

#--------CODE--------#

library(haven)
library(dplyr)
library(caret)
library(doParallel)
library(missForest)
library(Boruta)
library(vcd)
library(ROSE)
library(caret)
library(mlr3)
library(mlr3learners)
library(mlr3extralearners)
library(mlr3tuningspaces)
library(mlr3hyperband)
library(mlr3pipelines)
library(mlr3measures)
library(mlr3misc)

set.seed(111)
rm(list = ls())
setwd(dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path))))

# Read data
raw_data <- readRDS("data/data_extracted.RDS")

factor_deside <- function(data_column){
  var.nam <- attr(data_column,"label")
  var.lab <- attr(data_column,"labels")
  pairs_str <- paste(names(var.lab), var.lab, sep = " = ", collapse = ", ")
  result <- paste0('变量名称：',var.nam,'  变量标签值：',pairs_str)
  return(result)
}

# Remove 24 various ID Variables
cut_index <- c(
  1:which(colnames(raw_data) == "provcd18"),
  which(colnames(raw_data) %in% c(
    "countyid18", "cid18", "psu", "interviewerid18"
  ))
)
data <- raw_data[, -cut_index] %>% as.data.frame()
data[] <- lapply(data, as.numeric)

# Remove 843 variables with zero or near-zero variance
nzv <- nearZeroVar(data)
data <- data[, -nzv]

# Label options without meaningful as missing values.
data[data < 0] <- NA
data <- lapply(data, function(x) {
  x[x %in% c(77, 78, 79)] <- NA
  return(x)
}) %>% as.data.frame()

# Eliminate 273 variables with missing rates exceeding 30%
miss <- colSums(is.na(data))
del_index <- which(miss >= nrow(data) * 0.3)
data <- data[, -del_index]

factor_deside_table_data <- raw_data[colnames(data)]

factor_deside_table <- data.frame(varname = rep(0,ncol(data)),describe= rep(0,ncol(data)))
all_names <- colnames(factor_deside_table_data)
for (i in 1:ncol(factor_deside_table_data)) {
  factor_deside_table$varname[i] = all_names[i]
  factor_deside_table$describe[i] = factor_deside(factor_deside_table_data[[i]])
}

writexl::write_xlsx(factor_deside_table,"data/factor_deside_table.xlsx")

factor_deside_table <- readxl::read_xlsx("data/factor_deside_table_done.xlsx")

for (i in 1:ncol(data)) {
  if (factor_deside_table$type[i] == 4) {
    data[[i]][data[[i]] %in% c(5, 6)] <- NA
  } else if (factor_deside_table$type[i] == 5) {
    data[[i]][data[[i]] %in% c(6, 7)] <- NA
  } else if (factor_deside_table$type[i] == 10) {
    data[[i]][data[[i]] %in% c(10)] <- 0
    data[[i]] <- as.factor(data[[i]])
  } else if (factor_deside_table$type[i] == 1) {
    data[[i]] <- as.factor(data[[i]])
  }
}

data <- subset(data, select = -which(factor_deside_table$type==3))

data$treat <- as.factor(ifelse(data$treat == 2, 1, 0))

# Dataset Splitting
trainIndex <- createDataPartition(data$treat, p = 0.7, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Employ random forest imputation to fill in the missing data 
numCores <- detectCores()
print(paste("Number of cores detected:", numCores))
cl <- makeCluster(numCores)
registerDoParallel(cl)
getDoParWorkers()

traindatafull <- missForest(trainData,ntree = 100,verbose = TRUE,variablewise = TRUE,parallelize = "forests")
testdatafull <- missForest(testData,ntree = 100,verbose = TRUE,variablewise = TRUE,parallelize = "forests")

stopCluster(cl)


trainData <- traindatafull$ximp
testData <- testdatafull$ximp

data <- rbind(trainData,testData)

#For every group of variables with high correlation, keep just one.
factor_data <- data[,sapply(data, is.factor)]

num_cols <- ncol(factor_data)

results <- data.frame(Variable1 = character(),
                      Variable2 = character(),
                      Cramers_V = numeric(),
                      stringsAsFactors = FALSE)

for (i in 1:(num_cols - 1)) {
  for (j in (i + 1):num_cols) {
    col1 <- factor_data[, i]
    col2 <- factor_data[, j]
    contingency_table <- table(col1, col2)

    if (dim(contingency_table)[1] < 2 | dim(contingency_table)[2] < 2) {
      next
    }
    assoc_result <- assocstats(contingency_table)
    cramers_v <- assoc_result$cramer
    new_row <- data.frame(Variable1 = names(factor_data)[i],
                          Variable2 = names(factor_data)[j],
                          Cramers_V = cramers_v)
    results <- rbind(results, new_row)
  }
}
high_cor_fac_pairs <- results[which(results$Cramers_V > 0.75),]

data[] <- lapply(data,as.numeric)

cor_mat <- cor(data)
high_cor_pairs <- which(abs(cor_mat) > 0.75 & abs(cor_mat) < 1, arr.ind = TRUE)
output_pairs <- NULL
high_var_names <- c()

for (i in 1:nrow(high_cor_fac_pairs)) {
  var1 <- high_cor_fac_pairs[i,1]
  var2 <- high_cor_fac_pairs[i,2]
  pair <- paste(sort(c(var1, var2)), collapse = "-")

  high_var_names <- c(high_var_names,var1,var2)

  if (!(pair %in% output_pairs)) {
    cat("Pair:", var1, "-", var2, "\n")
    output_pairs <- c(output_pairs, pair)
  }
}

for (i in 1:nrow(high_cor_pairs)) {
  var1 <- colnames(data)[high_cor_pairs[i, 1]]
  var2 <- colnames(data)[high_cor_pairs[i, 2]]
  pair <- paste(sort(c(var1, var2)), collapse = "-")

  high_var_names <- c(high_var_names,var1,var2)

  if (!(pair %in% output_pairs)) {
    cat("Pair:", var1, "-", var2, "\n")
    output_pairs <- c(output_pairs, pair)
  }
}

high_var_names <- unique(high_var_names)

save_var <- c("age","cesd20sc","cfps2018edu","cmonth","cohabitn","employ","gender",
              "jobclass","mathlist","metotal","qf705_a_1","qg303code_isei","qg9_a_1",
              "school","qp605_s_1","cyear","jobstartn","qf603_a_1")

use_var <- c(setdiff(colnames(data),high_var_names),save_var)
data <- datafull$ximp[,use_var]
data <- rbind(traindatafull$ximp[,use_var],testdatafull$ximp[,use_var])


# Using the Boruta algorithm for feature selection,
set.seed(111)

var_sec <- Boruta(treat~.,data = data,doTrace=1,pValue = 0.05,maxRuns = 200)
var_sec_res <- as.data.frame(var_sec$finalDecision)
seclected_var <- row.names(var_sec_res)[which(var_sec_res$`var_sec$finalDecision` %in%
                                                c("Confirmed"))]
# plotImpHistory(var_sec)
# plot(var_sec)

data <- data[,c(seclected_var,"treat")]

# Rename
variable_mapping <- list(
  "kw2y" = "Year_leave_school",
  "kw0" = "Other_education_experience",
  "qg3011" = "One_way_commute_time",
  "qu801" = "TV_importance",
  "cfps2018edu" = "Highest_level_of_education",
  "qg9_a_1" = "Endowment_insurance",
  "gender" = "Gender",
  "age" = "Age",
  "school" = "Schooling_status",
  "wk9" = "In_relationship",
  "cohabitn" = "Cohabitation_stage_number",
  "qga1" = "Full_time_experience",
  "jobstartn" = "New_jobs_started",
  "jobclass" = "Main_job_type",
  "qg303code_isei" = "ISEI",
  "qg401" = "Job_income_satisfaction",
  "qg402" = "Job_security_satisfaction",
  "qg403" = "Job_environment_satisfaction",
  "qg404" = "Job_time_satisfaction",
  "qg406" = "Job_total_satisfaction",
  "qg6" = "Weekly_working_hours",
  "qg19" = "Use_computer_in_work",
  "qf701_a_1" = "Father_financial_help",
  "qka201" = "Ideal_marriage_age",
  "qu702" = "Internet_work_frequency",
  "qu703" = "Internet_social_frequency"
)
colnames(data) <- sapply(colnames(data), 
                         function(x) ifelse(x %in% names(variable_mapping), 
                                            variable_mapping[[x]], x))

# Sample Balance
data <- ovun.sample(treat~.,data = data,
                    p=0.5,seed=1,method = "both")$data

# Factor Encoding
raw_task <- as_task_classif(data, target = "treat")

factor_encoding <-  
  po("encode", method = "one-hot", 
     affect_columns = selector_cardinality_greater_than(2),
     id = "low_card_enc") %>>%  
  po("encode", method = "treatment", 
     affect_columns = selector_type("factor"), id = "binary_enc")

raw_task <- factor_encoding$train(raw_task)$binary_enc.output

train_task <- as_task_classif(raw_task$data()[1:1191,], target = "treat")
test_task <- as_task_classif(raw_task$data()[1192:1700,], target = "treat")

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
  result_learner_param_vals[[i$id]] <- instance$result_learner_param_vals
}

future::plan("sequential")

# Updata the learners
learners = list(learner_log,learner_knn,
                learner_svm,learner_rf,learner_xgboost,
                learner_lightgbm,learner_catboost)


saveRDS(train_task,paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/train_task.RDS"))
saveRDS(test_task,paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/test_task.RDS"))
saveRDS(learners,paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/learners.RDS"))

#calculate

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

color_function <- ggsci::scale_colour_lancet
color_function_kwargs <- list(alpha = 0.9)

# Read data
learners <- readRDS("learners.RDS")
train_task <- readRDS("train_task.RDS")
test_task <- readRDS("test_task.RDS")

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

# Initialize a data frame to store individual model predictions
pre_res <- data.frame(matrix(ncol = 7, nrow = 6))
predictions <- list()
colnames(pre_res) <- c("learner","classif.auc","classif.acc",
                       "classif.precision","classif.recall","classif.specificity","F1")

# Populate the data frame with model performance metrics
for (i in 1:length(learners)) {
  prediction <- learners[[i]]$train(train_task)$predict(test_task)
  predictions[[i]] <- prediction
  pre_res[i,1] <- learners[[i]]$id
  pre_res[i,2:6] <- prediction$score(measures)
}
pre_res$F1 <- 2*(pre_res$classif.precision*pre_res$classif.recall/
                   (pre_res$classif.precision+pre_res$classif.recall))
colnames(pre_res) <- capitalize(gsub("classif.", "", colnames(table)))

writexl::write_xlsx(pre_res,"Performance_Separate_impute.xlsx")

###Dependency####

setwd(dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path))))

library(shapviz)
library(ggplot2)
library(dplyr)
library(scales)
library(ggsci)
library(patchwork)
library(ggraph)
library(ggpubr)
library(ggtext)
library(igraph)
library(geomtextpath)
library(Cairo)

rm(list=ls())
# Define a function to create SHAP dependency plots
shap <- readRDS("data/shap.RDS")
var_name <- colnames(shap$X)


color_function <- ggsci::scale_fill_lancet
color_function_kwargs <- list(alpha = 1)
use_pal <- pal_lancet(alpha = 0.5)(7)
use_pal_2 <- pal_lancet(alpha = 1)(7)

# Read data
shap <- readRDS("data/shap.RDS")
use_option <- "inferno"

# Generate a 'beeswarm summary' plot to visualize the importance of features
bee_plot <- sv_importance(shap,kind = "bee",max_display = 30)
name_order <- bee_plot$data$feature
bee_plot$data$feature <- factor(gsub("_" ," ",name_order),levels = gsub("_" ," ",levels(name_order)))
bee_plot <- bee_plot+theme(
  axis.title.x =element_text(size = rel(1.1),face = "bold"),
  legend.title = element_text(size=rel(1.1),face = "bold"),
  axis.text.y =  element_text(size = rel(0.85), face = "bold"),
  legend.text = element_text(size = rel(0.65), face = "bold"))
# bee_plot <- bee_plot+do.call(scale_color_viridis_c,list(begin = 0.35, end = 0.95, option = use_option,alpha = 0.9))


# Get the importance data
importance_data <- sv_importance(shap,max_display = 60L,kind = "bar")$data
var_name <- rownames(importance_data)

prefixes <- c("Main_job_type","Schooling_status","Highest_level_of_education")
matched_indices <- list()
for (prefix in prefixes) {
  pattern <- paste0("^", prefix, "\\.[0-9]+$") 
  indices <- grep(pattern, var_name) 
  matched_indices[[prefix]] <- indices 
}

raw_varimp_table <- importance_data[-unlist(matched_indices),]

bind_part <- data.frame(feature = prefixes,
                        value = lapply(prefixes,
                                       function(prefixes) sum(importance_data[matched_indices[[prefixes]],2])) %>% unlist())

blank_part <- data.frame(feature = c("Work Related","Demographic Variables",
                                     "Education Related","Marital Status and Attitudes",
                                     "Media usage", "Family Connection","Social security"),value = c(0,0,0,0,0,0,0))
varimp_table <- rbind(raw_varimp_table,bind_part,blank_part)
orders <- c(
  "Work Related",
  "One_way_commute_time",
  "Weekly_working_hours",
  "New_jobs_started",
  "Job_time_satisfaction",
  "Job_security_satisfaction",
  "Job_income_satisfaction",
  "Job_environment_satisfaction",
  "Job_total_satisfaction",
  "Full_time_experience",
  "Main_job_type",
  "ISEI",
  "Demographic Variables",
  "Age",
  "Gender",
  "Education Related",
  "Year_leave_school",
  "Other_education_experience",
  "Schooling_status",
  "Highest_level_of_education",
  "Marital Status and Attitudes",
  "Ideal_marriage_age",
  "In_relationship",
  "Cohabitation_stage_number",
  "Media usage",
  "Internet_work_frequency",
  "TV_importance",
  "Internet_social_frequency",
  "Use_computer_in_work",
  "Family Connection",
  "Father_financial_help",
  "Social security",
  "Endowment_insurance"
)
varimp_table <- varimp_table[order(match(varimp_table$feature, orders)),]
varimp_table$type <- c(rep("Work Related",12),rep("Demographic Variables",3),
                       rep("Education Related",5),rep("Marital Status and Attitudes",4),
                       rep("Media usage",5),rep("Family Connection",2),
                       rep("Social security",2))
varimp_table$label <- ifelse(varimp_table$value == 0,NA,varimp_table$value) %>% round(3)

blank_index <- which(is.na(varimp_table$label))
for (i in 1:(length(blank_index)-1)) {
  need_order <- varimp_table[(blank_index[i]+1):(blank_index[i+1]-1), ] 
  need_order <- need_order[order(-need_order$label),]
  varimp_table[(blank_index[i]+1):(blank_index[i+1]-1),] <- need_order
}

varimp_table$row_feature <- factor(varimp_table$feature, 
                                   levels = rev(unique(varimp_table$feature)),
                                   ordered = TRUE)
varimp_table$type <- factor(varimp_table$type, 
                            levels = unique(varimp_table$type),
                            ordered = TRUE)
name_order <- varimp_table$row_feature
varimp_table$feature <- factor(gsub("_" ," ",name_order),levels = gsub("_" ," ",levels(name_order)))
varimp_table$color <- use_pal[match(varimp_table$type,unique(varimp_table$type))]
varimp_table$color2 <- use_pal_2[match(varimp_table$type,unique(varimp_table$type))]

# Create a bar plot for the importance of variables
create_mytheme1 <- function() {
  top.mar <- 0.2
  right.mar <- 0.2
  bottom.mar <- 0.2
  left.mar <- 0.2
  theme(
    
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    
    # plot.title = element_text(size = rel(1), hjust = 0.5, face = "bold"),
    # plot.subtitle = element_text(size = rel(0.8), hjust = 0.5, face = "bold"),
    axis.title = element_text(size = rel(1), face = "bold"),
    axis.title.x = element_text(size = rel(0.85), face = "bold"),
    axis.text.y = element_text(size = rel(1.1), colour = get_colors(), face = "bold"),
    legend.title = element_text(size = rel(0.85), face = "bold"),
    legend.text = element_text(size = rel(0.65), face = "bold"),
    # legend.position = "none",
    plot.margin = unit(x = c(top.mar, right.mar, bottom.mar, left.mar), units = "inches")
  )
}

get_colors <- function() {
  g <- ggplot_build(p_total)
  mycol <- g$data[[1]]["fill"]
  col <- rev(mycol[, 1])
  num <- rev(varimp_table$value)
  index <- which(num == 0)
  col[index] <- "grey10"
  return(col)
}

varimp_table1 <- varimp_table |> filter(type %in% c("Work Related","Education Related","Media usage"))

p_2 <- ggplot(varimp_table, aes(x = value, y = feature, fill = type, na.rm = FALSE)) +
  geom_bar(stat = "identity") +  # 移除这里的 fill=varimp_table1$color2
  do.call(color_function, color_function_kwargs) +
  geom_text(aes(x = value, y = feature, label = label), size = 2.5, hjust = "left", nudge_x = 0.005) +
  scale_x_continuous(limits = c(0, 0.13), expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Importance ranking of variables after categorisation and aggregation",
    x = "mean|SHAP value|", y = "", fill = "Aspect",
    subtitle = "Total mean|SHAP value|="
  )

p_total <- ggplot(varimp_table1, aes(x = value, y = feature, fill = type, na.rm = FALSE)) +
  geom_bar(stat = "identity") +  # 移除这里的 fill=varimp_table1$color2
  # 手动指定“type分类 → color2颜色”的映射（关键：生成图例）
  scale_fill_manual(
    name = "Aspect",  # 图例标题（和你的环形图图例一致）
    values = setNames(varimp_table1$color2, varimp_table1$type),  # 绑定type和颜色
    drop = FALSE  # 强制显示所有type类别，避免部分类别缺失导致图例不完整
  ) +  # do.call(color_function, color_function_kwargs) +
  geom_text(aes(x = value, y = feature, label = label), size = 2.5, hjust = "left", nudge_x = 0.005) +
  scale_x_continuous(limits = c(0, 0.13), expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Importance ranking of variables after categorisation and aggregation",
    x = "mean|SHAP value|", y = "", fill = "Aspect",
    subtitle = "Total mean|SHAP value|="
  )

mytheme1 <- create_mytheme1()

bar <- p_total+theme_bw()+mytheme1
bar2 <- p_2+theme_bw()+mytheme1
# Drew the Circular dendrogram plot

A <- as.character(varimp_table[which(varimp_table$value != 0),"row_feature"])
A <- data.frame(A,varimp_table[which(varimp_table$row_feature %in% A ),"type"])
colnames(A) <- c("feature","type")
B <- as.character(importance_data$feature)

for (i in 1:length(matched_indices)) {
  index <- which(A[1] == names(matched_indices)[i])
  cat_part <- data.frame(B[matched_indices[[i]]],rep(A[index,1],length(B[matched_indices[[i]]])))
  colnames(cat_part) <- c("feature","type")
  part1 <- A[1:index,]
  part2 <- A[(index+1):nrow(A),]
  A <- rbind(part1,cat_part,part2)
}


index_blank <- which(varimp_table$value==0)

d1 <- data.frame(from="origin", to=varimp_table[index_blank,"feature"])
d2 <- data.frame(from = A$type,to = A$feature)
edges <- rbind(d1, d2)
vertices = data.frame(
  name = unique(c("origin",as.character(varimp_table[index_blank,"feature"]),as.character(edges$from), as.character(edges$to))) , 
  value = 0
) 

match_index1 <- match(vertices$name, importance_data$feature)
value1 <- importance_data$value[match_index1]
match_index2 <- match(vertices$name, bind_part$feature)
value2 <- bind_part$value[match_index2]
vertices$value <- ifelse(!is.na(value1), value1, value2)

for (i in 1:7) {
  if (i != 7) {
    vertices[i+1,"value"] <- sum(varimp_table[index_blank[i]:index_blank[i+1],"value"])
  } else {
    vertices[i+1,"value"] <- varimp_table[33,"value"]
  }
}

vertices$group = edges$from[ match( vertices$name, edges$to ) ]

vertices$id=NA
new_order <- 1:nrow(vertices)
new_order[37:40] <- c(38,39,40,37)
vertices <- vertices[new_order,]
myleaves=which(is.na( match(vertices$name, edges$from) ))
nleaves=length(myleaves)
vertices$id[ myleaves ] = seq(1:nleaves)
vertices$angle= 90 - 360 * vertices$id / nleaves
vertices$hjust<-ifelse( vertices$angle < -90, 1, 0)

# flip angle BY to make them readable
vertices$angle<-ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)
vertices[1,2] <- sum(vertices[2:8,2])

# Create a graph object
mygraph <- graph_from_data_frame( edges, vertices=vertices )

# Make the plot
new_plot <- ggraph(mygraph, layout = 'dendrogram', circular = TRUE)

new_plot$data$leaf <-TRUE
new_plot$data$leaf[1] <-FALSE
new_plot$data$group[2:8] <- blank_part$feature

spiral <- data.frame(new_plot$data[which(new_plot$data$group %in% prefixes),])
spiral <- rbind(new_plot$data[2:8,],spiral)
new_data <- data.frame(do.call(rbind, replicate(20, spiral[21,], simplify = FALSE)))
spiral <- rbind(spiral,new_data)
use_angels <- seq(-2.7,-2, length.out = 23)
spiral$x[21:43] <- 0.985*cos(use_angels)
spiral$y[21:43] <- 0.985*sin(use_angels)


spiral$x <- 0.95*spiral$x 
spiral$y <- 0.95*spiral$y 
spiral$label <- spiral$group
spiral$label[1:7] <- NA
spiral$group[8:nrow(spiral)] <- as.character(varimp_table[match(spiral$group[8:nrow(spiral)],varimp_table$row_feature),"type"])
spiral$label <- gsub("_"," ",spiral$label)
spiral$label[21:43] <- " Schl. status                           ."
spiral$group <- factor(spiral$group,levels = blank_part$feature)


match_index <- match(new_plot$data$group,vertices$name[9:11])
new_plot$data$group[which(!is.na(match_index))] <- vertices$group[9:11][match_index[which(!is.na(match_index))]]

new_plot$data$group <- factor(new_plot$data$group,levels = c(NA,blank_part$feature))

new_plot <- new_plot + 
  geom_edge_diagonal(colour = "grey", linewidth = 1.5) 

name_conversion <- readxl::read_xlsx("data/name_conversion.xlsx",col_names = FALSE)
name_match <- match(new_plot$data$name[which(new_plot$data$name %in% name_conversion$...1)],name_conversion$...1)
new_plot$data$name[which(new_plot$data$name %in% name_conversion$...1)] <- name_conversion$...2[name_match]
new_plot$data$name <- gsub("_"," ",new_plot$data$name)

angles <- c(0.5,-1,-2,3.1,2.2,1.6,1.3)

new_plot$data$x[2:8] <- 0.34*cos(angles)
new_plot$data$y[2:8] <- 0.34*sin(angles)
new_plot$data$text <- !is.na(new_plot$data$angle)
new_plot$data$x[9:11] <- 1.1*new_plot$data$x[9:11]
new_plot$data$y[9:11] <- 1.1*new_plot$data$y[9:11]

circle_df <- data.frame(x0 = 0, y0 = 0,r = 0.55)

new_plot <- new_plot+  
  geom_node_point(aes(filter = leaf, x = x * 1.05, y = y * 1.05, colour = group, size = value*1.9, alpha = 0.2)) + 
  geom_node_text(aes(x = x * 1.15, y = y * 1.15, filter = text, label = name, angle = angle, hjust = hjust, colour = group),
                 size = 4, alpha = 1) +
  # geom_node_circle(data = circle_df, aes(x0 = x0, y0 = y0, r = r), color = "black", linetype = "dashed", size = 1.1, fill = NA) +
  geom_textline(data = spiral[1:12,],aes(x=x,y=y,label = label,colour = group),size = 3,alpha = 1)+
  geom_textline(data = spiral[13:20,],aes(x=x,y=y,label = label,colour = group),size = 3,alpha = 1)+
  geom_textline(data = spiral[21:43,],aes(x=x,y=y,label = label,colour = group),size = 3,alpha = 1)+
  theme_void() +
  scale_alpha_continuous(guide = FALSE)+
  scale_colour_manual(name = "Aspect",values = rep(use_pal, 30))+
  scale_size_continuous(name  = expression(bolditalic(MSV)),
                        range = c(2, 17),
                        guide = guide_legend(fill = "grey", override.aes = list(colour = "grey")))+
  theme(legend.title = element_text(size = rel(1), face = "bold"),
        legend.text = element_text(size = rel(1), face = "bold"),
        legend.position = c(.8,.5),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.background = element_blank())+
  expand_limits(x = c(-4.2, 5), y = c(-2.5, 2.5))


xlab <- function(var, loss = FALSE, method){
  
  char1 <- ifelse(method == "lm", "1", "")
  char2 <- ifelse(loss == TRUE,"2", "")
  
  if (var %in% name_conversion$...1) {
    xlab_base <- as.character(name_conversion[which(name_conversion$...1==var),2])
    char3 <- as.character(which(prefixes == vertices[which(vertices$name==var),"group"])+2)
  } else {
    xlab_base <- var
    char3 <- ""
  }
  xlab_base <- gsub("_"," ",xlab_base)
  footnote <- paste(c(char1, char2, char3)[nchar(c(char1, char2, char3)) > 0],collapse = ",")
  xlab <- paste0("**",xlab_base,"<sup>",footnote,"</sup>**")
  return(list(xlab_base,xlab))
}

shap_point <- function(var, xlimit = NULL, ylimit = NULL, add_x_line = NULL, breaks = NULL, method = loess, y_title = TRUE,loss = FALSE) {
  
  if (var %in% names(matched_indices)){
    combin_shap_data <- data.frame(shap = numeric(0), x = numeric(0))
    for (i in matched_indices[[var]]) {
      d <- sv_dependence(shap,v = var_name[i])$data
      d <- d[d[,2]==1,c(1,2)]
      if (nrow(d)>0) {
        d[,2] <- as.numeric(substr(colnames(d)[2],nchar(colnames(d)[2]),nchar(colnames(d)[2])))
      }
      colnames(d) <- c("shap","x")
      combin_shap_data  <- rbind(combin_shap_data,d)
    }
    use_data <- combin_shap_data
  } else {
    use_data <- sv_dependence(shap, v = var)$data
  }
  
  col <- varimp_table[which(varimp_table$feature==strsplit(gsub("_"," ",var),"\\.")[[1]][1]),"color2"]
  MSV <-paste("=",round(vertices[which(vertices$name==var),"value"],3))
  
  if (y_title) {
    axis_text_y_setting <- element_text(size=rel(1.1))
  } else {
    axis_text_y_setting <- element_blank()
  }
  
  
  p <- ggplot(use_data, aes(x = use_data[,2], y = use_data[,1])) +
    geom_point(position = position_jitter(width = 0.5), colour = col,alpha = 0.2) +
    stat_smooth(method = method, level = 0.95, color="grey15", size = 2, span = 0.75,fill ="grey40") +
    geom_hline(yintercept = 0, linetype = 'dashed', col = "black", size = 1) +
    labs(x = xlab(var, loss = loss, method = as.character(substitute(method)))[[1]], y = "SHAP value") +
    theme(plot.title = element_blank(),
          axis.title = element_text(size = rel(1.6), face = "bold"),
          axis.title.x = element_markdown(),
          axis.title.y = axis_text_y_setting,
          axis.text.y = element_text(size=rel(1.3)),
          axis.text.x = element_text(size=rel(1.3)),
          axis.line = element_line(color = "black", size = 0.5),
          legend.position = "none",
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank() )
  
  if (!is.null(xlimit) & !is.null(breaks)) {
    p <- p + scale_x_continuous(limits = xlimit,breaks = breaks)
  } else if (!is.null(xlimit)) {
    p <- p + scale_x_continuous(limits = xlimit)
  } else if (!is.null(breaks)) {
    p <- p + scale_x_continuous(breaks = breaks)
  }
  
  if (!is.null(add_x_line)) {
    p <- p + geom_vline(xintercept = add_x_line, linetype='dashed', col = "black", size = 1)
  }
  
  if (!is.null(ylimit)) {
    p <- p + scale_y_continuous(limits = ylimit)
  }
  
  p <- p+annotate('text',x = Inf, y = -Inf,label=expression(bolditalic(MSV)),size=7,colour = "black",hjust=2.7,vjust=-1.5)+
    annotate('text',x = Inf, y = -Inf,label=MSV,size=7,colour = "black",hjust=1.1,vjust=-1.5,fontface = "bold")
  filename <- paste("Revision/Round1/Dependency/Fig_SHAP_", var, ".eps", sep = "")
  cairo_ps(filename = filename, onefile = FALSE, fallback_resolution = 700, width = 6, height = 4)
  print(p)
  dev.off()
  
  filename <- paste("Revision/Round1/Dependency/Fig_SHAP_", var, ".jpeg", sep = "")
  CairoJPEG(filename = filename,width = 6, height = 4,units="in",dpi=200)
  print(p)
  dev.off()
  
  assign(var, p, envir = .GlobalEnv)
  return(p)
}



# Draw plots
shap_point(var = "Age",xlimit = c(20,40))
shap_point(var = "Ideal_marriage_age",xlimit = c(25,40),ylimit = c(-0.3,0.2))
shap$X$In_relationship <- ifelse(shap$X$In_relationship == 0,1,0)
shap_point(var = "In_relationship")
shap_point(var = "Gender")
shap_point(var = "ISEI",xlimit = c(25,75),ylimit = c(-0.05,0.05))
shap_point(var = "Weekly_working_hours",add_x_line = c(40,60),breaks = c(0,25,40,50,60,75,100),xlimit = c(0,120),ylimit = c(-0.15,0.15))
shap_point(var = "Internet_social_frequency",method = lm)
shap_point(var = "Internet_work_frequency",ylimit = c(-0.1,0.1))
shap_point(var = "Job_total_satisfaction",ylimit = c(-0.025,0.025))
shap_point(var = "Cohabitation_stage_number",method = lm,ylimit = c(-0.025,0.05))
shap_point(var = "Job_environment_satisfaction",ylimit = c(-0.025,0.025))
shap_point(var = "Father_financial_help",ylimit = c(-0.015,0.015))
shap_point(var = "Job_time_satisfaction",ylimit = c(-0.05,0.05))
shap_point(var = "New_jobs_started",ylimit = c(-0.1,0.1),xlimit = c(-0.5,3))
shap_point(var = "Job_security_satisfaction",ylimit = c(-0.08,0.05))
shap_point(var = "Use_computer_in_work",ylimit = c(-0.025,0.025))
shap_point(var = "Job_income_satisfaction",ylimit = c(-0.025,0.025))
shap_point(var = "Full_time_experience",ylimit = c(-0.015,0.015))
shap_point(var = "Schooling_status.0",method = lm,ylimit = c(-0.02,0.02))
shap_point(var = "Main_job_type.4",method = lm,ylimit = c(-0.015,0.015))
shap_point(var = "Schooling_status.1",method = lm,ylimit = c(-0.02,0.02))
shap_point(var = "Main_job_type.2",method = lm,ylimit = c(-0.025,0.015))
shap_point(var = "Main_job_type.5",method = lm,ylimit = c(-0.03,0.015))
shap_point(var = "Main_job_type.1",method = lm,ylimit = c(-0.01,0.015))
shap_point(var = "Schooling_status.5",method = lm)
shap_point(var = "Main_job_type.3",loss = TRUE,ylimit = c(-0.01,0.01))
shap_point(var = "Endowment_insurance",ylimit = c(-0.01,0.01))
shap_point(var = "TV_importance",ylimit = c(-0.05,0.05))
shap_point(var = "One_way_commute_time",ylimit = c(-0.2,0.1))
shap_point(var = "Other_education_experience",ylimit = c(-0.02,0.02))
shap_point(var = "Year_leave_school",ylimit = c(-0.1,0.1))
shap_point(var = "Highest_level_of_education.1",method = lm,ylimit = c(-0.01,0.03))
shap_point(var = "Highest_level_of_education.2",method = lm)
shap_point(var = "Highest_level_of_education.3",method = lm,ylimit = c(-0.02,0.02))
shap_point(var = "Highest_level_of_education.4",ylimit = c(-0.02,0.02))
shap_point(var = "Highest_level_of_education.5",method = lm,ylimit = c(-0.01,0.01))
shap_point(var = "Highest_level_of_education.6",method = lm,ylimit = c(-0.01,0.01))
shap_point(var = "Highest_level_of_education.7",method = lm,ylimit = c(-0.01,0.01))
shap_point(var = "Highest_level_of_education.8",method = lm)

areas <- function(t, l, b, r, coln, rown) {
  areas <- list()
  
  for (z in 0:(rown - 1)) {
    for (c in 0:(coln - 1)) {
      c_len <- (r - l +1) / coln
      r_len <- (b - t +1) / rown
      area_now <- area(t = t + z * r_len, l = l + c * c_len, b = t + (z+1) * r_len-1, r = l + (c+1) * c_len-1)
      if (length(areas) == 0) {
        areas <- c(area_now)
      } else {
        areas <- c(areas, area_now)
      }
    }
  }
  return(areas)
}

A <- as.character(varimp_table[which(varimp_table$value != 0),"row_feature"])
A <- data.frame(A,varimp_table[which(varimp_table$row_feature %in% A ),"type"])
colnames(A) <- c("feature","type")
B <- as.character(importance_data$feature)

for (i in 1:length(matched_indices)) {
  index <- which(A[1] == names(matched_indices)[i])
  cat_part <- data.frame(B[matched_indices[[i]]],rep(A[index,2],length(B[matched_indices[[i]]])))
  colnames(cat_part) <- c("feature","type")
  cat_part <- cat_part[order(cat_part$feature),]
  part1 <- A[1:index-1,]
  part2 <- A[(index+1):nrow(A),]
  A <- rbind(part1,cat_part,part2)
}

p <- get(A[1,1])
for (i in c(1,2,4,5,6,7,10,11,12,13,14,8,15,18,19,20,21,22,23,24,25,34,36)) {
  p_part <- get(A[i,1])
  if (i != 1) {
    p <- p + p_part
  }
}

bar <- bar+theme(
  legend.title = element_text(size = rel(1.5), face = "bold"),
  legend.text = element_text(size = rel(1.5), face = "bold"),
  plot.background = element_rect(color = "red", size = 3),
  plot.margin = margin(t = 0,  # 顶部边缘距离
                       r = 0,  # 右边边缘距离
                       b = 0,  # 底部边缘距离
                       l = 0,  # 左边边缘距离
                       unit = "cm"),
  legend.key.width = unit(0.7, "cm"),  # 图例符号宽度（调大更醒目）
  legend.key.height = unit(0.7, "cm"),   # 图例符号高度（调大更醒目）
  legend.spacing.y = unit(1, "cm"),   # 图例项之间的间距（避免拥挤）
  panel.spacing = unit(0, "cm"))

legend_grob <- get_legend(bar)
legend_ggplot <- as_ggplot(legend_grob)


legend_ggplot

p <- p+legend_ggplot

designs <- c(areas(1,1,11,13,4,5),
             areas(12,1,13,10,3,1),
             area(11,9,14,13))
p <- p +
  plot_layout(ncol = 4)+
  plot_annotation(title = "\n \n \n \n",
                  caption = "\n \n \n \n",
                  tag_levels = list(c(LETTERS[1:23],""))) & 
  theme(plot.tag = element_text(size = 20,face = "bold"))

ggsave(
  "Revision/Round1/Fig_Group_with_Legend.jpeg",
  p,
  width = 15,  # 根据图例位置调整宽度/高度
  height = 15,
  dpi = 700
)



p <- get(A[1,1])
for (i in 1:39) {
  p_part <- get(A[i,1])
  if (i != 1) {
    p <- p + p_part
  }
}

bar2 <- bar2+theme(
  legend.title = element_text(size = rel(1.5), face = "bold"),
  legend.text = element_text(size = rel(1.5), face = "bold"),
  plot.background = element_rect(color = "red", size = 3),
  plot.margin = margin(t = 0,  # 顶部边缘距离
                       r = 0,  # 右边边缘距离
                       b = 0,  # 底部边缘距离
                       l = 0,  # 左边边缘距离
                       unit = "cm"),
  legend.key.width = unit(0.7, "cm"),  # 图例符号宽度（调大更醒目）
  legend.key.height = unit(0.7, "cm"),   # 图例符号高度（调大更醒目）
  legend.spacing.y = unit(99, "cm"),   # 图例项之间的间距（避免拥挤）
  panel.spacing = unit(0, "cm"))

legend_grob2 <- get_legend(bar2)
legend_ggplot2 <- as_ggplot(legend_grob2)

p <- p +
  legend_ggplot2

design <- c(areas(t = 1,l = 1,b = 3,r = 15,coln = 5,rown = 3),
            areas(t = 4,l = 1,b = 4,r = 6,coln = 2,rown = 1),
            areas(t = 4,l = 7,b = 4,r = 15,coln = 3,rown = 1),
            areas(t = 5,l = 1,b = 7,r = 15,coln = 5,rown = 3),
            areas(t = 8,l = 1,b = 8,r = 12,coln = 4,rown = 1),
            area(8,13,8,15))

# design$r[which(design$r==15)] <- 17


p <- p +
  plot_layout(design = design)+
  plot_annotation(title = "\n \n \n \n",
                  caption = "\n \n \n \n",
                  tag_levels = list(c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10",
                                      "A11","A12","A13","A14","A15","B1","B2","C1","C2",
                                      "C3","C4","C5","C6","C7","C8","C9","C10","C11","C12",
                                      "C13","D1","D2","D3","E1","E2","E3","E4","F1","G1"))
  ) & theme(plot.tag = element_text(size = 20,face = "bold"))


CairoJPEG(filename = "Revision/Round1/total.jpeg",width = 25, height = 25,units="in",dpi=700)
p
dev.off()
