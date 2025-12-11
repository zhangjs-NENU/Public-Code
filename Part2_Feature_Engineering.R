#' Data : 2024.8.19
#' Title : Machine-Learning-Based-Prediction-and-Analysis-of-Chinese-Youth-Marriage-Decision
#' Part : 2 Feature Engineering

#' Package Versions : 
#  "haven 2.5.4"
#  "dplyr 1.1.4"
#  "caret 6.0.94"
#  "doParallel 1.0.17"
#  "missForest 1.5"
#  "Boruta 8.0.0"
#  "vcd 1.4.13"

#--------CODE--------#

library(haven)
library(dplyr)
library(caret)
library(doParallel)
library(missForest)
library(Boruta)
library(vcd)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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

# Employ random forest imputation to fill in the missing data 
set.seed(111)
numCores <- detectCores()
print(paste("Number of cores detected:", numCores))
cl <- makeCluster(numCores)
registerDoParallel(cl)
getDoParWorkers()

datafull <- missForest(data,ntree = 100,verbose = TRUE,variablewise = TRUE,parallelize = "forests")

stopCluster(cl)

saveRDS(datafull,"data/datafull.RDS")
data <- datafull$ximp

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

# Save the 'data' object to an RDS file named 'data_post_FE.RDS'
saveRDS(data,file = "data/data_post_FE.RDS")
