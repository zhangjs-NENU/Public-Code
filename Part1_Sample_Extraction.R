#' Data : 2024.8.19
#' Title : Machine-Learning-Based-Prediction-and-Analysis-of-Chinese-Youth-Marriage-Decision
#' Part : 1 Sample Extraction
#' Note : The CFPS data is restricted data that requires an application,
#'        I am not authorized to release the original file here, 
#'        for the data file please visit https://www.isss.pku.edu.cn/cfps/

#' Package Versions :
#  "haven 2.5.4"

#--------CODE--------#

library(haven)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read data
crossyear_data <- read_stata("data/cfps2020crossyearid.dta")
data2018 <- read_stata("data/cfps2018person.dta")

# Filter crossyear_data for individuals born between 1978 and 1998 with self-reported data
crossyear_data <- crossyear_data[crossyear_data$birthy >= 1978 &
  crossyear_data$birthy <= 1998 &
  crossyear_data$selfrpt18 == 1, ]

# Exclude individuals born after 1995 and identified as male
crossyear_data <- crossyear_data[-which(crossyear_data$gender == 1 &
  crossyear_data$birthy >= 1996), ]

# Identify married group individuals based on changes in marital status
treat_pid <- intersect(crossyear_data[crossyear_data$marriage_18 %in% c(1, 3) &
  crossyear_data$marriage_20 %in% c(2, 4, 5), ]$pid, data2018$pid)

# Identify unmarried group individuals based on consistent marital status
con_pid <- intersect(crossyear_data[crossyear_data$marriage_18 %in% c(1, 3) &
  crossyear_data$marriage_20 %in% c(1, 3), ]$pid, data2018$pid)

# Combine the treatment and control group IDs
total_pid <- c(con_pid, treat_pid)

data <- data2018[data2018$pid %in% total_pid, ]
data$treat <- as.factor(ifelse(data$pid %in% treat_pid, 1, 0))

# Check
abnormal_pid <- data[data$marriage_last %in% c(2,4,5) | data$marriage_last_update %in% c(2,4,5),"pid"]
data <- data[-which(data$pid %in% abnormal_pid$pid),]

# Save the 'data' object to an RDS file named 'data_extracted.RDS'
saveRDS(data,file = "data/data_extracted.RDS")
