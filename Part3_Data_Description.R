#' Data : 2024.8.19
#' Title : Machine-Learning-Based-Prediction-and-Analysis-of-Chinese-Youth-Marriage-Decision
#' Part : 3 Data Description

#' Package Versions : 
#  "dplyr 1.1.4"
#  "rempsyc 0.1.8"
#  "flextable 0.9.6"
#  "corrplot 0.94"
#  "Cairo 1.6.2"

#--------CODE--------#

library(dplyr)
library(rempsyc)
library(flextable)
library(corrplot)
library(Cairo)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read data
data <- readRDS(data,file = "data/data_post_FE.RDS")

# Select numeric and factor columns for analysis
numeric_columns <- data %>% select_if(is.numeric) %>% colnames()
factor_columns <- data %>% select_if(is.factor) %>% select(-treat) %>% colnames() 

# Calculate means and standard deviations for continuous variables
summary_results <- data.frame(matrix(ncol = 4))
colnames(summary_results) <- c("Variable","M(SD) \n Total","M(SD) \n treat = 0","M(SD) \n treat = 1")

for (column_index in 1:length(numeric_columns)) {
  column <- numeric_columns[column_index]
  column_result <- data.frame(matrix(ncol = 4))
  colnames(column_result) <- c("Variable","M(SD) \n Total","M(SD) \n treat = 0","M(SD) \n treat = 1")
  for (i in c(2,0,1)) {
    if (i == 2) {
      mean_value <- mean(data[[column]], na.rm = TRUE)
      sd_value <- sd(data[[column]], na.rm = TRUE)
      column_result[1,1] <- column
      column_result[1,2] <- paste(round(mean_value,2),
                                  "(", round(sd_value,2), ")", sep = "")
    }else{
      mean_value <- mean(data[which(data$treat == i),column], na.rm = TRUE)
      sd_value <- sd(data[which(data$treat == i),column], na.rm = TRUE)
      column_result[1,i+3] <- paste(round(mean_value,2),
                                    "(", round(sd_value,2), ")", sep = "")
    }
  }
  summary_results[column_index,] <- column_result
}

# Perform t-tests and bind results with summary data
t_test_res<- nice_t_test(data,
                         response = numeric_columns,
                         group = "treat",var.equal = TRUE)

summary_table <- cbind(summary_results,t_test_res[-1])

summary_table <- nice_table(summary_table,
                            note = "* p < .05, ** p < .01, *** p < .001")

summary_table


# Create a summary table with statistical tests and save as a Word document
flextable::save_as_docx(summary_table,path = "Table/Table_t_test.docx")


# Perform chi-square tests for factor columns and create contingency tables
summary_results2 <- data.frame(matrix(ncol = 8, nrow = 0))
colnames(summary_results2) = c("Variable","Type","N","N treat=0","N treat=1","χ²","df","p")
for (i in 1:length(factor_columns)) {
  start_row <- nrow(summary_results2)+1
  chisq_result <- chisq.test(table(data[[factor_columns[i]]],data$treat))
  summary_results2[start_row,1] = factor_columns[i]
  summary_results2[start_row,6] = chisq_result$statistic
  summary_results2[start_row,7] = chisq_result$parameter
  summary_results2[start_row,8] = chisq_result$p.value
  
  Contingency_table <- cbind(table(data[[factor_columns[i]]]),
                             table(data[[factor_columns[i]]],data$treat)) %>% as.data.frame()
  end_row <- start_row+nrow(Contingency_table)-1
  summary_results2[start_row:end_row,3:5] <- Contingency_table
  summary_results2[start_row:end_row,2] <- rownames(Contingency_table)
}

summary_table2 <- nice_table(summary_results2,
                             note = "* p < .05, ** p < .01, *** p < .001")
summary_table2

# Create a summary table for categorical variables and save as a Word document
flextable::save_as_docx(summary_table2,path = "Table/Table_chi-square_test.docx")


# Calculate the correlation matrix for continuous variables
M <- cor(data[which(colnames(data)%in%numeric_columns)])
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(data[which(colnames(data)%in%numeric_columns)])

# Plot and save correlation plots for continuous variables
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))


setEPS()
postscript("Figure/Fig_COR.eps")
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", #添加相关系数
         p.mat = p.mat, sig.level = 0.01, insig = "blank", #显著性筛选
         tl.col="black", tl.srt=90,tl.cex = 0.8,
         number.cex = 0.55, cl.cex = 0.9,#修改字体
         diag=TRUE )
dev.off()

CairoJPEG(filename = "Figure_small/Fig_COR.jpeg",width = 7,height = 7,units="in",dpi=200)
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", #添加相关系数
         p.mat = p.mat, sig.level = 0.01, insig = "blank", #显著性筛选
         tl.col="black", tl.srt=90,tl.cex = 0.8,
         number.cex = 0.55, cl.cex = 0.9,#修改字体
         diag=TRUE )
dev.off()
