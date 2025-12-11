#' Data : 2024.8.19
#' Title : Machine-Learning-Based-Prediction-and-Analysis-of-Chinese-Youth-Marriage-Decision
#' Part : 5 Model Evaluation

#' Package Versions : 
#  "mlr3 0.20.2"
#  "mlr3misc 0.15.1"
#  "mlr3viz 0.9.0"
#  "ggplot2 3.5.1"
#  "rempsyc 0.1.8"
#  "ggsci 3.2.0"
#  "scales 1.3.0"
#  "Cairo 1.6.2"

#--------CODE--------#

library(mlr3)
library(mlr3misc)
library(mlr3viz)
library(ggplot2)
library(rempsyc)
library(ggsci)
library(scales)
library(Cairo)


rm(list = ls())
set.seed(111)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

color_function <- ggsci::scale_colour_lancet
color_function_kwargs <- list(alpha = 0.9)

# Read data
learners <- readRDS("data/learners.RDS")
train_task <- readRDS("data/train_task.RDS")
test_task <- readRDS("data/test_task.RDS")

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

# Combine the aggregated and individual model results into a single table
performance_table <- rbind(table,pre_res)
my_table <- nice_table(performance_table)
my_table

# Save the formatted table as a .docx file
flextable::save_as_docx(my_table,path = "Table/Table_model_performance.docx")


# Create an ROC plot for selected models
plot <- autoplot(bmr$clone(deep = T)#$filter(learner_id = c("catboost","svm","randomForest","logistic","knn"))
                 ,type = "roc",)

data <- data.frame(x = plot$data$x,y = plot$data$y, Learners = plot$data$modname)

plot_ROC <- ggplot(data,aes(x=x,y=y,fill=Learners,colour = Learners))+
  stat_smooth(method = "gam",se=F)+
  do.call(color_function,color_function_kwargs)+
  labs(x = "False positive rate", y = "True positive rate") +
  scale_x_continuous(limits = c(-0.01,1.01),breaks = c(0,0.5,1))+
  scale_y_continuous(limits = c(-0.01,1.1),breaks = c(0,0.5,1))+
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               linetype = "dashed", color = "red",linewidth = 1.5)+
  # geom_segment(aes(x = 0.74, y = 0.76, xend = 0.64, yend = 0.86), 
  #              linetype = "solid", color = "black",linewidth = 2,
  #              arrow = arrow(length = unit(0.5, "cm")))+
  # geom_segment(aes(x = 0.76, y = 0.74, xend = 0.86, yend = 0.64), 
  #              linetype = "solid", color = "black",linewidth = 2,
  #              arrow = arrow(length = unit(0.5, "cm")))+
  geom_point(aes(x=0,y=1),color = "blue" ,size = 4, show.legend = FALSE)+
  theme(axis.title.x = element_text(size = rel(1.3), face = "bold"),
        axis.title.y = element_text(size = rel(1.3), face = "bold"),
        axis.text.y = element_text(size=rel(1.6)),
        axis.text.x = element_text(size=rel(1.6)),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey",linewidth = 0.8),
        panel.grid = element_line(colour = "grey",linewidth = 0.8),
        legend.text = element_text(size = rel(1.1), face = "bold"),
        legend.title = element_text(size = rel(1.3), face = "bold"))+
  annotate("text", x = 0.65, y = 0.6, 
             label = "Random classifier", color = "red", 
             hjust = 1, vjust = 1, angle = 45,size = 5,fontface = "bold")+
  # annotate("text", x = 0.9, y = 0.6, 
  #          label = "Worse", color = "black", size = 6,fontface = "bold")+
  # annotate("text", x = 0.6, y = 0.91, 
  #          label = "Better", color = "black",size = 6,fontface = "bold")+
  annotate("text", x = 0.12, y = 1.04, 
           label = "Perfect classifier", color = "blue",size = 5,fontface = "bold")

# Save the ROC plot as an .eps file
cairo_ps(filename = "Figure/Fig_ROC.eps", onefile = FALSE, fallback_resolution = 700, width = 7.5, height = 6)
print(plot_ROC)
dev.off()

CairoJPEG(filename = "Figure_small/Fig_ROC.jpeg", width = 7.5, height = 6,units="in",dpi=200)
print(plot_ROC)
dev.off()
