#' Data : 2025.11.7
#' Title : Machine-Learning-Based-Prediction-and-Analysis-of-Chinese-Youth-Marriage-Decision
#' Part : 8 Revision Round 1

#--------CODE--------#

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

rm(list = ls())
set.seed(111)
setwd(dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path))))

color_function <- ggsci::scale_fill_lancet
color_function_kwargs <- list(alpha = 0.5)
use_pal <- pal_lancet(alpha = 0.5)(7)
use_pal_2 <- pal_lancet(alpha = 0.7)(7)

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


# Save the plot
cairo_ps(filename = "Figure/Fig_bee.eps", onefile = FALSE, fallback_resolution = 700, width = 8, height = 6)
print(bee_plot)
dev.off()

CairoJPEG(filename = "Figure_small/Fig_bee.jpeg", width = 8, height = 6,units="in",dpi=200)
print(bee_plot)
dev.off()

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
    legend.position = "none",
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

p_total <- ggplot(varimp_table, aes(x = value, y = feature, fill = type, na.rm = FALSE)) +
  geom_bar(stat = "identity") +
  do.call(color_function, color_function_kwargs) +
  geom_text(aes(x = value, y = feature, label = label), size = 2.5, hjust = "left", nudge_x = 0.005) +
  scale_x_continuous(limits = c(0, 0.13), expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Importance ranking of variables after categorisation and aggregation",
    x = "mean|SHAP value|", y = "", fill = "Aspect",
    subtitle = "Total mean|SHAP value|="
  )

mytheme1 <- create_mytheme1()

cairo_ps(filename = "Figure/Fig_SHAP_total_bar.eps",onefile = FALSE,fallback_resolution = 700,width = 6.5,height = 6)
p_total+theme_bw()+mytheme1
dev.off()

CairoJPEG(filename = "Figure_small/Fig_SHAP_total_bar.jpeg",width = 6.5,height = 6,units="in",dpi=200)
p_total+theme_bw()+mytheme1
dev.off()

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


cairo_ps(filename = "Figure/Fig_Circular_dendrogram.eps",onefile = FALSE,fallback_resolution = 700,width = 15,height = 8)
print(new_plot)
dev.off()

CairoJPEG(filename = "Figure_small/Fig_Circular_dendrogram.jpeg",width = 15,height = 8,units="in",dpi=200)
print(new_plot)
dev.off()

p_blank <- ggplot() + theme_void()

p_combine <- p_total+theme_bw()+mytheme1+new_plot+p_blank+
  plot_layout(design = c(area(1,1,1,6),area(1,6,1,20),area(1,8,1,8)))+plot_annotation(tag_levels = list(c("A","","B"))) & theme(plot.tag = element_text(size = 24,face = "bold"))

cairo_ps(filename = "Figure/Fig_SHAP_combine.eps", onefile = FALSE, fallback_resolution = 700, width = 22, height = 9)
p_combine
dev.off()

CairoJPEG(filename = "Figure_small/Fig_SHAP_combine.jpeg",width = 22, height = 9,units="in",dpi=200)
p_combine
dev.off()

nnew_plot <- new_plot + theme(legend.margin = margin(l = 5, unit = "cm"),
                              plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
                              axis.title.x = element_text(colour = "white",
                                                          margin = margin(t = 5) 
                              ))
p_combine <- p_total + theme_bw() + mytheme1 + nnew_plot +
  plot_layout(design = c(area(1, 3, 1, 6),  
                         area(2, 1, 2, 9))) + 
  plot_annotation(tag_levels = list(c("A", "B"))) & 
  theme(plot.tag = element_text(size = 24, face = "bold"))

cairo_ps(filename = "Revision/Round1/Fig_SHAP_combine.eps", onefile = FALSE, fallback_resolution = 700, width = 13, height = 15.5)
p_combine
dev.off()

CairoJPEG(filename = "Revision/Round1/Fig_SHAP_combine.jpeg",width = 13, height = 15.5,units="in",dpi=700)
p_combine
dev.off()
