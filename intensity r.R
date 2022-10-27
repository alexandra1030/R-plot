library(tidyverse)
library(ggplot2)
library('readxl')
library(datasets)
library(sqldf)
library(viridis)
library(hrbrthemes)


setwd("/Users/yue.li/同步空间/project/article making/2. whole brain staining with shigeaki/figure3/intenisity/")
getwd()

temprt_condition_penetrat <- read_xlsx("5 signals.xlsx")

summary_datapene <- data.frame(temprt_condition_penetrat)

condition <- summary_datapene$Condition
sample_intensity<- summary_datapene$Intensity.Ratio

x = c("4°C","20°C","37°C", "55°C", "70°C")

summary_datapene$Condition = factor(summary_datapene$Condition, levels = x)
factor(summary_datapene$Condition)

box_pene <- ggplot(data = summary_datapene, aes (x = as.factor(Condition),y = Intensity.Ratio, color = as.factor(Condition), fill = as.factor(Condition))) + 
  geom_boxplot(alpha = 0.4) +  
  stat_summary(fun= mean, geom = "point", shape = 2, size = 5, color = "red")+
  geom_jitter(size = 2, alpha = 1)+
  theme_classic()+
  theme(
    legend.position = "none")+
  ggtitle("Quantification of HCR Efficiency") + theme(plot.title = element_text(hjust = 0.5, size = 28)) + 
  xlab("Temperature")+ 
  ylab("Normalized Signal Intensity") +
  theme(axis.text= element_text(color="black", size=24),
        axis.title = element_text(color="black", size=24),
        panel.grid.minor = element_blank())
box_pene 


ggsave(path = "/Users/yue.li/同步空间/project/article making/2. whole brain staining with shigeaki/figure3/intenisity/", 
       "5 intensity.tiff", 
       height =6, 
       width = 8, dpi = 320)

box_pene <- ggplot(data = Thy1_data, aes (x = as.factor(Temperature),y = Percentage.Penetration..., fill = as.factor(Temperature))) + 
  geom_boxplot(alpha = 0.4) +  
  stat_summary(fun= mean, geom = "point", shape = 10, size = 5, color = "yellow")+
  geom_jitter(size = 2, color = 20, alpha = 1)+
  theme_ipsum()+
  theme_minimal() +
  theme(
    legend.position = "none")+
  ggtitle("Thy1") + theme(plot.title = element_text(hjust = 0.5, size = 28)) + 
  xlab("Condition")+ 
  ylab("% Penetration Efficiency") +
  theme(axis.text= element_text(color="black", size=24),
        axis.title = element_text(color="black", size=24))
box_pene + theme_classic()

ggsave(path = "/Users/yue.li/同步空间/project/article making/2. whole brain staining with shigeaki/", 
       "box chart.png", 
       height = 5, 
       width = 7, dpi = 320)



ggplot(Thy1_data, aes(as.factor(condition), percentage_value, fill = as.factor(condition))) + geom_boxplot(alpha = 0.5, 
                                                                                                           size=0.5,fill="white",outlier.fill="white",outlier.color="white")+
  stat_summary(fun= mean, geom = "point", shape = 20, size = 3, color = "red")+
  geom_jitter(color = "black", size = 1, alpha = 1)+
  theme(
    legend.position = "none")+
  ggtitle("Thy1") + theme(plot.title = element_text(hjust = 0.5, size = 18)) + 
  xlab("Condition")+ 
  ylab("% Penetration Efficiency") +
  theme(axis.text= element_text(color="black", size=16),
        axis.title = element_text(color="black", size=16))

ggsave(path = "/Users/yue.li/同步空间/project/article making/2. whole brain staining with shigeaki/", 
       "chart.png", 
       height = 5, 
       width = 7, dpi = 320)

opar <- par(mfrow=c(1,2))
barplot(height = Thy1_data,main = 'a',sub = 'Thy1')
barplot(height = Malat1_data,main = 'b',sub = 'Malat1')

figure_circular_plot <- function(Thy1_data,title){
  condition <- Thy1_data$Temperature
  sample_num <- Thy1_data$Sample
  percentage_value <- Thy1_data$Percentage.Penetration...
  
  
  ggplot(Thy1_data, aes(x = condition, y = percentage_value, fill = sample_num)) + 
    geom_bar(stat = "identity", position = "dodge", alpha = 0.7)+
    coord_polar(start = 0)+theme_ipsum() +
    theme_minimal() +
    theme(
      legend.position = "none")+
    ggtitle(title) + theme(plot.title = element_text(hjust = 0.5, size = 28)) + 
    xlab("Condition")+ 
    ylab("% Penetration Efficiency") +
    theme(axis.text= element_text(color="black", size=24),
          axis.title = element_text(color="black", size=24)) +  
    
    ggsave(path = "/Users/yue.li/同步空间/project/article making/2. whole brain staining with shigeaki/", 
           "chart.png", 
           height = 5, 
           width = 7, dpi = 320)
  
}

figure_circular_plot(Malat1_data,"Malat1")
