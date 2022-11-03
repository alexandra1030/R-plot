library(tidyverse)
library(ggplot2)
library('readxl')
library(datasets)
library(sqldf)
library(viridis)
library(hrbrthemes)

setwd("/Users/yue.li/Library/CloudStorage/OneDrive-KarolinskaInstitutet/ARO-DIIFCO/data")
getwd()


area_brain<- read_xlsx("figure 3-1 clearing reduction in brain.xlsx")
summary_area <- data.frame(area_brain[1:4,1:3])

x = c("pre","post")

summary_area$Stage = factor(summary_area$Stage, levels = x)
factor(summary_area$Stage)

p<-ggplot(summary_area, aes(x=Group,y=Brain.area,fill=Stage))+
  geom_bar(stat="identity",width=0.5,position= "dodge")+
  ylab("Brain area (Pixels)") +
  scale_fill_manual(values=c("#62a3f8","#e08767"))+
  scale_alpha_manual(values=c(1,0.1)) +
  theme_classic() +
  theme(axis.text= element_text(color="black", size=20),
        axis.title = element_text(color="black", size=20),
        panel.grid.minor = element_blank())

ggsave(path = "/Users/yue.li/Library/CloudStorage/OneDrive-KarolinskaInstitutet/ARO-DIIFCO/data/",
"brain clearing.tiff", 
       height =12, 
       width = 6, dpi = 320)

       
barplot(summary_area,)
ggplot(summary_intensity,aes(x=Category,fill=Category))+
  geom_boxplot(aes(lower=mean-sd,upper=mean+sd,middle=mean,ymin=0,ymax=mean+3*sd),stat="identity")+
  scale_fill_manual(values=c("#ea402a","#90fcfd","#ea36f8","#fdf251")) +
  scale_alpha_manual(values=c(1,0.1)) +
  theme_classic() +
  ggtitle("Quantification of Signal Intensity on Four Secondary Antibodies") + theme(plot.title = element_text(hjust = 0.5, size = 24)) + 
  xlab("Category")+ 
  ylab("Signal Intensity") +
  theme(axis.text= element_text(color="black", size=24),
        axis.title = element_text(color="black", size=24),
        panel.grid.minor = element_blank())
p