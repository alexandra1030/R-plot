library(tidyverse)
library(ggplot2)
library('readxl')
library(datasets)
library(sqldf)
library(viridis)
library(hrbrthemes)
library(ggstatsplot)

setwd("/Users/yue.li/同步空间/project/article making/RE_DIIFCO/figure/figure2 in article")
getwd()

intensity_four_ab<- read_xlsx("intensity Results of four antibody.xltx")

summary_intensity <- data.frame(intensity_four_ab)



x = c("IgG","F(ab')2","Fabulight", "VHH")

summary_intensity$Category = factor(summary_intensity$Category, levels = x)
factor(summary_intensity$Category)


p<- ggplot(summary_intensity,aes(x=Category,fill=Category))+
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

## test for ggstatsplot

ggbetweenstats(summary_intensity,x = Category, y = mean, title = "Average Intensity: IgG vs. F(ab')2, Fabulight, VHH")
set.seed(123)

ggbetweenstats(
  data  = iris,
  x     = Species,
  y     = Sepal.Length,
  title = "Distribution of sepal length across Iris species"
)
