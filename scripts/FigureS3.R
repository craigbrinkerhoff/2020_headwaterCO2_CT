#Creator: Craig Brinkerhoff
#Date: Summer 2020
#Description: Create figure S3
#NOTE: there are many explicit file paths throughout this script that you will need to set manually in order to run
#This script also relies a bunch on external datasets you will need to download and map to yourself. Check the manuscript for where to access and download.

library(tidyverse)
library(cowplot)
library(RColorBrewer)
library(colorspace)
library(grid)
library(gridExtra)
theme_set(theme_cowplot())

rm(list = ls())

#Set working directory with results
setwd('C:\\Users\\craig\\Box Sync\\Ongoing Projects\\Watershed_Rules_of_Life\\results')

#function to orient number of lakes by order in plot
give.n <- function(x){
  return(c(y = median(x)*1.05, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}

#read in data
lakes <- read.csv('lakes_results.csv')

#Lakes relative evasion, binned by order
lakesPlot_by_order <- ggplot(lakes, aes(y=area, x=factor(order))) +
  geom_boxplot(size=1, fill='#1c9099') +
  xlab('Lake Order') +
  ylab('Surface Area [km\u00b2]') +
  theme(legend.position = 'none',
        axis.text=element_text(size=20),
        axis.title=element_text(size=24,face="bold")) +
  stat_summary(fun.data = give.n, geom = "text", fun = median,
               position = position_dodge(width = 0.75),
               size=6) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  annotation_logticks(sides = "l") 

ggsave(file="C:\\Users\\craig\\Box Sync\\Ongoing Projects\\Watershed_Rules_of_Life\\manuscript\\Figures\\FigS3.jpg", lakesPlot_by_order, width = 7, height = 7)