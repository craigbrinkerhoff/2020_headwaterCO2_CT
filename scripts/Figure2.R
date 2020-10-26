#Creator: Craig Brinkerhoff
#Date: Summer 2020
#Description: Create Figure 2
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

j <- 15 #set to 14 for nhd mean annual and 15 for routed mean annual for Fig 3

#Set working directory with results
setwd('~\\results')

#read in results
plot <- read.csv('field_validation_results.csv')

#get validation scores
lm_allflows <- round(summary(lm(plot$ObsConc ~ plot$modConc))$r.squared, 2)
rmse_allflows <- round(Metrics::rmse(plot$ObsConc, plot$modConc), 2)
high_flows <- filter(plot, Exceedance_Probability < 0.3) #high flows are less than 30%
lm_highflows <- round(summary(lm(ObsConc ~ modConc, data = high_flows))$r.squared, 2)
rmse_highflows <- round(Metrics::rmse(high_flows$ObsConc, high_flows$modConc), 2)

print(lm_allflows)
print(rmse_allflows)
print(lm_highflows)
print(rmse_highflows)

#Build subplots
valPlot_highflows <- ggplot(high_flows, aes(x=ObsConc, y=modConc, color=Stream)) + 
  geom_point(size=7) +
  annotate("text", x = 3, y = 11, label = expression(r^2:~0.52), size=8) +
  annotate("text", x = 3, y = 10.3, label = expression(RMSE:~1.88~mg/L), size=8) +
  xlim(0,11.5) +
  ylim(0,11.5)+
  geom_abline() +
  xlab('Observed [CO2] (mg/L)') +
  ylab('Modeled [CO2] (mg/L)') +
  scale_color_brewer(palette = 'Set2') +
  theme(legend.position = 'right')

valPlot_allflows <- ggplot(plot, aes(x=ObsConc, y=modConc, color=Stream)) + 
  geom_point(size=7) +
  annotate("text", x = 3, y = 11, label = expression(r^2:~0.15), size=8) +
  annotate("text", x = 3, y = 10.3, label = expression(RMSE:~2.53~mg/L), size=8) +
  xlim(0,11.5) +
  ylim(0,11.5)+
  geom_abline() +
  xlab('Observed [CO2] (mg/L)') +
  ylab('Modeled [CO2] (mg/L)') +
  scale_color_brewer(palette = 'Set2') +
  theme(legend.position = 'right')

#Build figure
gridPlot <- plot_grid(valPlot_allflows + theme(legend.position = 'none',
                                               axis.text = element_text(size=20),
                                               axis.title = element_text(size=24, face='bold')),
                      valPlot_highflows + theme(legend.position = 'none',
                                                axis.text = element_text(size=20),
                                                axis.title = element_text(size=24, face='bold')),
                      labels = c('auto'),
                      ncol = 2,
                      label_size = 26)

legend <- get_legend(valPlot_allflows + 
                       theme(legend.title = element_blank(), 
                             legend.text = element_text(size = 20),
                             legend.position = 'bottom') +
                       guides(shape = guide_legend(override.aes = list(size = 20)),
                              color = guide_legend(nrow=1)))

plotVal <- plot_grid(gridPlot, legend, ncol=1, rel_heights = c(1, .1))
plotVal
ggsave('Fig2.jpg', path = '~\\Figures\\', width = 13, height=8)