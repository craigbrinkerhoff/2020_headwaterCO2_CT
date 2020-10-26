#Creator: Craig Brinkerhoff
#Date: Summer 2020
#Description: Create Figure 4
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
setwd('C:\\Users\\craig\\Box Sync\\Ongoing Projects\\WROL_slash_CT_CO2\\results')

#Load in results
lakes <- read.csv('lakes_results.csv')
sumEvaded_by_order <- read.csv('evasion_by_order_MA.csv')
colnames(sumEvaded_by_order) <- c('StreamOrde', 'MA_evaded')
sa_by_order <- read.csv('wetted_sa_by_order.csv')
sa_by_order <- select(sa_by_order, c('StreamOrde', 'MA'))
sumEvaded_by_order <- left_join(sumEvaded_by_order, sa_by_order, by='StreamOrde')

#Create subplots
#Surface area vs evasion efficiency
bin <- 20
lakesPlot_area <- ggplot(lakes, aes(x=relEvasion*100, fill=factor(lakeAreaQuants))) +
  geom_histogram(size=0.75, binwidth=bin, color='black', position=position_dodge(bin-.4*(bin), preserve = 'total')) +
  scale_fill_brewer(palette='YlGnBu', name=paste0('Surface Area \nQuantiles [km\u00b2]'),
                    labels=c('0-0.001', '0.001-0.002', '0.002-97' ))+
  xlab('CO2 Evasion Efficiency [%]') +
  ylab("") +
  scale_x_continuous(limits=c(-10, 110), breaks = seq(0,110,25)) +
  #scale_y_log10()+
  theme(legend.position = c(0.08, 0.85),
        axis.text=element_text(size=20),
        axis.title=element_text(size=24,face="bold"),
        legend.text = element_text(size=17),
        legend.title = element_text(size=17, face='bold'))+
  ylim(0,4600)

#HRT vs evasion efficiency
lakesPlot_hrt <- ggplot(lakes, aes(x=relEvasion*100, fill=factor(lakeHRTQuants))) +
  geom_histogram(size=0.75, binwidth=bin, color='black', position=position_dodge(bin-.4*(bin), preserve = 'total')) +
  scale_fill_brewer(palette='YlGnBu', name=paste0('Residence Time \nQuantiles [dys]'),
                    labels=c('0-2', '2-7', '7-9,495'))+
  xlab('CO2 Evasion Efficiency [%]') +
  ylab("Lake Count") +
  # scale_y_log10()+
  theme(legend.position = c(0.08, 0.85),
        axis.text=element_text(size=20),
        axis.title=element_text(size=24,face="bold"),
        legend.text = element_text(size=17),
        legend.title = element_text(size=17, face='bold')) +
  scale_x_continuous(limits=c(-10, 110), breaks = seq(0,110,25)) +
  ylim(0,4600)

#total evasion by order
plot2 <- ggplot(sumEvaded_by_order, aes(x=factor(StreamOrde), y=MA_evaded/MA)) +
  geom_point(color='black', fill = '#66c2a5', shape=23, size=10, stroke=2) +
  xlab('Stream Order') +
  scale_y_log10(name = "Normalized Total Evasion \n [mg/L*km\u00b2]") +
  theme(legend.position = c(0.08, 0.85),
        axis.text=element_text(size=20),
        axis.title=element_text(size=24,face="bold"),
        legend.text = element_text(size=17),
        legend.title = element_text(size=17, face='bold'))

#Make figure
grid <- plot_grid(lakesPlot_hrt, lakesPlot_area, labels="auto", ncol = 2, label_size = 26)
grid <- plot_grid(grid, plot2, ncol=1, labels = c(NA, 'c'), label_size = 26)
ggsave('Fig4.jpg', path='C:\\Users\\craig\\Box Sync\\Ongoing Projects\\WROL_slash_CT_CO2\\manuscript\\Figures\\fin\\', width=13, height=12)