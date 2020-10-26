#Creator: Craig Brinkerhoff
#Date: Summer 2020
#Description: Create Figure 3
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

#forcing same decimals places for all axis labels
scaleFUN <- function(x) sprintf("%.2f", x)

#Set working directory with results
setwd('C:\\Users\\craig\\Box Sync\\Ongoing Projects\\WROL_slash_CT_co2\\results')

#read in results and wrangle data
perc_lakes <- read.csv('evasion_network.csv') #-1 is routed mean annual, -2 is NHD mean annual (1971-2000)
perc_lakes <- perc_lakes[-c(j,16),]

#subpanel a--------------------------------
qema <- as.data.frame(perc_lakes[14,10])
perc_lakes <- data.frame('perc_lakes' = perc_lakes$Network)#perc_lakes[1:11,]
perc_lakes$Q <- c(2,5,10,15,25,35,50,65,75,85,90,95,98,-1)#as.numeric(as.character(perc_lakes$Q))
perc_lakes <- perc_lakes[-14,]

plot1 <- ggplot(perc_lakes, aes(x=Q, y = perc_lakes*100)) +
  geom_point(size=7, color='#fc8d62') +
  geom_path(size=3, color='#fc8d62') + 
  xlab('') +
  ylab('Absolute Evasion [%]')+
  theme(legend.position = 'bottom',
        axis.text=element_text(size=16),
        axis.title = element_text(size=20)) +
  geom_hline(yintercept = qema$`perc_lakes[14, 10]`*100, size=2, color='#66c2a5') +
  geom_text(aes(y=30, x=75), label='Mean Annual Discharge', size=5, color='#66c2a5')+
  scale_x_reverse() +
  scale_color_manual(values = c('#8dd3c7', '#bebada')) +
  scale_y_continuous(limits=c(20, 40), breaks = seq(20,40,5)) 
#theme(axis.title=element_text(size=18,face="bold"))


#subpanel c--------------------------
order <- read.csv('evasion_network.csv')
order <- order[-c(j, 16),]
order <- order[,c(1:7)]
meanAnnual <- order[14,]
meanAnnual <- meanAnnual[,2:7]
colnames(meanAnnual) <- c('1', '2', '3', '4', '5', '6')
order <- order[-14,]

sumEvaded_by_order <- read.csv('evasion_by_order_MA.csv')
colnames(sumEvaded_by_order) <- c('StreamOrde', 'MA')

colnames(order) <- c('Q', '1', '2', '3', '4', '5', '6')
order <- gather(order, key=StreamOrde, value = value, c('1', '2', '3', '4', '5', '6'))
order$StreamOrde <- as.numeric(order$StreamOrde)
order <- left_join(order, sumEvaded_by_order, by='StreamOrde')

sa_by_order <- read.csv('wetted_sa_by_order.csv')

#normalize by surface area
order1 <- filter(order, StreamOrde==1)
order1$value <- (order1$value * 100) / t(sa_by_order[1,2:14])
order2 <- filter(order, StreamOrde==2)
order2$value <- (order2$value * 100) / t(sa_by_order[2,2:14])
order3 <- filter(order, StreamOrde==3)
order3$value <- (order3$value * 100) / t(sa_by_order[3,2:14])
order4 <- filter(order, StreamOrde==4)
order4$value <- (order4$value * 100) / t(sa_by_order[4,2:14])
order5 <- filter(order, StreamOrde==5)
order5$value <- (order5$value * 100) / t(sa_by_order[5,2:14])
order6 <- filter(order, StreamOrde==6)
order6$value <- (order6$value * 100) / t(sa_by_order[6,2:14])

#Make subplots
plot_order1 <- ggplot(order1, aes(x=Q, y=value)) +
  geom_hline(yintercept = (meanAnnual$`1`*100)/sa_by_order[1,15], size=2, color='#66c2a5') +
  geom_path(size=1.25, color='#7fc97f')+
  geom_point(size=5, color='#7fc97f') +
  xlab('') +
  ylab('Surface Area \nNormalized [%/km\u00b2]') +
  scale_x_reverse() +
  scale_y_continuous(labels = scaleFUN)+
   scale_y_continuous(limits=c(0, 1.35), breaks = seq(0,1.35,0.50)) +
  theme(legend.position = 'none') +
  ggtitle('Order 1')

#Order 2
plot_order2 <- ggplot(order2, aes(x=Q, y=(value))) +
  geom_hline(yintercept = (meanAnnual$`2`*100)/sa_by_order[2,15], size=2, color='#66c2a5') +
  geom_path(size=1.25, color='#beaed4')+
  geom_point(size=5, color='#beaed4') +
  xlab('') +
  ylab('') +
  scale_x_reverse() +
  scale_y_continuous(labels = scaleFUN)+
  scale_y_continuous(limits=c(0, 1.35), breaks = seq(0,1.35,0.50)) +
  theme(legend.position = 'none') +
  ggtitle('Order 2')

#Order 3
plot_order3 <- ggplot(order3, aes(x=Q, y=(value))) +
  geom_hline(yintercept = (meanAnnual$`3`*100)/sa_by_order[3,15], size=2, color='#66c2a5') +
  geom_path(size=1.25, color='#fdc086')+
  geom_point(size=5, color='#fdc086') +
  xlab('') +
  ylab('') +
  scale_x_reverse() +
  scale_y_continuous(labels = scaleFUN)+
  scale_y_continuous(limits=c(0, 1.35), breaks = seq(0,1.35,0.50)) +
  theme(legend.position = 'none') +
  ggtitle('Order 3')

#Order 4
plot_order4 <- ggplot(order4, aes(x=Q, y=(value))) +
  geom_hline(yintercept = (meanAnnual$`4`*100)/sa_by_order[4,15], size=2, color='#66c2a5') +
  geom_path(size=1.25, color='#386cb0')+
  geom_point(size=5, color='#386cb0') +
  xlab('') +
  ylab('Surface Area \nNormalized [%/km\u00b2]') +
  scale_x_reverse() +
  scale_y_continuous(labels = scaleFUN)+
  scale_y_continuous(limits=c(0, 1.35), breaks = seq(0,1.35,0.50)) +
  theme(legend.position = 'none') +
  ggtitle('Order 4')

#Order 5
plot_order5 <- ggplot(order5, aes(x=Q, y=(value))) +
  geom_hline(yintercept = (meanAnnual$`5`*100)/sa_by_order[5,15], size=2, color='#66c2a5') +
  geom_path(size=1.25, color='#f0027f')+
  geom_point(size=5, color='#f0027f') +
  xlab('') +
  ylab('') +
  scale_x_reverse() +
  scale_y_continuous(labels = scaleFUN)+
  scale_y_continuous(limits=c(0, 1.35), breaks = seq(0,1.35,0.50)) +
  theme(legend.position = 'none') +
  ggtitle('Order 5')

#Order 6
plot_order6 <- ggplot(order6, aes(x=Q, y=(value))) +
  geom_hline(yintercept = (meanAnnual$`6`*100)/sa_by_order[6,15], size=2, color='#66c2a5') +
  geom_path(size=1.25, color='#bf5b17')+
  geom_point(size=5, color='#bf5b17') +
  xlab('') +
  ylab('') +
  scale_x_reverse() +
  scale_y_continuous(labels = scaleFUN)+
  scale_y_continuous(limits=c(0, 1.35), breaks = seq(0,1.35,0.50)) +
  theme(legend.position = 'none') +
  ggtitle('Order 6')

#subpanel b--------------------------------------
for_panelc <-  rbind(order1, order2, order3, order4, order5, order6)
plot_all_orders <- ggplot(order, aes(x=Q, y=value*100, color=factor(StreamOrde))) +
  geom_path(size=1.5) +
  geom_point(size=5) +
  scale_color_manual(values=c('#7fc97f','#beaed4','#fdc086','#386cb0','#f0027f','#bf5b17'), name='Stream Order') +
  ylab('') +
  xlab('')+
  theme(legend.direction = "horizontal",
        legend.position = c(0.05, 0.1),
        axis.text=element_text(size=16),
        axis.title = element_text(size=20)) +
  scale_y_continuous(limits=c(20, 40), breaks = seq(20,40,5)) +
  scale_x_reverse()

#build figure----------------------------------
p <- plot_grid(plot_order1, plot_order2, plot_order3, plot_order4, plot_order5, plot_order6, ncol = 3)
g <- plot_grid(plot1, plot_all_orders, ncol=2, labels='auto', label_size = 26)
p <- plot_grid(g, p, ncol=1, labels=c(NA, 'c'), label_size = 26)
yTitle <- textGrob('CO2 Evasion Occuring in Lakes', gp=gpar(fontface="bold", col="black", fontsize=24), rot=90)
xTitle <- textGrob('Exceedance Probability for Q [%]', gp=gpar(fontface="bold", col="black", fontsize=24))
g <- grid.arrange(arrangeGrob(p, left = yTitle, bottom = xTitle))

#save figure----------------------------------
ggsave(file="C:\\Users\\craig\\Box Sync\\Ongoing Projects\\WROL_slash_CT_co2\\manuscript\\Figures\\fin\\Fig3.jpg", g, width = 11, height = 12)
