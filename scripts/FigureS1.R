#Creator: Craig Brinkerhoff
#Date: Summer 2020
#Description: Create figure S1
#NOTE: there are many explicit file paths throughout this script that you will need to set manually in order to run
  #This script also relies a bunch on external datasets you will need to download and map to yourself. Check the manuscript for where to access and download.

library(tidyverse)
library(cowplot)
library(RColorBrewer)
library(colorspace)
library(rgdal)
library(spdplyr)
library(sf)
library(grid)
library(gridExtra)
library(dataRetrieval)
theme_set(theme_cowplot())

rm(list = ls())

#Set working directory with results
setwd('~\\results')

#Read in data
validation <- read.csv('routing_validation_results.csv')
gasTransfer <- read.table('~\\inputs\\NHD_rivNetwork.txt', sep='\t')

#filter for channels with routed Q
routingIDs <- read.table("~\\inputs\\routingIDs.txt")
gasTransfer <- filter(gasTransfer, gasTransfer$NHDPlusID %in% routingIDs$NHDPlusID)
gasTransfer <- gasTransfer[order(gasTransfer$NHDPlusID),]
gasTransfer <- left_join(gasTransfer, routingIDs, by='NHDPlusID')
charQ <- readRDS('~\\inputs\\modeled_flowDuration_curves.rds')
charQ <- data.frame(matrix(unlist(charQ), nrow=length(charQ), byrow=T))
charQ$index <- 1:nrow(charQ)
charQ <- left_join(charQ, routingIDs, by=c('index'='V1'))

temp <- filter(validation, is.na(discharge_model) ==0)
lmValidation <- summary(lm(log10(validation$discharge_obs)~log10(validation$discharge_model)))$r.squared
rmseValidation <- 10^Metrics::rmse(log10(temp$discharge_model), log10(temp$discharge_obs))
break
mycolors <- diverging_hcl(13, palette = "tofino")

#plot validation total
total_validation <- ggplot(validation, aes(x=log10(discharge_obs), y=log10(discharge_model), color=factor(exceedanceProb))) +
  geom_abline(size=1.7)+
  geom_point(size=5) +
  ylab('log10 Modeled Discharge [m3/s]') +
  xlab('log10 Observed Discharge [m3/s]') +
  ggtitle('Observed versus Modeled Streamflow \n87 gauges (1979-1989)') +
  scale_color_manual(name='Discharge', values=mycolors, labels = c('2%', '5%', '10%', '15%', '25%','35%', '50%', '65%', '75%', '85%', '90%', '95%', '98%')) +
  ylim(-4,4)+
  xlim(-4,4) +
  geom_text(aes(y=3.6,x=-1.5), label='RMSE: 1.58 m3/s', color='black', size=5) +
  geom_text(aes(y=3.1,x=-2), label=expression(r^2:~0.97), color='black', size=5)

#Plot specifc gauge flow duration curves, pulled from NWIS records for 1979-1989
#Ashuelot River Gilsum, NH
dailyFlow <- readNWISdata(siteNumbers= '01157000', parameterCd='00060', startDate = '1979-01-01', endDate='1989-12-31')
fdCurve <- ecdf(dailyFlow$X_00060_00003) #built from mean daily flow for entire gauge record
plot_val <- as.data.frame((1-fdCurve(dailyFlow$X_00060_00003))*100)
colnames(plot_val) = 'exd_prob'
plot_val$discharge <- dailyFlow$X_00060_00003 * 0.028316847 #cfs to cms
plot_val$Flow <- 'Observed'

#modeled
i <- which(charQ$NHDPlusID == 10000900057508) #NHD ID, Found manually in QGIS
modeledQ <- as.data.frame(t(charQ[i,1:13]))
colnames(modeledQ) <- 'discharge'
modeledQ$exd_prob <- c(98, 95, 90, 85, 75, 65, 50, 35, 25, 15, 10, 5, 2)
modeledQ$Flow <- 'Modeled'

plot_val <- rbind(plot_val, modeledQ)

Ashuelot <- ggplot(plot_val, aes(x=exd_prob, y=log10(discharge), color=Flow, linetype=Flow)) +
  geom_line(size=1.5) +
  ylab('log10 Discharge [m3/s]') +
  xlab('Exceedance Probability [%]') +
  ggtitle('5th Order \nAshuelot River (USGS 01157000)') +
  scale_color_brewer(palette = 'Dark2') +
  theme(legend.position = 'none')

#Connecticut River main stem Thompsonville, CT
dailyFlow <- readNWISdata(siteNumbers= '01184000', parameterCd='00060', startDate = '1979-01-01', endDate='1989-12-31')
fdCurve <- ecdf(dailyFlow$X_00060_00003) #built from mean daily flow for entire gauge record
plot_val <- as.data.frame((1-fdCurve(dailyFlow$X_00060_00003))*100)
colnames(plot_val) = 'exd_prob'
plot_val$discharge <- dailyFlow$X_00060_00003 * 0.028316847 #cfs to cms
plot_val$Flow <- 'Observed'

#modeled
i <- which(charQ$NHDPlusID == 10000900016222) #NHD ID, Found manually in QGIS
modeledQ <- as.data.frame(t(charQ[i,1:13]))
colnames(modeledQ) <- 'discharge'
modeledQ$exd_prob <- c(98, 95, 90, 85, 75, 65, 50, 35, 25, 15, 10, 5, 2)
modeledQ$Flow <- 'Modeled'

plot_val <- rbind(plot_val, modeledQ)

thompsonville <- ggplot(plot_val, aes(x=exd_prob, y=log10(discharge), color=Flow, linetype=Flow)) +
  geom_line(size=1.5) +
  ylab('log10 Discharge [m3/s]') +
  xlab('Exceedance Probability [%]') +
  ggtitle('8th Order \nConnecticut River (USGS 01184000)') +
  scale_color_brewer(palette = 'Dark2') +
  theme(legend.position = 'none')

#Deerfield River Deerfield, MA
dailyFlow <- readNWISdata(siteNumbers= '01170000', parameterCd='00060', startDate = '1979-01-01', endDate='1989-12-31')
fdCurve <- ecdf(dailyFlow$X_00060_00003) #built from mean daily flow for entire gauge record
plot_val <- as.data.frame((1-fdCurve(dailyFlow$X_00060_00003))*100)
colnames(plot_val) = 'exd_prob'
plot_val$discharge <- dailyFlow$X_00060_00003 * 0.028316847 #cfs to cms
plot_val$Flow <- 'Observed'

#modeled
i <- which(charQ$NHDPlusID == 10000900052858) #NHD ID, Found manually in QGIS
modeledQ <- as.data.frame(t(charQ[i,1:13]))
colnames(modeledQ) <- 'discharge'
modeledQ$exd_prob <- c(98, 95, 90, 85, 75, 65, 50, 35, 25, 15, 10, 5, 2)
modeledQ$Flow <- 'Modeled'

plot_val <- rbind(plot_val, modeledQ)

deerfield <- ggplot(plot_val, aes(x=exd_prob, y=log10(discharge), color=Flow, linetype=Flow)) +
  geom_line(size=1.5) +
  ylab('log10 Discharge [m3/s]') +
  xlab('Exceedance Probability [%]') +
  ggtitle('6th Order \nDeerfield River (USGS 01170000)') +
  scale_color_brewer(palette = 'Dark2') +
  theme(legend.position = 'none')

#Moose River Victory, VT
dailyFlow <- readNWISdata(siteNumbers= '01134500', parameterCd='00060', startDate = '1979-01-01', endDate='1989-12-31')
fdCurve <- ecdf(dailyFlow$X_00060_00003) #built from mean daily flow for entire gauge record
plot_val <- as.data.frame((1-fdCurve(dailyFlow$X_00060_00003))*100)
colnames(plot_val) = 'exd_prob'
plot_val$discharge <- dailyFlow$X_00060_00003 * 0.028316847 #cfs to cms
plot_val$Flow <- 'Observed'

#modeled
i <- which(charQ$NHDPlusID == 10000900097652) #NHD ID, Found manually in QGIS
modeledQ <- as.data.frame(t(charQ[i,1:13]))
colnames(modeledQ) <- 'discharge'
modeledQ$exd_prob <- c(98, 95, 90, 85, 75, 65, 50, 35, 25, 15, 10, 5, 2)
modeledQ$Flow <- 'Modeled'

plot_val <- rbind(plot_val, modeledQ)

moose <- ggplot(plot_val, aes(x=exd_prob, y=log10(discharge), color=Flow, linetype=Flow)) +
  geom_line(size=1.5) +
  ylab('log10 Discharge [m3/s]') +
  xlab('Exceedance Probability [%]') +
  ggtitle('5th order \nMoose River (USGS 01134500)') +
  scale_color_brewer(palette = 'Dark2') +
  theme(legend.position = 'bottom')

#Crate figure
plot_fin <- plot_grid(thompsonville, Ashuelot, deerfield, moose, ncol=2, labels=c('auto'))
plot_fin <- plot_grid(plot_fin, total_validation, nrow=2, labels=c('', 'e'))

ggsave(file="~\\Figures\\FigS1.jpg", plot_fin, width = 9, height = 12) #saves g
