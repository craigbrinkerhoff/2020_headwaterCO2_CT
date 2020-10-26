#Creator: Craig Brinkerhoff
#Date: Summer 2020
#Description: Create Figure 1
#NOTE: there are many explicit file paths throughout this script that you will need to set manually in order to run
#This script also relies a bunch on external datasets you will need to download and map to yourself. Check the manuscript for where to access and download.

library(tidyverse)
library(cowplot)
library(RColorBrewer)
library(colorspace)
library(tmap)
library(rgdal)
library(spdplyr)
library(sf)
library(grid)
library(gridExtra)
theme_set(theme_cowplot())

rm(list = ls())

#Set working directory with results
setwd('C:\\Users\\craig\\Box Sync\\Ongoing Projects\\Watershed_Rules_of_Life\\results')

#Fig 1: Map for conceptual Model in Figure 1---------------------------------
NHD_HR_lines <- readOGR(dsn = "C:\\Users\\craig\\Box Sync\\Ongoing Projects\\Watershed_Rules_of_Life\\Study_watersheds\\NHDPlus_HR\\NHDPLUS_H_0108_HU4_GDB(1)\\NHDPLUS_H_0108_HU4_GDB.gdb", layer = "NHDFlowline") #flowlines clipped to basin
NHD_HR_VAA <- sf::st_read(dsn = "C:\\Users\\craig\\Box Sync\\Ongoing Projects\\Watershed_Rules_of_Life\\Study_watersheds\\NHDPlus_HR\\NHDPLUS_H_0108_HU4_GDB(1)\\NHDPLUS_H_0108_HU4_GDB.gdb", layer = "NHDPlusFlowlineVAA")
NHD_HR_merged <- left_join(NHD_HR_lines, NHD_HR_VAA, by='NHDPlusID')
NHD_HR_merged.df <- filter(NHD_HR_merged, StreamOrde >= 3)
states <- st_read('C:\\Users\\craig\\Box Sync\\states_21basic', layer='states')
states <- filter(states, STATE_ABBR %in% c('CT', 'MA', 'NH', 'VT', 'RI', 'ME','NY'))
  
ct_box <- st_bbox(c(xmin= -73.7254, ymin=40.99839, xmax=-70.5, ymax=45.4), crs=st_crs(4269))
  
#Make map
map <- tm_shape(NHD_HR_merged.df, bbox = ct_box) +
  tm_lines(lwd= 'StreamOrde',col='#08306b', scale=5, legend.lwd.show = FALSE)
  
states_map <- tm_shape(states, bbox = ct_box) + tm_fill(col = '#238443') + tm_borders(col='black', lwd=2) +   tm_scale_bar()
states_map
mapfin <- states_map + map
  
tmap_save(mapfin, 'C:\\Users\\craig\\Box Sync\\Ongoing Projects\\Watershed_Rules_of_Life\\manuscript\\Figures\\Fig1_map.jpg') 