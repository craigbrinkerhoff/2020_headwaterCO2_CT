#Name: Modeling lake/reservoir influences on CO2 Evasion
#Creator: Craig Brinkerhoff
#Date: Spring/Summer 2020
#Description: Network scale advection/evasion model for GHG evasion. Includes evasion in-channel and assumes a rectangular channel

#NOTE: Currently set up to run for CO2, but could be ran on other GHGs if you set the CO2 variable to a
  #different schmidt number and k.

#set up environment-------------------------------
setwd("working drive. set to inputs folder.")
#need to set manual path to MERIT hydrography after obtaining that.

#Inputs to model
  #inputOrder: stream order that GHG is introduced to. Set to 1 to run entire network. 1 was used for study.
  #t: water temperature (to calculate Schmidt numbers)
  #Cnaught: arbitrary gas concentration to introduce into every stream. Value is arbirtrary and doesn't change relative results.
  #meanAnnual: switch whether to run model using USGS's mean annual estimates (1971-2000)
  #volumeModel: which lake volume model to use in calculations
  #exceedanceProb: discharge exceedance probability you want to run the model at.
    #Options: 2, 5, 10, 15, 25, 35, 50, 65, 75, 85, 90, 95, 98, -1 (-1: mean annual flow)
  #lakeFlag: switch whether to save lake results for lake threshold analysis

inputOrder <- 1
t <- 15 #celsius
Cnaught <- 10 #mg/L
meanAnnual <- 0 #set to 1 to use USGS mean annual flow (1971-2000), otherwise 0
volumeModel <- 'normal' #set 'Cael' for scaling model, otherwise the normal model runs (paper used normal model runs)
exceedanceProb <- -1 #set to -1 for mean annual flow for 1979-1989
lakeFlag <- 0 #set to 1 to generate lake CO2 evasion efficiency results, set to 0 if not.

#Set up ----------------------------------------------------------------------
#package check
packages = c("rgdal", "rgeos", "ggfortify", "tidyverse", "spdplyr", "RColorBrewer", "cowplot", "sf")
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

#verify they are loaded
search()

#Necessary Functions------------------------------------------------------------
#get mode of some set of values
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#'Not in' function
`%notin%` <- Negate(`%in%`)

#water HRT function, accounting for rivers and lakes/reservoirs
restimeWater <- function(Vol,lengthKm, v, Q, waterbody){
  if (Q == 0){
    return(NA) #zero flow means no HRT
  }
  else if (waterbody == 'Lake/Reservoir') {
    return(((Vol)/(Q))/3600) #if lake throughflow line, calculate lake HRT for each throughflow line in hrs
  }
  else if (waterbody == 'River'){
    return(((lengthKm*1000)/(v))/3600) #if normal flowline, calcuate reach HRT in hrs
  }
  else{ #just in case, but this will never actually run
    return(NA)
  }
}

#GHG Transport Model
transportModel_inchannel <- function(StartFlag, Divergence, fromNode, HRT, ticker, order, startOrder, Cnaught, k, waterbody){
  if(order < startOrder){#set all orders upstream of starting order to NA
    Q[ticker] <- NA
    output <- NA
    return(output) 
  }
  else{ #if not upstream of selected order
    upstreamIndexes <- which(toNode_vec == fromNode) #get directly upstream reaches
    StartFlag <- ifelse(order == startOrder && startOrder %notin% StreamOrde_vec[upstreamIndexes], 1, StartFlag) #set startflag at any stream at startorder that isn't downstream of an equally ordered stream
    
    if (StartFlag == 1 || Divergence == 1 || all(is.na(hrt_vec[upstreamIndexes]))==1){ #if headwater, divergent reach, or all upstream reaches have HRT of NA (Q of 0) then run transport model using Cnaught
      return(Cnaught*exp(-(k)*HRT*3600)) #3600 to convert hrs to sec
    }
    else if (Divergence == 1) {
      return(NA)
    }
    else {
      weightedUpstreamConc <- weighted.mean(transported[upstreamIndexes], Q[upstreamIndexes], na.rm=TRUE)
      return(weightedUpstreamConc*exp(-(k)*HRT*3600)) #run transport model at current reach (3600 to convert hrs to sec)
    }
  }
}

#GHG Evasion Model
evasionModel_inchannel <- function(StartFlag, Divergence, fromNode, HRT, ticker, order, startOrder, Cnaught, k, waterbody){
  if(order < startOrder){#set all orders upstream of starting order to NA
    Q[ticker] <- NA
    output <- NA
    return(output) 
  }
  else{ #if not upstream of selected order
    upstreamIndexes <- which(toNode_vec == fromNode) #get directly upstream reaches
    StartFlag <- ifelse(order == startOrder && startOrder %notin% StreamOrde_vec[upstreamIndexes], 1, StartFlag) #set startflag at any stream at startorder that isn't downstream of an equally ordered stream
    
    if (StartFlag == 1 || Divergence == 1 || all(is.na(hrt_vec[upstreamIndexes]))==1){ #if divergent reach or starting order, run transport model using Cnaught
      return(Cnaught - (Cnaught*exp(-(k)*HRT*3600))) #3600 to convert hrs to sec
    }
    else { #if normal, downstream reach
      weightedUpstreamConc <- weighted.mean(transported[upstreamIndexes], Q[upstreamIndexes], na.rm=TRUE)
      return(weightedUpstreamConc - (weightedUpstreamConc*exp(-(k)*HRT*3600))) #run transport model at current reach (3600 to convert hrs to sec)
    }
  }
}

#Assign binned gas Transfer velocities using lake surface area as predictor. From Raymond et al. 2013 and Read et al. (2012)
lakek600_func <- function(lakeArea){ #km2
  classes <- c(0.1, 1, 10)
  output <- ifelse(lakeArea <= classes[1], 0.54,
                   ifelse(lakeArea <= classes[2], 1.16,
                          ifelse(lakeArea <- classes[3], 1.32, 1.90))) #m/day
  return(output)
}

#Build basin-wide DHG model to scale MERIT hydro widths using mean annual flow-------------------------------------
merit <- readOGR(dsn='path to MERIT hydrography', layer='MERIT hydrography shapefile')
meanAnnualQs <- read.csv('MERIT_hydro_Q.csv') #made using read_nc script
meanAnnualQs$meanAnnual <- rowMeans(meanAnnualQs[,2:37])
meanAnnualQs$COMID <- as.factor(meanAnnualQs$COMID)
merit <- left_join(merit@data, meanAnnualQs, by='COMID')
merit <- filter(merit, width > 0)
lm <- lm(log(width)~log(meanAnnual), data=filter(merit, meanAnnual>10)) #Only ran on reaches greater than 10m3/s discharge because of noise

#load in river network---------------------------------------------------
gasTransfer <- read.table('NHD_rivNetwork.txt', sep='\t')

#filter for routingIDs using routing table
routingIDs <- read.table("routingIDs.txt", sep = "" , header = T)
gasTransfer <- filter(gasTransfer, gasTransfer$NHDPlusID %in% routingIDs$NHDPlusID)
gasTransfer <- gasTransfer[order(gasTransfer$NHDPlusID),]

#Charactersitic discharges, by reach-----------------------------------------
charQ <- readRDS('modeled_flowDuration_curves.rds')
probs <- c(98, 95,90,85,75,65,50,35,25,15,10,5, 2, -1) #-1 is mean annual flow for the routed Q
k <- which(probs == exceedanceProb)

#extract desired charactersitics discharge for every reach in network
charQ <- data.frame(matrix(unlist(charQ), nrow=length(charQ), byrow=T))
charQ$index <- 1:nrow(charQ)
charQ <- left_join(charQ, routingIDs, by=c('index'='V1'))
charQ <- charQ[,c(k, 16)]#26
colnames(charQ) <- c('Q', 'NHDPlusID') #'n'

#join charactersitic discharge to river network
gasTransfer <- left_join(gasTransfer, charQ, by='NHDPlusID')

#get widths and Manning's n (with unit conversions for QEMA and VEMA)
gasTransfer$Width <- ifelse(exp(summary(lm)$coefficient[1])*(gasTransfer$QEMA*0.0283)^summary(lm)$coefficient[2] < 1, 1, exp(summary(lm)$coefficient[1])*(gasTransfer$QEMA*0.0283)^summary(lm)$coefficient[2])
gasTransfer$n <- ifelse(gasTransfer$VEMA > 0, (gasTransfer$Width*gasTransfer$Slope^(1/2)*(gasTransfer$QEMA*0.0283/(gasTransfer$Width*gasTransfer$VEMA*0.3048))^(3/2))/(gasTransfer$QEMA), 0.1) #if no velocity, i.e. waterbody, just use n = 0.1

if (meanAnnual == 1) { #if using USGS mean annual estimates (1971-2000), convert cfs to cms
  gasTransfer$Q <- gasTransfer$QEMA * 0.028316847 #cfs to cms
}

#HRR outputs Q to 3 decimals, so we round mean annual flow to 3 decimals as well
gasTransfer$Q <- round(gasTransfer$Q, 3)

#velocity calculations assume a rectangular channel via Dingman (2007)
gasTransfer$V <- gasTransfer$Slope^0.3 * gasTransfer$Width^(-0.4) * gasTransfer$n^(-0.6) * gasTransfer$Q^(0.4)

#Model--------------------------------------------------------------
#Raymond, et al. 2012 regression equations for Schmidt #s
cO2_schmidt <- 1742 + (-91.24*t) + (2.208*t^2) + (-0.0219*t^3)
ch4_schmidt <- 1824 + (-98.12*t) + (2.413*t^3) + (-0.0241*t^3)
o2_schmidt <- 1568 + (-86.04*t) + (2.142*t^2) + (-0.0216*t^3)
n2o_schmidt <- 2105 + (-130.08*t) + (3.486*t^2) + (-0.0365*t^3)

#gas transfer velocity calculation
gasTransfer$k600 <- gasTransfer$Slope*gasTransfer$V*2841+2.02 #m/days from Raymond et al. (2012)
gasTransfer$k600 <- ifelse(gasTransfer$waterbody == 'Lake/Reservoir', lakek600_func(gasTransfer$LakeAreaSqKm), gasTransfer$k600) #if lake/reservoir throughflow line, assign binned k600 values per lake surface area (Raymond, et al. 2013 & Read, et al. 2012)
gasTransfer$k600 <- gasTransfer$k600*1.1574e-5 #m/day to m/s

#m/s
gasTransfer$k_co2 <- gasTransfer$k600/((600/cO2_schmidt)^(-0.5))
gasTransfer$k_co2 <- ifelse(gasTransfer$k_co2 < 0, NA, gasTransfer$k_co2)
gasTransfer$k_ch4 <- gasTransfer$k600/((600/ch4_schmidt)^(-0.5))
gasTransfer$k_o2 <- gasTransfer$k600/((600/o2_schmidt)^(-0.5))
gasTransfer$k_n2o <- gasTransfer$k600/((600/n2o_schmidt)^(-0.5))

#get hydraulic residence time (hrs)
gasTransfer$HRT_hrs <- mapply(restimeWater, gasTransfer$frac_lakeVol_m3, gasTransfer$LengthKM, gasTransfer$V, gasTransfer$Q, gasTransfer$waterbody)

#run (vectorized) evasion model----------------------------------------------------------------
gasTransfer <- filter(gasTransfer, HydroSeq != 0)
gasTransfer <- gasTransfer[order(-gasTransfer$HydroSeq),] #sort descending

gasTransfer$ticker <- 1:nrow(gasTransfer)

StartFlag_vec <- as.vector(gasTransfer$StartFlag)
Divergence_vec <- as.vector(gasTransfer$Divergence)
fromNode_vec <- as.vector(gasTransfer$FromNode)
toNode_vec <- as.vector(gasTransfer$ToNode)
Q <- as.vector(gasTransfer$Q)
NHD <- as.vector(gasTransfer$NHDPlusID)
StreamOrde_vec <- as.vector(gasTransfer$StreamOrde)
hrt_vec <- as.vector(gasTransfer$HRT_hrs)
k_vec <- as.vector(gasTransfer$k_co2)
length_vec <- as.vector(gasTransfer$LengthKM)*1000 #km to m
waterbody_vec <- as.vector(gasTransfer$waterbody)

ticker <- 1:nrow(gasTransfer)
transported <- 1:nrow(gasTransfer)
evaded <- 1:nrow(gasTransfer)
diluted <- 1:nrow(gasTransfer)
for (i in 1:nrow(gasTransfer)) {
  transported[i] <- transportModel_inchannel(StartFlag_vec[i], Divergence_vec[i], fromNode_vec[i], hrt_vec[i], ticker[i], StreamOrde_vec[i], inputOrder, Cnaught, k_vec[i], waterbody_vec)
  evaded[i] <- evasionModel_inchannel(StartFlag_vec[i], Divergence_vec[i], fromNode_vec[i], hrt_vec[i], ticker[i], StreamOrde_vec[i], inputOrder, Cnaught, k_vec[i], waterbody_vec)
  print(i)
}
#save results to river network
gasTransfer$conc_mg_L <- transported
gasTransfer$evaded <- evaded

#seperate by rivers and lakes
rivs <- filter(gasTransfer, waterbody == 'River')
lakes <- filter(gasTransfer, waterbody == 'Lake/Reservoir')

#amount evaded away across entire network
totalEvaded <- sum(rivs$evaded, na.rm=T) + sum(lakes$evaded, na.rm=T)
throughflowEvaded <- sum(lakes$evaded, na.rm=T)

#print resilts
print(totalEvaded)
print(throughflowEvaded)
print(throughflowEvaded/totalEvaded)

#Parse out evasion by stream order----------------------------------------------
throughflow <- lakes %>%
  group_by(StreamOrde) %>%
  summarise(throughflow_evaded = sum(evaded, na.rm=TRUE))
rivers <- group_by(rivs, StreamOrde) %>% 
  summarise(riv_evaded = sum(evaded, na.rm=TRUE))

parsed_by_order <- full_join(throughflow, rivers, by='StreamOrde')

parsed_by_order$perc_lakes <- parsed_by_order$throughflow_evaded / (parsed_by_order$throughflow_evaded + parsed_by_order$riv_evaded)
parsed_by_order$sumEvaded <- parsed_by_order$throughflow_evaded + parsed_by_order$riv_evaded
parsed_by_order

#Ratio of rivers to lakes/reservoirs
print(nrow(rivs)/nrow(lakes))

#get surface area by order
gasTransfer$sa <- ifelse(gasTransfer$waterbody == 'River', gasTransfer$Width * 0.001 * gasTransfer$LengthKM, gasTransfer$lakePercent*gasTransfer$LakeAreaSqKm)
sa_by_order <- filter(gasTransfer, Q > 0) %>% group_by(StreamOrde) %>% summarise(sa_sum_km2 = sum(sa))

#Generate data for lake threshold analysis----------------------------------------------
if(lakeFlag== 1){
  lakes <- group_by(gasTransfer, WBArea_Permanent_Identifier) %>%
    summarise(sumEvaded = sum(evaded, na.rm=TRUE), 
              sumTransported = sum(conc_mg_L, na.rm=TRUE), 
              HRT = (mean(LakeVol_m3) / max(Q)) * 0.000277778, #sec to hrs. Also, taking mean Q across throughflow lines within a lake 
              area = mean(LakeAreaSqKm, na.rm = TRUE), 
              throughflow = n(),
              order = getmode(StreamOrde),
              medianSlope = median(Slope)) %>%
    filter(is.na(WBArea_Permanent_Identifier)==0)
  lakes$relEvasion <- lakes$sumEvaded/(lakes$sumTransported + lakes$sumEvaded) #CO2 evaded / total Co2 (evaded + transported)
  lakes$totalEvaded <- ifelse(lakes$relEvasion > 0.90, 1, 0)
  lakes <- drop_na(lakes) #no NAs a consequence of no flow/evasion in some super tiny lakes, or those with incorrect slopes, etc.
  lakes <- filter(lakes, area > 0) #lakes need a surface area
  
  #number rivers vs lakes
  nrow(rivs)
  nrow(lakes)
  
  #get quantiles for HRT and area
  lakeBins <- quantile(lakes$area, c(0.33, 0.66)) # quartiles
  lakes$lakeAreaQuants <- ifelse((lakes$area) <= lakeBins[1], 0.001, #Thresholds set using lower limit as the last class's upper bound
                                 ifelse((lakes$area) <= lakeBins[2], 0.004,97))

  lakeBins <- quantile(lakes$HRT, c(0.33, 0.66)) # quartiles
  lakes$lakeHRTQuants <- ifelse((lakes$HRT) <= lakeBins[1], 75.3, #Thresholds set using lower limit as the last class's upper bound
                                ifelse((lakes$HRT) <= lakeBins[2], 376.6,227872.2))
  #save results
  lakes <- select(lakes, c('WBArea_Permanent_Identifier', 'area', 'HRT', 'throughflow', 'order', 'relEvasion', 'lakeAreaQuants', 'lakeHRTQuants'))
  write.csv(lakes, 'lakes_results.csv')
}