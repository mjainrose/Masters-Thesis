##Minor exploration in looking at median values of RB hourly data

## -----------------------------------------------------------------------------------
rm(list=ls())


## -----------------------------------------------------------------------------------
library(mapproj)
library(Rcpp)
library(tidyverse)
library(stats)
library(plyr)
library(dplyr)
library(ggpubr)
library(scales)
library(chron)
library(taRifx)
library(aweek)
library(renv)
library(here)
library(ggthemes)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(recipes) 
library(cranlogs)
library(knitr)
library(lubridate)
library(easypackages)
library(tidyquant)
library(openair)
library(suncalc)


## -----------------------------------------------------------------------------------
RB <- read.csv("RB_2016-2021.csv", header = TRUE)


## -----------------------------------------------------------------------------------
RB$Datetime <- as.POSIXct(RB$Datetime, format = c("%Y-%m-%d %H:%M:%S"), tz = "UTC")
hrs <- 8*60*60
RB$Datetime <- RB$Datetime - hrs #PST time


## -----------------------------------------------------------------------------------
RB$Day <- as.numeric(format(RB$Datetime, "%d"))
RB$Month <- as.numeric(format(RB$Datetime, "%m"))
RB$Year <- as.numeric(format(RB$Datetime, "%Y"))
RB$Hour <- as.numeric(format(RB$Datetime, "%H"))


months <- as.numeric(format(as.Date(RB$Datetime, '%Y-%m-%d %H:%M:%S'), '%m'))
indx <- setNames( rep(c('Winter', 'Spring', 'Summer',
                        'Fall'),each=3), c(12,1:11))
RB$Season <- unname(indx[as.character(months)])


## -----------------------------------------------------------------------------------
RB$Site <- "RB"

RB <- subset(RB, select = c(Site, Date, Datetime, Water_Temp, Depth, Sal, pH, DO, Day, Month, Year, Season, Hour, Median_pH, Median_DO, Median_Sal, Median_Water_Temp, Median_Depth))


## -----------------------------------------------------------------------------------
Median_pH <- with(RB, aggregate(list(pH), by = list(Season, Hour), 
                   FUN = function(x) { Median_pH = median(x, na.rm = TRUE) } ))

Median_pH <- do.call(data.frame, Median_pH)
colnames(Median_pH) <- c('Season', 'Hour', 'Median_pH')
Median_pH

RB <- merge(RB, Median_pH, by = c("Hour", "Season"))

RB <- RB[order(RB$Datetime), ] 

RB <- subset(RB, select = c(Date, Datetime, Water_Temp, Depth, Sal, pH, DO, Day, Month, Year, Season, Hour, Median_pH))



## -----------------------------------------------------------------------------------
Median_DO <- with(RB, aggregate(list(DO), by = list(Season, Hour), 
                   FUN = function(x) { Median_DO = median(x, na.rm = TRUE) } ))

Median_DO <- do.call(data.frame, Median_DO)
colnames(Median_DO) <- c('Season', 'Hour', 'Median_DO')

RB <- merge(RB, Median_DO, by = c("Hour", "Season"))

RB <- RB[order(RB$Datetime), ] 

RB <- subset(RB, select = c(Date, Datetime, Water_Temp, Depth, Sal, pH, DO, Day, Month, Year, Season, Hour, Median_pH, Median_DO))



## -----------------------------------------------------------------------------------
Median_Sal <- with(RB, aggregate(list(Sal), by = list(Season, Hour), 
                   FUN = function(x) { Median_Sal = median(x, na.rm = TRUE) } ))

Median_Sal <- do.call(data.frame, Median_Sal)
colnames(Median_Sal) <- c('Season', 'Hour', 'Median_Sal')

RB <- merge(RB, Median_Sal, by = c("Hour", "Season"))

RB <- RB[order(RB$Datetime), ] 

RB <- subset(RB, select = c(Date, Datetime, Water_Temp, Depth, Sal, pH, DO, Day, Month, Year, Season, Hour, Median_pH, Median_DO, Median_Sal))



## -----------------------------------------------------------------------------------
Median_Temp <- with(RB, aggregate(list(Water_Temp), by = list(Season, Hour), 
                   FUN = function(x) { Median_Water_Temp = median(x, na.rm = TRUE) } ))

Median_Temp <- do.call(data.frame, Median_Temp)
colnames(Median_Temp) <- c('Season', 'Hour', 'Median_Water_Temp')

RB <- merge(RB, Median_Temp, by = c("Hour", "Season"))

RB <- RB[order(RB$Datetime), ] 

RB <- subset(RB, select = c(Date, Datetime, Water_Temp, Depth, Sal, pH, DO, Day, Month, Year, Season, Hour, Median_pH, Median_DO, Median_Sal, Median_Water_Temp))



## -----------------------------------------------------------------------------------
Median_Depth <- with(RB, aggregate(list(Water_Temp), by = list(Season, Hour), 
                   FUN = function(x) { Median_Water_Temp = median(x, na.rm = TRUE) } ))

Median_Depth <- do.call(data.frame, Median_Depth)
colnames(Median_Depth) <- c('Season', 'Hour', 'Median_Depth')

RB <- merge(RB, Median_Depth, by = c("Hour", "Season"))

RB <- RB[order(RB$Datetime), ] 

RB <- subset(RB, select = c(Date, Datetime, Water_Temp, Depth, Sal, pH, DO, Day, Month, Year, Season, Hour, Median_pH, Median_DO, Median_Sal, Median_Water_Temp, Median_Depth))


