##Merging EOS parameter data 2016-2021##

#clearing anything in the environment
rm(list=ls())

#uploading any necessary packages to perform function below
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


#uploading EOS data for each parameter
EOS_DO_mgl <- read.csv("EOS_DO_mgl.csv", header = TRUE)

EOS_DO_pct <- read.csv("EOS_DO_pct.csv", header = TRUE)

EOS_pH <- read.csv("EOS_pH.csv", header = TRUE)

EOS_Sal <- read.csv("EOS_Sal.csv", header = TRUE)

EOS_Temp <- read.csv("EOS_Temp.csv", header = TRUE)

EOS_Depth <- read.csv("EOS_Depth.csv", header = TRUE)

#Combining all the hourly_median parameters into one dataframe
#merging data 
new_df <- merge(EOS_Temp, EOS_Depth, by = c("Datetime", "Date"), all = TRUE)

new_df1 <- merge(new_df, EOS_Sal, by = c("Datetime", "Date"), all = TRUE)

new_df2 <- merge(new_df1, EOS_pH, by = c("Datetime", "Date"), all = TRUE)

new_df3 <- merge(new_df2, EOS_DO_mgl, by = c("Datetime", "Date"), all = TRUE)

new_df4 <- merge(new_df3, EOS_DO_pct, by = c("Datetime", "Date"), all = TRUE)


#creating a site column
new_df4$Site <- "EOS Center"

##subsetting columns to columns i want
new_df4 <- subset(new_df4, select = c(Site, Date, Datetime, Water_Temp, Depth, Sal, pH, DO_mgl, DO_pct))

#updating csv file
write.csv(new_df4, "/Users/mehakjain/Downloads/Thesis/R:data exploration/EOS hourly median 2016-2021.csv")


