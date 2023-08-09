##Merging CS parameter data 2016-2021##

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


#uploading CS data
CS_DO_mgl <- read.csv("CS_DO_mgl.csv", header = TRUE)

CS_DO_pct <- read.csv("CS_DO_pct.csv")

CS_pH <- read.csv("CS_pH.csv", header = TRUE)

CS_Sal <- read.csv("CS_Sal.csv", header = TRUE)

CS_Temp <- read.csv("CS_Temp.csv", header = TRUE)

CS_Depth <- read.csv("CS_Depth.csv", header = TRUE)


#Combining the hourly_median  dataframes to create a dataframe 
#with all variables according to datetime
new_df <- merge(CS_Temp, CS_Depth, by = c("Datetime", "Date"), all = TRUE)

new_df1 <- merge(new_df, CS_Sal, by = c("Datetime", "Date"), all = TRUE)

new_df2 <- merge(new_df1, CS_pH, by = c("Datetime", "Date"), all = TRUE)

new_df3 <- merge(new_df2, CS_DO_mgl, by = c("Datetime", "Date"), all = TRUE)

new_df4 <- merge(new_df3, CS_DO_pct, by = c("Datetime", "Date"), all = TRUE)


#creating a site column
new_df4$Site <- "Carquinez Strait"

#subsetting data to the columns I want
new_df4 <- subset(new_df4, select = c(Site, Date, Datetime, Water_Temp, Depth, Sal, pH, DO_mgl, DO_pct))

#updating csv file
write.csv(new_df4, "/Users/mehakjain/Downloads/Thesis/R:data exploration/CS hourly median 2016-2021.csv")




