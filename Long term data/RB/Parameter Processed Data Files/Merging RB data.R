##Merging RB 2017-2019 data to RB 2020 data##

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
library(data.table)

#reading in data
Water_Temp <- read.csv("RB_Temp.csv", header = TRUE)

Depth <- read.csv("rb_depth.csv", header = TRUE)

Sal <- read.csv("rb_sal.csv", header = TRUE)

pH <- read.csv("rb_ph.csv", header = TRUE)

DO_mgl <- read.csv("RB_DO_mgl.csv", header = TRUE)

DO_pct <- read.csv("RB_DO_pct.csv", header = TRUE)


#merging data 
new_df <- merge(Water_Temp, Depth, by = c("Datetime", "Date"), all = TRUE)

new_df1 <- merge(new_df, Sal, by = c("Datetime", "Date"), all = TRUE)

new_df2 <- merge(new_df1, pH, by = c("Datetime", "Date"), all = TRUE)

new_df3 <- merge(new_df2, DO_mgl, by = c("Datetime", "Date"), all = TRUE)

new_df4 <- merge(new_df3, DO_pct, by = c("Datetime", "Date"), all = TRUE)

#creating a site column
new_df4$Site <- "Richardson Bay"

##subsetting columns to columns i want
new_df4 <- subset(new_df4, select = c(Site, Date, Datetime, Water_Temp, Depth, Sal, pH, DO_mgl, DO_pct))


write.csv(new_df4, "/Users/mehakjain/Downloads/Thesis/R:data exploration/RB hourly median 2016-2021.csv")

