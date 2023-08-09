##merge of all sites to one large dataframe

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


#uploading hourly_median data from 2016-2021
RB <- read.csv("RB hourly median 2016-2022.csv", header = TRUE)

EOS <- read.csv("EOS hourly median 2016-2021.csv", header = TRUE)

CS <- read.csv("CS hourly median 2016-2021.csv", header = TRUE)

GC <- read.csv("GC_all_parameters_hr_med_2016-21.csv", header = TRUE)

CC <- read.csv("CC_all_parameters_hr_med_2016-21.csv", header = TRUE) 

#making site columns
GC$Site <- "Gallinas Creek"
CC$Site <- "China Camp"

#renaming existing columns
names(CC)[7] <- "Water_Temp"
names(GC)[7] <- "Water_Temp"

#select columns I want
GC <- subset(GC, select = c(Site, Date, Datetime, Water_Temp, Depth, Sal, pH, DO_mgl, DO_pct))
CC <- subset(CC, select = c(Site, Date, Datetime, Water_Temp, Depth, Sal, pH, DO_mgl, DO_pct))
CS <- subset(CS, select = c(Site, Date, Datetime, Water_Temp, Depth, Sal, pH, DO_mgl, DO_pct))
RB <- subset(RB, select = c(Site, Date, Datetime, Water_Temp, Depth, Sal, pH, DO_mgl, DO_pct))
EOS <- subset(EOS, select = c(Site, Date, Datetime, Water_Temp, Depth, Sal, pH, DO_mgl, DO_pct))

#merging all dataframes together
new_df <- rbind(RB, EOS, CS, GC, CC)

#ordering large dataframe by datetime
new_df <- new_df[order(new_df$Datetime), ] 

#filtering dataset to only take into account from April
new_df <- new_df %>%
  filter(Date >= "2016-04-15")

#rounding pH column to tenth place 
new_df$pH_rounded <- round(new_df$pH ,digit=1)

#reordering columns
new_df <- subset(new_df, select = c(Site, Date, Datetime, Water_Temp, Depth, Sal, pH, pH_rounded, DO_mgl, DO_pct))

#creating new csv file with all my sites
write.csv(new_df, "/Users/mehakjain/Downloads/Thesis/R:data exploration/All sites hourly median 2016-2022.csv")





