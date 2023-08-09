#####Preproccessing#####


####EOS 2016-2021####

rm(list=ls())

library(stats)
library(plyr)
library(tidyverse)
library(ggpubr)
library(scales)
library(chron)
library(aweek)
library(ggthemes)
library(lubridate)
library(tidyquant)
library(suncalc)


####pH###

#creating a function to calculate the median, stdev, and 95% confidence 
#interval of values in dataframe return the values in a different set of 
#columns
stat_func <- function(x, na.rm=TRUE){
  m <- median(x, na.rm=TRUE)
  sd <- sd(x, na.rm=TRUE)
  hi_sd <- m+2*sd
  lo_sd <-m-2*sd
  ret <- c(median = m, stdev = sd, hi.95 = hi_sd, lo.95 = lo_sd)
}

##EOS 2016-2021 pH dataframe creation from raw data##

#loading in EOS-2021 raw data file
eos <- read.csv("eos_2016-2021_rawdata.csv", header = TRUE)

#creating a new date column in correct format
names(eos)[names(eos) == "time"] <- "Datetime"
eos$Date <- as.Date(eos$Date, format = c("%Y-%m-%d"))

#creating a new datetime column in correct format
eos$Datetime <- as.POSIXct(eos$Datetime, format = c("%Y-%m-%dT%H:%M:%SZ"))

#subsetting raw data to the columns I want
eos <- subset(eos, select=c(Date, Datetime, sea_water_ph_reported_on_total_scale, sea_water_ph_reported_on_total_scale_qc_agg))

#changing column names
names(eos)[names(eos) == "sea_water_ph_reported_on_total_scale"] <- "pH"
names(eos)[names(eos) == "sea_water_ph_reported_on_total_scale_qc_agg"] <- "pH_flag"

eos %>%
  ggplot(aes(x=Datetime, y=pH, color=pH))+
  geom_point(alpha=0.5)+
  labs(title="pH data from EOS resesarch pier: All data",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

#Removing flagged data that did not pass QAQC= 3 & 4
eos <- eos[- grep("3", eos$pH_flag),]

eos <- eos[- grep("4", eos$pH_flag),]

eos %>%
  ggplot(aes(x=Date, y=pH, color=pH))+
  geom_point(alpha=0.5)+
  labs(title="pH data from EOS resesarch pier: data flagged fail removed",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

#Applying a rolling median for a 4hr window
eos <- eos %>%
  tq_mutate(
    select = pH,
    mutate_fun = rollapply,
    width = 40,
    align = "right",
    by.column = FALSE,
    FUN = stat_func,
    na.rm = TRUE)

#Filter to remove values that are +/- 2 standard deviations from the rolling 
#median
eos <- filter(eos, pH <hi.95 & pH >lo.95)

ggplot(data = eos, mapping = aes(x = Datetime, y = pH, color=pH)) + geom_point()+
  labs(title="pH data from EOS resesarch pier: +/- 2sd data removed ",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

##Hourly median dataframe
#holder name
names(eos)[1] <- "datex"

#column named "date" with time stamp for hourly median to work
names(eos)[2] <- "date"
eos$date<-as.POSIXct(eos$date, format = c("%Y-%m-%dT%H:%M:%SZ"))

#Creating hourly median dataframe of pH
eos.1hr.med <- timeAverage(eos,
                          avg.time = "1 hour",
                          data.thresh = 0,
                          statistic = "median",
                          interval = "6 min",
                          remove.calm = FALSE,
                          type = "default")

#removing columns from hourly median dataframe for pH
eos.1hr.med <- subset(eos.1hr.med, select=(-c(median, stdev, hi.95, lo.95)))

eos.1hr.med <- subset(eos.1hr.med, select=(-c(pH_flag)))

#renaming existing columns
names(eos.1hr.med)[1] <- "Datetime"
names(eos.1hr.med)[2] <- "Date"

#scatterplot of EOS hourly median data from 2020
eos_pH <- eos.1hr.med %>% ggplot(aes(x=Date, y=pH, color=pH))+
  geom_point(alpha=0.5)+
  labs(title="Hourly median pH from EOS resesarch pier",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

eos_pH

#updating csv file
write.csv(eos.1hr.med, "/Users/mehakjain/Downloads/Thesis/R:data exploration/EOS_pH.csv")



####DO_mgl#####



#creating a function to calculate the median, stdev, and 95% confidence 
#interval of values in dataframe return the values in a different set of 
#columns
stat_func <- function(x, na.rm=TRUE){
  m <- median(x, na.rm=TRUE)
  sd <- sd(x, na.rm=TRUE)
  hi_sd <- m+2*sd
  lo_sd <-m-2*sd
  ret <- c(median = m, stdev = sd, hi.95 = hi_sd, lo.95 = lo_sd)
}

##EOS 2016-2021 pH dataframe creation from raw data##

#loading in EOS-2020 raw data file
eos <- read.csv("eos_2016-2021_rawdata.csv", header = TRUE)

#creating a new date column in correct format
names(eos)[names(eos) == "time"] <- "Datetime"
eos$Date <- as.Date(eos$Date, format = c("%Y-%m-%d"))

#creating a new datetime column in correct format
eos$Datetime <- as.POSIXct(eos$Datetime, format = c("%Y-%m-%dT%H:%M:%SZ"))

#subsetting raw data to the columns I want
eos <- subset(eos, select=c(Date, Datetime, mass_concentration_of_oxygen_in_sea_water, mass_concentration_of_oxygen_in_sea_water_qc_agg))

#changing column names
names(eos)[names(eos) == "mass_concentration_of_oxygen_in_sea_water"] <- "DO_mgl"
names(eos)[names(eos) == "mass_concentration_of_oxygen_in_sea_water_qc_agg"] <- "DO_flag"

eos %>%
  ggplot(aes(x=Datetime, y=DO_mgl, color=DO_mgl))+
  geom_point(alpha=0.5)+
  labs(title="DO data from EOS resesarch pier: All data",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

#Removing flagged data that did not pass QAQC= 3 & 4
eos <- eos[- grep("3", eos$DO_flag),]

eos <- eos[- grep("4", eos$DO_flag),]

eos %>%
  ggplot(aes(x=Date, y=DO_mgl, color=DO_mgl))+
  geom_point(alpha=0.5)+
  labs(title="DO data from EOS resesarch pier: data flagged fail removed",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

#Applying a rolling median for a 4hr window
eos <- eos %>%
  tq_mutate(
    select = DO_mgl,
    mutate_fun = rollapply,
    width = 40,
    align = "right",
    by.column = FALSE,
    FUN = stat_func,
    na.rm = TRUE)

#Filter to remove values that are +/- 2 standard deviations from the rolling 
#median
eos <- filter(eos, DO_mgl <hi.95 & DO_mgl >lo.95)

ggplot(data = eos, mapping = aes(x = Datetime, y = DO_mgl, color=DO_mgl)) + geom_point()+
  labs(title="DO data from EOS resesarch pier: +/- 2sd data removed ",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

##Hourly median dataframe
#holder name
names(eos)[1] <- "datex"

#column named "date" with time stamp for hourly median to work
names(eos)[2] <- "date"
eos$date<-as.POSIXct(eos$date, format = c("%Y-%m-%dT%H:%M:%SZ"))

#Creating hourly median dataframe of DO
eos.1hr.med <- timeAverage(eos,
                           avg.time = "1 hour",
                           data.thresh = 0,
                           statistic = "median",
                           interval = "6 min",
                           remove.calm = FALSE,
                           type = "default")

#removing columns from hourly median dataframe for pH
eos.1hr.med <- subset(eos.1hr.med, select=(-c(median, stdev, hi.95, lo.95)))

eos.1hr.med <- subset(eos.1hr.med, select=(-c(DO_flag)))

#renaming existing columns
names(eos.1hr.med)[1] <- "Datetime"
names(eos.1hr.med)[2] <- "Date"

#scatterplot of EOS hourly median data from 2020
eos_DO_mgl <- eos.1hr.med %>% ggplot(aes(x=Date, y=DO_mgl, color=DO_mgl))+
  geom_point(alpha=0.5)+
  labs(title="Hourly median DO from EOS resesarch pier",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

eos_DO_mgl

#updating csv file
write.csv(eos.1hr.med, "/Users/mehakjain/Downloads/Thesis/R:data exploration/EOS_DO_mgl.csv")



####DO_pct#####



#creating a function to calculate the median, stdev, and 95% confidence 
#interval of values in dataframe return the values in a different set of 
#columns
stat_func <- function(x, na.rm=TRUE){
  m <- median(x, na.rm=TRUE)
  sd <- sd(x, na.rm=TRUE)
  hi_sd <- m+2*sd
  lo_sd <-m-2*sd
  ret <- c(median = m, stdev = sd, hi.95 = hi_sd, lo.95 = lo_sd)
}

##EOS 2016-2021 DO_pct dataframe creation from raw data##

#loading in EOS-2020 raw data file
eos <- read.csv("eos_2016-2021_rawdata.csv", header = TRUE)

#creating a new date column in correct format
names(eos)[names(eos) == "time"] <- "Datetime"
eos$Date <- as.Date(eos$Date, format = c("%Y-%m-%d"))

#creating a new datetime column in correct format
eos$Datetime <- as.POSIXct(eos$Datetime, format = c("%Y-%m-%dT%H:%M:%SZ"))

#subsetting raw data to the columns I want
eos <- subset(eos, select=c(Date, Datetime, fractional_saturation_of_oxygen_in_sea_water, fractional_saturation_of_oxygen_in_sea_water_qc_agg))

#changing column names
names(eos)[names(eos) == "fractional_saturation_of_oxygen_in_sea_water"] <- "DO_pct"
names(eos)[names(eos) == "fractional_saturation_of_oxygen_in_sea_water_qc_agg"] <- "DO_flag"

eos %>%
  ggplot(aes(x=Datetime, y=DO_pct, color=DO_pct))+
  geom_point(alpha=0.5)+
  labs(title="DO data from EOS resesarch pier: All data",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

#Removing flagged data that did not pass QAQC= 3 & 4
eos <- eos[- grep("3", eos$DO_flag),]

eos <- eos[- grep("4", eos$DO_flag),]

eos %>%
  ggplot(aes(x=Date, y=DO_pct, color=DO_pct))+
  geom_point(alpha=0.5)+
  labs(title="DO data from EOS resesarch pier: data flagged fail removed",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

#Applying a rolling median for a 4hr window
eos <- eos %>%
  tq_mutate(
    select = DO_pct,
    mutate_fun = rollapply,
    width = 40,
    align = "right",
    by.column = FALSE,
    FUN = stat_func,
    na.rm = TRUE)

#Filter to remove values that are +/- 2 standard deviations from the rolling 
#median
eos <- filter(eos, DO_pct <hi.95 & DO_pct >lo.95)

ggplot(data = eos, mapping = aes(x = Datetime, y = DO_pct, color=DO_pct)) + geom_point()+
  labs(title="DO data from EOS resesarch pier: +/- 2sd data removed ",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

##Hourly median dataframe
#holder name
names(eos)[1] <- "datex"

#column named "date" with time stamp for hourly median to work
names(eos)[2] <- "date"
eos$date<-as.POSIXct(eos$date, format = c("%Y-%m-%dT%H:%M:%SZ"))

#Creating hourly median dataframe of DO
eos.1hr.med <- timeAverage(eos,
                           avg.time = "1 hour",
                           data.thresh = 0,
                           statistic = "median",
                           interval = "6 min",
                           remove.calm = FALSE,
                           type = "default")

#removing columns from hourly median dataframe for pH
eos.1hr.med <- subset(eos.1hr.med, select=(-c(median, stdev, hi.95, lo.95)))

eos.1hr.med <- subset(eos.1hr.med, select=(-c(DO_flag)))

#renaming existing columns
names(eos.1hr.med)[1] <- "Datetime"
names(eos.1hr.med)[2] <- "Date"

#scatterplot of EOS hourly median data from 2020
eos_DO_pct <- eos.1hr.med %>% ggplot(aes(x=Date, y=DO_pct, color=DO_pct))+
  geom_point(alpha=0.5)+
  labs(title="Hourly median DO from EOS resesarch pier",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

eos_DO_pct

#updating csv file
write.csv(eos.1hr.med, "/Users/mehakjain/Downloads/Thesis/R:data exploration/EOS_DO_pct.csv")




####Salinity#####



#creating a function to calculate the median, stdev, and 95% confidence 
#interval of values in dataframe return the values in a different set of 
#columns
stat_func <- function(x, na.rm=TRUE){
  m <- median(x, na.rm=TRUE)
  sd <- sd(x, na.rm=TRUE)
  hi_sd <- m+2*sd
  lo_sd <-m-2*sd
  ret <- c(median = m, stdev = sd, hi.95 = hi_sd, lo.95 = lo_sd)
}

##EOS 2016-2021 Sal dataframe creation from raw data##

#loading in EOS-2020 raw data file
eos <- read.csv("eos_2016-2021_rawdata.csv", header = TRUE)

#creating a new date column in correct format
names(eos)[names(eos) == "time"] <- "Datetime"
eos$Date <- as.Date(eos$Date, format = c("%Y-%m-%d"))

#creating a new datetime column in correct format
eos$Datetime <- as.POSIXct(eos$Datetime, format = c("%Y-%m-%dT%H:%M:%SZ"))

#subsetting raw data to the columns I want
eos <- subset(eos, select=c(Date, Datetime, sea_water_practical_salinity, sea_water_practical_salinity_qc_agg))

#changing column names
names(eos)[names(eos) == "sea_water_practical_salinity"] <- "Sal"
names(eos)[names(eos) == "sea_water_practical_salinity_qc_agg"] <- "Sal_flag"

eos %>%
  ggplot(aes(x=Datetime, y=Sal, color=Sal))+
  geom_point(alpha=0.5)+
  labs(title="Sal data from EOS resesarch pier: All data",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

#Removing flagged data that did not pass QAQC= 3 & 4
eos <- eos[- grep("3", eos$Sal_flag),]

eos <- eos[- grep("4", eos$Sal_flag),]

eos %>%
  ggplot(aes(x=Date, y=Sal, color=Sal))+
  geom_point(alpha=0.5)+
  labs(title="Sal data from EOS resesarch pier: data flagged fail removed",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

#Applying a rolling median for a 4hr window
eos <- eos %>%
  tq_mutate(
    select = Sal,
    mutate_fun = rollapply,
    width = 40,
    align = "right",
    by.column = FALSE,
    FUN = stat_func,
    na.rm = TRUE)

#Filter to remove values that are +/- 2 standard deviations from the rolling 
#median
eos <- filter(eos, Sal <hi.95 & Sal >lo.95)

ggplot(data = eos, mapping = aes(x = Datetime, y = Sal, color=Sal)) + geom_point()+
  labs(title="Sal data from EOS resesarch pier: +/- 2sd data removed ",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

##Hourly median dataframe
#holder name
names(eos)[1] <- "datex"

#column named "date" with time stamp for hourly median to work
names(eos)[2] <- "date"
eos$date<-as.POSIXct(eos$date, format = c("%Y-%m-%dT%H:%M:%SZ"))

#Creating hourly median dataframe of Sal
eos.1hr.med <- timeAverage(eos,
                           avg.time = "1 hour",
                           data.thresh = 0,
                           statistic = "median",
                           interval = "6 min",
                           remove.calm = FALSE,
                           type = "default")

#removing columns from hourly median dataframe for pH
eos.1hr.med <- subset(eos.1hr.med, select=(-c(median, stdev, hi.95, lo.95)))

eos.1hr.med <- subset(eos.1hr.med, select=(-c(Sal_flag)))

#renaming existing columns
names(eos.1hr.med)[1] <- "Datetime"
names(eos.1hr.med)[2] <- "Date"

#scatterplot of EOS hourly median data from 2020
eos_Sal <- eos.1hr.med %>% ggplot(aes(x=Date, y=Sal, color=Sal))+
  geom_point(alpha=0.5)+
  labs(title="Hourly median Sal from EOS resesarch pier",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

eos_Sal

#updating csv file
write.csv(eos.1hr.med, "/Users/mehakjain/Downloads/Thesis/R:data exploration/EOS_Sal.csv")



####Water_Temp#####



#creating a function to calculate the median, stdev, and 95% confidence 
#interval of values in dataframe return the values in a different set of 
#columns
stat_func <- function(x, na.rm=TRUE){
  m <- median(x, na.rm=TRUE)
  sd <- sd(x, na.rm=TRUE)
  hi_sd <- m+2*sd
  lo_sd <-m-2*sd
  ret <- c(median = m, stdev = sd, hi.95 = hi_sd, lo.95 = lo_sd)
}

##EOS 2016-2021 Water_Temp dataframe creation from raw data##

#loading in EOS-2020 raw data file
eos <- read.csv("eos_2016-2021_rawdata.csv", header = TRUE)

#creating a new date column in correct format
names(eos)[names(eos) == "time"] <- "Datetime"
eos$Date <- as.Date(eos$Date, format = c("%Y-%m-%d"))

#creating a new datetime column in correct format
eos$Datetime <- as.POSIXct(eos$Datetime, format = c("%Y-%m-%dT%H:%M:%SZ"))

#subsetting raw data to the columns I want
eos <- subset(eos, select=c(Date, Datetime, sea_water_temperature, sea_water_temperature_qc_agg))

#changing column names
names(eos)[names(eos) == "sea_water_temperature"] <- "Temp"
names(eos)[names(eos) == "sea_water_temperature_qc_agg"] <- "Temp_flag"

eos %>%
  ggplot(aes(x=Datetime, y=Temp, color=Temp))+
  geom_point(alpha=0.5)+
  labs(title="Temp data from EOS resesarch pier: All data",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

#Removing flagged data that did not pass QAQC= 3 & 4
eos <- eos[- grep("3", eos$Temp_flag),]

eos <- eos[- grep("4", eos$Temp_flag),]

eos %>%
  ggplot(aes(x=Date, y=Temp, color=Temp))+
  geom_point(alpha=0.5)+
  labs(title="Temp data from EOS resesarch pier: data flagged fail removed",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

#Applying a rolling median for a 4hr window
eos <- eos %>%
  tq_mutate(
    select = Temp,
    mutate_fun = rollapply,
    width = 40,
    align = "right",
    by.column = FALSE,
    FUN = stat_func,
    na.rm = TRUE)

#Filter to remove values that are +/- 2 standard deviations from the rolling 
#median
eos <- filter(eos, Temp <hi.95 & Temp >lo.95)

ggplot(data = eos, mapping = aes(x = Datetime, y = Temp, color=Temp)) + geom_point()+
  labs(title="Temp data from EOS resesarch pier: +/- 2sd data removed ",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

##Hourly median dataframe
#holder name
names(eos)[1] <- "datex"

#column named "date" with time stamp for hourly median to work
names(eos)[2] <- "date"
eos$date<-as.POSIXct(eos$date, format = c("%Y-%m-%dT%H:%M:%SZ"))

#Creating hourly median dataframe of Temp
eos.1hr.med <- timeAverage(eos,
                           avg.time = "1 hour",
                           data.thresh = 0,
                           statistic = "median",
                           interval = "6 min",
                           remove.calm = FALSE,
                           type = "default")

#removing columns from hourly median dataframe for Temp
eos.1hr.med <- subset(eos.1hr.med, select=(-c(median, stdev, hi.95, lo.95)))

eos.1hr.med <- subset(eos.1hr.med, select=(-c(Temp_flag)))

#renaming existing columns
names(eos.1hr.med)[1] <- "Datetime"
names(eos.1hr.med)[2] <- "Date"
names(eos.1hr.med)[3] <- "Water_Temp"

#scatterplot of EOS hourly median data from 2020
eos_Temp <- eos.1hr.med %>% ggplot(aes(x=Date, y=Water_Temp, color=Water_Temp))+
  geom_point(alpha=0.5)+
  labs(title="Hourly median Water_Temp from EOS resesarch pier",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

eos_Temp

#updating csv file
write.csv(eos.1hr.med, "/Users/mehakjain/Downloads/Thesis/R:data exploration/EOS_Temp.csv")




####Depth#####



#creating a function to calculate the median, stdev, and 95% confidence 
#interval of values in dataframe return the values in a different set of 
#columns
stat_func <- function(x, na.rm=TRUE){
  m <- median(x, na.rm=TRUE)
  sd <- sd(x, na.rm=TRUE)
  hi_sd <- m+2*sd
  lo_sd <-m-2*sd
  ret <- c(median = m, stdev = sd, hi.95 = hi_sd, lo.95 = lo_sd)
}

##EOS 2016-2021 Depth dataframe creation from raw data##

#loading in EOS-2020 raw data file
eos <- read.csv("eos_depth_rawdata.csv", header = TRUE)

#creating a new date column in correct format
names(eos)[names(eos) == "time"] <- "Datetime"
eos$Date <- as.Date(eos$Date, format = c("%Y-%m-%d"))

#creating a new datetime column in correct format
eos$Datetime <- as.POSIXct(eos$Datetime, format = c("%Y-%m-%dT%H:%M:%SZ"))

#changing column names
names(eos)[names(eos) == "depth"] <- "Depth"

eos %>%
  ggplot(aes(x=Datetime, y=Depth, color=Depth))+
  geom_point(alpha=0.5)+
  labs(title="Depth data from EOS resesarch pier: All data",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

#Applying a rolling median for a 4hr window
eos <- eos %>%
  tq_mutate(
    select = Depth,
    mutate_fun = rollapply,
    width = 40,
    align = "right",
    by.column = FALSE,
    FUN = stat_func,
    na.rm = TRUE)

#Filter to remove values that are +/- 2 standard deviations from the rolling 
#median
eos <- filter(eos, Depth <hi.95 & Depth >lo.95)

ggplot(data = eos, mapping = aes(x = Datetime, y = Depth, color=Depth)) + geom_point()+
  labs(title="Depth data from EOS resesarch pier: +/- 2sd data removed ",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

##Hourly median dataframe
#holder name
names(eos)[3] <- "datex"

#column named "date" with time stamp for hourly median to work
names(eos)[1] <- "date"
eos$date<-as.POSIXct(eos$date, format = c("%Y-%m-%dT%H:%M:%SZ"))

#Creating hourly median dataframe of Depth
eos.1hr.med <- timeAverage(eos,
                           avg.time = "1 hour",
                           data.thresh = 0,
                           statistic = "median",
                           interval = "6 min",
                           remove.calm = FALSE,
                           type = "default")

#removing columns from hourly median dataframe for Depth
eos.1hr.med <- subset(eos.1hr.med, select=(-c(median, stdev, hi.95, lo.95)))

#renaming existing columns
names(eos.1hr.med)[1] <- "Datetime"
names(eos.1hr.med)[3] <- "Date"

#scatterplot of EOS hourly median data from 2020
eos_Depth <- eos.1hr.med %>% ggplot(aes(x=Date, y=Depth, color=Depth))+
  geom_point(alpha=0.5)+
  labs(title="Hourly median Depth from EOS resesarch pier",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

eos_Depth

#updating csv file
write.csv(eos.1hr.med, "/Users/mehakjain/Downloads/Thesis/R:data exploration/EOS_Depth.csv")


