#####Preproccessing#####


####2016-2021 Carquinez####

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

##CS 2016-2021 pH dataframe creation from raw data##

#loading in CSraw data file
cs <- read.csv("cs_2016-2021_rawdata.csv", header = TRUE)

#creating a new date column in correct format
names(cs)[names(cs) == "time"] <- "Datetime"
cs$Date <- as.Date(cs$Date, format = c("%Y-%m-%d"))

#creating a new datetime column in correct format
cs$Datetime <- as.POSIXct(cs$Datetime, format = c("%Y-%m-%dT%H:%M:%SZ"))

#subsetting raw data to the columns I want
cs <- subset(cs, select=c(Date, Datetime, ph))

#changing column names
names(cs)[names(cs) == "ph"] <- "pH"

cs %>%
  ggplot(aes(x=Datetime, y=pH, color=pH))+
  geom_point(alpha=0.5)+
  labs(title="pH data from CS: All data",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

#Applying a rolling median for a 4hr window
cs <- cs %>%
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
cs <- filter(cs, pH <hi.95 & pH >lo.95)

ggplot(data = cs, mapping = aes(x = Datetime, y = pH, color=pH)) + geom_point()+
  labs(title="pH data from CS: +/- 2sd data removed ",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

#filter to remove any outliers
cs <- filter(cs, pH >1)

##Hourly median dataframe
#holder name
names(cs)[1] <- "datex"

#column named "date" with time stamp for hourly median to work
names(cs)[2] <- "date"
cs$date<-as.POSIXct(cs$date, format = c("%Y-%m-%dT%H:%M:%SZ"))

#Creating hourly median dataframe of pH
cs.1hr.med <- timeAverage(cs,
                           avg.time = "1 hour",
                           data.thresh = 0,
                           statistic = "median",
                           interval = "6 min",
                           remove.calm = FALSE,
                           type = "default")

#removing columns from hourly median dataframe for pH
cs.1hr.med <- subset(cs.1hr.med, select=(-c(median, stdev, hi.95, lo.95)))

#renaming existing columns
names(cs.1hr.med)[1] <- "Datetime"
names(cs.1hr.med)[2] <- "Date"

#scatterplot of EOS hourly median data from 2020
cs_pH <- cs.1hr.med %>% ggplot(aes(x=Date, y=pH, color=pH))+
  geom_point(alpha=0.5)+
  labs(title="Hourly median pH from CS",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

cs_pH

#updating csv file
write.csv(cs.1hr.med, "/Users/mehakjain/Downloads/Thesis/R:data exploration/CS_pH.csv")



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

##CS 2016-2021 DO_mgl dataframe creation from raw data##

#loading in CS raw data file
cs <- read.csv("cs_2016-2021_rawdata.csv", header = TRUE)

#creating a new date column in correct format
names(cs)[names(cs) == "time"] <- "Datetime"
cs$Date <- as.Date(cs$Date, format = c("%Y-%m-%d"))

#creating a new datetime column in correct format
cs$Datetime <- as.POSIXct(cs$Datetime, format = c("%Y-%m-%dT%H:%M:%SZ"))

#subsetting raw data to the columns I want
cs <- subset(cs, select=c(Date, Datetime, odo))

#changing column names
names(cs)[names(cs) == "odo"] <- "DO_mgl"

cs %>%
  ggplot(aes(x=Datetime, y=DO_mgl, color=DO_mgl))+
  geom_point(alpha=0.5)+
  labs(title="DO data from CS: All data",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

#Applying a rolling median for a 4hr window
cs <- cs %>%
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
cs <- filter(cs, DO_mgl <hi.95 & DO_mgl >lo.95)

ggplot(data = cs, mapping = aes(x = Datetime, y = DO_mgl, color=DO_mgl)) + geom_point()+
  labs(title="DO data from CS: +/- 2sd data removed ",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

##Hourly median dataframe
#holder name
names(cs)[1] <- "datex"

#column named "date" with time stamp for hourly median to work
names(cs)[2] <- "date"
cs$date<-as.POSIXct(cs$date, format = c("%Y-%m-%dT%H:%M:%SZ"))

#Creating hourly median dataframe of DO
cs.1hr.med <- timeAverage(cs,
                           avg.time = "1 hour",
                           data.thresh = 0,
                           statistic = "median",
                           interval = "6 min",
                           remove.calm = FALSE,
                           type = "default")

#removing columns from hourly median dataframe for pH
cs.1hr.med <- subset(cs.1hr.med, select=(-c(median, stdev, hi.95, lo.95)))

#renaming existing columns
names(cs.1hr.med)[1] <- "Datetime"
names(cs.1hr.med)[2] <- "Date"

#scatterplot of EOS hourly median data from 2020
cs_DO_mgl <- cs.1hr.med %>% ggplot(aes(x=Date, y=DO_mgl, color=DO_mgl))+
  geom_point(alpha=0.5)+
  labs(title="Hourly median DO from CS",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

cs_DO_mgl

#updating csv file
write.csv(cs.1hr.med, "/Users/mehakjain/Downloads/Thesis/R:data exploration/CS_DO_mgl.csv")



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

##CS 2016-2021 DO_pct dataframe creation from raw data##

#loading in CS raw data file
cs <- read.csv("cs_2016-2021_rawdata.csv", header = TRUE)

#creating a new date column in correct format
names(cs)[names(cs) == "time"] <- "Datetime"
cs$Date <- as.Date(cs$Date, format = c("%Y-%m-%d"))

#creating a new datetime column in correct format
cs$Datetime <- as.POSIXct(cs$Datetime, format = c("%Y-%m-%dT%H:%M:%SZ"))

#subsetting raw data to the columns I want
cs <- subset(cs, select=c(Date, Datetime, odosat))

#changing column names
names(cs)[names(cs) == "odosat"] <- "DO_pct"

cs %>%
  ggplot(aes(x=Datetime, y=DO_pct, color=DO_pct))+
  geom_point(alpha=0.5)+
  labs(title="DO data from CS: All data",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

#Applying a rolling median for a 4hr window
cs <- cs %>%
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
cs <- filter(cs, DO_pct <hi.95 & DO_pct >lo.95)

ggplot(data = cs, mapping = aes(x = Datetime, y = DO_pct, color=DO_pct)) + geom_point()+
  labs(title="DO data from CS: +/- 2sd data removed ",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

##Hourly median dataframe
#holder name
names(cs)[1] <- "datex"

#column named "date" with time stamp for hourly median to work
names(cs)[2] <- "date"
cs$date<-as.POSIXct(cs$date, format = c("%Y-%m-%dT%H:%M:%SZ"))

#Creating hourly median dataframe of DO
cs.1hr.med <- timeAverage(cs,
                          avg.time = "1 hour",
                          data.thresh = 0,
                          statistic = "median",
                          interval = "6 min",
                          remove.calm = FALSE,
                          type = "default")

#removing columns from hourly median dataframe for pH
cs.1hr.med <- subset(cs.1hr.med, select=(-c(median, stdev, hi.95, lo.95)))

#renaming existing columns
names(cs.1hr.med)[1] <- "Datetime"
names(cs.1hr.med)[2] <- "Date"

#scatterplot of EOS hourly median data from 2020
cs_DO_pct <- cs.1hr.med %>% ggplot(aes(x=Date, y=DO_pct, color=DO_pct))+
  geom_point(alpha=0.5)+
  labs(title="Hourly median DO from CS",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

cs_DO_pct

#updating csv file
write.csv(cs.1hr.med, "/Users/mehakjain/Downloads/Thesis/R:data exploration/CS_DO_pct.csv")





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

##CS 2016-2021 Sal dataframe creation from raw data##

#loading in CS raw data file
cs <- read.csv("cs_2016-2021_rawdata.csv", header = TRUE)

#creating a new date column in correct format
names(cs)[names(cs) == "time"] <- "Datetime"
cs$Date <- as.Date(cs$Date, format = c("%Y-%m-%d"))

#creating a new datetime column in correct format
cs$Datetime <- as.POSIXct(cs$Datetime, format = c("%Y-%m-%dT%H:%M:%SZ"))

#subsetting raw data to the columns I want
cs <- subset(cs, select=c(Date, Datetime, salinity))

#changing column names
names(cs)[names(cs) == "salinity"] <- "Sal"

cs %>%
  ggplot(aes(x=Datetime, y=Sal, color=Sal))+
  geom_point(alpha=0.5)+
  labs(title="Sal data from CS: All data",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

#Applying a rolling median for a 4hr window
cs <- cs %>%
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
cs <- filter(cs, Sal <hi.95 & Sal >lo.95)

ggplot(data = cs, mapping = aes(x = Datetime, y = Sal, color=Sal)) + geom_point()+
  labs(title="Sal data from EOS resesarch pier: +/- 2sd data removed ",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

##Hourly median dataframe
#holder name
names(cs)[1] <- "datex"

#column named "date" with time stamp for hourly median to work
names(cs)[2] <- "date"
cs$date<-as.POSIXct(cs$date, format = c("%Y-%m-%dT%H:%M:%SZ"))

#Creating hourly median dataframe of Sal
cs.1hr.med <- timeAverage(cs,
                           avg.time = "1 hour",
                           data.thresh = 0,
                           statistic = "median",
                           interval = "6 min",
                           remove.calm = FALSE,
                           type = "default")

#removing columns from hourly median dataframe for pH
cs.1hr.med <- subset(cs.1hr.med, select=(-c(median, stdev, hi.95, lo.95)))

#renaming existing columns
names(cs.1hr.med)[1] <- "Datetime"
names(cs.1hr.med)[2] <- "Date"

#scatterplot of EOS hourly median data from 2020
cs_Sal <- cs.1hr.med %>% ggplot(aes(x=Date, y=Sal, color=Sal))+
  geom_point(alpha=0.5)+
  labs(title="Hourly median Sal from CS",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

cs_Sal

#updating csv file
write.csv(cs.1hr.med, "/Users/mehakjain/Downloads/Thesis/R:data exploration/CS_Sal.csv")



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

##CS 2016-2021 Water_Temp dataframe creation from raw data##

#loading in CS raw data file
cs <- read.csv("cs_2016-2021_rawdata.csv", header = TRUE)

#creating a new date column in correct format
names(cs)[names(cs) == "time"] <- "Datetime"
cs$Date <- as.Date(cs$Date, format = c("%Y-%m-%d"))

#creating a new datetime column in correct format
cs$Datetime <- as.POSIXct(cs$Datetime, format = c("%Y-%m-%dT%H:%M:%SZ"))

#subsetting raw data to the columns I want
cs <- subset(cs, select=c(Date, Datetime, temperature))

#changing column names
names(cs)[names(cs) == "temperature"] <- "Temp"

cs %>%
  ggplot(aes(x=Datetime, y=Temp, color=Temp))+
  geom_point(alpha=0.5)+
  labs(title="Temp data from EOS resesarch pier: All data",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

#Applying a rolling median for a 4hr window
cs <- cs %>%
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
cs <- filter(cs, Temp <hi.95 & Temp >lo.95)

ggplot(data = cs, mapping = aes(x = Datetime, y = Temp, color=Temp)) + geom_point()+
  labs(title="Temp data from CS: +/- 2sd data removed ",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

#filter out any outliers
cs <- filter(cs, Temp <25)

##Hourly median dataframe
#holder name
names(cs)[1] <- "datex"

#column named "date" with time stamp for hourly median to work
names(cs)[2] <- "date"
cs$date<-as.POSIXct(cs$date, format = c("%Y-%m-%dT%H:%M:%SZ"))

#Creating hourly median dataframe of Temp
cs.1hr.med <- timeAverage(cs,
                           avg.time = "1 hour",
                           data.thresh = 0,
                           statistic = "median",
                           interval = "6 min",
                           remove.calm = FALSE,
                           type = "default")

#removing columns from hourly median dataframe for Temp
cs.1hr.med <- subset(cs.1hr.med, select=(-c(median, stdev, hi.95, lo.95)))

#renaming existing columns
names(cs.1hr.med)[1] <- "Datetime"
names(cs.1hr.med)[2] <- "Date"
names(cs.1hr.med)[3] <- "Water_Temp"

#scatterplot of EOS hourly median data from 2020
cs_Temp <- cs.1hr.med %>% ggplot(aes(x=Date, y=Water_Temp, color=Water_Temp))+
  geom_point(alpha=0.5)+
  labs(title="Hourly median Water_Temp from CS",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

cs_Temp

#updating csv file
write.csv(cs.1hr.med, "/Users/mehakjain/Downloads/Thesis/R:data exploration/CS_Temp.csv")




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

##CS 2016-2021 Depth dataframe creation from raw data##

#loading in CS raw data file
cs <- read.csv("cs_2016-2021_rawdata.csv", header = TRUE)

#creating a new date column in correct format
names(cs)[names(cs) == "time"] <- "Datetime"
cs$Date <- as.Date(cs$Date, format = c("%Y-%m-%d"))

#creating a new datetime column in correct format
cs$Datetime <- as.POSIXct(cs$Datetime, format = c("%Y-%m-%dT%H:%M:%SZ"))

#subsetting raw data to the columns I want
cs <- subset(cs, select=c(Date, Datetime, depth))

#changing column names
names(cs)[names(cs) == "depth"] <- "Depth"

cs %>%
  ggplot(aes(x=Datetime, y=Depth, color=Depth))+
  geom_point(alpha=0.5)+
  labs(title="Depth data from CS: All data",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

#Applying a rolling median for a 4hr window
cs <- cs %>%
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
cs <- filter(cs, Depth <hi.95 & Depth >lo.95)

ggplot(data = cs, mapping = aes(x = Datetime, y = Depth, color=Depth)) + geom_point()+
  labs(title="Depth data from CS: +/- 2sd data removed ",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

##Hourly median dataframe
#holder name
names(cs)[1] <- "datex"

#column named "date" with time stamp for hourly median to work
names(cs)[2] <- "date"
cs$date<-as.POSIXct(cs$date, format = c("%Y-%m-%dT%H:%M:%SZ"))

#Creating hourly median dataframe of Depth
cs.1hr.med <- timeAverage(cs,
                           avg.time = "1 hour",
                           data.thresh = 0,
                           statistic = "median",
                           interval = "6 min",
                           remove.calm = FALSE,
                           type = "default")

#removing columns from hourly median dataframe for Depth
cs.1hr.med <- subset(cs.1hr.med, select=(-c(median, stdev, hi.95, lo.95)))

#renaming existing columns
names(cs.1hr.med)[1] <- "Datetime"
names(cs.1hr.med)[2] <- "Date"

#scatterplot of EOS hourly median data from 2020
cs_Depth <- cs.1hr.med %>% ggplot(aes(x=Date, y=Depth, color=Depth))+
  geom_point(alpha=0.5)+
  labs(title="Hourly median Depth from CS",
       subtitle= "04/15/2016 - 12/31/2021",
       caption= "data courtesy of CeNCOOS")

cs_Depth

#updating csv file
write.csv(cs.1hr.med, "/Users/mehakjain/Downloads/Thesis/R:data exploration/CS_Depth.csv")

