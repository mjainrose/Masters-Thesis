####Code for pre-processing raw environmental data####
##Richardson Bay 2016-2021##


#remove anything from environment
rm(list=ls())

#Uploading any necessary packages
library(MASS)
library(readxl)
library(pracma)
library(NlcOptim)
library(stats)
library(plyr)
library(tidyverse)
library(ggpubr)
library(chron)
library(lubridate)
library(tidyquant)
library(seacarb)
library(openair)




#calling all years of richardson bay data
a <- read.csv("RB-2016-rawdata.csv", header = TRUE)
b <- read.csv("RB-2017-2019-rawdata.csv", header = TRUE)
c <- read.csv("RB-2020-rawdata.csv", header = TRUE)
d <- read.csv("RB-2021-rawdata.csv", header = TRUE)
e <- read.csv("RB-2022-rawdata.csv", header = TRUE)

#combining all years of RB data
all_years <- rbind(a, b, c, d, e)

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

#creating a new date column in correct format
all_years$date <- as.POSIXct(all_years$DateTimeStamp, format = c("%m/%d/%y"))

#creating a new datetime column in correct format
all_years$datetime <- as.POSIXct(all_years$DateTimeStamp, format = c("%m/%d/%y %H:%M"))


###pH###



#subsetting raw data to the columns I want
all_years <- subset(all_years, select = c(date, datetime, pH, F_pH))

all_years %>%
  ggplot(aes(x=datetime, y=pH, color=pH))+
  geom_point(alpha=0.5)+
  labs(title="pH data from Richardson Bay: All data points",
       subtitle= "04/19/2016 - 07/07/2022",
       caption= "data courtesy of NERR")

#Removing flagged data that did not pass QAQC= -3 & suspect data = 1, missing data = -2
all_years <- all_years[- grep("-3", all_years$F_pH),]

all_years <- all_years[- grep("1", all_years$F_pH),]

all_years <- all_years[- grep("-2", all_years$F_pH),]

all_years <- all_years %>% 
  drop_na(pH)

all_years %>%
  ggplot(aes(x=datetime, y=pH, color=pH))+
  geom_point(alpha=0.5)+
  labs(title="pH data from Richardson Bay: Failed QC data removed",
       subtitle= "04/19/2016 - 07/07/2022",
       caption= "data courtesy of NERR")

#Applying a rolling median for a 4hr window
all_years <- all_years %>%
  tq_mutate(
    select = pH,
    mutate_fun = rollapply,
    width = 16,
    align = "right",
    by.column = FALSE,
    FUN = stat_func,
    na.rm = TRUE)

#Filter to remove values that are +/- 2 standard deviations from the rolling 
#median
all_years <- filter(all_years, pH < hi.95 & pH > lo.95)

all_years %>%
  ggplot(aes(x=datetime, y=pH, color=pH)) +
  geom_point(alpha=0.5) +
  labs(title ="pH data from Richardson Bay: Failed QC data & +/-2sd removed",
       subtitle = "04/19/2016 - 07/07/2022",
       caption = "data courtesy of NERR")

##Hourly median dataframe
#holder name
names(all_years)[1] <- "datex"

#column named "date" with time stamp for hourly median to work
names(all_years)[2] <- "date"

#Creating hourly median dataframe of pH
RB_1hr_med<- timeAverage(all_years,
                         avg.time = "1 hour",
                         data.thresh = 0,
                         statistic = "median",
                         interval = "15 min",
                         remove.calm = FALSE,
                         type = "default")

#removing columns from hourly median dataframe for pH
RB_1hr_med <- subset(RB_1hr_med, select = (-c(median, stdev, hi.95, lo.95)))

#renaming existing columns
names(RB_1hr_med)[1] <- "Datetime"
names(RB_1hr_med)[2] <- "Date"
names(RB_1hr_med)[3] <- "pH"

#scatterplot of RB hourly median data
RB_pH <-RB_1hr_med %>%
  ggplot(aes(x = Date, y = pH, color = pH)) +
  geom_point(alpha=0.5) +
  labs(title ="Hourly median pH data from Richardson Bay",
       subtitle = "04/19/2016 - 07/07/2022",
       caption = "data courtesy of NERR")

RB_pH

#updating csv file
write.csv(RB_1hr_med, "/Users/mehakjain/Downloads/Thesis/R:data exploration/RB_pH.csv")



####DO####


#remove anything from environment
rm(list=ls())

#calling all years of richardson bay data
a <- read.csv("rb_2016_rawdata.csv", header = TRUE)
b <- read.csv("richardson_bay.csv", header = TRUE)
c <- read.csv("RB-2020-raw-data.csv", header = TRUE)
d <- read.csv("rb-2021-rawdata.csv", header = TRUE)
e <- read.csv("RB-2022-rawdata.csv", header = TRUE)

#combining all years of RB data
all_years <- rbind(a, b, c, d, e)

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

#creating a new date column in correct format
all_years$date <- as.POSIXct(all_years$DateTimeStamp, format = c("%m/%d/%y"))

#creating a new datetime column in correct format
all_years$datetime <- as.POSIXct(all_years$DateTimeStamp, format = c("%m/%d/%y %H:%M"))


##RB-DO dataframe creation from raw data##


#subsetting raw data to the columns I want
all_years <- subset(all_years, select = c(date, datetime, DO_mgl, F_DO_mgl))

all_years %>%
  ggplot(aes(x=datetime, y=DO_mgl, color=DO_mgl))+
  geom_point(alpha=0.5)+
  labs(title="DO data from Richardson Bay: All data points",
       subtitle= "04/19/2016 - 07/07/2022",
       caption= "data courtesy of NERR")

#Removing flagged data that did not pass QAQC= -3 and suspect data = 1
all_years <- all_years[- grep("-3", all_years$F_DO_mgl),]

all_years <- all_years[- grep("-2", all_years$F_DO_mgl),]

all_years <- all_years[- grep("1", all_years$F_DO_mgl),]

all_years <- all_years %>% 
  drop_na(DO_mgl)

all_years %>%
  ggplot(aes(x=datetime, y=DO_mgl, color=DO_mgl))+
  geom_point(alpha=0.5)+
  labs(title="DO data from Richardson Bay: Failed QC data removed",
       subtitle=  "04/19/2016 - 07/07/2022",
       caption= "data courtesy of NERR")

#Applying a rolling median for a 4hr window
all_years <- all_years %>%
  tq_mutate(
    select = DO_mgl,
    mutate_fun = rollapply,
    width = 16,
    align = "right",
    by.column = FALSE,
    FUN = stat_func,
    na.rm = TRUE)

#Filter to remove values that are +/- 2 standard deviations from the rolling 
#median
all_years <- filter(all_years, DO_mgl < hi.95 & DO_mgl > lo.95)

all_years %>%
  ggplot(aes(x=datetime, y=DO_mgl, color=DO_mgl))+
  geom_point(alpha=0.5)+
  labs(title="DO data from Richardson Bay: Failed QC data & +/-2sd removed",
       subtitle=  "04/19/2016 - 07/07/2022",
       caption= "data courtesy of NERR")

##Hourly median dataframe
#holder name
names(all_years)[1] <- "datex"

#column named "date" with time stamp for hourly median to work
names(all_years)[2] <- "date"

#Creating hourly median dataframe of pH
RB_1hr_med<- timeAverage(all_years,
                              avg.time = "1 hour",
                              data.thresh = 0,
                              statistic = "median",
                              interval = "15 min",
                              remove.calm = FALSE,
                              type = "default")

#removing columns from hourly median dataframe for DO
RB_1hr_med <- subset(RB_1hr_med, select = (-c(median, stdev, hi.95, lo.95)))

#renaming existing columns
names(RB_1hr_med)[1] <- "Datetime"
names(RB_1hr_med)[2] <- "Date"
names(RB_1hr_med)[3] <- "DO_mgl"

#scatterplot of RB hourly median data 
RB_DO_mgl <-RB_1hr_med %>%
  ggplot(aes(x = Date, y = DO_mgl, color = DO_mgl)) +
  geom_point(alpha=0.5) +
  labs(title ="Hourly median pH data from Richardson Bay",
       subtitle =  "04/19/2016 - 07/07/2022",
       caption = "data courtesy of NERR")

RB_DO_mgl

#updating csv file
write.csv(RB_1hr_med, "/Users/mehakjain/Downloads/Thesis/R:data exploration/RB_DO_mgl.csv")




####Salinity####



#remove anything from environment
rm(list=ls())

#calling all years of richardson bay data
a <- read.csv("rb_2016_rawdata.csv", header = TRUE)
b <- read.csv("richardson_bay.csv", header = TRUE)
c <- read.csv("RB-2020-raw-data.csv", header = TRUE)
d <- read.csv("rb-2021-rawdata.csv", header = TRUE)
e <- read.csv("RB-2022-rawdata.csv", header = TRUE)

#combining all years of RB data
all_years <- rbind(a, b, c, d, e)

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

#creating a new date column in correct format
all_years$date <- as.POSIXct(all_years$DateTimeStamp, format = c("%m/%d/%y"))

#creating a new datetime column in correct format
all_years$datetime <- as.POSIXct(all_years$DateTimeStamp, format = c("%m/%d/%y %H:%M"))

#subsetting raw data to the columns I want
all_years <- subset(all_years, select = c(date, datetime, Sal, F_Sal))

all_years %>%
  ggplot(aes(x=datetime, y=Sal, color=Sal))+
  geom_point(alpha=0.5)+
  labs(title="Sal data from Richardson Bay: All data points",
       subtitle=  "04/19/2016 - 07/07/2022",
       caption= "data courtesy of NERR")

#Removing flagged data that did not pass QAQC= -3 and suspect data = 1, missing data = -2
all_years <- all_years[- grep("-3", all_years$F_Sal),]

all_years <- all_years[- grep("-2", all_years$F_Sal),]

all_years <- all_years[- grep("1", all_years$F_Sal),]

all_years <- all_years %>% 
  drop_na(Sal)

all_years %>%
  ggplot(aes(x=datetime, y=Sal, color=Sal))+
  geom_point(alpha=0.5)+
  labs(title="Sal data from Richardson Bay: Failed QC data removed",
       subtitle=  "04/19/2016 - 07/07/2022",
       caption= "data courtesy of NERR")

#Applying a rolling median for a 4hr window
all_years <- all_years %>%
  tq_mutate(
    select = Sal,
    mutate_fun = rollapply,
    width = 16,
    align = "right",
    by.column = FALSE,
    FUN = stat_func,
    na.rm = TRUE)

#Filter to remove values that are +/- 2 standard deviations from the rolling 
#median
all_years <- filter(all_years, Sal < hi.95 & Sal > lo.95)

all_years %>%
  ggplot(aes(x=datetime, y=Sal, color=Sal)) +
  geom_point(alpha=0.5) +
  labs(title ="Sal data from Richardson Bay: Failed QC data & +/-2sd removed",
       subtitle =  "04/19/2016 - 07/07/2022",
       caption = "data courtesy of NERR")

##Hourly median dataframe
#holder name
names(all_years)[1] <- "datex"

#column named "date" with time stamp for hourly median to work
names(all_years)[2] <- "date"

#Creating hourly median dataframe of Sal
RB_1hr_med<- timeAverage(all_years,
                              avg.time = "1 hour",
                              data.thresh = 0,
                              statistic = "median",
                              interval = "15 min",
                              remove.calm = FALSE,
                              type = "default")

#removing columns from hourly median dataframe for Sal
RB_1hr_med <- subset(RB_1hr_med, select = (-c(median, stdev, hi.95, lo.95)))

#renaming existing columns
names(RB_1hr_med)[1] <- "Datetime"
names(RB_1hr_med)[2] <- "Date"
names(RB_1hr_med)[3] <- "Sal"

#scatterplot of RB hourly median data 
RB_Sal <-RB_1hr_med %>%
  ggplot(aes(x = Date, y = Sal, color = Sal)) +
  geom_point(alpha=0.5) +
  labs(title ="Hourly median Sal data from Richardson Bay",
       subtitle =  "04/19/2016 - 07/07/2022",
       caption = "data courtesy of NERR")

RB_Sal

#updating csv file
write.csv(RB_1hr_med, "/Users/mehakjain/Downloads/Thesis/R:data exploration/RB_Sal.csv")



####Water Temp####



#remove anything from environment
rm(list=ls())

#calling all years of richardson bay data
a <- read.csv("rb_2016_rawdata.csv", header = TRUE)
b <- read.csv("richardson_bay.csv", header = TRUE)
c <- read.csv("RB-2020-raw-data.csv", header = TRUE)
d <- read.csv("rb-2021-rawdata.csv", header = TRUE)
e <- read.csv("RB-2022-rawdata.csv", header = TRUE)

#combining all years of RB data
all_years <- rbind(a, b, c, d, e)

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

#creating a new date column in correct format
all_years$date <- as.POSIXct(all_years$DateTimeStamp, format = c("%m/%d/%y"))

#creating a new datetime column in correct format
all_years$datetime <- as.POSIXct(all_years$DateTimeStamp, format = c("%m/%d/%y %H:%M"))

#subsetting raw data to the columns I want
all_years <- subset(all_years, select = c(date, datetime, Temp, F_Temp))

all_years %>%
  ggplot(aes(x=datetime, y=Temp, color=Temp))+
  geom_point(alpha=0.5)+
  labs(title="Temp data from Richardson Bay: All data points",
       subtitle=  "04/19/2016 - 07/07/2022",
       caption= "data courtesy of NERR")

#Removing flagged data that did not pass QAQC= -3 and suspect data = 1
all_years <- all_years[- grep("-3", all_years$F_Temp),]

all_years <- all_years[- grep("-2", all_years$F_Temp),]

all_years <- all_years[- grep("1", all_years$F_Temp),]

all_years <- all_years %>% 
  drop_na(Temp)

#Applying a rolling median for a 4hr window
all_years <- all_years %>%
  tq_mutate(
    select = Temp,
    mutate_fun = rollapply,
    width = 16,
    align = "right",
    by.column = FALSE,
    FUN = stat_func,
    na.rm = TRUE)

#Filter to remove values that are +/- 2 standard deviations from the rolling 
#median
all_years <- filter(all_years, Temp < hi.95 & Temp > lo.95)

all_years %>%
  ggplot(aes(x=datetime, y=Temp, color=Temp)) +
  geom_point(alpha=0.5) +
  labs(title ="Temp data from Richardson Bay: Failed QC data & +/-2sd removed",
       subtitle =  "04/19/2016 - 07/07/2022",
       caption = "data courtesy of NERR")

##Hourly median dataframe
#holder name
names(all_years)[1] <- "datex"

#column named "date" with time stamp for hourly median to work
names(all_years)[2] <- "date"

#Creating hourly median dataframe of Temp
RB_1hr_med<- timeAverage(all_years,
                              avg.time = "1 hour",
                              data.thresh = 0,
                              statistic = "median",
                              interval = "15 min",
                              remove.calm = FALSE,
                              type = "default")

#removing columns from hourly median dataframe for Temp
RB_1hr_med <- subset(RB_1hr_med, select = (-c(median, stdev, hi.95, lo.95)))

#renaming existing columns
names(RB_1hr_med)[1] <- "Datetime"
names(RB_1hr_med)[2] <- "Date"
names(RB_1hr_med)[3] <- "Water_Temp"

#scatterplot of RB hourly median data 
RB_Temp <-RB_1hr_med %>%
  ggplot(aes(x = Date, y = Water_Temp, color = Water_Temp)) +
  geom_point(alpha=0.5) +
  labs(title ="Hourly median Water Temperature data from Richardson Bay",
       subtitle =  "04/19/2016 - 07/07/2022",
       caption = "data courtesy of NERR")

RB_Temp

#updating csv file
write.csv(RB_1hr_med, "/Users/mehakjain/Downloads/Thesis/R:data exploration/RB_Temp.csv")



####Depth####



#remove anything from environment
rm(list=ls())

#calling all years of richardson bay data
a <- read.csv("rb_2016_rawdata.csv", header = TRUE)
b <- read.csv("richardson_bay.csv", header = TRUE)
c <- read.csv("RB-2020-raw-data.csv", header = TRUE)
d <- read.csv("rb-2021-rawdata.csv", header = TRUE)
e <- read.csv("RB-2022-rawdata.csv", header = TRUE)

#combining all years of RB data
all_years <- rbind(a, b, c, d, e)

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

#creating a new date column in correct format
all_years$date <- as.POSIXct(all_years$DateTimeStamp, format = c("%m/%d/%y"))

#creating a new datetime column in correct format
all_years$datetime <- as.POSIXct(all_years$DateTimeStamp, format = c("%m/%d/%y %H:%M"))

#subsetting raw data to the columns I want
all_years <- subset(all_years, select = c(date, datetime, Depth, F_Depth))

all_years %>%
  ggplot(aes(x=datetime, y=Depth, color=Depth))+
  geom_point(alpha=0.5)+
  labs(title="Depth data from Richardson Bay: All data points",
       subtitle=  "04/19/2016 - 07/07/2022",
       caption= "data courtesy of NERR")

#Removing flagged data that did not pass QAQC= -3 and suspect data = 1
all_years <- all_years[- grep("-3", all_years$F_Depth),]

all_years <- all_years[- grep("-2", all_years$F_Depth),]

all_years <- all_years[- grep("1", all_years$F_Depth),]

all_years <- all_years %>% 
  drop_na(Depth)

all_years %>%
  ggplot(aes(x=datetime, y=Depth, color=Depth)) +
  geom_point(alpha=0.5) +
  labs(title ="Depth data from Richardson Bay: Failed QC data removed",
       subtitle =  "04/19/2016 - 07/07/2022",
       caption = "data courtesy of NERR")

#Applying a rolling median for a 4hr window
all_years <- all_years %>%
  tq_mutate(
    select = Depth,
    mutate_fun = rollapply,
    width = 16,
    align = "right",
    by.column = FALSE,
    FUN = stat_func,
    na.rm = TRUE)

#Filter to remove values that are +/- 2 standard deviations from the rolling 
#median
all_years <- filter(all_years, Depth < hi.95 & Depth > lo.95)

all_years %>%
  ggplot(aes(x=datetime, y=Depth, color=Depth)) +
  geom_point(alpha=0.5) +
  labs(title ="Depth data from Richardson Bay: Failed QC data & +/-2sd removed",
       subtitle =  "04/19/2016 - 07/07/2022",
       caption = "data courtesy of NERR")

##Hourly median dataframe
#holder name
names(all_years)[1] <- "datex"

#column named "date" with time stamp for hourly median to work
names(all_years)[2] <- "date"

#Creating hourly median dataframe of Depth
RB_1hr_med<- timeAverage(all_years,
                              avg.time = "1 hour",
                              data.thresh = 0,
                              statistic = "median",
                              interval = "15 min",
                              remove.calm = FALSE,
                              type = "default")

#removing columns from hourly median dataframe for Temp
RB_1hr_med <- subset(RB_1hr_med, select = (-c(median, stdev, hi.95, lo.95)))

#renaming existing columns
names(RB_1hr_med)[1] <- "Datetime"
names(RB_1hr_med)[2] <- "Date"
names(RB_1hr_med)[3] <- "Depth"

#scatterplot of RB hourly median data 
RB_depth <-RB_1hr_med %>%
  ggplot(aes(x = Date, y = Depth, color = Depth)) +
  geom_point(alpha=0.5) +
  labs(title ="Hourly median Depth data from Richardson Bay 2017-2019",
       subtitle =  "04/19/2016 - 07/07/2022",
       caption = "data courtesy of NERR")

RB_depth

#updating csv file
write.csv(RB_1hr_med, "/Users/mehakjain/Downloads/Thesis/R:data exploration/RB_depth.csv")




###DO Saturation###



#remove anything from environment
rm(list=ls())

#calling all years of richardson bay data
a <- read.csv("rb_2016_rawdata.csv", header = TRUE)
b <- read.csv("richardson_bay.csv", header = TRUE)
c <- read.csv("RB-2020-raw-data.csv", header = TRUE)
d <- read.csv("rb-2021-rawdata.csv", header = TRUE)
e <- read.csv("RB-2022-rawdata.csv", header = TRUE)

#combining all years of RB data
all_years <- rbind(a, b, c, d, e)

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

#creating a new date column in correct format
all_years$date <- as.POSIXct(all_years$DateTimeStamp, format = c("%m/%d/%y"))

#creating a new datetime column in correct format
all_years$datetime <- as.POSIXct(all_years$DateTimeStamp, format = c("%m/%d/%y %H:%M"))

#subsetting raw data to the columns I want
all_years <- subset(all_years, select = c(date, datetime, DO_pct, F_DO_pct))

all_years %>%
  ggplot(aes(x=datetime, y=DO_pct, color=DO_pct))+
  geom_point(alpha=0.5)+
  labs(title="DO Saturation data from Richardson Bay: All data points",
       subtitle=  "04/19/2016 - 07/07/2022",
       caption= "data courtesy of NERR")

#Removing flagged data that did not pass QAQC= -3 and suspect data = 1
all_years <- all_years[- grep("-3", all_years$F_DO_pct),]

all_years <- all_years[- grep("-2", all_years$F_DO_pct),]

all_years <- all_years[- grep("1", all_years$F_DO_pct),]

all_years <- all_years %>% 
  drop_na(DO_pct)

all_years %>%
  ggplot(aes(x=datetime, y=DO_pct, color=DO_pct)) +
  geom_point(alpha=0.5) +
  labs(title ="Depth data from Richardson Bay: Failed QC data removed",
       subtitle =  "04/19/2016 - 07/07/2022",
       caption = "data courtesy of NERR")

#Applying a rolling median for a 4hr window
all_years <- all_years %>%
  tq_mutate(
    select = DO_pct,
    mutate_fun = rollapply,
    width = 16,
    align = "right",
    by.column = FALSE,
    FUN = stat_func,
    na.rm = TRUE)

#Filter to remove values that are +/- 2 standard deviations from the rolling 
#median
all_years <- filter(all_years, DO_pct < hi.95 & DO_pct > lo.95)

all_years %>%
  ggplot(aes(x=datetime, y=DO_pct, color=DO_pct)) +
  geom_point(alpha=0.5) +
  labs(title ="Depth data from Richardson Bay: Failed QC data & +/-2sd removed",
       subtitle =  "04/19/2016 - 07/07/2022",
       caption = "data courtesy of NERR")

##Hourly median dataframe
#holder name
names(all_years)[1] <- "datex"

#column named "date" with time stamp for hourly median to work
names(all_years)[2] <- "date"

#Creating hourly median dataframe 
RB_1hr_med<- timeAverage(all_years,
                         avg.time = "1 hour",
                         data.thresh = 0,
                         statistic = "median",
                         interval = "15 min",
                         remove.calm = FALSE,
                         type = "default")

#removing columns from hourly median dataframe 
RB_1hr_med <- subset(RB_1hr_med, select = (-c(median, stdev, hi.95, lo.95)))

#renaming existing columns
names(RB_1hr_med)[1] <- "Datetime"
names(RB_1hr_med)[2] <- "Date"
names(RB_1hr_med)[3] <- "DO_pct"

#scatterplot of RB hourly median data 
RB_DO_pct <-RB_1hr_med %>%
  ggplot(aes(x = Date, y = DO_pct, color = DO_pct)) +
  geom_point(alpha=0.5) +
  labs(title ="Hourly median Depth data from Richardson Bay 2017-2019",
       subtitle =  "04/19/2016 - 07/07/2022",
       caption = "data courtesy of NERR")

RB_DO_pct

#updating csv file
write.csv(RB_1hr_med, "/Users/mehakjain/Downloads/Thesis/R:data exploration/RB_DO_pct.csv")

