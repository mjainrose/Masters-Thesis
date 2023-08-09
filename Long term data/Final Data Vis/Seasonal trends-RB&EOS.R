---
title:"Seasonal Trends between Ricahrdson Bay (RB) and the Estuary and Ocean Science (EOS) Center"
author:"MJ"
date:"3/22/2022"
output:html_document
---
  
  
#clearing anything in the environment
rm(list = ls())

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

#set the working directory
##by using -> setwd()


#uploading hourly_median data from 2016-2022 for all sites
all_sites <-
  read.csv("All sites hourly median 2016-2022.csv", header = TRUE)

#converting datetime from a character string to a numerical string
all_sites$Datetime <-
  as.POSIXct(all_sites$Datetime, format = c("%Y-%m-%d %H:%M:%S"))

#creating a day, month, and year column for further processing
all_sites$Day <- as.numeric(format(all_sites$Datetime, "%d"))
all_sites$Month <- as.numeric(format(all_sites$Datetime, "%m"))
all_sites$Year <- as.numeric(format(all_sites$Datetime, "%Y"))
all_sites$Hour <- as.numeric(format(all_sites$Datetime, "%H"))

#creating a season column
months <-
  as.numeric(format(as.Date(all_sites$Datetime, '%Y-%m-%d %H:%M:%S'), '%m'))
indx <- setNames(rep(c('Winter', 'Spring', 'Summer',
                       'Fall'), each = 3), c(12, 1:11))
all_sites$Season <- unname(indx[as.character(months)])


#Filtering out RB and EOS
RB_EOS <- all_sites %>%
  filter(Site == c("Richardson Bay", "EOS Center"))




#Seasonal Trends in pH




stat_pH <- function(y) {
  return(data.frame(
    y = median(y) + 1.3,
    label = paste(
      "n =", format(length(y), big.mark = ","), "\n"
    )
  ))
}


#Box plot of 2016-2022 data looking at pH
pH <- ggplot(RB_EOS, aes(
  x = factor(Season, level = c("Winter", "Spring", "Summer", "Fall")),
  y = pH_rounded,
  color = Site
)) +
  geom_boxplot(outlier.size = 0.5) +
  # geom_point(aes(color = Site),
  #            pch = 16,
  #            size = 0.1,
  #            alpha = 0.5,
  #            position = position_jitterdodge(
  #              jitter.height = 0.5,
  #              jitter.width = 0.5
  #            )) +
  stat_summary(fun.data = stat_pH, geom = "text", hjust = .8, vjust = 1, size = 4.5) +
  # geom_jitter(color = "black", shape = 1, size=0.4, alpha=5) +
  theme_bw(base_size = 16) +
  theme(legend.position = "top",
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) +
  scale_color_discrete(name = "Stations") +
  xlab("Season") +
  ylab("pH") 
  # ggtitle("Seasonal Trends in pH between Richardson Bay and the EOS Center") +
  # labs(subtitle = "2016-2022",
  #      caption = "Data courtesy of NERR & CeNCOOS")
pH

#Summary of results
##RB only
RB <- RB_EOS %>%
  filter(Site == c("Richardson Bay"))

tapply(RB$pH_rounded, RB$Season, summary)


##EOS only
EOS <- RB_EOS %>%
  filter(Site == c("EOS Center"))

tapply(EOS$pH_rounded, EOS$Season, summary)


#Figure 1. Seasonal trends of hourly median pH values rounded to the tenths between an eelgrass
#dominated bay, Richardson Bay (blue), and a deep, mainstream water channel in Central Bay near
#the EOS Center (red) from the years 2016-2022. Long-term datasets sourced by the National Estuarine
#Research Reserve (NERR) for Richardson Bay and Central and Northern California Ocean Observing
#System (CeNCOOS) for the EOS Center.





#Seasonal Trends in Dissolved Oxygen

stat_DO <- function(y) {
  return(data.frame(
    y = IQR(y) + 250,
    label = paste(
      "n =", format(length(y), big.mark = ","), "\n"
    )
  ))
}


#Box plot of 2016-2022 data looking at Dissolved Oxygen (% saturation)
DO <- ggplot(RB_EOS, aes(
  x = factor(Season, level = c("Winter", "Spring", "Summer", "Fall")),
  y = DO_pct,
  color = Site
)) +
  geom_boxplot(outlier.size = 0.5) +
  stat_summary(fun.data = stat_DO, geom = "text", hjust = .8, vjust = 1, size = 4) +
  # geom_point(aes(color = Site),
  #            pch = 16,
  #            size = 0.1,
  #            alpha = 0.5,
  #            position = position_jitterdodge(
  #              jitter.height = 0.5,
  #              jitter.width = 0.5
  #            )) +
  theme_bw(base_size = 16) +
  theme(legend.position = "top",
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) +
  scale_color_discrete(name = "Stations") +
  xlab("Season") +
  ylab("Dissolved Oxygen (% saturation)") 
  # ggtitle("Seasonal Trends in Dissolved Oxygen between Richardson Bay and the EOS Center") +
  # labs(subtitle = "2016-2022",
  #      caption = "Data courtesy of NERR & CeNCOOS")
DO

#Summary of results
##RB only
RB <- RB_EOS %>%
  filter(Site == c("Richardson Bay"))

tapply(RB$DO_pct, RB$Season, summary)


##EOS only
EOS <- RB_EOS %>%
  filter(Site == c("EOS Center"))

tapply(EOS$DO_pct, EOS$Season, summary)


#Figure 2. Seasonal trends of hourly median dissolved oxygen (DO) values in percent saturation
#between an eelgrass dominated bay, Richardson Bay (blue), and a deep, mainstream water channel
#in Central Bay near the EOS Center (red) from the years 2016-2022. Long-term datasets sourced
#by the National Estuarine Research Reserve (NERR) for Richardson Bay and Central and Northern
#California Ocean Observing System (CeNCOOS) for the EOS Center.




#Seasonal Trends in Salinity

stat_Sal <- function(y) {
  return(data.frame(
    y = median(y) + 10,
    label = paste(
      "n =", format(length(y), big.mark = ","), "\n"
    )
  ))
}


#Box plot of 2016-2022 data looking at Salinity
Sal <- ggplot(RB_EOS, aes(
  x = factor(Season, level = c("Winter", "Spring", "Summer", "Fall")),
  y = Sal,
  color = Site
)) +
  geom_boxplot(outlier.size = 0.5) +
  stat_summary(fun.data = stat_Sal, geom = "text", hjust = .8, vjust = 1, size = 5) +
  # geom_point(aes(color = Site),
  #            pch = 16,
  #            size = 0.1,
  #            alpha = 0.5,
  #            position = position_jitterdodge(
  #              jitter.height = 0.5,
  #              jitter.width = 0.5
  #            )) +
  theme_bw(base_size = 16) +
  theme(legend.position = "top",
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) +
  scale_color_discrete(name = "Stations") +
  xlab("Season") +
  ylab("Salinity") 
  # ggtitle("Seasonal Trends in Salinity between Richardson Bay and the EOS Center") +
  # labs(subtitle = "2016-2022",
  #      caption = "Data courtesy of NERR & CeNCOOS")
Sal

#Summary of results
##RB only
RB <- RB_EOS %>%
  filter(Site == c("Richardson Bay"))

tapply(RB$Sal, RB$Season, summary)


##EOS only
EOS <- RB_EOS %>%
  filter(Site == c("EOS Center"))

tapply(EOS$Sal, EOS$Season, summary)


#Figure 3. Seasonal trends of hourly median salinity values in practical salinity units between an
#eelgrass dominated bay, Richardson Bay (blue), and a deep, mainstream water channel in Central
#Bay near the EOS Center (red) from the years 2016-2022. Long-term datasets sourced by the
#National Estuarine Research Reserve (NERR) for Richardson Bay and Central and Northern California
#Ocean Observing System (CeNCOOS) for the EOS Center.





#Seasonal Trends in Water Temperature

stat_Temp <- function(y) {
  return(data.frame(
    y = max(y) +10,
    label = paste(
      "n =", format(length(y), big.mark = ","), "\n"
    )
  ))
}


#Box plot of 2016-2022 data looking at water temperature (degrees C)
Temp <- ggplot(RB_EOS, aes(
  x = factor(Season, level = c("Winter", "Spring", "Summer", "Fall")),
  y = Water_Temp,
  color = Site
)) +
  geom_boxplot(outlier.size = 0.5) +
  stat_summary(fun.data = stat_Temp, geom = "text", hjust = .8, vjust = 1, size = 5) +
  # geom_point(aes(color = Site),
  #            pch = 16,
  #            size = 0.1,
  #            alpha = 0.5,
  #            position = position_jitterdodge(
  #              jitter.height = 0.5,
  #              jitter.width = 0.5
  #            )) +
  theme_bw(base_size = 16) +
  theme(legend.position = "top",
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) +
  scale_color_discrete(name = "Stations") +
  xlab("Season") +
  ylab("Water Temperature (degrees C)")
  # ggtitle("Seasonal Trends in Water Temperature between Richardson Bay and the EOS Center") +
  # labs(subtitle = "2016-2022",
  #      caption = "Data courtesy of NERR & CeNCOOS")
Temp

#Summary of results
##RB only
RB <- RB_EOS %>%
  filter(Site == c("Richardson Bay"))

tapply(RB$Water_Temp, RB$Season, summary)


##EOS only
EOS <- RB_EOS %>%
  filter(Site == c("EOS Center"))

tapply(EOS$Water_Temp, EOS$Season, summary)


#Figure 4. Seasonal trends of hourly median water temperature values in degrees celsius between an
#eelgrass dominated bay, Richardson Bay (blue), and a deep, mainstream water channel in Central
#Bay near the EOS Center (red) from the years 2016-2022. Long-term datasets sourced by the
#National Estuarine Research Reserve (NERR) for Richardson Bay and Central and Northern California
#Ocean Observing System (CeNCOOS) for the EOS Center.





# #Seasonal Trends in Water Depth-this is sonde water depth 
# 
# 
# 
# 
# #Box plot of 2016-2022 data looking at water depth (m))
# ggplot(RB_EOS, aes(
#   x = factor(Season, level = c("Winter", "Spring", "Summer", "Fall")),
#   y = Depth,
#   color = Site
# )) +
#   geom_boxplot() +
#   theme_bw(base_size = 13) +
#   theme(legend.position = "top") +
#   xlab("Season") +
#   ylab("Water Depth (m)") +
#   ggtitle("Seasonal Trends in Water Depth between Richardson Bay and the EOS Center") +
#   labs(subtitle = "2016-2022",
#        caption = "Data courtesy of NERR & CeNCOOS")
# 
# 
# #Summary of results
# ##RB only
# RB <- RB_EOS %>%
#   filter(Site == c("Richardson Bay"))
# 
# tapply(RB$Depth, RB$Season, summary)
# 
# 
# ##EOS only
# EOS <- RB_EOS %>%
#   filter(Site == c("EOS Center"))
# 
# tapply(EOS$Depth, EOS$Season, summary)


#Figure 5. Seasonal trends of hourly median water depth values in meters between an eelgrass
#dominated bay, Richardson Bay (blue), and a deep, mainstream water channel in Central Bay near
#the EOS Center (red) from the years 2016-2022. Long-term datasets sourced by the National
#Estuarine Research Reserve (NERR) for Richardson Bay and Central and Northern California Ocean
#Observing System (CeNCOOS) for the EOS Center.


#plotting graphs on top of each other
all <- ggarrange(pH, DO, Sal, Temp, ncol = 2, nrow = 2, common.legend = TRUE, legend="top")

annotate_figure(
  all,
  top = text_grob("Seasonal Trends in Water Quality between Richardson Bay and the EOS Center \n 2016-2022", size = 12),
  bottom = text_grob("Data courtesy of NERR & CeNCOOS", size = 8)
)

