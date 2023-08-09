---
  title: "Climatology hourly average values"
author: "MJ"
date: "3/18/2022"
output: html_document
---

# Hourly Climatology Data for RB and EOS


#Clearing anything in the environment
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

#Uploading hourly_median data from 2016-2022 from all sites
allsites <-
  read.csv("All sites hourly median 2016-2022.csv", header = TRUE)

#Converting datetime
##to UTC
allsites$Datetime <-
  as.POSIXct(allsites$Datetime,
             format = c("%Y-%m-%d %H:%M:%S"),
             tz = "UTC")

##to PST
hrs <- 8 * 60 * 60
allsites$Datetime <- allsites$Datetime - hrs #PST time


#Creating a day, month, year and season column for further processing
allsites$Day <- as.numeric(format(allsites$Datetime, "%d"))
allsites$Month <- as.numeric(format(allsites$Datetime, "%m"))
allsites$Year <- as.numeric(format(allsites$Datetime, "%Y"))
allsites$Hour <- as.numeric(format(allsites$Datetime, "%H"))

months <-
  as.numeric(format(as.Date(allsites$Datetime, '%Y-%m-%d %H:%M:%S'), '%m'))
indx <- setNames(rep(c('Winter', 'Spring', 'Summer',
                       'Fall'), each = 3), c(12, 1:11))
allsites$Season <- unname(indx[as.character(months)])


#Average_values




##pH





# #Hour and Season
# 
# 
# 
# 
# 
# #Calculating the hourly Average pH for each hour by season
# ##average value of pH by Hour and Season
# Average_pH <-
#   with(allsites, aggregate(
#     list(pH_rounded),
#     by = list(Site, Hour, Season),
#     FUN = function(x) {
#       Average_pH = mean(x, na.rm = TRUE)
#     }
#   ))
# 
# ##renaming columns
# Average_pH <- do.call(data.frame, Average_pH)
# colnames(Average_pH) <-
#   c("Site", 'Hour', "Season", 'Average_pH_hour')
# 
# ##rounding average pH values
# #Average_pH <-
# #  Average_pH %>% mutate_at(vars(starts_with("Average_pH")), funs(round(., 1)))
# 
# ##merging average dataframe to large dataframe
# allsites <-
#   merge(allsites, Average_pH, by = c("Hour", "Site", "Season"))
# 
# ##ordering large dataframe by datetime
# allsites <- allsites[order(allsites$Datetime),]
# 
# ##organizing columns
# allsites <-
#   subset(
#     allsites,
#     select = c(
#       Site,
#       Date,
#       Datetime,
#       Water_Temp,
#       Depth,
#       Sal,
#       pH,
#       pH_rounded,
#       DO_mgl,
#       DO_pct,
#       Day,
#       Month,
#       Year,
#       Season,
#       Hour,
#       Average_pH_hour
#     )
#   )
# 
# 
# 
# #Calculating the hourly standard deviation of pH for each hour and season
# SD_pH <-
#   with(allsites, aggregate(
#     list(pH_rounded),
#     by = list(Site, Hour, Season),
#     FUN = function(x) {
#       SD_pH = (sd(x, na.rm = TRUE))
#     }
#   ))
# 
# ##renaming columns
# SD_pH <- do.call(data.frame, SD_pH)
# colnames(SD_pH) <- c("Site", 'Hour', "Season", 'SD_pH_hour')
# 
# ##merging average dataframe to large dataframe
# allsites <- merge(allsites, SD_pH, by = c("Hour", "Site", "Season"))
# 
# ##ordering large dataframe by datetime
# allsites <- allsites[order(allsites$Datetime),]
# 
# ##organizing columns
# allsites <-
#   subset(
#     allsites,
#     select = c(
#       Site,
#       Date,
#       Datetime,
#       Water_Temp,
#       Depth,
#       Sal,
#       pH,
#       pH_rounded,
#       DO_mgl,
#       DO_pct,
#       Day,
#       Month,
#       Year,
#       Season,
#       Hour,
#       Average_pH_hour,
#       SD_pH_hour
#     )
#   )
# 
# 
# 
# #Calculating the hourly standard error of pH for each hour and season
# SE_pH <-
#   with(allsites, aggregate(
#     list(pH_rounded),
#     by = list(Site, Hour, Season),
#     FUN = function(x) {
#       SE_pH = (sd(x, na.rm = TRUE)) / sqrt(length(x))
#     }
#   ))
# 
# ##renaming columns
# SE_pH <- do.call(data.frame, SE_pH)
# colnames(SE_pH) <- c("Site", 'Hour', "Season", 'SE_pH_hour')
# 
# ##merging average dataframe to large dataframe
# allsites <- merge(allsites, SE_pH, by = c("Hour", "Site", "Season"))
# 
# ##ordering large dataframe by datetime
# allsites <- allsites[order(allsites$Datetime),]
# 
# ##organizing columns
# allsites <-
#   subset(
#     allsites,
#     select = c(
#       Site,
#       Date,
#       Datetime,
#       Water_Temp,
#       Depth,
#       Sal,
#       pH,
#       pH_rounded,
#       DO_mgl,
#       DO_pct,
#       Day,
#       Month,
#       Year,
#       Season,
#       Hour,
#       Average_pH_hour,
#       SD_pH_hour,
#       SE_pH_hour
#     )
#   )
# 
# 
# 
# #Calculating the hourly minimum pH for each hour and season
# ##minimum value of pH by Hour and season
# Minimum_pH <-
#   with(allsites, aggregate(
#     list(pH_rounded),
#     by = list(Site, Hour, Season),
#     FUN = function(x) {
#       Minimum_pH = min(x, na.rm = TRUE)
#     }
#   ))
# 
# ##renaming columns
# Minimum_pH <- do.call(data.frame, Minimum_pH)
# colnames(Minimum_pH) <- c("Site", 'Hour', "Season", 'Minimum_pH_hour')
# 
# ##rounding minimum pH values
# #Minimum_pH <-
# #  Minimum_pH %>% mutate_at(vars(starts_with("Minimum_pH")), funs(round(., 1)))
# 
# ##merging minimum dataframe to large dataframe
# allsites <- merge(allsites, Minimum_pH, by = c("Hour", "Site", "Season"))
# 
# ##ordering large dataframe by datetime
# allsites <- allsites[order(allsites$Datetime),]
# 
# ##organizing columns
# allsites <-
#   subset(
#     allsites,
#     select = c(
#       Site,
#       Date,
#       Datetime,
#       Water_Temp,
#       Depth,
#       Sal,
#       pH,
#       pH_rounded,
#       DO_mgl,
#       DO_pct,
#       Day,
#       Month,
#       Year,
#       Season,
#       Hour,
#       Average_pH_hour,
#       SD_pH_hour,
#       SE_pH_hour,
#       Minimum_pH_hour
#     )
#   )
# 
# 
# 
# 
# #Calculating the hourly maximum pH for each hour and season
# ##maximum value of pH by Hour and season
# Maximum_pH <-
#   with(allsites, aggregate(
#     list(pH_rounded),
#     by = list(Site, Hour, Season),
#     FUN = function(x) {
#       Maximum_pH = max(x, na.rm = TRUE)
#     }
#   ))
# 
# ##renaming columns
# Maximum_pH <- do.call(data.frame, Maximum_pH)
# colnames(Maximum_pH) <- c("Site", 'Hour', "Season", 'Maximum_pH_hour')
# 
# ##rounding minimum pH values
# #Maximum_pH <-
# #  Maximum_pH %>% mutate_at(vars(starts_with("Maximum_pH")), funs(round(., 1)))
# 
# ##merging maximum dataframe to large dataframe
# allsites <- merge(allsites, Maximum_pH, by = c("Hour", "Site", "Season"))
# 
# ##ordering large dataframe by datetime
# allsites <- allsites[order(allsites$Datetime),]
# 
# ##organizing columns
# allsites <-
#   subset(
#     allsites,
#     select = c(
#       Site,
#       Date,
#       Datetime,
#       Water_Temp,
#       Depth,
#       Sal,
#       pH,
#       pH_rounded,
#       DO_mgl,
#       DO_pct,
#       Day,
#       Month,
#       Year,
#       Season,
#       Hour,
#       Average_pH_hour,
#       SD_pH_hour,
#       SE_pH_hour,
#       Minimum_pH_hour,
#       Maximum_pH_hour
#     )
#   )





#Hour only




#Calculating the hourly Average pH for each hour
##average value of pH by Hour
Average_pH <-
  with(allsites, aggregate(
    list(pH_rounded),
    by = list(Site, Hour),
    FUN = function(x) {
      Average_pH = mean(x, na.rm = TRUE)
    }
  ))

##renaming columns
Average_pH <- do.call(data.frame, Average_pH)
colnames(Average_pH) <- c("Site", 'Hour', 'Average_pH_hour')

##rounding average pH values
#Average_pH <-
#  Average_pH %>% mutate_at(vars(starts_with("Average_pH")), funs(round(., 1)))

##merging average dataframe to large dataframe
allsites <- merge(allsites, Average_pH, by = c("Hour", "Site"))

##ordering large dataframe by datetime
allsites <- allsites[order(allsites$Datetime),]

##organizing columns
allsites <-
  subset(
    allsites,
    select = c(
      Site,
      Date,
      Datetime,
      Water_Temp,
      Depth,
      Sal,
      pH,
      pH_rounded,
      DO_mgl,
      DO_pct,
      Day,
      Month,
      Year,
      Season,
      Hour,
      Average_pH_hour
    )
  )


# #Calculating the hourly standard deviation of pH for each hour
# SD_pH <-
#   with(allsites, aggregate(
#     list(pH_rounded),
#     by = list(Site, Hour),
#     FUN = function(x) {
#       SD_pH = (sd(x, na.rm = TRUE))
#     }
#   ))
# 
# ##renaming columns
# SD_pH <- do.call(data.frame, SD_pH)
# colnames(SD_pH) <- c("Site", 'Hour', 'SD_pH_hour')
# 
# ##merging average dataframe to large dataframe
# allsites <- merge(allsites, SD_pH, by = c("Hour", "Site"))
# 
# ##ordering large dataframe by datetime
# allsites <- allsites[order(allsites$Datetime),]
# 
# ##organizing columns
# allsites <-
#   subset(
#     allsites,
#     select = c(
#       Site,
#       Date,
#       Datetime,
#       Water_Temp,
#       Depth,
#       Sal,
#       pH,
#       pH_rounded,
#       DO_mgl,
#       DO_pct,
#       Day,
#       Month,
#       Year,
#       Season,
#       Hour,
#       Average_pH_hour,
#       SD_pH_hour
#     )
#   )



#Calculating the hourly standard error of pH for each hour
SE_pH <-
  with(allsites, aggregate(
    list(pH_rounded),
    by = list(Site, Hour),
    FUN = function(x) {
      SE_pH = (sd(x, na.rm = TRUE)) / sqrt(length(x))
    }
  ))

##renaming columns
SE_pH <- do.call(data.frame, SE_pH)
colnames(SE_pH) <- c("Site", 'Hour', 'SE_pH_hour')

##merging average dataframe to large dataframe
allsites <- merge(allsites, SE_pH, by = c("Hour", "Site"))

##ordering large dataframe by datetime
allsites <- allsites[order(allsites$Datetime),]

##organizing columns
allsites <-
  subset(
    allsites,
    select = c(
      Site,
      Date,
      Datetime,
      Water_Temp,
      Depth,
      Sal,
      pH,
      pH_rounded,
      DO_mgl,
      DO_pct,
      Day,
      Month,
      Year,
      Season,
      Hour,
      Average_pH_hour,
      # SD_pH_hour,
      SE_pH_hour
    )
  )




# #Calculating the hourly minimum pH for each hour
# ##minimum value of pH by Hour
# Minimum_pH <-
#   with(allsites, aggregate(
#     list(pH_rounded),
#     by = list(Site, Hour),
#     FUN = function(x) {
#       Minimum_pH = min(x, na.rm = TRUE)
#     }
#   ))
# 
# ##renaming columns
# Minimum_pH <- do.call(data.frame, Minimum_pH)
# colnames(Minimum_pH) <- c("Site", 'Hour', 'Minimum_pH_hour')
# 
# ##rounding minimum pH values
# #Minimum_pH <-
# #  Minimum_pH %>% mutate_at(vars(starts_with("Minimum_pH")), funs(round(., 1)))
# 
# ##merging minimum dataframe to large dataframe
# allsites <- merge(allsites, Minimum_pH, by = c("Hour", "Site"))
# 
# ##ordering large dataframe by datetime
# allsites <- allsites[order(allsites$Datetime),]
# 
# ##organizing columns
# allsites <-
#   subset(
#     allsites,
#     select = c(
#       Site,
#       Date,
#       Datetime,
#       Water_Temp,
#       Depth,
#       Sal,
#       pH,
#       pH_rounded,
#       DO_mgl,
#       DO_pct,
#       Day,
#       Month,
#       Year,
#       Season,
#       Hour,
#       Average_pH_hour,
#       SD_pH_hour,
#       SE_pH_hour,
#       Minimum_pH_hour
#     )
#   )
# 
# 
# 
# 
# #Calculating the hourly maximum pH for each hour
# ##maximum value of pH by Hour
# Maximum_pH <-
#   with(allsites, aggregate(
#     list(pH_rounded),
#     by = list(Site, Hour),
#     FUN = function(x) {
#       Maximum_pH = max(x, na.rm = TRUE)
#     }
#   ))
# 
# ##renaming columns
# Maximum_pH <- do.call(data.frame, Maximum_pH)
# colnames(Maximum_pH) <- c("Site", 'Hour', 'Maximum_pH_hour')
# 
# ##rounding minimum pH values
# #Maximum_pH <-
# #  Maximum_pH %>% mutate_at(vars(starts_with("Maximum_pH")), funs(round(., 1)))
# 
# ##merging maximum dataframe to large dataframe
# allsites <- merge(allsites, Maximum_pH, by = c("Hour", "Site"))
# 
# ##ordering large dataframe by datetime
# allsites <- allsites[order(allsites$Datetime),]
# 
# ##organizing columns
# allsites <-
#   subset(
#     allsites,
#     select = c(
#       Site,
#       Date,
#       Datetime,
#       Water_Temp,
#       Depth,
#       Sal,
#       pH,
#       pH_rounded,
#       DO_mgl,
#       DO_pct,
#       Day,
#       Month,
#       Year,
#       Season,
#       Hour,
#       Average_pH_hour,
#       SD_pH_hour,
#       SE_pH_hour,
#       Minimum_pH_hour,
#       Maximum_pH_hour
#     )
#   )




#Comparing pH between RB & EOS Center




#Filtering out RB and EOS
RB_EOS <- allsites %>%
  filter(Site == c("Richardson Bay", "EOS Center"))




# #Hour and Season
# 
# 
# 
# #Climate graph of the Average_pH by season with SD
# ggplot(data = RB_EOS, aes(x = Hour, y = Average_pH_hour, color = Site)) +
#   geom_point(aes(shape = Site), position = position_dodge(0.2)) +
#   geom_line(position = position_dodge(0.5)) +
#   geom_errorbar(
#     aes(ymin = Average_pH_hour - SD_pH_hour, ymax = Average_pH_hour + SD_pH_hour),
#     width = 0,
#     position = position_dodge(0.2)
#   ) +
#   theme_bw(base_size = 15) +
#   theme(legend.position = "top") +
#   ggtitle("Comparison of Hourly pH Averages by Season") +
#   labs(subtitle = "2016-2022",
#        caption = "Data courtesy of NERR & CeNCOOS") +
#   xlab("Hour (PST)") +
#   ylab("Average_pH") +
#   facet_wrap( ~ factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")), nrow = 4)
# 
# 
# #Summary of results
# tapply(RB_EOS$Average_pH_hour, RB_EOS$Site, summary)
# 
# #Figure 1. Diel pH trends by season of average hourly-median values rounded to the tenths on the y-axis and 
# #hours in pacific standard time (PST) on the x-axis with standard deviation between an eelgrass dominated bay, Richardson 
# #Bay (blue), and a deep, mainstream water channel in Central Bay near the EOS Center (red) from 
# #the years 2016-2022. Long-term datasets sourced by the National Estuarine Research Reserve (NERR) 
# #for Richardson Bay and Central and Northern California Ocean Observing System (CeNCOOS) for the 
# #EOS Center.
# 
# 
# #Climate graph of the Average_pH by season with SE
# ggplot(data = RB_EOS, aes(x = Hour, y = Average_pH_hour, color = Site)) +
#   geom_point(aes(shape = Site), position = position_dodge(0.2)) +
#   geom_line(position = position_dodge(0.5)) +
#   geom_errorbar(
#     aes(ymin = Average_pH_hour - SE_pH_hour, ymax = Average_pH_hour + SE_pH_hour),
#     width = 0,
#     position = position_dodge(0.2)
#   ) +
#   theme_bw(base_size = 15) +
#   theme(legend.position = "top") +
#   ggtitle("Comparison of Hourly pH Averages by Season") +
#   labs(subtitle = "2016-2022",
#        caption = "Data courtesy of NERR & CeNCOOS") +
#   xlab("Hour (PST)") +
#   ylab("Average_pH") +
#   facet_wrap( ~ factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")), nrow = 4)
# 
# 
# #Summary of results
# tapply(RB_EOS$Average_pH_hour, RB_EOS$Site, summary)
# 
# #Figure 2. Diel pH trends by season of average hourly-median values rounded to the tenths on the y-axis and 
# #hours in pacific standard time (PST) on the x-axis with standard error between an eelgrass dominated bay, Richardson 
# #Bay (blue), and a deep, mainstream water channel in Central Bay near the EOS Center (red) from 
# #the years 2016-2022. Long-term datasets sourced by the National Estuarine Research Reserve (NERR) 
# #for Richardson Bay and Central and Northern California Ocean Observing System (CeNCOOS) for the 
# #EOS Center.
# 
# 
# #Min-Max-Ave pH Graph by Season
# ggplot(data = RB_EOS, aes(x = Hour, y = Average_pH_hour, color = Site)) +
#   geom_line() +
#   geom_point() +
#   geom_ribbon(aes(ymax = Maximum_pH_hour, ymin = Minimum_pH_hour), alpha = .1) +
#   theme_bw() +
#   theme(legend.position = "top") +
#   ggtitle("Comparison of Hourly pH Average, Minimum, and Maximum by Season") +
#   labs(subtitle = "2016-2022",
#        caption = "Data courtesy of NERR & CeNCOOS") +
#   xlab("Hour (PST)") +
#   ylab("Average_pH") +
#   facet_wrap( ~ factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")), nrow = 4)
# 
# 
# #Summary of results
# ##average pH
# tapply(RB_EOS$Average_pH_hour, RB_EOS$Site, summary)
# 
# ##minimum pH
# tapply(RB_EOS$Minimum_pH_hour, RB_EOS$Site, summary)
# 
# ##maximum pH
# tapply(RB_EOS$Maximum_pH_hour, RB_EOS$Site, summary)
# 
# 
# 
# #Figure 4. Diel pH trends by season of minimum, average, and maximum hourly-median values rounded to the 
# #tenths on the y-axis and hours in pacific standard time (PST) on the x-axis between an eelgrass 
# #dominated bay, Richardson Bay (blue), and a deep, mainstream water channel in Central Bay near 
# #the EOS Center (red) from the years 2016-2022. Long-term datasets sourced by the National 
# #Estuarine Research Reserve (NERR) for Richardson Bay and Central and Northern California Ocean 
# #Observing System (CeNCOOS) for the EOS Center.





#Hour only



# #Climate graph of the Average_pH with SD
# ggplot(data = RB_EOS, aes(x = Hour, y = Average_pH_hour, color = Site)) +
#   geom_point(aes(shape = Site), position = position_dodge(0.3)) +
#   geom_line(position = position_dodge(0.5)) +
#   geom_errorbar(
#     aes(ymin = Average_pH_hour - SD_pH_hour, ymax = Average_pH_hour + SD_pH_hour),
#     width = 0,
#     position = position_dodge(0.3)
#   ) +
#   theme_bw(base_size = 15) +
#   theme(legend.position = "top") +
#   ggtitle("Comparison of Hourly pH Averages") +
#   labs(subtitle = "2016-2022",
#        caption = "Data courtesy of NERR & CeNCOOS") +
#   xlab("Hour (PST)") +
#   ylab("Average_pH") 


# #Summary of results
# tapply(RB_EOS$Average_pH_hour, RB_EOS$Site, summary)
# tapply(RB_EOS$SD_pH_hour, RB_EOS$Site, summary)

#Figure 5. Diel pH trends of average hourly-median values rounded to the tenths on the y-axis and 
#hours in pacific standard time (PST) on the x-axis with standard deviation between an eelgrass dominated bay, Richardson 
#Bay (blue), and a deep, mainstream water channel in Central Bay near the EOS Center (red) from 
#the years 2016-2022. Long-term datasets sourced by the National Estuarine Research Reserve (NERR) 
#for Richardson Bay and Central and Northern California Ocean Observing System (CeNCOOS) for the 
#EOS Center.


#Climate graph of the Average_pH with SE
stat_pH <- function(y) {
  return(data.frame(
    y = min(y) + .03,
    label = paste(
      "n =", format(length(y), big.mark = ","), "\n"
    )
  ))
}

pH <- ggplot(data = RB_EOS, aes(x = Hour, y = Average_pH_hour, color = Site)) +
  geom_point(pch = 16,
             size = 0.1,
             alpha = 0.2,
             position = position_jitterdodge(
               jitter.height = 0.5,
               jitter.width = 0.5
             )) +
  # geom_ribbon(aes(ymin = Average_pH_hour - SE_pH_hour, ymax = Average_pH_hour + SE_pH_hour), alpha = .8) +
  geom_line(position = position_dodge(0.5)) +
  #stat_summary(fun.data = stat_pH, geom = "text", hjust = .8, vjust = 1, size = 3.5) +
  geom_errorbar(
    aes(ymin = Average_pH_hour - SE_pH_hour, ymax = Average_pH_hour + SE_pH_hour),
    width = .25,
    position = position_dodge(0.3)
  ) +
  theme_bw(base_size = 16) +
  theme(legend.position = "top", 
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) +
  scale_color_discrete(name = "Stations") +
  # ggtitle("Comparison of Hourly pH Averages") +
  # labs(subtitle = "2016-2022",
  #      caption = "Data courtesy of NERR & CeNCOOS") +
  xlab("Hour (PST)") +
  ylab("Average_pH") 
pH

#Summary of results
tapply(RB_EOS$Average_pH_hour, RB_EOS$Site, summary)
tapply(RB_EOS$SE_pH_hour, RB_EOS$Site, summary)

#Summary of results
##RB only
RB <- RB_EOS %>%
  filter(Site == c("Richardson Bay"))

tapply(RB$Average_pH_hour, RB$Hour, summary)

#Summary of results
##EOS only
EOS <- RB_EOS %>%
  filter(Site == c("EOS Center"))

tapply(EOS$Average_pH_hour, EOS$Hour, summary)

#Figure 6. Diel pH trends of average hourly-median values rounded to the tenths on the y-axis and 
#hours in pacific standard time (PST) on the x-axis with standard error between an eelgrass dominated bay, Richardson 
#Bay (blue), and a deep, mainstream water channel in Central Bay near the EOS Center (red) from 
#the years 2016-2022. Long-term datasets sourced by the National Estuarine Research Reserve (NERR) 
#for Richardson Bay and Central and Northern California Ocean Observing System (CeNCOOS) for the 
#EOS Center.


# #Min-Max-Ave pH Graph
# ggplot(data = RB_EOS, aes(x = Hour, y = Average_pH_hour, color = Site)) +
#   geom_line() +
#   geom_point() +
#   geom_ribbon(aes(ymax = Maximum_pH_hour, ymin = Minimum_pH_hour), alpha = .1) +
#   theme_bw() +
#   theme(legend.position = "top") +
#   ggtitle("Comparison of Hourly pH Average, Minimum, and Maximum") +
#   labs(subtitle = "2016-2022",
#        caption = "Data courtesy of NERR & CeNCOOS") +
#   xlab("Hour (PST)") +
#   ylab("Average_pH") +
#   facet_wrap( ~ Site, ncol = 2) 
# 
# 
# #Summary of results
# ##average pH
# tapply(RB_EOS$Average_pH_hour, RB_EOS$Site, summary)
# 
# ##minimum pH
# tapply(RB_EOS$Minimum_pH_hour, RB_EOS$Site, summary)
# 
# ##maximum pH
# tapply(RB_EOS$Maximum_pH_hour, RB_EOS$Site, summary)

#Figure 7. Diel pH trends of minimum, average, and maximum hourly-median values rounded to the 
#tenths on the y-axis and hours in pacific standard time (PST) on the x-axis between an eelgrass 
#dominated bay, Richardson Bay (blue), and a deep, mainstream water channel in Central Bay near 
#the EOS Center (red) from the years 2016-2022. Long-term datasets sourced by the National 
#Estuarine Research Reserve (NERR) for Richardson Bay and Central and Northern California Ocean 
#Observing System (CeNCOOS) for the EOS Center.




#DO_pct




#Hour and Season



# #Calculating the hourly Average DO_pct for each hour and season
# ##average value of DO_pct by Hour and season
# Average_DO_pct <-
#   with(allsites, aggregate(
#     list(DO_pct),
#     by = list(Site, Hour, Season),
#     FUN = function(x) {
#       Average_DO_pct = mean(x, na.rm = TRUE)
#     }
#   ))
# 
# ##renaming columns
# Average_DO_pct <- do.call(data.frame, Average_DO_pct)
# colnames(Average_DO_pct) <- c("Site", 'Hour', "Season", 'Average_DO_pct_hour')
# 
# ##merging average dataframe to large dataframe
# allsites <- merge(allsites, Average_DO_pct, by = c("Hour", "Site", "Season"))
# 
# ##ordering large dataframe by datetime
# allsites <- allsites[order(allsites$Datetime),]
# 
# ##organizing columns
# allsites <-
#   subset(
#     allsites,
#     select = c(
#       Site,
#       Date,
#       Datetime,
#       Water_Temp,
#       Depth,
#       Sal,
#       pH,
#       pH_rounded,
#       DO_mgl,
#       DO_pct,
#       Day,
#       Month,
#       Year,
#       Season,
#       Hour,
#       Average_DO_pct_hour
#     )
#   )
# 
# 
# #Calculating the hourly standard deviation of DO_pct for each hour and season
# SD_DO <-
#   with(allsites, aggregate(
#     list(DO_pct),
#     by = list(Site, Hour, Season),
#     FUN = function(x) {
#       SD_DO = (sd(x, na.rm = TRUE))
#     }
#   ))
# 
# ##renaming columns
# SD_DO <- do.call(data.frame, SD_DO)
# colnames(SD_DO) <- c("Site", 'Hour', "Season", 'SD_DO_pct_hour')
# 
# ##merging average dataframe to large dataframe
# allsites <- merge(allsites, SD_DO, by = c("Hour", "Site", "Season"))
# 
# ##ordering large dataframe by datetime
# allsites <- allsites[order(allsites$Datetime),]
# 
# ##organizing columns
# allsites <-
#   subset(
#     allsites,
#     select = c(
#       Site,
#       Date,
#       Datetime,
#       Water_Temp,
#       Depth,
#       Sal,
#       pH,
#       pH_rounded,
#       DO_mgl,
#       DO_pct,
#       Day,
#       Month,
#       Year,
#       Season,
#       Hour,
#       Average_DO_pct_hour,
#       SD_DO_pct_hour
#     )
#   )
# 
# 
# 
# #Calculating the hourly standard error of DO_pct for each hour and season
# SE_DO <-
#   with(allsites, aggregate(
#     list(DO_pct),
#     by = list(Site, Hour, Season),
#     FUN = function(x) {
#       SE_DO = (sd(x, na.rm = TRUE)) / sqrt(length(x))
#     }
#   ))
# 
# ##renaming columns
# SE_DO <- do.call(data.frame, SE_DO)
# colnames(SE_DO) <- c("Site", 'Hour', "Season", 'SE_DO_pct_hour')
# 
# ##merging average dataframe to large dataframe
# allsites <- merge(allsites, SE_Depth, by = c("Hour", "Site", "Season"))
# 
# ##ordering large dataframe by datetime
# allsites <- allsites[order(allsites$Datetime),]
# 
# ##organizing columns
# allsites <-
#   subset(
#     allsites,
#     select = c(
#       Site,
#       Date,
#       Datetime,
#       Water_Temp,
#       Depth,
#       Sal,
#       pH,
#       pH_rounded,
#       DO_mgl,
#       DO_pct,
#       Day,
#       Month,
#       Year,
#       Season,
#       Hour,
#       Average_DO_pct_hour,
#       SD_DO_pct_hour,
#       SE_DO_pct_hour
#     )
#   )
# 
# 
# 
# #Calculating the hourly minimum DO_pct for each hour and season
# ##minimum value of DO_pct by Hour and Season
# Minimum_DO_pct <-
#   with(allsites, aggregate(
#     list(DO_pct),
#     by = list(Site, Hour, Season),
#     FUN = function(x) {
#       Minimum_DO_pct = min(x, na.rm = TRUE)
#     }
#   ))
# 
# ##renaming columns
# Minimum_DO_pct <- do.call(data.frame, Minimum_DO_pct)
# colnames(Minimum_DO_pct) <- c("Site", 'Hour', "Season", 'Minimum_DO_pct_hour')
# 
# ##merging minimum dataframe to large dataframe
# allsites <- merge(allsites, Minimum_DO_pct, by = c("Hour", "Site", "Season"))
# 
# ##ordering large dataframe by datetime
# allsites <- allsites[order(allsites$Datetime),]
# 
# ##organizing columns
# allsites <-
#   subset(
#     allsites,
#     select = c(
#       Site,
#       Date,
#       Datetime,
#       Water_Temp,
#       Depth,
#       Sal,
#       pH,
#       pH_rounded,
#       DO_mgl,
#       DO_pct,
#       Day,
#       Month,
#       Year,
#       Season,
#       Hour,
#       Average_DO_pct_hour,
#       SD_DO_pct_hour,
#       SE_DO_pct_hour,
#       Minimum_DO_pct_hour
#     )
#   )
# 
# 
# 
# #Calculating the hourly maximum DO_pct for each hour and Season
# ##maximum value of DO_pct by Hour
# Maximum_DO_pct <-
#   with(allsites, aggregate(
#     list(DO_pct),
#     by = list(Site, Hour, Season),
#     FUN = function(x) {
#       Maximum_DO_pct = max(x, na.rm = TRUE)
#     }
#   ))
# 
# ##renaming columns
# Maximum_DO_pct <- do.call(data.frame, Maximum_DO_pct)
# colnames(Maximum_DO_pct) <- c("Site", 'Hour', "Season", 'Maximum_DO_pct_hour')
# 
# ##merging maximum dataframe to large dataframe
# allsites <- merge(allsites, Maximum_DO_pct, by = c("Hour", "Site", "Season"))
# 
# ##ordering large dataframe by datetime
# allsites <- allsites[order(allsites$Datetime),]
# 
# ##organizing columns
# allsites <-
#   subset(
#     allsites,
#     select = c(
#       Site,
#       Date,
#       Datetime,
#       Water_Temp,
#       Depth,
#       Sal,
#       pH,
#       pH_rounded,
#       DO_mgl,
#       DO_pct,
#       Day,
#       Month,
#       Year,
#       Season,
#       Hour,
#       Average_DO_pct_hour,
#       SD_DO_pct_hour,
#       SE_DO_pct_hour,
#       Minimum_DO_pct_hour,
#       Maximum_DO_pct_hour
#     )
#   )





#Hour only




#Calculating the hourly Average DO_pct for each hour
##average value of DO_pct by Hour
Average_DO_pct <-
  with(allsites, aggregate(
    list(DO_pct),
    by = list(Site, Hour),
    FUN = function(x) {
      Average_DO_pct = mean(x, na.rm = TRUE)
    }
  ))

##renaming columns
Average_DO_pct <- do.call(data.frame, Average_DO_pct)
colnames(Average_DO_pct) <- c("Site", 'Hour', 'Average_DO_pct_hour')

##merging average dataframe to large dataframe
allsites <- merge(allsites, Average_DO_pct, by = c("Hour", "Site"))

##ordering large dataframe by datetime
allsites <- allsites[order(allsites$Datetime),]

##organizing columns
allsites <-
  subset(
    allsites,
    select = c(
      Site,
      Date,
      Datetime,
      Water_Temp,
      Depth,
      Sal,
      pH,
      pH_rounded,
      DO_mgl,
      DO_pct,
      Day,
      Month,
      Year,
      Season,
      Hour,
      Average_DO_pct_hour
    )
  )


# #Calculating the hourly standard deviation of DO_pct for each hour 
# SD_DO <-
#   with(allsites, aggregate(
#     list(DO_pct),
#     by = list(Site, Hour),
#     FUN = function(x) {
#       SD_DO = (sd(x, na.rm = TRUE))
#     }
#   ))
# 
# ##renaming columns
# SD_DO <- do.call(data.frame, SD_DO)
# colnames(SD_DO) <- c("Site", 'Hour', 'SD_DO_pct_hour')
# 
# ##merging average dataframe to large dataframe
# allsites <- merge(allsites, SD_DO, by = c("Hour", "Site"))
# 
# ##ordering large dataframe by datetime
# allsites <- allsites[order(allsites$Datetime),]
# 
# ##organizing columns
# allsites <-
#   subset(
#     allsites,
#     select = c(
#       Site,
#       Date,
#       Datetime,
#       Water_Temp,
#       Depth,
#       Sal,
#       pH,
#       pH_rounded,
#       DO_mgl,
#       DO_pct,
#       Day,
#       Month,
#       Year,
#       Season,
#       Hour,
#       Average_DO_pct_hour,
#       SD_DO_pct_hour
#     )
#   )



#Calculating the hourly standard error of DO_pct for each hour 
SE_DO <-
  with(allsites, aggregate(
    list(DO_pct),
    by = list(Site, Hour),
    FUN = function(x) {
      SE_DO = (sd(x, na.rm = TRUE)) / sqrt(length(x))
    }
  ))

##renaming columns
SE_DO <- do.call(data.frame, SE_DO)
colnames(SE_DO) <- c("Site", 'Hour', 'SE_DO_pct_hour')

##merging average dataframe to large dataframe
allsites <- merge(allsites, SE_DO, by = c("Hour", "Site"))

##ordering large dataframe by datetime
allsites <- allsites[order(allsites$Datetime),]

##organizing columns
allsites <-
  subset(
    allsites,
    select = c(
      Site,
      Date,
      Datetime,
      Water_Temp,
      Depth,
      Sal,
      pH,
      pH_rounded,
      DO_mgl,
      DO_pct,
      Day,
      Month,
      Year,
      Season,
      Hour,
      Average_DO_pct_hour,
      # SD_DO_pct_hour,
      SE_DO_pct_hour
    )
  )



# #Calculating the hourly minimum DO_pct for each hour
# ##minimum value of DO_pct by Hour
# Minimum_DO_pct <-
#   with(allsites, aggregate(
#     list(DO_pct),
#     by = list(Site, Hour),
#     FUN = function(x) {
#       Minimum_DO_pct = min(x, na.rm = TRUE)
#     }
#   ))
# 
# ##renaming columns
# Minimum_DO_pct <- do.call(data.frame, Minimum_DO_pct)
# colnames(Minimum_DO_pct) <- c("Site", 'Hour', 'Minimum_DO_pct_hour')
# 
# ##merging minimum dataframe to large dataframe
# allsites <- merge(allsites, Minimum_DO_pct, by = c("Hour", "Site"))
# 
# ##ordering large dataframe by datetime
# allsites <- allsites[order(allsites$Datetime),]
# 
# ##organizing columns
# allsites <-
#   subset(
#     allsites,
#     select = c(
#       Site,
#       Date,
#       Datetime,
#       Water_Temp,
#       Depth,
#       Sal,
#       pH,
#       pH_rounded,
#       DO_mgl,
#       DO_pct,
#       Day,
#       Month,
#       Year,
#       Season,
#       Hour,
#       Average_DO_pct_hour,
#       SD_DO_pct_hour,
#       SE_DO_pct_hour,
#       Minimum_DO_pct_hour
#     )
#   )
# 
# 
# 
# #Calculating the hourly maximum DO_pct for each hour
# ##maximum value of DO_pct by Hour
# Maximum_DO_pct <-
#   with(allsites, aggregate(
#     list(DO_pct),
#     by = list(Site, Hour),
#     FUN = function(x) {
#       Maximum_DO_pct = max(x, na.rm = TRUE)
#     }
#   ))
# 
# ##renaming columns
# Maximum_DO_pct <- do.call(data.frame, Maximum_DO_pct)
# colnames(Maximum_DO_pct) <- c("Site", 'Hour', 'Maximum_DO_pct_hour')
# 
# ##merging maximum dataframe to large dataframe
# allsites <- merge(allsites, Maximum_DO_pct, by = c("Hour", "Site"))
# 
# ##ordering large dataframe by datetime
# allsites <- allsites[order(allsites$Datetime),]
# 
# ##organizing columns
# allsites <-
#   subset(
#     allsites,
#     select = c(
#       Site,
#       Date,
#       Datetime,
#       Water_Temp,
#       Depth,
#       Sal,
#       pH,
#       pH_rounded,
#       DO_mgl,
#       DO_pct,
#       Day,
#       Month,
#       Year,
#       Season,
#       Hour,
#       Average_DO_pct_hour,
#       SD_DO_pct_hour,
#       SE_DO_pct_hour,
#       Minimum_DO_pct_hour,
#       Maximum_DO_pct_hour
#     )
#   )




#Comparing RB & EOS Center
RB_EOS <- allsites %>%
  filter(Site == c("Richardson Bay", "EOS Center"))




#Hour and Season





# #Climate graph of the Average_DO_pct by Season with SD
# ggplot(data = RB_EOS, aes(x = Hour, y = Average_DO_pct_hour, color = Site)) +
#   geom_point(aes(shape = Site), position = position_dodge(0.2)) +
#   geom_line(position = position_dodge(0.5)) +
#   geom_errorbar(
#     aes(ymin = Average_DO_pct_hour - SD_DO_pct_hour, ymax = Average_DO_pct_hour + SD_DO_pct_hour),
#     width = 0,
#     position = position_dodge(0.2)
#   ) +
#   theme_bw(base_size = 15) +
#   theme(legend.position = "top") +
#   ggtitle("Comparison of Hourly DO (% saturation) Averages by Season") +
#   labs(subtitle = "2016-2022",
#        caption = "Data courtesy of NERR & CeNCOOS") +
#   xlab("Hour (PST)") +
#   ylab("Average_DO (% saturation)") +
#   facet_wrap( ~ factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")), nrow = 4)
# 
# 
# #Summary of results
# tapply(RB_EOS$Average_DO_pct_hour, RB_EOS$Site, summary)
# 
# 
# #Figure 8. Diel dissolved oxygen (DO) trends by season of average hourly-median values in percent 
# #saturation on the y-axis and hours in pacific standard time (PST) on the x-axis with standard 
# #deviation between an eelgrass dominated bay, Richardson Bay (blue), and a deep, mainstream water 
# #channel in Central Bay near the EOS Center (red) from the years 2016-2021. Long-term datasets 
# #sourced by the National Estuarine Research Reserve (NERR) for Richardson Bay and Central and 
# #Northern California Ocean Observing System (CeNCOOS) for the EOS Center.
# 
# 
# 
# #Climate graph of the Average_DO_pct by Season with SE
# ggplot(data = RB_EOS, aes(x = Hour, y = Average_DO_pct_hour, color = Site)) +
#   geom_point(aes(shape = Site), position = position_dodge(0.2)) +
#   geom_line(position = position_dodge(0.5)) +
#   geom_errorbar(
#     aes(ymin = Average_DO_pct_hour - SE_DO_pct_hour, ymax = Average_DO_pct_hour + SE_DO_pct_hour),
#     width = 0,
#     position = position_dodge(0.2)
#   ) +
#   theme_bw(base_size = 15) +
#   theme(legend.position = "top") +
#   ggtitle("Comparison of Hourly DO (% saturation) Averages by Season") +
#   labs(subtitle = "2016-2022",
#        caption = "Data courtesy of NERR & CeNCOOS") +
#   xlab("Hour (PST)") +
#   ylab("Average_DO (% saturation)") +
#   facet_wrap( ~ factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")), nrow = 4)
# 
# 
# #Summary of results
# tapply(RB_EOS$Average_DO_pct_hour, RB_EOS$Site, summary)
# 
# #Figure 9. Diel dissolved oxygen (DO) trends by season of average hourly-median values in percent 
# #saturation on the y-axis and hours in pacific standard time (PST) on the x-axis with standard 
# #error between an eelgrass dominated bay, Richardson Bay (blue), and a deep, mainstream water 
# #channel in Central Bay near the EOS Center (red) from the years 2016-2021. Long-term datasets 
# #sourced by the National Estuarine Research Reserve (NERR) for Richardson Bay and Central and 
# #Northern California Ocean Observing System (CeNCOOS) for the EOS Center.
# 
# 
# #Min-Max-Ave DO_pct Graph by Season
# ggplot(data = RB_EOS, aes(x = Hour, y = Average_DO_pct_hour, color = Site)) +
#   geom_line() +
#   geom_point() +
#   geom_ribbon(aes(ymax = Maximum_DO_pct_hour, ymin = Minimum_DO_pct_hour), alpha = .1) +
#   theme_bw() +
#   theme(legend.position = "top") +
#   ggtitle("Comparison of Hourly DO (% saturation) Average, Minimum, and Maximum by Season") +
#   labs(subtitle = "2016-2022",
#        caption = "Data courtesy of NERR & CeNCOOS") +
#   xlab("Hour (PST)") +
#   ylab("Average_DO (% saturation)") +
#   facet_wrap( ~ factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")), nrow = 4)
# 
# 
# #Summary of results
# ##average DO_pct
# tapply(RB_EOS$Average_DO_pct_hour, RB_EOS$Site, summary)
# 
# ##minimum DO_pct
# tapply(RB_EOS$Minimum_DO_pct_hour, RB_EOS$Site, summary)
# 
# ##maximum DO_pct
# tapply(RB_EOS$Maximum_DO_pct_hour, RB_EOS$Site, summary)
# 
# 
# 
# #Figure 10. Diel dissolved oxygen (DO) trends by season of minimum, average, and maximum 
# #hourly-median values in percent saturation on the y-axis and hours in pacific standard time 
# #(PST) on the x-axis between an eelgrass dominated bay, Richardson Bay (blue), and a deep, 
# #mainstream water channel in Central Bay near the EOS Center (red) from the years 2016-2022. 
# #Long-term datasets sourced by the National Estuarine Research Reserve (NERR) for Richardson 
# #Bay and Central and Northern California Ocean Observing System (CeNCOOS) for the EOS Center.







#Hour only





# #Climate graph of the Average_DO_pct with SD
# ggplot(data = RB_EOS, aes(x = Hour, y = Average_DO_pct_hour, color = Site)) +
#   geom_point(aes(shape = Site), position = position_dodge(0.2)) +
#   geom_line(position = position_dodge(0.5)) +
#   geom_errorbar(
#     aes(ymin = Average_DO_pct_hour - SD_DO_pct_hour, ymax = Average_DO_pct_hour + SD_DO_pct_hour),
#     width = 0,
#     position = position_dodge(0.2)
#   ) +
#   theme_bw(base_size = 15) +
#   theme(legend.position = "top") +
#   ggtitle("Comparison of Hourly DO (% saturation) Averages") +
#   labs(subtitle = "2016-2022",
#        caption = "Data courtesy of NERR & CeNCOOS") +
#   xlab("Hour (PST)") +
#   ylab("Average_DO (% saturation)")


# #Summary of results
# tapply(RB_EOS$Average_DO_pct_hour, RB_EOS$Site, summary)
# tapply(RB_EOS$SD_DO_pct_hour, RB_EOS$Site, summary)

#Figure 11. Diel dissolved oxygen (DO) trends of average hourly-median values in percent 
#saturation on the y-axis and hours in pacific standard time (PST) on the x-axis with standard 
#deviation between an eelgrass dominated bay, Richardson Bay (blue), and a deep, mainstream water 
#channel in Central Bay near the EOS Center (red) from the years 2016-2021. Long-term datasets 
#sourced by the National Estuarine Research Reserve (NERR) for Richardson Bay and Central and 
#Northern California Ocean Observing System (CeNCOOS) for the EOS Center.



#Climate graph of the Average_DO_pct with SE
stat_DO <- function(y) {
  return(data.frame(
    y = min(y) + .3,
    label = paste(
      "n =", format(length(y), big.mark = ","), "\n"
    )
  ))
}

DO <- ggplot(data = RB_EOS, aes(x = Hour, y = Average_DO_pct_hour, color = Site)) +
  geom_point(pch = 16,
             size = 0.1,
             alpha = 0.03,
             position = position_jitterdodge(
               jitter.height = 0.5,
               jitter.width = 0.5
             )) +
  # stat_summary(fun.data = stat_DO, geom = "text", hjust = .8, vjust = 1, size = 3.5) +
  geom_line(position = position_dodge(0.5)) +
  geom_errorbar(
    aes(ymin = Average_DO_pct_hour - SE_DO_pct_hour, ymax = Average_DO_pct_hour + SE_DO_pct_hour),
    width = .25,
    position = position_dodge(0.2)
  ) +
  theme_bw(base_size = 16) +
  theme(legend.position = "top",
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) +
  scale_color_discrete(name = "Stations") +
  # ggtitle("Comparison of Hourly DO (% saturation) Averages") +
  # labs(subtitle = "2016-2022",
  #      caption = "Data courtesy of NERR & CeNCOOS") +
  xlab("Hour (PST)") +
  ylab("Average_DO (% saturation)") 
DO

#Summary of results
tapply(RB_EOS$Average_DO_pct_hour, RB_EOS$Site, summary)
tapply(RB_EOS$SE_DO_pct_hour, RB_EOS$Site, summary)

#Summary of results
##RB only
RB <- RB_EOS %>%
  filter(Site == c("Richardson Bay"))

tapply(RB$Average_DO_pct_hour, RB$Hour, summary)
tapply(RB$SE_DO_pct_hour, RB$Hour, summary)

#Summary of results
##EOS only
EOS <- RB_EOS %>%
  filter(Site == c("EOS Center"))

tapply(EOS$Average_DO_pct_hour, EOS$Hour, summary)
tapply(EOS$SE_DO_pct_hour, EOS$Hour, summary)

#Figure 12. Diel dissolved oxygen (DO) trends of average hourly-median values in percent 
#saturation on the y-axis and hours in pacific standard time (PST) on the x-axis with standard 
#error between an eelgrass dominated bay, Richardson Bay (blue), and a deep, mainstream water 
#channel in Central Bay near the EOS Center (red) from the years 2016-2021. Long-term datasets 
#sourced by the National Estuarine Research Reserve (NERR) for Richardson Bay and Central and 
#Northern California Ocean Observing System (CeNCOOS) for the EOS Center.


# #Min-Max-Ave DO_pct Graph 
# ggplot(data = RB_EOS, aes(x = Hour, y = Average_DO_pct_hour, color = Site)) +
#   geom_line() +
#   geom_point() +
#   geom_ribbon(aes(ymax = Maximum_DO_pct_hour, ymin = Minimum_DO_pct_hour), alpha = .1) +
#   theme_bw() +
#   theme(legend.position = "top") +
#   ggtitle("Comparison of Hourly DO (% saturation) Average, Minimum, and Maximum") +
#   labs(subtitle = "2016-2022",
#        caption = "Data courtesy of NERR & CeNCOOS") +
#   xlab("Hour (PST)") +
#   ylab("Average_DO (% saturation)") +
#   facet_wrap( ~ Site, ncol = 2)
# 
# 
# #Summary of results
# ##average DO_pct
# tapply(RB_EOS$Average_DO_pct_hour, RB_EOS$Site, summary)
# 
# ##minimum DO_pct
# tapply(RB_EOS$Minimum_DO_pct_hour, RB_EOS$Site, summary)
# 
# ##maximum DO_pct
# tapply(RB_EOS$Maximum_DO_pct_hour, RB_EOS$Site, summary)



#Figure 13. Diel dissolved oxygen (DO) trends of minimum, average, and maximum 
#hourly-median values in percent saturation on the y-axis and hours in pacific standard time 
#(PST) on the x-axis between an eelgrass dominated bay, Richardson Bay (blue), and a deep, 
#mainstream water channel in Central Bay near the EOS Center (red) from the years 2016-2022. 
#Long-term datasets sourced by the National Estuarine Research Reserve (NERR) for Richardson 
#Bay and Central and Northern California Ocean Observing System (CeNCOOS) for the EOS Center.





#Salinity




#Calculating the hourly Average Salinity for each hour
##Average value of salinity by Hour
Average_Sal <-
  with(allsites, aggregate(
    list(Sal),
    by = list(Site, Hour),
    FUN = function(x) {
      Average_Sal = mean(x, na.rm = TRUE)
    }
  ))

##renaming columns
Average_Sal <- do.call(data.frame, Average_Sal)
colnames(Average_Sal) <- c("Site", 'Hour', 'Average_Sal_hour')

##merging Average dataframe to large dataframe
allsites <- merge(allsites, Average_Sal, by = c("Hour", "Site"))

##ordering large dataframe by datetime
allsites <- allsites[order(allsites$Datetime),]

##organizing columns
allsites <-
  subset(
    allsites,
    select = c(
      Site,
      Date,
      Datetime,
      Water_Temp,
      Depth,
      Sal,
      pH,
      pH_rounded,
      DO_mgl,
      DO_pct,
      Day,
      Month,
      Year,
      Season,
      Hour,
      Average_Sal_hour
    )
  )


# #Calculating the hourly standard deviation of salinity for each hour 
# SD_Sal <-
#   with(allsites, aggregate(
#     list(Sal),
#     by = list(Site, Hour),
#     FUN = function(x) {
#       SD_Sal = (sd(x, na.rm = TRUE))
#     }
#   ))
# 
# ##renaming columns
# SD_Sal <- do.call(data.frame, SD_Sal)
# colnames(SD_Sal) <- c("Site", 'Hour', 'SD_Sal_hour')
# 
# ##merging average dataframe to large dataframe
# allsites <- merge(allsites, SD_Sal, by = c("Hour", "Site"))
# 
# ##ordering large dataframe by datetime
# allsites <- allsites[order(allsites$Datetime),]
# 
# ##organizing columns
# allsites <-
#   subset(
#     allsites,
#     select = c(
#       Site,
#       Date,
#       Datetime,
#       Water_Temp,
#       Depth,
#       Sal,
#       pH,
#       pH_rounded,
#       DO_mgl,
#       DO_pct,
#       Day,
#       Month,
#       Year,
#       Season,
#       Hour,
#       Average_Sal_hour,
#       SD_Sal_hour
#     )
#   )



#Calculating the hourly standard error of salinity for each hour 
SE_Sal <-
  with(allsites, aggregate(
    list(Sal),
    by = list(Site, Hour),
    FUN = function(x) {
      SE_Sal = (sd(x, na.rm = TRUE)) / sqrt(length(x))
    }
  ))

##renaming columns
SE_Sal <- do.call(data.frame, SE_Sal)
colnames(SE_Sal) <- c("Site", 'Hour', 'SE_Sal_hour')

##merging average dataframe to large dataframe
allsites <- merge(allsites, SE_Sal, by = c("Hour", "Site"))

##ordering large dataframe by datetime
allsites <- allsites[order(allsites$Datetime),]

##organizing columns
allsites <-
  subset(
    allsites,
    select = c(
      Site,
      Date,
      Datetime,
      Water_Temp,
      Depth,
      Sal,
      pH,
      pH_rounded,
      DO_mgl,
      DO_pct,
      Day,
      Month,
      Year,
      Season,
      Hour,
      Average_Sal_hour,
      # SD_Sal_hour,
      SE_Sal_hour
    )
  )



#Comparing RB & EOS Center
RB_EOS <- allsites %>%
  filter(Site == c("Richardson Bay", "EOS Center"))


# #Climate graph of the Average_Sal with SD
# ggplot(data = RB_EOS, aes(x = Hour, y = Average_Sal_hour, color = Site)) +
#   geom_point(aes(shape = Site), position = position_dodge(0.2)) +
#   geom_line(position = position_dodge(0.5)) +
#   geom_errorbar(
#     aes(ymin = Average_Sal_hour - SD_Sal_hour, ymax = Average_Sal_hour + SD_Sal_hour),
#     width = 0,
#     position = position_dodge(0.2)
#   ) +
#   theme_bw(base_size = 15) +
#   theme(legend.position = "top") +
#   ggtitle("Comparison of Hourly Salinity Averages") +
#   labs(subtitle = "2016-2022",
#        caption = "Data courtesy of NERR & CeNCOOS") +
#   xlab("Hour (PST)") +
#   ylab("Average_Salinity (psu)") 


# #Summary of results
# tapply(RB_EOS$Average_Sal_hour, RB_EOS$Site, summary)
# tapply(RB_EOS$SD_Sal_hour, RB_EOS$Site, summary)


#Figure 14. Diel salinity trends of average hourly-median values in practical salinity units on the y-axis 
#and hours in pacific standard time (PST) on the x-axis with standard deviation between an eelgrass dominated bay, Richardson 
#Bay (blue), and a deep, mainstream water channel in Central Bay near the EOS Center (red) from the 
#years 2016-2022. Long-term datasets sourced by the National Estuarine Research Reserve (NERR) for 
#Richardson Bay and Central and Northern California Ocean Observing System (CeNCOOS) for the EOS 
#Center.






#Climate graph of the Average_Sal with SE
  stat_Sal <- function(y) {
    return(data.frame(y = min(y) + 2,
                      label = paste(
                        "n =", format(length(y), big.mark = ","), "\n"
                      )))
  }

Sal <- ggplot(data = RB_EOS, aes(x = Hour, y = Average_Sal_hour, color = Site)) +
  geom_line(position = position_dodge(0.5)) +
  geom_point(pch = 16,
             size = 0.1,
             alpha = 0.1,
             position = position_jitterdodge(
               jitter.height = 0.5,
               jitter.width = 0.5
             )) +
  # stat_summary(fun.data = stat_Sal, geom = "text", hjust = .8, vjust = 1, size = 3.5) +
  geom_errorbar(
    aes(ymin = Average_Sal_hour - SE_Sal_hour, ymax = Average_Sal_hour + SE_Sal_hour),
    width = .25,
    position = position_dodge(0.2)
  ) +
  theme_bw(base_size = 16) +
  theme(legend.position = "top",
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) +
  scale_color_discrete(name = "Stations") +
  # ggtitle("Comparison of Hourly Salinity Averages") +
  # labs(subtitle = "2016-2022",
  #      caption = "Data courtesy of NERR & CeNCOOS") +
  xlab("Hour (PST)") +
  ylab("Average_Salinity") 
Sal

#Summary of results
tapply(RB_EOS$Average_Sal_hour, RB_EOS$Site, summary)
tapply(RB_EOS$SE_Sal_hour, RB_EOS$Site, summary)

#Summary of results
##RB only
RB <- RB_EOS %>%
  filter(Site == c("Richardson Bay"))

tapply(RB$Average_Sal_hour, RB$Hour, summary)
tapply(RB$SE_Sal_hour, RB$Hour, summary)

#Summary of results
##EOS only
EOS <- RB_EOS %>%
  filter(Site == c("EOS Center"))

tapply(EOS$Average_Sal_hour, EOS$Hour, summary)
tapply(EOS$SE_Sal_hour, EOS$Hour, summary)


#Figure 15. Diel salinity trends of average hourly-median values in practical salinity units on the y-axis 
#and hours in pacific standard time (PST) on the x-axis with standard error between an eelgrass dominated bay, Richardson 
#Bay (blue), and a deep, mainstream water channel in Central Bay near the EOS Center (red) from the 
#years 2016-2022. Long-term datasets sourced by the National Estuarine Research Reserve (NERR) for 
#Richardson Bay and Central and Northern California Ocean Observing System (CeNCOOS) for the EOS 
#Center.




#Water_Temperature




#Calculating the hourly Average for water temperature for each hour
##Average value of water temp by Hour
Average_Temp <-
  with(allsites, aggregate(
    list(Water_Temp),
    by = list(Site, Hour),
    FUN = function(x) {
      Average_Temp = mean(x, na.rm = TRUE)
    }
  ))

##renaming columns
Average_Temp <- do.call(data.frame, Average_Temp)
colnames(Average_Temp) <- c("Site", 'Hour', 'Average_Temp_hour')

##merging Average dataframe to large dataframe
allsites <- merge(allsites, Average_Temp, by = c("Hour", "Site"))

##ordering large dataframe by datetime
allsites <- allsites[order(allsites$Datetime),]

##organizing columns
allsites <-
  subset(
    allsites,
    select = c(
      Site,
      Date,
      Datetime,
      Water_Temp,
      Depth,
      Sal,
      pH,
      pH_rounded,
      DO_mgl,
      DO_pct,
      Day,
      Month,
      Year,
      Season,
      Hour,
      Average_Temp_hour
    )
  )


# #Calculating the hourly standard deviation of water temperature for each hour 
# SD_Temp <-
#   with(allsites, aggregate(
#     list(Water_Temp),
#     by = list(Site, Hour),
#     FUN = function(x) {
#       SD_Temp = (sd(x, na.rm = TRUE))
#     }
#   ))
# 
# ##renaming columns
# SD_Temp <- do.call(data.frame, SD_Temp)
# colnames(SD_Temp) <- c("Site", 'Hour', 'SD_Temp_hour')
# 
# ##merging average dataframe to large dataframe
# allsites <- merge(allsites, SD_Temp, by = c("Hour", "Site"))
# 
# ##ordering large dataframe by datetime
# allsites <- allsites[order(allsites$Datetime),]
# 
# ##organizing columns
# allsites <-
#   subset(
#     allsites,
#     select = c(
#       Site,
#       Date,
#       Datetime,
#       Water_Temp,
#       Depth,
#       Sal,
#       pH,
#       pH_rounded,
#       DO_mgl,
#       DO_pct,
#       Day,
#       Month,
#       Year,
#       Season,
#       Hour,
#       Average_Temp_hour,
#       SD_Temp_hour
#     )
#   )



#Calculating the hourly standard error of water temperature for each hour 
SE_Temp <-
  with(allsites, aggregate(
    list(Water_Temp),
    by = list(Site, Hour),
    FUN = function(x) {
      SE_Temp = (sd(x, na.rm = TRUE)) / sqrt(length(x))
    }
  ))

##renaming columns
SE_Temp <- do.call(data.frame, SE_Temp)
colnames(SE_Temp) <- c("Site", 'Hour', 'SE_Temp_hour')

##merging average dataframe to large dataframe
allsites <- merge(allsites, SE_Temp, by = c("Hour", "Site"))

##ordering large dataframe by datetime
allsites <- allsites[order(allsites$Datetime),]

##organizing columns
allsites <-
  subset(
    allsites,
    select = c(
      Site,
      Date,
      Datetime,
      Water_Temp,
      Depth,
      Sal,
      pH,
      pH_rounded,
      DO_mgl,
      DO_pct,
      Day,
      Month,
      Year,
      Season,
      Hour,
      Average_Temp_hour,
      # SD_Temp_hour,
      SE_Temp_hour
    )
  )

#Comparing RB & EOS Center
RB_EOS <- allsites %>%
  filter(Site == c("Richardson Bay", "EOS Center"))


# #Climate graph of the Average_Temp with SD
# ggplot(data = RB_EOS, aes(x = Hour, y = Average_Temp_hour, color = Site)) +
#   geom_point(aes(shape = Site), position = position_dodge(0.2)) +
#   geom_line(position = position_dodge(0.5)) +
#   geom_errorbar(
#     aes(ymin = Average_Temp_hour - SD_Temp_hour, ymax = Average_Temp_hour + SD_Temp_hour),
#     width = 0,
#     position = position_dodge(0.2)
#   ) +
#   theme_bw(base_size = 15) +
#   theme(legend.position = "top") +
#   ggtitle("Comparison of Hourly Water Temperature Averages") +
#   labs(subtitle = "2016-2022",
#        caption = "Data courtesy of NERR & CeNCOOS") +
#   xlab("Hour (PST)") +
#   ylab("Average_Water Temperature (C)") 
# 
# 
# #Summary of results
# tapply(RB_EOS$Average_Temp_hour, RB_EOS$Site, summary)
# tapply(RB_EOS$SD_Temp_hour, RB_EOS$Site, summary)


#Figure 16. Diel water temperature trends of average hourly-median values in degrees celsius on the 
#y-axis and hours in pacific standard time (PST) on the x-axis with standard deviation between an eelgrass dominated bay, 
#Richardson Bay (blue), and a deep, mainstream water channel in Central Bay near the EOS Center 
#(red) from the years 2016-2022. Long-term datasets sourced by the National Estuarine Research 
#Reserve (NERR) for Richardson Bay and Central and Northern California Ocean Observing System 
#(CeNCOOS) for the EOS Center.


#Climate graph of the Average_Temp with SE
Temp <- ggplot(data = RB_EOS, aes(x = Hour, y = Average_Temp_hour, color = Site)) +
  geom_line(position = position_dodge(0.5)) +
  geom_point(pch = 16,
             size = 0.1,
             alpha = 0.1,
             position = position_jitterdodge(
               jitter.height = 0.5,
               jitter.width = 0.5
             )) +
  geom_errorbar(
    aes(ymin = Average_Temp_hour - SE_Temp_hour, ymax = Average_Temp_hour + SE_Temp_hour),
    width = .25,
    position = position_dodge(0.2)
  ) +
  theme_bw(base_size = 16) +
  theme(legend.position = "top",
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) +
  scale_color_discrete(name = "Stations") +
  # ggtitle("Comparison of Hourly Water Temperature Averages") +
  # labs(subtitle = "2016-2022",
  #      caption = "Data courtesy of NERR & CeNCOOS") +
  xlab("Hour (PST)") +
  ylab("Average_Water Temperature (C)") 
Temp

#Summary of results
tapply(RB_EOS$Average_Temp_hour, RB_EOS$Site, summary)
tapply(RB_EOS$SE_Temp_hour, RB_EOS$Site, summary)

#Summary of results
##RB only
RB <- RB_EOS %>%
  filter(Site == c("Richardson Bay"))

tapply(RB$Average_Temp_hour, RB$Hour, summary)
tapply(RB$SE_Temp_hour, RB$Hour, summary)

#Summary of results
##EOS only
EOS <- RB_EOS %>%
  filter(Site == c("EOS Center"))

tapply(EOS$Average_Temp_hour, EOS$Hour, summary)
tapply(EOS$SE_Temp_hour, EOS$Hour, summary)

#Figure 17. Diel water temperature trends of average hourly-median values in degrees celsius on the 
#y-axis and hours in pacific standard time (PST) on the x-axis with standard error between an eelgrass dominated bay, 
#Richardson Bay (blue), and a deep, mainstream water channel in Central Bay near the EOS Center 
#(red) from the years 2016-2022. Long-term datasets sourced by the National Estuarine Research 
#Reserve (NERR) for Richardson Bay and Central and Northern California Ocean Observing System 
#(CeNCOOS) for the EOS Center.





# #Depth-this is sonde water depth 
# 
# 
# 
# 
# #Calculating the hourly Average for Depth for each hour
# ##Average value of depth by Hour
# Average_Depth <-
#   with(allsites, aggregate(
#     list(Depth),
#     by = list(Site, Hour),
#     FUN = function(x) {
#       Average_Depth = mean(x, na.rm = TRUE)
#     }
#   ))
# 
# ##renaming columns
# Average_Depth <- do.call(data.frame, Average_Depth)
# colnames(Average_Depth) <- c("Site", 'Hour', 'Average_Depth_hour')
# 
# ##merging Average dataframe to large dataframe
# allsites <- merge(allsites, Average_Depth, by = c("Hour", "Site"))
# 
# ##ordering large dataframe by datetime
# allsites <- allsites[order(allsites$Datetime),]
# 
# ##organizing columns
# allsites <-
#   subset(
#     allsites,
#     select = c(
#       Site,
#       Date,
#       Datetime,
#       Water_Temp,
#       Depth,
#       Sal,
#       pH,
#       pH_rounded,
#       DO_mgl,
#       DO_pct,
#       Day,
#       Month,
#       Year,
#       Season,
#       Hour,
#       Average_Depth_hour
#     )
#   )
# 
# 
# #Calculating the hourly standard deviation of water depth for each hour 
# SD_Depth <-
#   with(allsites, aggregate(
#     list(Depth),
#     by = list(Site, Hour),
#     FUN = function(x) {
#       SD_Depth = (sd(x, na.rm = TRUE))
#     }
#   ))
# 
# ##renaming columns
# SD_Depth <- do.call(data.frame, SD_Depth)
# colnames(SD_Depth) <- c("Site", 'Hour', 'SD_Depth_hour')
# 
# ##merging average dataframe to large dataframe
# allsites <- merge(allsites, SD_Depth, by = c("Hour", "Site"))
# 
# ##ordering large dataframe by datetime
# allsites <- allsites[order(allsites$Datetime),]
# 
# ##organizing columns
# allsites <-
#   subset(
#     allsites,
#     select = c(
#       Site,
#       Date,
#       Datetime,
#       Water_Temp,
#       Depth,
#       Sal,
#       pH,
#       pH_rounded,
#       DO_mgl,
#       DO_pct,
#       Day,
#       Month,
#       Year,
#       Season,
#       Hour,
#       Average_Depth_hour,
#       SD_Depth_hour
#     )
#   )
# 
# 
# 
# #Calculating the hourly standard error of water depth for each hour 
# SE_Depth <-
#   with(allsites, aggregate(
#     list(Depth),
#     by = list(Site, Hour),
#     FUN = function(x) {
#       SE_Depth = (sd(x, na.rm = TRUE)) / sqrt(length(x))
#     }
#   ))
# 
# ##renaming columns
# SE_Depth <- do.call(data.frame, SE_Depth)
# colnames(SE_Depth) <- c("Site", 'Hour', 'SE_Depth_hour')
# 
# ##merging average dataframe to large dataframe
# allsites <- merge(allsites, SE_Depth, by = c("Hour", "Site"))
# 
# ##ordering large dataframe by datetime
# allsites <- allsites[order(allsites$Datetime),]
# 
# ##organizing columns
# allsites <-
#   subset(
#     allsites,
#     select = c(
#       Site,
#       Date,
#       Datetime,
#       Water_Temp,
#       Depth,
#       Sal,
#       pH,
#       pH_rounded,
#       DO_mgl,
#       DO_pct,
#       Day,
#       Month,
#       Year,
#       Season,
#       Hour,
#       Average_Depth_hour,
#       SD_Depth_hour,
#       SE_Depth_hour
#     )
#   )
# 
# 
# 
# #Comparing RB & EOS Center
# RB_EOS <- allsites %>%
#   filter(Site == c("Richardson Bay", "EOS Center"))
# 
# 
# #Climate graph of the Average_Depth with SD
# ggplot(data = RB_EOS, aes(x = Hour, y = Average_Depth_hour, color = Site)) +
#   geom_point(aes(shape = Site), position = position_dodge(0.2)) +
#   geom_line(position = position_dodge(0.5)) +
#   geom_errorbar(
#     aes(ymin = Average_Depth_hour - SD_Depth_hour, ymax = Average_Depth_hour + SD_Depth_hour),
#     width = 0,
#     position = position_dodge(0.2)
#   ) +
#   theme_bw(base_size = 15) +
#   theme(legend.position = "top") +
#   ggtitle("Comparison of Hourly Water Depth Averages") +
#   labs(subtitle = "2016-2022",
#        caption = "Data courtesy of NERR & CeNCOOS") +
#   xlab("Hour (PST)") +
#   ylab("Average_Water Depth (m)") 
# 
# 
# #Summary of results
# tapply(RB_EOS$Average_Depth_hour, RB_EOS$Site, summary)
# tapply(RB_EOS$SD_Depth_hour, RB_EOS$Site, summary)
# 
# 
# #Figure 18. Diel water depth trends of average hourly-median values in meters on the y-axis and hours 
# #in pacific standard time (PST) on the x-axis with standard deviation between an eelgrass dominated bay, Richardson Bay 
# #(blue), and a deep, mainstream water channel in Central Bay near the EOS Center (red) from the 
# #years 2016-2022. Long-term datasets sourced by the National Estuarine Research Reserve (NERR) for 
# #Richardson Bay and Central and Northern California Ocean Observing System (CeNCOOS) for the EOS 
# #Center.
# 
# 
# #Climate graph of the Average_Depth with SE
# ggplot(data = RB_EOS, aes(x = Hour, y = Average_Depth_hour, color = Site)) +
#   geom_point(aes(shape = Site), position = position_dodge(0.2)) +
#   geom_line(position = position_dodge(0.5)) +
#   geom_errorbar(
#     aes(ymin = Average_Depth_hour - SE_Depth_hour, ymax = Average_Depth_hour + SE_Depth_hour),
#     width = 0,
#     position = position_dodge(0.2)
#   ) +
#   theme_bw(base_size = 15) +
#   theme(legend.position = "top") +
#   ggtitle("Comparison of Hourly Water Depth Averages") +
#   labs(subtitle = "2016-2022",
#        caption = "Data courtesy of NERR & CeNCOOS") +
#   xlab("Hour (PST)") +
#   ylab("Average_Water Depth (m)") 
# 
# 
# #Summary of results
# tapply(RB_EOS$Average_Depth_hour, RB_EOS$Site, summary)
# tapply(RB_EOS$SE_Depth_hour, RB_EOS$Site, summary)


#Figure 19. Diel water depth trends of average hourly-median values in meters on the y-axis and hours 
#in pacific standard time (PST) on the x-axis with standard error between an eelgrass dominated bay, Richardson Bay 
#(blue), and a deep, mainstream water channel in Central Bay near the EOS Center (red) from the 
#years 2016-2022. Long-term datasets sourced by the National Estuarine Research Reserve (NERR) for 
#Richardson Bay and Central and Northern California Ocean Observing System (CeNCOOS) for the EOS 
#Center.


#plotting graphs on top of each other
all <- ggarrange(pH, DO, Sal, Temp, ncol = 2, nrow = 2, common.legend = TRUE, legend="top")

annotate_figure(
  all,
  top = text_grob("Diurnal Trends in Water Quality between Richardson Bay and the EOS Center \n 2016-2022", size = 12),
  bottom = text_grob("Data courtesy of NERR & CeNCOOS", size = 8)
)

