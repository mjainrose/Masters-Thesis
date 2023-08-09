---title:"Is there a correlation between pH and dissolved oxygen between Richardson Bay and the Estuary and Ocean Science (EOS) Center?"
author:"MJ"
date:"2/24/2022"

  
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




## Correlation of pH and DO (% Saturation)


#### Filtering out RB and EOS

#Filtering for RB&EOS only
RB_EOS <- all_sites %>%
  filter(Site == c("Richardson Bay", "EOS Center"))

RB_EOS <- RB_EOS[order(RB_EOS$Site),]



# ### By Year
# 
# 
# #Scatter plot of 2016-2022 data looking at pH ~DO_pct by year
# ggplot(RB_EOS, aes(x = DO_pct, y = pH_rounded, color = Site)) +
#   geom_point(aes(fill = Site),
#              color = "transparent",
#              shape = 24,
#              alpha = 1) +
#   geom_point(
#     aes(fill = Site),
#     data = subset(RB_EOS, Site == 'EOS Center'),
#     shape = 21,
#     color = "transparent",
#     alpha = 1
#   ) +
#   geom_smooth(aes(color = Site), method = lm, size = 1.5) +
#   geom_smooth(
#     aes(color = Site),
#     data = subset(RB_EOS, Site == "EOS Center"),
#     method = lm,
#     size = 2
#   ) +
#   stat_cor(
#     aes(label = ..rr.label..),
#     method = "pearson",
#     label.x.npc = .70,
#     label.y.npc = .60,
#     digits = 2,
#     hjust = 0,
#     size = 5
#   ) +
#   scale_colour_manual(values = c("#D55E00", "blue")) +
#   scale_fill_manual(values = c("#F8766D", "#00BFC4")) +
#   theme_bw(base_size = 13) +
#   theme(legend.position = "bottom") +
#   xlab("DO (% saturation)") +
#   ylab("pH") +
#   ggtitle("Correlation of pH and Dissolved Oxygen (% saturation), by Year") +
#   labs(subtitle = "2016-2022",
#        caption = "Data courtesy of NERR & CeNCOOS") +
#   facet_wrap( ~ Year)


#Figure 1. Correlation graph of pH values rounded to the tenths and dissolved oxygen (DO) values of
#percent saturation between an eelgrass dominated bay, Richardson Bay (blue), and a deep, mainstream
#water channel in Central Bay near the EOS Center (red) for each year from 2016-2022. Long-term
#datasets sourced by the National Estuarine Research Reserve (NERR) for Richardson Bay and Central
#and Northern California Ocean Observing System (CeNCOOS) for the EOS Center.




#By Season




#Scatter plot of 2016-2022 data looking at pH ~DO_pct by season
ggplot(RB_EOS, aes(x = DO_pct, y = pH_rounded, color = Site)) +
  geom_point(aes(fill = Site),
             color = "transparent",
             shape = 24,
             alpha = 1) +
  geom_point(
    aes(fill = Site),
    data = subset(RB_EOS, Site == 'EOS Center'),
    shape = 21,
    color = "transparent",
    alpha = 1
  ) +
  geom_smooth(aes(color = Site), method = lm, size = 1) +
  geom_smooth(
    aes(color = Site),
    data = subset(RB_EOS, Site == "EOS Center"),
    method = lm,
    size = 1
  ) +
  stat_cor(
    aes(label = ..rr.label..),
    method = "pearson",
    label.x.npc = .65,
    label.y.npc = .45,
    digits = 2,
    hjust = 0,
    size = 7
  ) +
  scale_colour_manual(values = c("#D55E00", "blue"),
                      name = "Stations") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4"),
                    name = "Stations") +
  theme_bw(base_size = 16) +
  theme(legend.position = "top",
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) +
  xlab("DO (% saturation)") +
  ylab("pH") +
  ggtitle("Correlation of pH and Dissolved Oxygen (% saturation), Seasonally") +
  labs(subtitle = "2016-2022",
       caption = "Data courtesy of NERR & CeNCOOS") +
  facet_wrap( ~ factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")), nrow = 2, ncol = 2)


#Summary of results

model <- lm(pH_rounded ~ DO_pct, data = RB_EOS)
summary(model)

##RB only
RB <- RB_EOS %>%
  filter(Site == c("Richardson Bay"))

tapply(RB$pH_rounded, RB$Season, summary)
tapply(RB$DO_pct, RB$Season, summary)

##EOS only
EOS <- RB_EOS %>%
  filter(Site == c("EOS Center"))

tapply(EOS$pH_rounded, EOS$Season, summary)
tapply(EOS$DO_pct, EOS$Season, summary)



#Figure 2. A Pearson correlation of pH values rounded to the tenths and dissolved oxygen (DO) values of 
#percent saturation between an eelgrass dominated bay, Richardson Bay (blue), and a deep, mainstream 
#water channel in Central Bay near the EOS Center (red) from 2016-2022, by season. Long-term datasets 
#sourced by the National Estuarine Research Reserve (NERR) for Richardson Bay and Central and Northern 
#California Ocean Observing System (CeNCOOS) for the EOS Center.



#### Filtering out GC and CC

#Filtering for RB&EOS only
GC_CC <- all_sites %>%
  filter(Site == c("Gallinas Creek", "China Camp"))

GC_CC <- GC_CC[order(GC_CC$Site),]



#By Season




#Scatter plot of 2016-2022 data looking at pH ~DO_pct by season
ggplot(GC_CC, aes(x = DO_pct, y = pH_rounded, color = Site)) +
  geom_point(aes(fill = Site),
             color = "transparent",
             shape = 24,
             alpha = 1) +
  geom_point(
    aes(fill = Site),
    data = subset(GC_CC, Site == 'Gallinas Creek'),
    shape = 21,
    color = "transparent",
    alpha = 1
  ) +
  geom_smooth(aes(color = Site), method = lm, size = 1) +
  geom_smooth(
    aes(color = Site),
    data = subset(GC_CC, Site == "China Camp"),
    method = lm,
    size = 1
  ) +
  stat_cor(
    aes(label = ..rr.label..),
    method = "pearson",
    label.x.npc = .65,
    label.y.npc = .45,
    digits = 2,
    hjust = 0,
    size = 7
  ) +
  scale_colour_manual(values = c("purple", "yellow"),
                      name = "Stations") +
  scale_fill_manual(values = c("#C77CFF", "#E7BE08"),
                    name = "Stations") +
  theme_bw(base_size = 16) +
  theme(legend.position = "top",
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) +
  xlab("DO (% saturation)") +
  ylab("pH") +
  ggtitle("Correlation of pH and Dissolved Oxygen (% saturation), Seasonally") +
  labs(subtitle = "2016-2022",
       caption = "Data courtesy of NERR & CeNCOOS") +
  facet_wrap( ~ factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")), nrow = 2, ncol = 2)




