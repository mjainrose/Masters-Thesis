###Tidal trends in pH and TA between RB and EOS###
###Tidal Trends in carbonate saturation states between all four sites###
  


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



#Uploading Tidal processed data for All sites
RB_tidal <- read.csv("RB_carb_tidal.csv", header = TRUE)

EOS_tidal <- read.csv("EOS_carb_tidal.csv", header = TRUE)

GC_tidal <- read.csv("GC_carb_tidal.csv", header = TRUE)

CC_tidal <- read.csv("CC_carb_tidal.csv", header = TRUE)


#Merging all datasets
RB_EOS <- rbind(RB_tidal, EOS_tidal)

RB_EOS_GC <- rbind(RB_EOS, GC_tidal)

All_tidal <- rbind(RB_EOS_GC, CC_tidal)


#Converting datetime
##to UTC
RB_EOS$DateTime.Sampled <-
  as.POSIXct(RB_EOS$DateTime.Sampled, format = c("%m/%d/%y %H:%M"))
All_tidal$DateTime.Sampled <-
  as.POSIXct(All_tidal$DateTime.Sampled, format = c("%m/%d/%y %H:%M"))


#Creating just a time column
RB_EOS$Time <-
  format(as.POSIXct(RB_EOS$DateTime.Sampled, format = "%m/%d/%y %H:%M"),
         "%H:%M")
All_tidal$Time <-
  format(as.POSIXct(All_tidal$DateTime.Sampled, format = "%m/%d/%y %H:%M"),
         "%H:%M")


#Ordering new dataframe by datetime
RB_EOS <- RB_EOS[order(RB_EOS$DateTime.Sampled),]
All_tidal <- All_tidal[order(All_tidal$DateTime.Sampled),]

#Reordering sites for plots
All_tidal$Site <- factor(All_tidal$Site,
                         levels = c("China Camp", "Gallinas Creek", "Richardson Bay", "EOS Center"))



#pH



# #Creating pH Time series plots both sites trying to add geom area for tidal data
# ggplot(data = RB_EOS,
#        aes(
#          x = Time,
#          y = pH_corrected_average,
#          color = Site,
#          group = Site,
#          fill = Site
#        )) +
#   geom_line() +
#   geom_area(aes(y = 7.55 + Tidal_Depth_ft * 0.075),
#             linetype = 2) +
#   # scale_y_continuous(
#   #   name = "pH",
#   #   limits = c(7.55, 7.85),
#   #   breaks = seq(7.55, 7.85, 0.05),
#   #   sec.axis = sec_axis( ~ (. - 7.55) / 0.075,
#   #                        name = "Tidal Height (ft)",
#   #                        breaks = seq(0, 4, 1))
#   # ) +
#   coord_cartesian() +
#   geom_point() +
#   #geom_point(aes(y = 7.55 + Tidal_Depth_ft*0.075)) +
#   facet_wrap(~ Week, nrow = 3) +
#   theme_bw(base_size = 13) +
#   theme(legend.position = "top") +
#   ggtitle("pH Tidal Trends") +
#   labs(subtitle = "Summer 2022") +
#   xlab("Time (PST)") +
#   ylab("pH") +
#   scale_x_discrete(guide = guide_axis(angle = 90))


# #Creating pH Time series plots both sites
# pH <- ggplot(data = RB_EOS,
#        aes(
#          x = Time,
#          color = Site,
#          group = Site
#        )) +
#   geom_line(aes(y = pH_corrected_average)) +
#   geom_line(aes(y = 7.55 + Tidal_Depth_ft * 0.075), 
#             linetype = 2) +
#   scale_y_continuous(
#     name = "pH",
#     limits = c(7.55, 7.85),
#     breaks = seq(7.55, 7.85, 0.05),
#     sec.axis = sec_axis( ~ (. - 7.55) / 0.075,
#                          name = "Tidal Height (ft)",
#                          breaks = seq(0, 4, 1))
#   ) +
#   geom_point(aes(y = pH_corrected_average)) +
#   geom_vline(data = filter(RB, Week == "1"),
#              aes(xintercept = 13.31),
#              color = "#00BFC4") +
#   geom_vline(data = filter(RB, Week == "2"),
#              aes(xintercept = 9.88)) +
#   geom_vline(data = filter(RB, Week == "3"),
#              aes(xintercept = 5.12)) +
#   #geom_point(aes(y = 7.55 + Tidal_Depth_ft*0.075)) +
#   facet_wrap(~ Week, nrow = 3) +
#   theme_bw(base_size = 13) +
#   theme(legend.position = "top") +
#   ggtitle("pH Tidal Trends") +
#   labs(subtitle = "Summer 2022") +
#   xlab("Time (PST)") +
#   ylab("pH") +
#   scale_x_discrete(guide = guide_axis(angle = 90))
# 
# pH
# 
# 
# #Figure 1. Tidal trends of measured pH values on the primary y-axis (bold-line) and tidal height in
# #feet on the secondary y-axis (dashed line) with hours in pacific standard time (PST) on the x-axis.
# #Comparison of an eelgrass dominated bay in Richardson Bay (blue) and a deep, mainstream
# #water channel in Central Bay near the EOS Center (red) from the sampling period in Summer
# #2022. Low tide indicated by the black vertical lines.
# 
# 
# #Filtering out RB
# RB <- RB_EOS %>%
#   filter(Site == c("Richardson Bay"))
# 
# #Creating pH Time series RB
# RB_week.labs <- c("06/09/2022", "06/21/2022", "07/05/2022")
# names(RB_week.labs) <- c("1", "2", "3")
# 
# RB_pH_plot <- ggplot(data = RB,
#                      aes(
#                        x = Time,
#                        y = pH_corrected_average,
#                        color = Site,
#                        group = Site
#                      )) +
#   geom_line() +
#   geom_line(aes(y = 7.55 + Tidal_Depth_ft * 0.075),
#             linetype = "dotdash") +
#   scale_y_continuous(
#     name = "pH",
#     limits = c(7.55, 7.85),
#     breaks = seq(7.55, 7.85, 0.05)
#   ) +
#   #   sec.axis = sec_axis(~ (. - 7.55) / 0.075,
#   #                       name = "Tidal Height (ft)",
#   #                       breaks = seq(0, 4, 1))
#   # ) +
#   geom_point() +
#   #geom_point(aes(y = 7.55 + Tidal_Depth_ft*0.075)) +
#   scale_color_manual(values = c(
#     "EOS Center" = "#F8766D",
#     "Richardson Bay" = "#00BFC4"
#   )) +
#   geom_vline(data = filter(RB, Week == "1"),
#              aes(xintercept = 13.31),
#              color = "black") +
#   geom_vline(data = filter(RB, Week == "2"),
#              aes(xintercept = 9.88),
#              color = "black") +
#   geom_vline(data = filter(RB, Week == "3"),
#              aes(xintercept = 5.12),
#              color = "black") +
#   facet_wrap(~ Week, nrow = 3, labeller = labeller(Week = RB_week.labs)) +
#   theme_bw(base_size = 12) +
#   theme(legend.position = "top") +
#   #ggtitle("pH Tidal Trends") +
#   #labs(subtitle = "Summer 2022") +
#   xlab("Time (PST)") +
#   ylab("pH") +
#   scale_x_discrete(guide = guide_axis(angle = 90))
# RB_pH_plot
# 
# 
# #Filtering out EOS
# EOS <- RB_EOS %>%
#   filter(Site == c("EOS Center"))
# 
# #Creating pH Time series EOS
# EOS_week.labs <- c("06/06/2022", "06/23/2022", "07/07/2022")
# names(EOS_week.labs) <- c("1", "2", "3")
# 
# EOS_pH_plot <- ggplot(data = EOS,
#                       aes(
#                         x = Time,
#                         y = pH_corrected_average,
#                         color = Site,
#                         group = Site
#                       )) +
#   geom_line() +
#   geom_line(aes(y = 7.55 + Tidal_Depth_ft * 0.075),
#             linetype = "dotdash") +
#   scale_y_continuous(
#     name = "pH",
#     limits = c(7.55, 7.85),
#     breaks = seq(7.55, 7.85, 0.05),
#     sec.axis = sec_axis(~ (. - 7.55) / 0.075,
#                         name = "Tidal Height (ft)",
#                         breaks = seq(0, 4, 1))
#   ) +
#   geom_point() +
#   #geom_point(aes(y = 7.55 + Tidal_Depth_ft*0.075)) +
#   scale_color_manual(values = c(
#     "EOS Center" = "#F8766D",
#     "Richardson Bay" = "#00BFC4"
#   )) +
#   geom_vline(data = filter(EOS, Week == "1"),
#              aes(xintercept = 7.11),
#              color = "black") +
#   geom_vline(data = filter(EOS, Week == "2"),
#              aes(xintercept = 13.39),
#              color = "black") +
#   geom_vline(data = filter(EOS, Week == "3"),
#              aes(xintercept = 7.87),
#              color = "black") +
#   facet_wrap( ~ Week, nrow = 3, labeller = labeller(Week = EOS_week.labs)) +
#   theme_bw(base_size = 12) +
#   theme(legend.position = "top") +
#   #ggtitle("pH Tidal Trends") +
#   #labs(subtitle = "Summer 2022") +
#   xlab("Time (PST)") +
#   ylab("pH") +
#   scale_x_discrete(guide = guide_axis(angle = 90))
# EOS_pH_plot


#Creating pH time series plots RB and EOS sites with separate grids
all_week.labs <- c("June 06-09", "June 20-23", "July 04-07")
names(all_week.labs) <- c("1", "2", "3")

pH_RB_EOS <- ggplot(data = RB_EOS,
       aes(
         x = Time,
         y = pH_corrected_average,
         color = Site,
         group = Site
       )) +
  scale_color_manual(
    values = c(
      "EOS Center" = "#F8766D",
      "Richardson Bay" = "#00BFC4",
      "China Camp" = "#E7BE08",
      "Gallinas Creek" = "#C77CFF"
    ),
    name = "Stations"
  ) +
  geom_line() +
  geom_line(aes(y = 7.55 + Tidal_Depth_ft * 0.075),
            linetype = "dotdash") +
  scale_y_continuous(
    name = "pH",
    limits = c(7.55, 7.85),
    breaks = seq(7.55, 7.85, 0.1),
    sec.axis = sec_axis(~ (. - 7.55) / 0.075,
                        name = "Tidal Height (ft)",
                        breaks = seq(0, 4, 1))
  ) +
  geom_point() +
  geom_vline(data = filter(All_tidal, Site == "EOS Center", Week == "1"),
             aes(xintercept = 7.11),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "EOS Center", Week == "2"),
             aes(xintercept = 13.39),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "EOS Center", Week == "3"),
             aes(xintercept = 7.87),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "Richardson Bay", Week == "1"),
             aes(xintercept = 13.31),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "Richardson Bay", Week == "2"),
             aes(xintercept = 9.88),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "Richardson Bay", Week == "3"),
             aes(xintercept = 5.12),
             color = "black") +
  facet_grid(Week ~ Site, labeller = labeller(Week = all_week.labs), scales = "free") +
  theme_bw(base_size = 14) +
  theme(legend.position = "top", 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14)) +
  # ggtitle("Aragonite Saturation State Tidal Trends") +
  # labs(subtitle = "Summer 2022") +
  # xlab("Time (PST)") +
  ylab("pH")
  # scale_x_discrete(guide = guide_axis(angle = 90))

pH_RB_EOS

#Summary of results
##RB only
tapply(RB_tidal$pH_corrected_average, RB_tidal$Week, summary)
tapply(RB_tidal$pH_corrected_average, RB_tidal$DateTime.Sampled, summary)

##EOS Center only
tapply(EOS_tidal$pH_corrected_average, EOS_tidal$Week, summary)
tapply(EOS_tidal$pH_corrected_average, EOS_tidal$DateTime.Sampled, summary)


#Figure 1. Tidal trends of measured pH values on the primary y-axis (bold line) and tidal height in
#feet on the secondary y-axis (dashed line) with hours in pacific standard time (PST) on the x-axis.
#Comparison of an eelgrass dominated bay in Richardson Bay (blue) and a deep, mainstream
#water channel in Central Bay near the EOS Center (red) from the sampling period in Summer
#2022. Low tide indicated by the black vertical lines.



#TA



# #Creating TA Time series plots both sites
# 
# ggplot(data = RB_EOS,
#        aes(
#          x = Time,
#          y = TAlk_measured_average,
#          color = Site,
#          group = Site
#        )) +
#   geom_line() +
#   geom_line(aes(y = 2100 + Tidal_Depth_ft * 75),
#             linetype = "dotdash") +
#   scale_y_continuous(
#     name = "Total Alkalinity (umol/kg)",
#     limits = c(2100, 2400),
#     breaks = seq(2100, 2380, 50),
#     sec.axis = sec_axis( ~ (. - 2100) / 75,
#                          name = "Tidal Height (ft)",
#                          breaks = seq(0, 4, 1))
#   ) +
#   geom_point() +
#   #geom_point(aes(y = 2100 + Tidal_Depth_ft*75)) +
#   facet_wrap(~ Week, nrow = 3) +
#   theme_bw(base_size = 13) +
#   theme(legend.position = "top") +
#   ggtitle("Total Alkalinity Tidal Trends") +
#   labs(subtitle = "Summer 2022") +
#   xlab("Time (PST)") +
#   ylab("Total Alkalinity (umol/kg)") +
#   scale_x_discrete(guide = guide_axis(angle = 90))
# 
# #Figure 2. Tidal trends of measured total alkalinity values in umol/kg on the
# #primary y-axis (bold line) and tidal height in feet on the secondary y-axis (dashed line) with
# #hours in pacific standard time (PST) on the x-axis. Comparison of an eelgrass dominated bay in
# #Richardson Bay (blue) and a deep, mainstream channel in Central Bay near the EOS Center
# #(red) from the sampling period in Summer 2022. Low tide indicated by the black vertical lines.
# 
# 
# #Creating TA Time series RB
# RB_TA_plot <- ggplot(data = RB,
#                      aes(
#                        x = Time,
#                        y = TAlk_measured_average,
#                        color = Site,
#                        group = Site
#                      )) +
#   geom_line() +
#   geom_line(aes(y = 2100 + Tidal_Depth_ft * 75),
#             linetype = "dotdash") +
#   scale_y_continuous(
#     name = "Total Alkalinity (umol/kg)",
#     limits = c(2100, 2400),
#     breaks = seq(2100, 2380, 50)
#   ) +
#   # sec.axis = sec_axis(~(.-2100)/75,
#   #                     name = "Tidal Height (ft)",
#   #                     breaks = seq(0, 4, 1))) +
#   geom_point() +
#   #geom_point(aes(y = 2100 + Tidal_Depth_ft*75)) +
#   scale_color_manual(values = c(
#     "EOS Center" = "#F8766D",
#     "Richardson Bay" = "#00BFC4"
#   )) +
#   geom_vline(data = filter(RB, Week == "1"),
#              aes(xintercept = 13.31),
#              color = "black") +
#   geom_vline(data = filter(RB, Week == "2"),
#              aes(xintercept = 9.88),
#              color = "black") +
#   geom_vline(data = filter(RB, Week == "3"),
#              aes(xintercept = 5.12),
#              color = "black") +
#   facet_wrap(~ Week, nrow = 3, labeller = labeller(Week = RB_week.labs)) +
#   theme_bw(base_size = 12) +
#   theme(legend.position = "none") +
#   #ggtitle("Total Alkalinity Tidal Trends") +
#   #labs(subtitle = "Summer 2022") +
#   xlab("Time (PST)") +
#   ylab("Total Alkalinity (umol/kg)") +
#   scale_x_discrete(guide = guide_axis(angle = 90))
# RB_TA_plot
# 
# 
# #Creating TA Time series EOS
# EOS_TA_plot <- ggplot(data = EOS,
#                       aes(
#                         x = Time,
#                         y = TAlk_measured_average,
#                         color = Site,
#                         group = Site
#                       )) +
#   geom_line() +
#   geom_line(aes(y = 2100 + Tidal_Depth_ft * 75),
#             linetype = "dotdash") +
#   scale_y_continuous(
#     name = "Total Alkalinity (umol/kg)",
#     limits = c(2100, 2400),
#     breaks = seq(2100, 2380, 50),
#     sec.axis = sec_axis(~ (. - 2100) / 75,
#                         name = "Tidal Height (ft)",
#                         breaks = seq(0, 4, 1))
#   ) +
#   geom_point() +
#   #geom_point(aes(y = 2100 + Tidal_Depth_ft*75)) +
#   scale_color_manual(values = c(
#     "EOS Center" = "#F8766D",
#     "Richardson Bay" = "#00BFC4"
#   )) +
#   geom_vline(data = filter(EOS, Week == "1"),
#              aes(xintercept = 7.11),
#              color = "black") +
#   geom_vline(data = filter(EOS, Week == "2"),
#              aes(xintercept = 13.39),
#              color = "black") +
#   geom_vline(data = filter(EOS, Week == "3"),
#              aes(xintercept = 7.87),
#              color = "black") +
#   facet_wrap(~ Week, nrow = 3, labeller = labeller(Week = EOS_week.labs)) +
#   theme_bw(base_size = 12) +
#   theme(legend.position = "none") +
#   #ggtitle("Total Alkalinity Tidal Trends") +
#   #labs(subtitle = "Summer 2022") +
#   xlab("Time (PST)") +
#   ylab("Total Alkalinity (umol/kg)") +
#   scale_x_discrete(guide = guide_axis(angle = 90))
# EOS_TA_plot


#Creating TA time series plots RB and EOS sites with separate grids
all_week.labs <- c("June 06-09", "June 20-23", "July 04-07")
names(all_week.labs) <- c("1", "2", "3")

TA_RB_EOS <- ggplot(data = RB_EOS,
                 aes(
                   x = Time,
                   y = TAlk_measured_average,
                   color = Site,
                   group = Site
                 )) +
  scale_color_manual(
    values = c(
      "EOS Center" = "#F8766D",
      "Richardson Bay" = "#00BFC4",
      "China Camp" = "#E7BE08",
      "Gallinas Creek" = "#C77CFF"
    )
  ) +
  geom_line() +
  geom_line(aes(y = 2100 + Tidal_Depth_ft * 75),
            linetype = "dotdash") +
  scale_y_continuous(
    name = "Total Alkalinity (umol/kg)",
    limits = c(2100, 2400),
    breaks = seq(2100, 2400, 100),
    sec.axis = sec_axis(~ (. - 2100) / 75,
                        name = "Tidal Height (ft)",
                        breaks = seq(0, 4, 1))
  ) +
  geom_point() +
  geom_vline(data = filter(All_tidal, Site == "EOS Center", Week == "1"),
             aes(xintercept = 7.11),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "EOS Center", Week == "2"),
             aes(xintercept = 13.39),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "EOS Center", Week == "3"),
             aes(xintercept = 7.87),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "Richardson Bay", Week == "1"),
             aes(xintercept = 13.31),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "Richardson Bay", Week == "2"),
             aes(xintercept = 9.88),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "Richardson Bay", Week == "3"),
             aes(xintercept = 5.12),
             color = "black") +
  facet_grid(Week ~ Site, labeller = labeller(Week = all_week.labs), scales = "free") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) +
  xlab("Time (PST)") +
  ylab("Total Alkalinity (umol/kg)") +
  scale_x_discrete(guide = guide_axis(angle = 90))
TA_RB_EOS

#Summary of results
##RB only
tapply(RB_tidal$TAlk_measured_average, RB_tidal$Week, summary)
tapply(RB_tidal$TAlk_measured_average, RB_tidal$DateTime.Sampled, summary)

##EOS Center only
tapply(EOS_tidal$TAlk_measured_average, EOS_tidal$Week, summary)
tapply(EOS_tidal$TAlk_measured_average, EOS_tidal$DateTime.Sampled, summary)

#Figure 2. Tidal trends of measured total alkalinity values in umol/kg on the
#primary y-axis (bold line) and tidal height in feet on the secondary y-axis (dashed line) with
#hours in pacific standard time (PST) on the x-axis. Comparison of an eelgrass dominated bay in
#Richardson Bay (blue) and a deep, mainstream channel in Central Bay near the EOS Center
#(red) from the sampling period in Summer 2022. Low tide indicated by the black vertical lines.


#putting RB and EOS pH and TA plots together and customizing it
pH_TA <- ggarrange(pH_RB_EOS, TA_RB_EOS, nrow = 2)

annotate_figure(
  pH_TA,
  top = text_grob("Tidal Trends in pH and TA \n Summer 2022", size = 12)
)



#Aragonite Saturation State



# #Creating Arag Time series plots all sites
# ggplot(data = All_tidal,
#        aes(
#          x = Time,
#          y = Arag,
#          color = Site,
#          group = Site
#        )) +
#   geom_line() +
#   geom_line(aes(y = Tidal_Depth_ft * 1.25),
#             linetype = "dotdash") +
#   scale_y_continuous(
#     name = "Aragonite Saturation State (omega)",
#     limits = c(0, 5),
#     breaks = seq(0, 5, 1),
#     sec.axis = sec_axis( ~ (.) / 1.25,
#                          name = "Tidal Height (ft)",
#                          breaks = seq(0, 4, 1))
#   ) +
#   geom_point() +
#   #geom_point(aes(y = Tidal_Depth_ft * 1.25)) +
#   #facet_wrap( ~ Week, nrow = 3) +
#   scale_color_manual(
#     values = c(
#       "EOS Center" = "#F8766D",
#       "Richardson Bay" = "#00BFC4",
#       "China Camp" = "#E7BE08",
#       "Gallinas Creek" = "#C77CFF"
#     )
#   ) +
#   facet_wrap(~ Week, nrow = 3) +
#   theme_bw(base_size = 13) +
#   theme(legend.position = "top") +
#   ggtitle("Aragonite Saturation State Tidal Trends") +
#   labs(subtitle = "Summer 2022") +
#   xlab("Time (PST)") +
#   ylab("Aragonite Saturation State (omega)") +
#   scale_x_discrete(guide = guide_axis(angle = 90))



#Creating Arag Time series plots all sites with separate grids
Arag_all <- ggplot(data = All_tidal,
       aes(
         x = Time,
         y = Arag,
         color = Site,
         group = Site
       )) +
  scale_color_manual(
    values = c(
      "EOS Center" = "#F8766D",
      "Richardson Bay" = "#00BFC4",
      "China Camp" = "#E7BE08",
      "Gallinas Creek" = "#C77CFF"
    ),
    name = "Stations"
  ) +
  geom_line() +
  geom_hline(yintercept = 1, color = "red") +
  geom_line(aes(y = Tidal_Depth_ft * 1.25),
            linetype = "dotdash") +
  scale_y_continuous(
    name = "Aragonite Saturation State (omega)",
    limits = c(0, 5),
    breaks = seq(0, 5, 1),
    sec.axis = sec_axis( ~ (.) / 1.25,
                         name = "Tidal Height (ft)",
                         breaks = seq(0, 4, 1))
  ) +
  geom_point() +
  geom_vline(data = filter(All_tidal, Site == "EOS Center", Week == "1"),
             aes(xintercept = 7.11),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "EOS Center", Week == "2"),
             aes(xintercept = 13.39),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "EOS Center", Week == "3"),
             aes(xintercept = 7.87),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "Richardson Bay", Week == "1"),
             aes(xintercept = 13.31),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "Richardson Bay", Week == "2"),
             aes(xintercept = 9.88),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "Richardson Bay", Week == "3"),
             aes(xintercept = 5.12),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "Gallinas Creek", Week == "1"),
             aes(xintercept = 11.10),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "Gallinas Creek", Week == "2"),
             aes(xintercept = 11.42),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "Gallinas Creek", Week == "3"),
             aes(xintercept = 3.97),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "China Camp", Week == "1"),
             aes(xintercept = 9.94),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "China Camp", Week == "2"),
             aes(xintercept = 9.33),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "China Camp", Week == "3"),
             aes(xintercept = 9.05),
             color = "black") +
  facet_grid(Week ~ Site, labeller = labeller(Week = all_week.labs), scales = "free") +
  theme_bw(base_size = 16) +
  theme(legend.position = "top", 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14)) +
  # ggtitle("Aragonite Saturation State Tidal Trends") +
  # labs(subtitle = "Summer 2022") +
  # xlab("Time (PST)") +
  ylab("Aragonite Saturation State (omega)")
  # scale_x_discrete(guide = guide_axis(angle = 90))

Arag_all

#Figure 3. Tidal trends of calculated aragonite saturation state values on the primary y-axis
#(bold line) and tidal height in feet on the secondary y-axis (dashed line) with hours in pacific
#standard time (PST) on the x-axis. Comparison of a shallow-water mudflat in China Camp (green),
#a tidal creek and marsh habitat in Gallinas Creek (purple), an eelgrass dominated bay in
#Richardson Bay (blue), and a deep, mainstream channel in Central Bay near the EOS Center (red)
#from the sampling period in Summer 2022. Low tide indicated by the black vertical lines.


# #Filtering out CC
# CC <- All_tidal %>%
#   filter(Site == c("China Camp"))
# 
# #Creating aragonite Time series CC
# CC_arag_plot <- ggplot(data = CC,
#                        aes(
#                          x = Time,
#                          y = Arag,
#                          color = Site,
#                          group = Site
#                        )) +
#   scale_color_manual(
#     values = c(
#       "EOS Center" = "#F8766D",
#       "Richardson Bay" = "#00BFC4",
#       "China Camp" = "#E7BE08",
#       "Gallinas Creek" = "#C77CFF"
#     )
#   ) +
#   geom_line() +
#   geom_line(aes(y = Tidal_Depth_ft * 1.25),
#             linetype = "dotdash") +
#   scale_y_continuous(
#     name = "Aragonite Saturation State (omega)",
#     limits = c(0, 5),
#     breaks = seq(0, 5, 1),
#     sec.axis = sec_axis( ~ (.) / 1.25,
#                          name = "Tidal Height (ft)",
#                          breaks = seq(0, 4, 1))
#   ) +
#   geom_point() +
#   #geom_point(aes(y = Tidal_Depth_ft * 1.25)) +
#   geom_vline(data = filter(CC, Week == "1"),
#              aes(xintercept = 9.94),
#              color = "black") +
#   geom_vline(data = filter(CC, Week == "2"),
#              aes(xintercept = 9.33),
#              color = "black") +
#   geom_vline(data = filter(CC, Week == "3"),
#              aes(xintercept = 9.05),
#              color = "black") +
#   facet_wrap(~ Week, nrow = 3) +
#   theme_bw(base_size = 13) +
#   theme(legend.position = "top") +
#   #ggtitle("Aragonite Saturation State Tidal Trends") +
#   #labs(subtitle = "Summer 2022") +
#   xlab("Time (PST)") +
#   ylab("Aragonite Saturation State (omega)") +
#   scale_x_discrete(guide = guide_axis(angle = 90))
# CC_arag_plot
# 
# #plot 2 arag CC
# CC_arag_plot_2 <- ggplot(data = CC,
#                          aes(
#                            x = Time,
#                            y = Arag,
#                            color = Site,
#                            group = Site
#                          )) +
#   scale_color_manual(
#     values = c(
#       "EOS Center" = "#F8766D",
#       "Richardson Bay" = "#00BFC4",
#       "China Camp" = "#E7BE08",
#       "Gallinas Creek" = "#C77CFF"
#     )
#   ) +
#   geom_line() +
#   geom_line(aes(y = Tidal_Depth_ft * 1.25),
#             linetype = "dotdash") +
#   scale_y_continuous(
#     name = "Aragonite Saturation State (omega)",
#     limits = c(0, 5),
#     breaks = seq(0, 5, 1)) +
#   #   sec.axis = sec_axis( ~ (.) / 1.25,
#   #                        name = "Tidal Height (ft)",
#   #                        breaks = seq(0, 4, 1))
#   # ) +
#   geom_point() +
#   #geom_point(aes(y = Tidal_Depth_ft * 1.25)) +
#   geom_vline(data = filter(CC, Week == "1"),
#              aes(xintercept = 9.94),
#              color = "black") +
#   geom_vline(data = filter(CC, Week == "2"),
#              aes(xintercept = 9.33),
#              color = "black") +
#   geom_vline(data = filter(CC, Week == "3"),
#              aes(xintercept = 9.05),
#              color = "black") +
#   facet_wrap(~ Week, nrow = 3) +
#   theme_bw(base_size = 13) +
#   theme(legend.position = "top") +
#   #ggtitle("Aragonite Saturation State Tidal Trends") +
#   #labs(subtitle = "Summer 2022") +
#   xlab("Time (PST)") +
#   ylab("Aragonite Saturation State (omega)") +
#   scale_x_discrete(guide = guide_axis(angle = 90))
# CC_arag_plot_2
# 
# 
# #Filtering out GC
# GC <- All_tidal %>%
#   filter(Site == c("Gallinas Creek"))
# 
# #Creating aragonite Time series GC
# GC_arag_plot <- ggplot(data = GC,
#                        aes(
#                          x = Time,
#                          y = Arag,
#                          color = Site,
#                          group = Site
#                        )) +
#   scale_color_manual(
#     values = c(
#       "EOS Center" = "#F8766D",
#       "Richardson Bay" = "#00BFC4",
#       "China Camp" = "#E7BE08",
#       "Gallinas Creek" = "#C77CFF"
#     )
#   ) +
#   geom_line() +
#   geom_line(aes(y = Tidal_Depth_ft * 1.25),
#             linetype = "dotdash") +
#   scale_y_continuous(
#     name = "Aragonite Saturation State (omega)",
#     limits = c(0, 5),
#     breaks = seq(0, 5, 1),
#     sec.axis = sec_axis( ~ (.) / 1.25,
#                          name = "Tidal Height (ft)",
#                          breaks = seq(0, 4, 1))
#   ) +
#   geom_point() +
#   #geom_point(aes(y = Tidal_Depth_ft * 1.25)) +
#   geom_vline(data = filter(GC, Week == "1"),
#              aes(xintercept = 11.1),
#              color = "black") +
#   geom_vline(data = filter(GC, Week == "2"),
#              aes(xintercept = 11.42),
#              color = "black") +
#   geom_vline(data = filter(GC, Week == "3"),
#              aes(xintercept = 3.98),
#              color = "black") +
#   facet_wrap(~ Week, nrow = 3) +
#   theme_bw(base_size = 13) +
#   theme(legend.position = "top") +
#   #ggtitle("Aragonite Saturation State Tidal Trends") +
#   #labs(subtitle = "Summer 2022") +
#   xlab("Time (PST)") +
#   ylab("Aragonite Saturation State (omega)") +
#   scale_x_discrete(guide = guide_axis(angle = 90))
# GC_arag_plot
# 
# #plot 2 arag GC
# GC_arag_plot_2 <- ggplot(data = GC,
#                          aes(
#                            x = Time,
#                            y = Arag,
#                            color = Site,
#                            group = Site
#                          )) +
#   scale_color_manual(
#     values = c(
#       "EOS Center" = "#F8766D",
#       "Richardson Bay" = "#00BFC4",
#       "China Camp" = "#E7BE08",
#       "Gallinas Creek" = "#C77CFF"
#     )
#   ) +
#   geom_line() +
#   geom_line(aes(y = Tidal_Depth_ft * 1.25),
#             linetype = "dotdash") +
#   scale_y_continuous(
#     name = "Aragonite Saturation State (omega)",
#     limits = c(0, 5),
#     breaks = seq(0, 5, 1)) +
#   #   sec.axis = sec_axis( ~ (.) / 1.25,
#   #                        name = "Tidal Height (ft)",
#   #                        breaks = seq(0, 4, 1))
#   # ) +
#   geom_point() +
#   #geom_point(aes(y = Tidal_Depth_ft * 1.25)) +
#   geom_vline(data = filter(GC, Week == "1"),
#              aes(xintercept = 11.1),
#              color = "black") +
#   geom_vline(data = filter(GC, Week == "2"),
#              aes(xintercept = 11.42),
#              color = "black") +
#   geom_vline(data = filter(GC, Week == "3"),
#              aes(xintercept = 3.98),
#              color = "black") +
#   facet_wrap(~ Week, nrow = 3) +
#   theme_bw(base_size = 13) +
#   theme(legend.position = "top",
#         axis.text.y=element_blank()) +
#   #ggtitle("Aragonite Saturation State Tidal Trends") +
#   #labs(subtitle = "Summer 2022") +
#   xlab("Time (PST)") +
#   ylab("Aragonite Saturation State (omega)") +
#   scale_x_discrete(guide = guide_axis(angle = 90))
# GC_arag_plot_2
# 
# 
# #Creating aragonite Time series RB
# RB_arag_plot <- ggplot(data = RB,
#                        aes(
#                          x = Time,
#                          y = Arag,
#                          color = Site,
#                          group = Site
#                        )) +
#   scale_color_manual(
#     values = c(
#       "EOS Center" = "#F8766D",
#       "Richardson Bay" = "#00BFC4",
#       "China Camp" = "#E7BE08",
#       "Gallinas Creek" = "#C77CFF"
#     )
#   ) +
#   geom_line() +
#   geom_line(aes(y = Tidal_Depth_ft * 1.25),
#             linetype = "dotdash") +
#   scale_y_continuous(
#     name = "Aragonite Saturation State (omega)",
#     limits = c(0, 5),
#     breaks = seq(0, 5, 1),
#     sec.axis = sec_axis( ~ (.) / 1.25,
#                          name = "Tidal Height (ft)",
#                          breaks = seq(0, 4, 1))
#   ) +
#   geom_point() +
#   #geom_point(aes(y = Tidal_Depth_ft * 1.25)) +
#   geom_vline(data = filter(RB, Week == "1"),
#              aes(xintercept = 13.31),
#              color = "black") +
#   geom_vline(data = filter(RB, Week == "2"),
#              aes(xintercept = 9.88),
#              color = "black") +
#   geom_vline(data = filter(RB, Week == "3"),
#              aes(xintercept = 5.12),
#              color = "black") +
#   facet_wrap(~ Week, nrow = 3) +
#   theme_bw(base_size = 13) +
#   theme(legend.position = "top") +
#   #ggtitle("Aragonite Saturation State Tidal Trends") +
#   #labs(subtitle = "Summer 2022") +
#   xlab("Time (PST)") +
#   ylab("Aragonite Saturation State (omega)") +
#   scale_x_discrete(guide = guide_axis(angle = 90))
# RB_arag_plot
# 
# #plot 2 arag RB
# RB_arag_plot_2 <- ggplot(data = RB,
#                        aes(
#                          x = Time,
#                          y = Arag,
#                          color = Site,
#                          group = Site
#                        )) +
#   scale_color_manual(
#     values = c(
#       "EOS Center" = "#F8766D",
#       "Richardson Bay" = "#00BFC4",
#       "China Camp" = "#E7BE08",
#       "Gallinas Creek" = "#C77CFF"
#     )
#   ) +
#   geom_line() +
#   geom_line(aes(y = Tidal_Depth_ft * 1.25),
#             linetype = "dotdash") +
#   scale_y_continuous(
#     name = "Aragonite Saturation State (omega)",
#     limits = c(0, 5),
#     breaks = seq(0, 5, 1)) +
#   #   sec.axis = sec_axis( ~ (.) / 1.25,
#   #                        name = "Tidal Height (ft)",
#   #                        breaks = seq(0, 4, 1))
#   # ) +
#   geom_point() +
#   #geom_point(aes(y = Tidal_Depth_ft * 1.25)) +
#   geom_vline(data = filter(RB, Week == "1"),
#              aes(xintercept = 13.31),
#              color = "black") +
#   geom_vline(data = filter(RB, Week == "2"),
#              aes(xintercept = 9.88),
#              color = "black") +
#   geom_vline(data = filter(RB, Week == "3"),
#              aes(xintercept = 5.12),
#              color = "black") +
#   facet_wrap(~ Week, nrow = 3) +
#   theme_bw(base_size = 13) +
#   theme(legend.position = "top",
#         axis.text.y=element_blank()) +
#   #ggtitle("Aragonite Saturation State Tidal Trends") +
#   #labs(subtitle = "Summer 2022") +
#   xlab("Time (PST)") +
#   ylab("Aragonite Saturation State (omega)") +
#   scale_x_discrete(guide = guide_axis(angle = 90))
# RB_arag_plot_2
# 
# 
# #Creating aragonite Time series EOS
# EOS_arag_plot <- ggplot(data = EOS,
#                         aes(
#                           x = Time,
#                           y = Arag,
#                           color = Site,
#                           group = Site
#                         )) +
#   scale_color_manual(
#     values = c(
#       "EOS Center" = "#F8766D",
#       "Richardson Bay" = "#00BFC4",
#       "China Camp" = "#E7BE08",
#       "Gallinas Creek" = "#C77CFF"
#     )
#   ) +
#   geom_line() +
#   geom_line(aes(y = Tidal_Depth_ft * 1.25),
#             linetype = "dotdash") +
#   scale_y_continuous(
#     name = "Aragonite Saturation State (omega)",
#     limits = c(0, 5),
#     breaks = seq(0, 5, 1),
#     sec.axis = sec_axis( ~ (.) / 1.25,
#                          name = "Tidal Height (ft)",
#                          breaks = seq(0, 4, 1))
#   ) +
#   geom_point() +
#   #geom_point(aes(y = Tidal_Depth_ft * 1.25)) +
#   geom_vline(data = filter(EOS, Week == "1"),
#              aes(xintercept = 7.11),
#              color = "black") +
#   geom_vline(data = filter(EOS, Week == "2"),
#              aes(xintercept = 13.39),
#              color = "black") +
#   geom_vline(data = filter(EOS, Week == "3"),
#              aes(xintercept = 7.87),
#              color = "black") +
#   facet_wrap(~ Week, nrow = 3) +
#   theme_bw(base_size = 13) +
#   theme(legend.position = "top") +
#   #ggtitle("Aragonite Saturation State Tidal Trends") +
#   #labs(subtitle = "Summer 2022") +
#   xlab("Time (PST)") +
#   ylab("Aragonite Saturation State (omega)") +
#   scale_x_discrete(guide = guide_axis(angle = 90))
# EOS_arag_plot
# 
# #plot 2 arag EOS
# EOS_arag_plot_2 <- ggplot(data = EOS,
#                         aes(
#                           x = Time,
#                           y = Arag,
#                           color = Site,
#                           group = Site
#                         )) +
#   scale_color_manual(
#     values = c(
#       "EOS Center" = "#F8766D",
#       "Richardson Bay" = "#00BFC4",
#       "China Camp" = "#FFCC02",
#       "Gallinas Creek" = "#C77CFF"
#     )
#   ) +
#   geom_line() +
#   geom_line(aes(y = Tidal_Depth_ft * 1.25),
#             linetype = "dotdash") +
#   scale_y_continuous(
#     name = "Aragonite Saturation State (omega)",
#     limits = c(0, 5),
#     breaks = seq(0, 5, 1),
#     sec.axis = sec_axis( ~ (.) / 1.25,
#                          name = "Tidal Height (ft)",
#                          breaks = seq(0, 4, 1))
#   ) +
#   geom_point() +
#   #geom_point(aes(y = Tidal_Depth_ft * 1.25)) +
#   geom_vline(data = filter(EOS, Week == "1"),
#              aes(xintercept = 7.11),
#              color = "black") +
#   geom_vline(data = filter(EOS, Week == "2"),
#              aes(xintercept = 13.39),
#              color = "black") +
#   geom_vline(data = filter(EOS, Week == "3"),
#              aes(xintercept = 7.87),
#              color = "black") +
#   facet_wrap(~ Week, nrow = 3) +
#   theme_bw(base_size = 13) +
#   theme(legend.position = "top") +
#   #ggtitle("Aragonite Saturation State Tidal Trends") +
#   #labs(subtitle = "Summer 2022") +
#   xlab("Time (PST)") +
#   ylab("Aragonite Saturation State (omega)") +
#   scale_x_discrete(guide = guide_axis(angle = 90))
# EOS_arag_plot_2
# 
# #putting all sites plots together and customizing it
# figure_arag <-
#   ggarrange(
#     CC_arag_plot,
#     GC_arag_plot,
#     RB_arag_plot,
#     EOS_arag_plot,
#     ncol = 2,
#     nrow = 2
#   )
# 
# annotate_figure(
#   figure_arag,
#   top = text_grob("Aragonite Saturation State Tidal Trends", size = 24),
#   fig.lab = "Summer 2022",
#   fig.lab.size = 14
# )
# 
# #putting all sites plots together and customizing it plot 2
# figure_arag_2 <-
#   ggarrange(
#     CC_arag_plot_2,
#     GC_arag_plot_2 + rremove("ylab"),
#     RB_arag_plot_2 + rremove("ylab"),
#     EOS_arag_plot_2,
#     ncol = 4,
#     nrow = 1
#   )
# 
# annotate_figure(
#   figure_arag_2,
#   top = text_grob("Aragonite Saturation State Tidal Trends", size = 24),
#   fig.lab = "Summer 2022",
#   fig.lab.size = 14
# )

#Summary of results
##RB only
tapply(RB_tidal$Arag, RB_tidal$Week, summary)
tapply(RB_tidal$Arag, RB_tidal$DateTime.Sampled, summary)

##EOS Center only
tapply(EOS_tidal$Arag, EOS_tidal$Week, summary)
tapply(EOS_tidal$Arag, EOS_tidal$DateTime.Sampled, summary)

##GC only
tapply(GC_tidal$Arag, GC_tidal$Week, summary)
tapply(GC_tidal$Arag, GC_tidal$DateTime.Sampled, summary)

##CC only
tapply(CC_tidal$Arag, CC_tidal$Week, summary)
tapply(CC_tidal$Arag, CC_tidal$DateTime.Sampled, summary)

#Figure 3. Tidal trends of calculated aragonite saturation state values on the primary y-axis
#(bold line) and tidal height in feet on the secondary y-axis (dashed line) with hours in pacific
#standard time (PST) on the x-axis. Comparison of a shallow-water mudflat in China Camp (green),
#a tidal creek and marsh habitat in Gallinas Creek (purple), an eelgrass dominated bay in
#Richardson Bay (blue), and a deep, mainstream channel in Central Bay near the EOS Center (red)
#from the sampling period in Summer 2022. Low tide indicated by the black vertical lines.



#Calcite Saturation State



# #Creating Calcite Time series plots all sites
# ggplot(data = All_tidal,
#        aes(
#          x = Time,
#          y = Calcite,
#          color = Site,
#          group = Site
#        )) +
#   scale_color_manual(
#     values = c(
#       "EOS Center" = "#F8766D",
#       "Richardson Bay" = "#00BFC4",
#       "China Camp" = "#E7BE08",
#       "Gallinas Creek" = "#C77CFF"
#     )
#   ) +
#   geom_line() +
#   geom_line(aes(y = Tidal_Depth_ft * 1.75),
#             linetype = "dotdash") +
#   scale_y_continuous(
#     name = "Calcite Saturation State (omega)",
#     limits = c(0, 7),
#     breaks = seq(0, 7, 1),
#     sec.axis = sec_axis( ~ (.) / 1.75,
#                          name = "Tidal Height (ft)",
#                          breaks = seq(0, 4, 1))
#   ) +
#   geom_point() +
#   #geom_point(aes(y = Tidal_Depth_ft * 1.75)) +
#   #facet_wrap( ~ Week, nrow = 3) +
#   facet_grid(cols = vars(Site), rows = vars(Week)) +
#   theme_bw(base_size = 13) +
#   theme(legend.position = "top") +
#   ggtitle("Calcite Saturation State Tidal Trends") +
#   labs(subtitle = "Summer 2022") +
#   xlab("Time (PST)") +
#   ylab("Calcite Saturation State (omega)") +
#   scale_x_discrete(guide = guide_axis(angle = 90))


#Creating calcite Time series plots all sites with separate grids
Calc_all <- ggplot(data = All_tidal,
       aes(
         x = Time,
         y = Calcite,
         color = Site,
         group = Site
       )) +
  scale_color_manual(
    values = c(
      "EOS Center" = "#F8766D",
      "Richardson Bay" = "#00BFC4",
      "China Camp" = "#E7BE08",
      "Gallinas Creek" = "#C77CFF"
    )
  ) +
  geom_line() +
  geom_hline(yintercept = 1, color = "red") +
  geom_line(aes(y = Tidal_Depth_ft * 1.75),
            linetype = "dotdash") +
  scale_y_continuous(
        name = "Calcite Saturation State (omega)",
        limits = c(0, 7),
        breaks = seq(0, 7, 1),
        sec.axis = sec_axis( ~ (.) / 1.75,
                             name = "Tidal Height (ft)",
                             breaks = seq(0, 4, 1))
      ) +
  geom_point() +
  geom_vline(data = filter(All_tidal, Site == "EOS Center", Week == "1"),
             aes(xintercept = 7.11),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "EOS Center", Week == "2"),
             aes(xintercept = 13.39),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "EOS Center", Week == "3"),
             aes(xintercept = 7.87),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "Richardson Bay", Week == "1"),
             aes(xintercept = 13.31),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "Richardson Bay", Week == "2"),
             aes(xintercept = 9.88),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "Richardson Bay", Week == "3"),
             aes(xintercept = 5.12),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "Gallinas Creek", Week == "1"),
             aes(xintercept = 11.10),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "Gallinas Creek", Week == "2"),
             aes(xintercept = 11.42),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "Gallinas Creek", Week == "3"),
             aes(xintercept = 3.97),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "China Camp", Week == "1"),
             aes(xintercept = 9.94),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "China Camp", Week == "2"),
             aes(xintercept = 9.33),
             color = "black") +
  geom_vline(data = filter(All_tidal, Site == "China Camp", Week == "3"),
             aes(xintercept = 9.05),
             color = "black") +
  facet_grid(Week ~ Site, labeller = labeller(Week = all_week.labs), scales = "free") +
  theme_bw(base_size = 16) +
  theme(legend.position = "none",
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=14)) +
  # ggtitle("Aragonite Saturation State Tidal Trends") +
  # labs(subtitle = "Summer 2022") +
  xlab("Time (PST)") +
  ylab("Calcite Saturation State (omega)") +
  scale_x_discrete(guide = guide_axis(angle = 90))

Calc_all

#Figure 4. Tidal trends of calculated calcite saturation state values on the primary y-axis
#(bold line) and tidal height in feet on the secondary y-axis (dashed line) with hours in pacific
#standard time (PST) on the x-axis. Comparison of a shallow-water mudflat in China Camp (green),
#a tidal creek and marsh habitat in Gallinas Creek (purple), an eelgrass dominated bay in
#Richardson Bay (blue), and a deep, mainstream channel in Central Bay near the EOS Center (red)
#from the sampling period in Summer 2022. Low tide indicated by the black vertical lines.


#putting RB and EOS pH and TA plots together and customizing it
Arag_Calc <- ggarrange(Arag_all, Calc_all, nrow = 2)

annotate_figure(
  Arag_Calc,
  top = text_grob("Tidal Trends in Aragonite and Calcite Saturation State \n Summer 2022", size = 12)
)



# #Creating calcite Time series CC
# CC_calcite_plot <- ggplot(data = CC,
#                           aes(
#                             x = Time,
#                             y = Calcite,
#                             color = Site,
#                             group = Site
#                           )) +
#   scale_color_manual(
#     values = c(
#       "EOS Center" = "#F8766D",
#       "Richardson Bay" = "#00BFC4",
#       "China Camp" = "#E7BE08",
#       "Gallinas Creek" = "#C77CFF"
#     )
#   ) +
#   geom_line() +
#   geom_line(aes(y = Tidal_Depth_ft * 1.75),
#             linetype = "dotdash") +
#   scale_y_continuous(
#     name = "Calcite Saturation State (omega)",
#     limits = c(0, 7),
#     breaks = seq(0, 7, 1),
#     sec.axis = sec_axis( ~ (.) / 1.75,
#                          name = "Tidal Height (ft)",
#                          breaks = seq(0, 4, 1))
#   ) +
#   geom_point() +
#   #geom_point(aes(y = Tidal_Depth_ft * 1.75)) +
#   geom_vline(data = filter(CC, Week == "1"),
#              aes(xintercept = 9.94),
#              color = "black") +
#   geom_vline(data = filter(CC, Week == "2"),
#              aes(xintercept = 9.33),
#              color = "black") +
#   geom_vline(data = filter(CC, Week == "3"),
#              aes(xintercept = 9.05),
#              color = "black") +
#   facet_wrap(~ Week, nrow = 3) +
#   theme_bw(base_size = 13) +
#   theme(legend.position = "top") +
#   #ggtitle("Calcite Saturation State Tidal Trends") +
#   #labs(subtitle = "Summer 2022") +
#   xlab("Time (PST)") +
#   ylab("Calcite Saturation State (omega)") +
#   scale_x_discrete(guide = guide_axis(angle = 90))
# CC_calcite_plot
# 
# #plot 2 calcite CC
# CC_calcite_plot_2 <- ggplot(data = CC,
#                             aes(
#                               x = Time,
#                               y = Calcite,
#                               color = Site,
#                               group = Site
#                             )) +
#   scale_color_manual(
#     values = c(
#       "EOS Center" = "#F8766D",
#       "Richardson Bay" = "#00BFC4",
#       "China Camp" = "#E7BE08",
#       "Gallinas Creek" = "#C77CFF"
#     )
#   ) +
#   geom_line() +
#   geom_line(aes(y = Tidal_Depth_ft * 1.75),
#             linetype = "dotdash") +
#   scale_y_continuous(
#     name = "Calcite Saturation State (omega)",
#     limits = c(0, 7),
#     breaks = seq(0, 7, 1)) +
#   #   sec.axis = sec_axis( ~ (.) / 1.75,
#   #                        name = "Tidal Height (ft)",
#   #                        breaks = seq(0, 4, 1))
#   # ) +
#   geom_point() +
#   #geom_point(aes(y = Tidal_Depth_ft * 1.75)) +
#   geom_vline(data = filter(CC, Week == "1"),
#              aes(xintercept = 9.94),
#              color = "black") +
#   geom_vline(data = filter(CC, Week == "2"),
#              aes(xintercept = 9.33),
#              color = "black") +
#   geom_vline(data = filter(CC, Week == "3"),
#              aes(xintercept = 9.05),
#              color = "black") +
#   facet_wrap(~ Week, nrow = 3) +
#   theme_bw(base_size = 13) +
#   theme(legend.position = "top") +
#   #ggtitle("Calcite Saturation State Tidal Trends") +
#   #labs(subtitle = "Summer 2022") +
#   xlab("Time (PST)") +
#   ylab("Calcite Saturation State (omega)") +
#   scale_x_discrete(guide = guide_axis(angle = 90))
# CC_calcite_plot_2
# 
# 
# #Creating calcite Time series GC
# GC_calcite_plot <- ggplot(data = GC,
#                           aes(
#                             x = Time,
#                             y = Calcite,
#                             color = Site,
#                             group = Site
#                           )) +
#   scale_color_manual(
#     values = c(
#       "EOS Center" = "#F8766D",
#       "Richardson Bay" = "#00BFC4",
#       "China Camp" = "#E7BE08",
#       "Gallinas Creek" = "#C77CFF"
#     )
#   ) +
#   geom_line() +
#   geom_line(aes(y = Tidal_Depth_ft * 1.75),
#             linetype = "dotdash") +
#   scale_y_continuous(
#     name = "Calcite Saturation State (omega)",
#     limits = c(0, 7),
#     breaks = seq(0, 7, 1),
#     sec.axis = sec_axis( ~ (.) / 1.75,
#                          name = "Tidal Height (ft)",
#                          breaks = seq(0, 4, 1))
#   ) +
#   geom_point() +
#   #geom_point(aes(y = Tidal_Depth_ft * 1.75)) +
#   geom_vline(data = filter(GC, Week == "1"),
#              aes(xintercept = 11.1),
#              color = "black") +
#   geom_vline(data = filter(GC, Week == "2"),
#              aes(xintercept = 11.42),
#              color = "black") +
#   geom_vline(data = filter(GC, Week == "3"),
#              aes(xintercept = 3.98),
#              color = "black") +
#   facet_wrap(~ Week, nrow = 3) +
#   theme_bw(base_size = 13) +
#   theme(legend.position = "top") +
#   #ggtitle("Calcite Saturation State Tidal Trends") +
#   #labs(subtitle = "Summer 2022") +
#   xlab("Time (PST)") +
#   ylab("Calcite Saturation State (omega)") +
#   scale_x_discrete(guide = guide_axis(angle = 90))
# GC_calcite_plot
# 
# #plot 2 calcite GC
# GC_calcite_plot_2 <- ggplot(data = GC,
#                             aes(
#                               x = Time,
#                               y = Calcite,
#                               color = Site,
#                               group = Site
#                             )) +
#   scale_color_manual(
#     values = c(
#       "EOS Center" = "#F8766D",
#       "Richardson Bay" = "#00BFC4",
#       "China Camp" = "#E7BE08",
#       "Gallinas Creek" = "#C77CFF"
#     )
#   ) +
#   geom_line() +
#   geom_line(aes(y = Tidal_Depth_ft * 1.75),
#             linetype = "dotdash") +
#   scale_y_continuous(
#     name = "Calcite Saturation State (omega)",
#     limits = c(0, 7),
#     breaks = seq(0, 7, 1)) +
#   #   sec.axis = sec_axis( ~ (.) / 1.75,
#   #                        name = "Tidal Height (ft)",
#   #                        breaks = seq(0, 4, 1))
#   # ) +
#   geom_point() +
#   #geom_point(aes(y = Tidal_Depth_ft * 1.75)) +
#   geom_vline(data = filter(GC, Week == "1"),
#              aes(xintercept = 11.1),
#              color = "black") +
#   geom_vline(data = filter(GC, Week == "2"),
#              aes(xintercept = 11.42),
#              color = "black") +
#   geom_vline(data = filter(GC, Week == "3"),
#              aes(xintercept = 3.98),
#              color = "black") +
#   facet_wrap(~ Week, nrow = 3) +
#   theme_bw(base_size = 13) +
#   theme(legend.position = "top",
#         axis.text.y=element_blank()) +
#   #ggtitle("Calcite Saturation State Tidal Trends") +
#   #labs(subtitle = "Summer 2022") +
#   xlab("Time (PST)") +
#   ylab("Calcite Saturation State (omega)") +
#   scale_x_discrete(guide = guide_axis(angle = 90))
# GC_calcite_plot_2
# 
# 
# #Creating calcite Time series RB
# RB_calcite_plot <- ggplot(data = RB,
#                           aes(
#                             x = Time,
#                             y = Calcite,
#                             color = Site,
#                             group = Site
#                           )) +
#   scale_color_manual(
#     values = c(
#       "EOS Center" = "#F8766D",
#       "Richardson Bay" = "#00BFC4",
#       "China Camp" = "#E7BE08",
#       "Gallinas Creek" = "#C77CFF"
#     )
#   ) +
#   geom_line() +
#   geom_line(aes(y = Tidal_Depth_ft * 1.75),
#             linetype = "dotdash") +
#   scale_y_continuous(
#     name = "Calcite Saturation State (omega)",
#     limits = c(0, 7),
#     breaks = seq(0, 7, 1),
#     sec.axis = sec_axis( ~ (.) / 1.75,
#                          name = "Tidal Height (ft)",
#                          breaks = seq(0, 4, 1))
#   ) +
#   geom_point() +
#   #geom_point(aes(y = Tidal_Depth_ft * 1.75)) +
#   geom_vline(data = filter(RB, Week == "1"),
#              aes(xintercept = 13.31),
#              color = "black") +
#   geom_vline(data = filter(RB, Week == "2"),
#              aes(xintercept = 9.88),
#              color = "black") +
#   geom_vline(data = filter(RB, Week == "3"),
#              aes(xintercept = 5.12),
#              color = "black") +
#   facet_wrap(~ Week, nrow = 3) +
#   theme_bw(base_size = 13) +
#   theme(legend.position = "top") +
#   #ggtitle("Calcite Saturation State Tidal Trends") +
#   #labs(subtitle = "Summer 2022") +
#   xlab("Time (PST)") +
#   ylab("Calcite Saturation State (omega)") +
#   scale_x_discrete(guide = guide_axis(angle = 90))
# RB_calcite_plot
# 
# #plot 2 calcite RB
# RB_calcite_plot_2 <- ggplot(data = RB,
#                           aes(
#                             x = Time,
#                             y = Calcite,
#                             color = Site,
#                             group = Site
#                           )) +
#   scale_color_manual(
#     values = c(
#       "EOS Center" = "#F8766D",
#       "Richardson Bay" = "#00BFC4",
#       "China Camp" = "#E7BE08",
#       "Gallinas Creek" = "#C77CFF"
#     )
#   ) +
#   geom_line() +
#   geom_line(aes(y = Tidal_Depth_ft * 1.75),
#             linetype = "dotdash") +
#   scale_y_continuous(
#     name = "Calcite Saturation State (omega)",
#     limits = c(0, 7),
#     breaks = seq(0, 7, 1)) +
#   #   sec.axis = sec_axis( ~ (.) / 1.75,
#   #                        name = "Tidal Height (ft)",
#   #                        breaks = seq(0, 4, 1))
#   # ) +
#   geom_point() +
#   #geom_point(aes(y = Tidal_Depth_ft * 1.75)) +
#   geom_vline(data = filter(RB, Week == "1"),
#              aes(xintercept = 13.31),
#              color = "black") +
#   geom_vline(data = filter(RB, Week == "2"),
#              aes(xintercept = 9.88),
#              color = "black") +
#   geom_vline(data = filter(RB, Week == "3"),
#              aes(xintercept = 5.12),
#              color = "black") +
#   facet_wrap(~ Week, nrow = 3) +
#   theme_bw(base_size = 13) +
#   theme(legend.position = "top",
#         axis.text.y=element_blank()) +
#   #ggtitle("Calcite Saturation State Tidal Trends") +
#   #labs(subtitle = "Summer 2022") +
#   xlab("Time (PST)") +
#   ylab("Calcite Saturation State (omega)") +
#   scale_x_discrete(guide = guide_axis(angle = 90))
# RB_calcite_plot_2
# 
# 
# #Creating calcite Time series EOS
# EOS_calcite_plot <- ggplot(data = EOS,
#                            aes(
#                              x = Time,
#                              y = Arag,
#                              color = Site,
#                              group = Site
#                            )) +
#   scale_color_manual(
#     values = c(
#       "EOS Center" = "#F8766D",
#       "Richardson Bay" = "#00BFC4",
#       "China Camp" = "#E7BE08",
#       "Gallinas Creek" = "#C77CFF"
#     )
#   ) +
#   geom_line() +
#   geom_line(aes(y = Tidal_Depth_ft * 1.75),
#             linetype = "dotdash") +
#   scale_y_continuous(
#     name = "Calcite Saturation State (omega)",
#     limits = c(0, 7),
#     breaks = seq(0, 7, 1),
#     sec.axis = sec_axis( ~ (.) / 1.75,
#                          name = "Tidal Height (ft)",
#                          breaks = seq(0, 4, 1))
#   ) +
#   geom_point() +
#   #geom_point(aes(y = Tidal_Depth_ft * 1.75)) +
#   geom_vline(data = filter(EOS, Week == "1"),
#              aes(xintercept = 7.11),
#              color = "black") +
#   geom_vline(data = filter(EOS, Week == "2"),
#              aes(xintercept = 13.39),
#              color = "black") +
#   geom_vline(data = filter(EOS, Week == "3"),
#              aes(xintercept = 7.87),
#              color = "black") +
#   facet_wrap(~ Week, nrow = 3) +
#   theme_bw(base_size = 13) +
#   theme(legend.position = "top") +
#   #ggtitle("Calcite Saturation State Tidal Trends") +
#   #labs(subtitle = "Summer 2022") +
#   xlab("Time (PST)") +
#   ylab("Calcite Saturation State (omega)") +
#   scale_x_discrete(guide = guide_axis(angle = 90))
# EOS_calcite_plot
# 
# #plot 2 calcite EOS
# EOS_calcite_plot_2 <- ggplot(data = EOS,
#                            aes(
#                              x = Time,
#                              y = Arag,
#                              color = Site,
#                              group = Site
#                            )) +
#   scale_color_manual(
#     values = c(
#       "EOS Center" = "#F8766D",
#       "Richardson Bay" = "#00BFC4",
#       "China Camp" = "#E7BE08",
#       "Gallinas Creek" = "#C77CFF"
#     )
#   ) +
#   geom_line() +
#   geom_line(aes(y = Tidal_Depth_ft * 1.75),
#             linetype = "dotdash") +
#   scale_y_continuous(
#     name = "Calcite Saturation State (omega)",
#     limits = c(0, 7),
#     breaks = seq(0, 7, 1),
#     sec.axis = sec_axis( ~ (.) / 1.75,
#                          name = "Tidal Height (ft)",
#                          breaks = seq(0, 4, 1))
#   ) +
#   geom_point() +
#   #geom_point(aes(y = Tidal_Depth_ft * 1.75)) +
#   geom_vline(data = filter(EOS, Week == "1"),
#              aes(xintercept = 7.11),
#              color = "black") +
#   geom_vline(data = filter(EOS, Week == "2"),
#              aes(xintercept = 13.39),
#              color = "black") +
#   geom_vline(data = filter(EOS, Week == "3"),
#              aes(xintercept = 7.87),
#              color = "black") +
#   facet_wrap(~ Week, nrow = 3) +
#   theme_bw(base_size = 13) +
#   theme(legend.position = "top") +
#   #ggtitle("Calcite Saturation State Tidal Trends") +
#   #labs(subtitle = "Summer 2022") +
#   xlab("Time (PST)") +
#   ylab("Calcite Saturation State (omega)") +
#   scale_x_discrete(guide = guide_axis(angle = 90))
# EOS_calcite_plot_2
# 
# #putting all sites plots together and customizing it
# figure_calcite <-
#   ggarrange(
#     CC_calcite_plot,
#     GC_calcite_plot,
#     RB_calcite_plot,
#     EOS_calcite_plot,
#     ncol = 2,
#     nrow = 2
#   )
# 
# annotate_figure(
#   figure_calcite,
#   top = text_grob("Calcite Saturation State Tidal Trends", size = 24),
#   fig.lab = "Summer 2022",
#   fig.lab.size = 14
# )
# 
# #putting all sites plots together and customizing it plot 2
# figure_calcite_2 <-
#   ggarrange(
#     CC_calcite_plot_2,
#     GC_calcite_plot_2 + rremove("ylab"),
#     RB_calcite_plot_2 + rremove("ylab"),
#     EOS_calcite_plot_2,
#     ncol = 4,
#     nrow = 1
#   )
# 
# annotate_figure(
#   figure_calcite_2,
#   top = text_grob("Calcite Saturation State Tidal Trends", size = 24),
#   fig.lab = "Summer 2022",
#   fig.lab.size = 14
# )


#Summary of results
##RB only
tapply(RB_tidal$Calcite, RB_tidal$Week, summary)
tapply(RB_tidal$Calcite, RB_tidal$DateTime.Sampled, summary)

##EOS Center only
tapply(EOS_tidal$Calcite, EOS_tidal$Week, summary)
tapply(EOS_tidal$Calcite, EOS_tidal$DateTime.Sampled, summary)

##GC only
tapply(GC_tidal$Calcite, GC_tidal$Week, summary)
tapply(GC_tidal$Calcite, GC_tidal$DateTime.Sampled, summary)

##CC only
tapply(CC_tidal$Calcite, CC_tidal$Week, summary)
tapply(CC_tidal$Calcite, CC_tidal$DateTime.Sampled, summary)

#Figure 4. Tidal trends of calculated calcite saturation state values on the primary y-axis
#(bold line) and tidal height in feet on the secondary y-axis (dashed line) with hours in pacific
#standard time (PST) on the x-axis. Comparison of a shallow-water mudflat in China Camp (green),
#a tidal creek and marsh habitat in Gallinas Creek (purple), an eelgrass dominated bay in
#Richardson Bay (blue), and a deep, mainstream channel in Central Bay near the EOS Center (red)
#from the sampling period in Summer 2022. Low tide indicated by the black vertical lines.
