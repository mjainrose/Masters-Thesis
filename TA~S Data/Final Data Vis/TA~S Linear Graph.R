#TA~S Relationship for North San Francisco Bay#
  
  
  
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


#2018-2022 All Salinity and TA data


#Uploading 2018-2022 salinity and TA data
S_ALK <-
  read.csv("TALK_Salinity Tidy By-hand Data.csv", header = TRUE)


#Creating TA~S linear graph
ggplot(data = S_ALK,
       aes(x = Salinity_psu,
           y = TALK_umol.Kg)) +
  geom_point(color = "blue") +
  geom_smooth(method = lm,
              fill = "darkgreen",
              color = "blue") +
  stat_regline_equation(label.x = 2,
                        label.y = 2200,
                        size = 7,
                        color = "blue") +
  theme_bw(base_size = 16) +
  ggtitle("Linear Model of Salinity and Total Alkalinity") +
  labs(subtitle = "North Bay 2018-2022") +
  xlab("Salinity") +
  ylab("Total Alkalinity (umol/kg)")

#Summary of results
model <- lm(TALK_umol.Kg ~ Salinity_psu, data = S_ALK)
summary(model)

##Linear regression line/R-squared/std error (intercept & Sal_psu)
#Linear regression --> y=1361 + 27.85x
#Adjusted R-squared --> 0.89
#Std. Error (intercept) --> 30.935
#Std. Error (Sal_psu) --> 1.54

#Use this code to change significant digits in linear regression line
trace(ggpubr:::.stat_lm, edit = TRUE)

##then change code lines 13-14 to # of sigfigs wanted (4)
eq.char <- as.character(signif(polynom::as.polynomial(coefs),
                               4))

#Figure 1. Linear regression model of total alkalinity as a function of salinity from surface
#samples collected in North SF Bay from the year 2018-2022.




##All 1980-1981 North Bay Salinity and TA data



#Uploading all NB 1980-1981 salinity and TA data
S_ALK_All <-
  read.csv("TALK_Salinity 1983 All NB samples.csv", header = TRUE)

#Creating TA~S linear graph
ggplot(data = S_ALK_All,
       aes(x = Salinity_psu,
           y = TALK_umol.Kg)) +
  geom_point(color = "darkorange") +
  geom_smooth(method = lm, fill = "darkgreen") +
  stat_regline_equation(label.x = 2,
                        label.y = 2200,
                        size = 7,
                        color = "darkorange") +
  theme_bw(base_size = 16) +
  ggtitle("Linear Model of Salinity and Total Alkalinity") +
  labs(subtitle = "North Bay 1980-1981") +
  xlab("Salinity (psu)") +
  ylab("Total Alkalinity (umol/Kg)")

#Summary of results
model <- lm(TALK_umol.Kg ~ Salinity_psu, data = S_ALK_All)
summary(model)

##Linear regression line/R-squared/std error (intercept & Sal_psu)
#Linear regression --> y=1183 + 32.19x
#Adjusted R-squared --> 0.95
#Std. Error (intercept) --> 7.45
#Std. Error (Sal_psu) --> 0.40


#Use this code to change significant digits in linear regression line
trace(ggpubr:::.stat_lm, edit = TRUE)

##then change code lines 13-14 to # of sigfigs wanted (4)
eq.char <- as.character(signif(polynom::as.polynomial(coefs),
                               4))

#Figure 2. Linear regression model of total alkalinity as a function of salinity from surface
#samples collected in North SF Bay from the year 1980-1981.




##All 1980-1981/2018-2022 North Bay Salinity and TA data-Comparison



#Uploading all NB 1980-1981/2018-2022 salinity and TA data
S_ALK_All_New <-
  read.csv("TALK_Salinity 1983 All NB samples with all new 2020 samples.csv",
           header = TRUE)


#Creating TA~S linear graph of TA data plotted against each other by decade
ggplot(data = S_ALK_All_New,
       aes(x = Salinity_psu,
           y = TALK_umol.Kg,
           color = Decade)) +
  geom_point(aes(color = Decade), 
             data = subset(S_ALK_All_New, Decade == '2020s'),
             size = 2) +
  geom_point(aes(color = Decade), 
             data = subset(S_ALK_All_New, Decade == '1980s'),
             shape = 20,
             alpha = 0.5,
             size = 1) +
  geom_smooth(method = lm, fill = "darkgreen") +
  stat_regline_equation(label.x = c(9, 9),
                        label.y = c(2250, 2175),
                        size = 6) +
  theme_bw(base_size = 16) +
  theme(axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=14),
        legend.position = 'none') +
  scale_colour_manual(values = c("darkorange","blue"), 
                      labels = c("1980s", "2018-2022")) +
  #ggtitle("Linear Model of Salinity and Total Alkalinity") +
  #labs(subtitle = "North Bay 1980-1981 & 2018-2022") +
  xlab("Salinity") +
  ylab("Total Alkalinity (umol/kg)")

#Creating model
model <- dlply(S_ALK_All_New, "Decade", function(df)
  lm(TALK_umol.Kg ~ Salinity_psu, data = df))

#Summary of model results
ldply(model, coef)

l_ply(model, summary, .print = T)


#Use this code to change significant digits in linear regression line
trace(ggpubr:::.stat_lm, edit = TRUE)

##then change code lines 13-14 to # of sigfigs wanted (4)
eq.char <- as.character(signif(polynom::as.polynomial(coefs),
                               4))

#Figure 2. Linear model of total alkalinity in umol/kg as a function of salinity in 
#practical salinity units with 95% confidence interval bands (green), comparing an 
#older model from the years 1980-1981 (orange) to a recent model from the years 
#2018-2022 (blue). Surface samples collected in North SF Bay. 



#Creating TA~S +D +S:D graph of TA data plotted against each other by decade


#declare new model here to be used in plot
new_lm <-
  lm(formula = TALK_umol.Kg ~ Salinity_psu + Decade + Salinity_psu:Decade,
     data = S_ALK_All_New)

# #plot with new model
# ggplot(data = S_ALK_All_New,
#        aes(x = Salinity_psu,
#            y = TALK_umol.Kg,
#            color = Decade)) +
#   geom_point() +
#   geom_smooth(
#     method = lm,
#     mapping = aes(y = predict(new_lm, S_ALK_All_New)),
#     fill = "purple"
#   ) +
#   stat_regline_equation(
#     label.x = c(3, 3),
#     label.y = c(2250, 2100),
#     size = 7
#   ) +
#   theme_bw(base_size = 16) +
#   scale_colour_manual(values = c("darkgreen", "blue")) +
#   ggtitle("Linear Model of Salinity and Total Alkalinity") +
#   labs(subtitle = "North Bay 1980-1981 & 2018-2022") +
#   xlab("Salinity (psu)") +
#   ylab("Total Alkalinity (umol/Kg)")

#Summary of new model
summary(new_lm)

##model suggests that alkalinity has increased: source of freshwater from watersheds
#--> ecologically/biogeologically meaningful?
#converging higher in salinity ...calculate at >30 salinity 
#talk to Wim/Frances/Tomoko/Andy/Matt...what drives alkalinity in watersheds (think more of the sources here for discussion)
##what are the differences between then and now (droughts (well-buffered)/rainfall (stressful) =climate change)
#-->will these changes be more extreme in the future?? = large shifts/disturbances in ecology 
##chemical weathering/erosion can contribute to TA##--read tomokos papers


##maybe think about running diluted CRMs##-thinking about publishing 
##read paper that tomoko sent##
##plot 1980s winter dataset across summer dataset##



##Summer (May-July) seasonal subset of 1980-1981 and 2018-2022 North Bay TA samples-comparison 



#Uploading a seasonal subset of NB 1980-1981 and seasonal subset of 2018-2022 TA data from May-July
S_ALK_season_subset <-
  read.csv("TALK_Salinity 1983 and 2020 summer season subset.csv", header = TRUE)


#Creating TA~S linear graph of TA data from same season plotted against each other by decade
ggplot(data = S_ALK_season_subset,
       aes(x = Salinity_psu,
           y = TALK_umol.Kg,
           color = Decade)) +
  geom_point() +
  geom_smooth(method = lm, fill = "darkgreen") +
  stat_regline_equation(label.x = c(2, 2),
                        label.y = c(2200,2100),
                        size = 7) +
  theme_bw(base_size = 16) +
  scale_colour_manual(values = c("darkorange","blue")) + 
  ggtitle("Linear Model of Salinity and Total Alkalinity") +
  labs(subtitle = "North Bay Seasonal Subset of 1980-1981 with 2018-2022 Samples",
       caption = "May-July Samples") +
  xlab("Salinity (psu)") +
  ylab("Total Alkalinity (umol/Kg)")

#Creating model
model <- dlply(S_ALK_season_subset, "Decade", function(df)
  lm(TALK_umol.Kg ~ Salinity_psu, data = df))

#Summary of model results
ldply(model, coef)

l_ply(model, summary, .print = T)

###Also to include mean +/- std.deviation or variance

#Use this code to change significant digits in linear regression line
trace(ggpubr:::.stat_lm, edit = TRUE)

##then change code lines 13-14 to # of sigfigs wanted (4)
eq.char <- as.character(signif(polynom::as.polynomial(coefs),
                               4))

#Figure 3. Linear regression model of total alkalinity as a function of salinity 
#from a seasonal subset of surface samples collected in North SF Bay from the year 
#1980-1981 and comparing it to surface samples collected in North SF Bay from the 
#year 2018-2022 in the same plot.



#Creating TA~S +D +S:D graph of TA data from same season plotted against each other by decade



#declare new model here to be used in plot
new_lm <-
  lm(formula = TALK_umol.Kg ~ Salinity_psu + Decade + Salinity_psu:Decade,
     data = S_ALK_season_subset)

# #plot with new model
# ggplot(data = S_ALK_season_subset,
#        aes(x = Salinity_psu,
#            y = TALK_umol.Kg,
#            color = Decade)) +
#   geom_point() +
#   geom_smooth(method = lm, 
#               mapping = aes(y = predict(new_lm, S_ALK_season_subset)),
#               fill = "purple") +
#   stat_regline_equation(label.x = c(2, 2),
#                         label.y = c(2100,2200),
#                         size = 7) +
#   theme_bw(base_size = 16) +
#   scale_colour_manual(values = c("darkgreen","blue")) + 
#   ggtitle("Linear Model of Salinity and Total Alkalinity") +
#   labs(subtitle = "North Bay Seasonal Subset of 1980-1981 with 2018-2022 Samples",
#        caption = "May-July Samples") +
#   xlab("Salinity (psu)") +
#   ylab("Total Alkalinity (umol/Kg)")

#Summary of new model
summary(new_lm)



####Winter (Nov-Jan) seasonal subset of 1980-1981 and summer (May-July) 
#seasonal subset of 2018-2022 North Bay TA samples-comparison



#Uploading a winter seasonal subset of NB 1980-1981 and a summer seasonal subset of 
#2018-2022 TA data
S_ALK_mixed_season_subset <-
  read.csv("TALK_Salinity 1983 winter season subset with new 2020 summer season subset.csv", header = TRUE)


#Creating TA~S linear graph of TA data opposite seasons plotted against each other by decade
ggplot(data = S_ALK_mixed_season_subset,
       aes(x = Salinity_psu,
           y = TALK_umol.Kg,
           color = Decade)) +
  geom_point() +
  geom_smooth(method = lm, fill = "darkgreen") +
  stat_regline_equation(label.x = c(2, 2),
                        label.y = c(2200,2100),
                        size = 7) +
  theme_bw(base_size = 16) +
  scale_colour_manual(values = c("darkorange","blue")) + 
  ggtitle("Linear Model of Salinity and Total Alkalinity") +
  labs(subtitle = "North Bay Seasonal Subset of Winter 1980-1981 with Summer 2018-2022 Samples") +
  xlab("Salinity (psu)") +
  ylab("Total Alkalinity (umol/Kg)")

#Creating model
model <- dlply(S_ALK_mixed_season_subset, "Decade", function(df)
  lm(TALK_umol.Kg ~ Salinity_psu, data = df))

#Summary of model results
ldply(model, coef)

l_ply(model, summary, .print = T)

###Also to include mean +/- std.deviation or variance

#Use this code to change significant digits in linear regression line
trace(ggpubr:::.stat_lm, edit = TRUE)

##then change code lines 13-14 to # of sigfigs wanted (4)
eq.char <- as.character(signif(polynom::as.polynomial(coefs),
                               4))

#Figure 4. Linear regression model of total alkalinity as a function of salinity 
#from a seasonal subset of surface samples collected in North SF Bay from the year 
#1980-1981 and comparing it to surface samples collected in North SF Bay from the 
#year 2018-2022 in the same plot.



#Creating TA~S +D +S:D graph of TA data from opposite seasons plotted against each other by decade



#declare new model here to be used in plot
new_lm <-
  lm(formula = TALK_umol.Kg ~ Salinity_psu + Decade + Salinity_psu:Decade,
     data = S_ALK_mixed_season_subset)

# #plot with new model
# ggplot(data = S_ALK_season_subset,
#        aes(x = Salinity_psu,
#            y = TALK_umol.Kg,
#            color = Decade)) +
#   geom_point() +
#   geom_smooth(method = lm, 
#               mapping = aes(y = predict(new_lm, S_ALK_season_subset)),
#               fill = "purple") +
#   stat_regline_equation(label.x = c(2, 2),
#                         label.y = c(2100,2200),
#                         size = 7) +
#   theme_bw(base_size = 16) +
#   scale_colour_manual(values = c("darkgreen","blue")) + 
#   ggtitle("Linear Model of Salinity and Total Alkalinity") +
#   labs(subtitle = "North Bay Seasonal Subset of 1980-1981 with 2018-2022 Samples",
#        caption = "May-July Samples") +
#   xlab("Salinity (psu)") +
#   ylab("Total Alkalinity (umol/Kg)")

#Summary of new model
summary(new_lm)



#Uploading a winter seasonal subset of NB 1980-1981 and a summer seasonal subset of 
#1980-1981 TA data
S_ALK_mixed_season_subset_1980 <-
  read.csv("TALK_Salinity 1983 winter and summer season subset.csv", header = TRUE)


#Creating TA~S linear graph of TA data opposite seasons plotted against each other by decade
ggplot(data = S_ALK_mixed_season_subset_1980,
       aes(x = Salinity_psu,
           y = TALK_umol.Kg,
           color = Season)) +
  geom_point() +
  geom_smooth(method = lm, fill = "darkgreen") +
  stat_regline_equation(label.x = c(2, 2),
                        label.y = c(2200,2100),
                        size = 7) +
  theme_bw(base_size = 16) +
  scale_colour_manual(values = c("darkorange","blue")) + 
  ggtitle("Linear Model of Salinity and Total Alkalinity") +
  labs(subtitle = "North Bay Seasonal Subset of Winter and Summer 1980-1981") +
  xlab("Salinity (psu)") +
  ylab("Total Alkalinity (umol/Kg)")

#Creating model
model <- dlply(S_ALK_mixed_season_subset_1980, "Season", function(df)
  lm(TALK_umol.Kg ~ Salinity_psu, data = df))

#Summary of model results
ldply(model, coef)

l_ply(model, summary, .print = T)

###Also to include mean +/- std.deviation or variance

#Use this code to change significant digits in linear regression line
trace(ggpubr:::.stat_lm, edit = TRUE)

##then change code lines 13-14 to # of sigfigs wanted (4)
eq.char <- as.character(signif(polynom::as.polynomial(coefs),
                               4))

#Figure 4. Linear regression model of total alkalinity as a function of salinity 
#from a seasonal subset of surface samples collected in North SF Bay from the year 
#1980-1981 and comparing it to surface samples collected in North SF Bay from the 
#year 2018-2022 in the same plot.



#Creating TA~S +D +S:D graph of TA data from opposite seasons plotted against each other by decade



#declare new model here to be used in plot
new_lm <-
  lm(formula = TALK_umol.Kg ~ Salinity_psu + Season + Salinity_psu:Season,
     data = S_ALK_mixed_season_subset_1980)

# #plot with new model
# ggplot(data = S_ALK_season_subset,
#        aes(x = Salinity_psu,
#            y = TALK_umol.Kg,
#            color = Decade)) +
#   geom_point() +
#   geom_smooth(method = lm, 
#               mapping = aes(y = predict(new_lm, S_ALK_season_subset)),
#               fill = "purple") +
#   stat_regline_equation(label.x = c(2, 2),
#                         label.y = c(2100,2200),
#                         size = 7) +
#   theme_bw(base_size = 16) +
#   scale_colour_manual(values = c("darkgreen","blue")) + 
#   ggtitle("Linear Model of Salinity and Total Alkalinity") +
#   labs(subtitle = "North Bay Seasonal Subset of 1980-1981 with 2018-2022 Samples",
#        caption = "May-July Samples") +
#   xlab("Salinity (psu)") +
#   ylab("Total Alkalinity (umol/Kg)")

#Summary of new model
summary(new_lm) #no statistical seasonal difference in the slope but there was a statistacal 
#difference between y-intercept 




##All 1980-1981 & only 2018-2022 neap tide TA data North Bay because 1980 sampling period was during neap tides



#Uploading Neap Tide only samples from 1980-1981(all)/2018-2022 TA data
S_ALK_All_New_Neap <-
  read.csv("TALK_Salinity 1983 All NB samples with new 2020 neap tide samples.csv",
           header = TRUE)

#Creating TA~S linear graph of Neap tide data points
ggplot(data = S_ALK_All_New_Neap,
       aes(x = Salinity_psu,
           y = TALK_umol.Kg,
           color = Decade)) +
  geom_point() +
  geom_smooth(method = lm,
              fill = "darkgreen") +
  stat_regline_equation(label.x = c(2, 2),
                        label.y = c(2300, 2200),
                        size = 7) +
  scale_colour_manual(values = c("orange","blue")) + 
  theme_bw(base_size = 16) +
  ggtitle("Linear Model of Salinity and Total Alkalinity") +
  labs(subtitle = "North Bay 1980-1981/2018-2022 (neap tide only)") +
  xlab("Salinity (psu)") +
  ylab("Total Alkalinity (umol/Kg)")

#Creating model
model <- dlply(S_ALK_All_New_Neap, "Decade", function(df)
  lm(TALK_umol.Kg ~ Salinity_psu, data = df))

#Summary of model results
ldply(model, coef)

l_ply(model, summary, .print = T)

  ###Also to include mean +/- std.deviation or variance

#Use this code to change significant digits in linear regression line
trace(ggpubr:::.stat_lm, edit = TRUE)

##then change code lines 13-14 to # of sigfigs wanted (4)
eq.char <- as.character(signif(polynom::as.polynomial(coefs),
                               4))

#Figure 5. Linear regression model of total alkalinity as a function of salinity 
#from a geographic and seasonal subset of surface samples collected in during the 
#neap tide in North SF Bay from the year 1980-1981 and the year 2018-2022 in the same plot.



##2018-2022 Salinity and TA data-mean by month/# observations by month



#converting datetime from a character string to a numerical string
S_ALK$Date <-
  as.POSIXct(S_ALK$Date, format = c("%m/%d/%y"))

#creating a month column
S_ALK$Month <- as.numeric(format(S_ALK$Date, "%m"))

#months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec")

#S_ALK$MonthAbb <- months [S_ALK$Month]


#Creating a new data frame with # of observations in a month/mean of TA in each month
N_month <- S_ALK %>%
  group_by(Month) %>%
  dplyr::mutate(N = n()) %>%
  mutate(MeanTA = mean(TALK_umol.Kg))


#Creating linear graph of # of observation per month
ggplot(data = N_month,
       aes(x = Month,
           y = N)) +
  geom_point() +
  geom_line() +
  theme_bw(base_size = 16) +
  ggtitle("Number of Observations By Month") +
  labs(subtitle = "North Bay 2018-2022") +
  xlab("Month") +
  ylab("# of Observations")

#Creating linear graph of MeanTA per month
ggplot(data = N_month,
       aes(x = Month,
           y = MeanTA)) +
  geom_point() +
  geom_line() +
  theme_bw(base_size = 16) +
  ggtitle("Mean Total Alkalinity By Month") +
  labs(subtitle = "North Bay 2018-2022") +
  xlab("Month") +
  ylab("Total Alkalinity (umol/Kg)")


