#### Calculation of Total Alkalinity ####



# Total Alkalinity from a Gran Titration
# Section 7.3 of the SOP 3b in the Guide to Best Practices for Ocean CO2 Measurements (version 3.01): Gran Titration to calculate Alkalinity.
# Sections 1, 6.1, 7.2.1, and 7.2.4 of Chapter 5 in the Guide to Best Practices for Ocean CO2 Measurements (version 3.01): Constants used in Section 7.3



# Clear all plots and data

rm(list = ls())

# Load Packages
# MASS package must be installed

library(MASS)
library(readxl)
library(pracma)
library(NlcOptim)

# Get the working directory

getwd()

# Set your working directory to the directory that contains the pH data file on your computer

setwd("/Users/918846310/Documents/EOS_SFSU Research Tech/Nielsen Lab/Alkalinity/Processing Data/TALK") 

# List all files in the working directory

list.files()

# Enter your TALK data file name: use the output from list files function to copy and paste just the file name (leave off .csv) into the prompt

enterfilename <- readline(prompt="Enter file name: ")

# Index and rename TALK data file to produce results document later

filename <- enterfilename

results <- paste0(filename, "_processed")

# Index and read TALK data file
# readxl package must be installed

data <- read_excel(paste0(filename,".xlsx"))

# Add desired columns into file for calculation outputs

data$TA <- NA

data$E0 <- NA


## Indexing Data ##

# Mass of Sample in g

m0 <- head(data$m0,1)

# Salinity of Sample

Salinity <- head(data$Salinity,1)

# Temperature of Laboratory in Degree C, changed from Degree F

TempLabC <- (5/9) * (head(data$TempLab,1)-32)

# Concentration of HCl in mol/kg

Conc <- head(data$conc_HCl,1)  

# Density of HCl in g/cm^3; *IMPORTANT* the equation will change with a different batch used!
# Comment in (delete "#") or comment out (insert "#") to use a certain Batch equation 

# BATCH A22
# dHCl <- 1.02881 - ((1.1116 * 10^-4) * TempLabC) - ((4.00 * 10^-6) * (TempLabC^2))

# BATCH A25
dHCl <- 1.02887 - ((1.108 * 10^-4) * TempLabC) - ((4.00 * 10^-6) * (TempLabC^2))

# HCl Doses in g, changed from mL

mass_HCl <- (data$corr_vHCl[!is.na(data$corr_vHCl)]) * dHCl

# pH change in V, changed from mV

E <- (data$P2mV[!is.na(data$P2mV)]) / 1000

# Temperature of Sample in Degree K, changed from Degree C

TempK <- (data$P2Temp[!is.na(data$P2Temp)]) + 273.15



## Creating Estimates ##

# Gran Function

F1 <- (m0 + mass_HCl) * exp(E / ((8.314472 * TempK) / 96485.3399))

# Creates a table of data for Linear regression

regression.data <-data.frame(mass_HCl,F1)

# Linear regression

regression <- lm(F1 ~ mass_HCl, data = regression.data)

# Stores linear regression coefficient data 

matrix_coef <- summary(regression)$coefficients

# Pulls slope values from matrix

slope <- matrix_coef[2,1]

# Pulls y intercept values from matrix

yintercept <- matrix_coef[1,1]

# Zero point of regression

zero_m = (-1 * yintercept) / slope

# Estimate of Total Alkalinity in mol/kg 

TA_EST <- (zero_m * Conc) / m0

# Estimate of E0 in V

E0_EST <- E - ((8.314472 * TempK) / 96485.3399) * log(((-m0 * TA_EST) + (mass_HCl * Conc)) / (m0 + mass_HCl))

# Mean of EO_EST

E0_mean <- mean(E0_EST)

# Total Hydrogen ion Concentration 

H <- exp((E - E0_mean) / ((8.314472 * TempK) / 96485.3399))

# Estimate of f

f_EST <- 1


## Establishing Constants ##
  
# Ionic Strength of Sample

I_mo <- (19.924 * (Salinity)) / (1000 - (1.005 * (Salinity)))

# Sulfate Concentration

ST <- 0.02824 * (Salinity/35)

# Dissociation Constant of HSO4- in mol/kg

KS <- exp((-4276.1 / TempK) + 141.328 - (23.093 * log(TempK))  + (((-13856 / TempK) + 324.57 - (47.986 * log(TempK))) * (I_mo^0.5))  +  (((35474 / TempK) - 771.54 + (114.723 * log(TempK))) * I_mo)  - ((2698 / TempK) * (I_mo^1.5))  +  ((1776 / TempK) * (I_mo^2))  +  log(1 - (0.001005 * Salinity)))
 
# Fluoride Concentration
  
FT <- 0.00007 * (Salinity/35)

# Dissociation Constant of HF in mol/kg

KF <- exp((874 / TempK) - 9.68 + (0.111 * (Salinity^0.5)))

# Z

Z <- 1 + (ST/KS)


## Non-linear Least Squares to Best Fit Data to Adjust TA and E0 ##

# MUST HAVE 'pracma' and 'NlcOptim' PACKAGES INSTALLED!

# Defining Sum of Squares Function

Sumsq_Calc <- function(TA_EST, f_EST, ST, KS, H, Z, FT, KF, m0, mass_HCl, Conc) {
  
  Del <- TA_EST + ((ST) / (1 + ((KS * Z) / (f_EST * H)))) + ((FT) / (1 + (KF / (f_EST * H)))) + (((m0 + mass_HCl) / m0) * ((f_EST * H) / Z)) - ((mass_HCl / m0) * Conc)
  
  Sum_Sq <- 1000000000000 * sum(Del^2)
  
  }

# Indexing starting points

x0 <- c(TA_EST, f_EST) 

# Indexing function (Sum of Squares) to be minimized by varying TA_EST and f_EST

fn <- function(x0) Sumsq_Calc(x0[1], x0[2], ST, KS, H, Z, FT, KF, m0, mass_HCl, Conc)

# Setting lower boundaries for TA_EST and f_EST

lb_1 <- c(TA_EST - 0.000050, f_EST - 0.5)

# Setting upper boundaries for TA_EST and f_EST

ub_1 <- c(TA_EST + 0.000050, f_EST + 0.5)

# Using fmincon to perform nonlinear least squares minimization 

res <- fmincon(x0, fn, lb = lb_1, ub = ub_1)

# Adjusted TA 

TA <- res$par[1] * 1000000

# Adjusted f

f <- res$par[2]

# Adjusted Sum of Squares Value

Sum_square <- res$value

# Adjusted E0

E0 <- mean(E0_mean + (((8.314472 * TempK) / 96485.3399) * log(f)))


# Appends calculation results to data file

data$TA <- TA

data$E0 <- E0

# Saves a new csv file with appended results 

write.csv(data, file = paste0(results,".csv"), row.names = F) 


