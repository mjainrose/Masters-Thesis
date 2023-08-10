#### Calculation of Total Alkalinity ####



# The following variables and calculations are worked up as written in section 7.3 of
# the SOP 3b (version 3.01) in the Guide to Best Practices for Ocean CO2 Measurements



# Clear all plots and data

rm(list = ls())

# Load Packages
library(readxl)
library(pracma)
library(NlcOptim)

# Get the working directory
##set working directory 

# List all files in the working directory

list.files()

# Enter your pH data file name: use the output from list files function to copy and paste just the file name (leave off .csv) into the prompt

enterfilename <- readline(prompt="Enter file name: ")

# Index and rename pH data file to produce results document later

filename <- enterfilename

results <- paste0(filename, "_processed")

# Index and read pH data file
# readxl package must be installed

data <- read_excel(paste0(filename,".xlsx"))

# Add desired columns into file for calculation outputs

data$TA <- NA

data$E0 <- NA


## Indexing Data ##

# Mass of Sample in g

mass <- data$m0

m0 <- head(mass,1)

#  of Sample

Sal <- data$Salinity
  
Salinity <- head(Sal,1)

# Temperature of Sample in Degree C

TempC <- data$EQPTemp

# Temperature of Sample in Degree K

TempK <- TempC + 273.15

# Temperature of Laboratory in Degree F

Temp2 <- data$TempLab

TempLabF <- head(Temp2,1)

# Temperature of Laboratory in Degree C

TempLabC <- (5 / 9) * (TempLabF - 32)

# Density of Batch A25 HCl in g/cm^3; equation will change with a different batch used

dHCl <- 1.02887 - ((1.108 * 10^-4) * TempLabC) - ((4.00 * 10^-6) * (TempLabC^2))
 
# Concentration of HCl in mol/kg

Con <- data$conc_HCl

Conc <- head(Con,1)  

# HCl Doses in mL

Doses <- data$corr_vHCl

# HCl Doses in g

mass_HCl <- Doses * dHCl

# pH change in mV

mV <- data$EQPmV

# pH change in V

E <- mV / 1000


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

#Zero point of regression

zero_m = (-1 * yintercept) / slope

# Estimate of Total Alkalinity in mol/kg 

TA_EST <- (zero_m * Conc) / m0

# Estimate of E0 in V

E0_EST <- E - ((8.314472 * TempK) / 96485.3399) * log(((-m0 * TA_EST) + (mass_HCl * Conc)) / (m0 + mass_HCl))

# Mean of EO_EST

E0_mean <- mean(E0_EST)

# Total Hydrogen ion Concentration 

H <- exp((E - E0_mean) / ((8.314472 * TempK) / 96485.3399))

#Estimate of f

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

lb_1 <- c(TA_EST - 0.000020, f_EST - 0.5)

# Setting upper boundaries for TA_EST and f_EST

ub_1 <- c(TA_EST + 0.000020, f_EST + 0.5)

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


