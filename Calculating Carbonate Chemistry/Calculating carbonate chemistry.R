###calculating carbonate chemistry parameters for each tidal site using seacarb###


#loading packages
library(seacarb)


#set the working directory
##by using -> setwd()


#Uploading Tidal processed data for All sites
RB_tidal <- read.csv("RB_carb_tidal.csv", header = TRUE)

EOS_tidal <- read.csv("EOS_carb_tidal.csv", header = TRUE)

GC_tidal <- read.csv("GC_carb_tidal.csv", header = TRUE)

CC_tidal <- read.csv("CC_carb_tidal.csv", header = TRUE)


#add desired columns into each file for calculation outputs
RB_tidal$Arag <- NA
RB_tidal$Calcite <- NA

EOS_tidal$Arag <- NA
EOS_tidal$Calcite <- NA

GC_tidal$Arag <- NA
GC_tidal$Calcite <- NA

CC_tidal$Arag <- NA
CC_tidal$Calcite <- NA



#RB


#creating a for loop to loop through seacarb function for RB samples
for (i in 1:nrow(RB_tidal)) {
  
  #Obtaining values from each row
  S <- RB_tidal$Salinity[i]
  T <- RB_tidal$Temperature[i]
  Alk <- RB_tidal$TAlk_measured_average_mol_kg[i]
  pH <- RB_tidal$pH_corrected_average[i]
  
  #Calculating seawater carbonate chemistry w/seacarb
  result <-
    carb(
      flag = 8,
      var1 = pH,
      var2 = Alk,
      S = S,
      T = T,
      Patm = 1,
      P = 0,
      Pt = 0,
      Sit = 0,
      k1k2 = "l",
      kf = "pf",
      ks = "d",
      pHscale = "T",
      b = "l10",
      gas = "potential",
      warn = "y",
      eos = "eos80"
    )
  
  #storing results in data frame
  RB_tidal$Arag[i] <- result$OmegaAragonite
  RB_tidal$Calcite[i] <- result$OmegaCalcite
  RB_tidal$DIC[i] <- result$DIC / (10^-6)

}

#saving a new csv file with appended results for RB
write.csv(RB_tidal, "RB_carb_tidal.csv", row.names = F)


#EOS



#creating a for loop to loop through seacarb function for EOS samples
for (i in 1:nrow(EOS_tidal)) {
  
  #Obtaining values from each row
  S <- EOS_tidal$Salinity[i]
  T <- EOS_tidal$Temperature[i]
  Alk <- EOS_tidal$TAlk_measured_average_mol_kg[i]
  pH <- EOS_tidal$pH_corrected_average[i]
  
  #Calculating seawater carbonate chemistry w/seacarb
  result <-
    carb(
      flag = 8,
      var1 = pH,
      var2 = Alk,
      S = S,
      T = T,
      Patm = 1,
      P = 0,
      Pt = 0,
      Sit = 0,
      k1k2 = "l",
      kf = "pf",
      ks = "d",
      pHscale = "T",
      b = "l10",
      gas = "potential",
      warn = "y",
      eos = "eos80"
    )
  
  #storing results in data frame
  EOS_tidal$Arag[i] <- result$OmegaAragonite
  EOS_tidal$Calcite[i] <- result$OmegaCalcite
  EOS_tidal$DIC[i] <- result$DIC / (10^-6)
  
}

#saving a new csv file with appended results for EOS
write.csv(EOS_tidal, "EOS_carb_tidal.csv", row.names = F)



#GC



#creating a for loop to loop through seacarb function for GC samples
for (i in 1:nrow(GC_tidal)) {
  
  #Obtaining values from each row
  S <- GC_tidal$Salinity[i]
  T <- GC_tidal$Temperature[i]
  Alk <- GC_tidal$TAlk_measured_average_mol_kg[i]
  pH <- GC_tidal$pH_corrected_average[i]
  
  #Calculating seawater carbonate chemistry w/seacarb
  result <-
    carb(
      flag = 8,
      var1 = pH,
      var2 = Alk,
      S = S,
      T = T,
      Patm = 1,
      P = 0,
      Pt = 0,
      Sit = 0,
      k1k2 = "l",
      kf = "pf",
      ks = "d",
      pHscale = "T",
      b = "l10",
      gas = "potential",
      warn = "y",
      eos = "eos80"
    )
  
  #storing results in data frame
  GC_tidal$Arag[i] <- result$OmegaAragonite
  GC_tidal$Calcite[i] <- result$OmegaCalcite
  GC_tidal$DIC[i] <- result$DIC / (10^-6)
  
}

#saving a new csv file with appended results for GC
write.csv(GC_tidal, "GC_carb_tidal.csv", row.names = F)


#CC


#creating a for loop to loop through seacarb function for CC samples
for (i in 1:nrow(CC_tidal)) {
  
  #Obtaining values from each row
  S <- CC_tidal$Salinity[i]
  T <- CC_tidal$Temperature[i]
  Alk <- CC_tidal$TAlk_measured_average_mol_kg[i]
  pH <- CC_tidal$pH_corrected_average[i]
  
  #Calculating seawater carbonate chemistry w/seacarb
  result <-
    carb(
      flag = 8,
      var1 = pH,
      var2 = Alk,
      S = S,
      T = T,
      Patm = 1,
      P = 0,
      Pt = 0,
      Sit = 0,
      k1k2 = "l",
      kf = "pf",
      ks = "d",
      pHscale = "T",
      b = "l10",
      gas = "potential",
      warn = "y",
      eos = "eos80"
    )
  
  #storing results in data frame
  CC_tidal$Arag[i] <- result$OmegaAragonite
  CC_tidal$Calcite[i] <- result$OmegaCalcite
  CC_tidal$DIC[i] <- result$DIC / (10^-6)
  
}

#saving a new csv file with appended results for CC
write.csv(CC_tidal, "CC_carb_tidal.csv", row.names = F)

