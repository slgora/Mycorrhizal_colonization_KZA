#### BioStats MC Project- Data Analysis ####
###  Gora_MC-Project_Analysis_FINAL_BIO709.R
###  by Sarah Gora
###  Date created: April 18, 2022

# set WD 
setwd("~/Desktop/Biostats_2/Gora_MC_Project")

#load packages
install.packages("tidyr")
library(tidyr)
install.packages("dplyr")
library(dplyr)
install.packages("stats")
library(stats)
install.packages("lme4")
library(lme4)
install.packages("ggplot2")
library(ggplot2)
install.packages("visreg")
library(visreg)
install.packages("lattice")
library(lattice)
install.packages("minqa")
library(minqa)
install.packages("Rcpp")
library(Rcpp)



# Load data 
library(readr)
MC_Data <- read_csv("Gora_MC_Datasheet_FINAL.csv")

# View Data 
head(MC_Data)
View(MC_Data)
colnames(MC_Data)



#### Subset out MC Data by Species
DO_MC <- subset(MC_Data, Species=="Dichanthelium_oligosanthes") # N=34
AP_MC <- subset(MC_Data, Species=="Ambrosia_psilostachya")   # N=34
SM_MC <- subset(MC_Data, Species=="Solidago_missouriensis")  # N=30
AG_MC <- subset(MC_Data, Species=="Andropogon_gerardii")  # N=35
SN_MC <- subset(MC_Data, Species=="Sorghastrum_nutans")  # N=35


#### Subset out N0N10 MC data for each species 
DO_N0N10_MC <- subset(DO_MC, Treatment2 == "0"| Treatment2== "10")
AP_N0N10_MC <- subset(AP_MC, Treatment2 == "0"| Treatment2== "10")
SM_N0N10_MC <- subset(SM_MC, Treatment2 == "0"| Treatment2== "10")
AG_N0N10_MC <- subset(AG_MC, Treatment2 == "0"| Treatment2== "10")
SN_N0N10_MC <- subset(SN_MC, Treatment2 == "0"| Treatment2== "10")


#### Subset out N0 MC data for each species 
DO_N0_MC <- subset(DO_MC, Treatment2 == "0")
AP_N0_MC <- subset(AP_MC, Treatment2 == "0")
SM_N0_MC <- subset(SM_MC, Treatment2 == "0")
AG_N0_MC <- subset(AG_MC, Treatment2 == "0")
SN_N0_MC <- subset(SN_MC, Treatment2 == "0")

#### Subset out N10 MC data for each species 
DO_N10_MC <- subset(DO_MC, Treatment2== "10")
AP_N10_MC <- subset(AP_MC, Treatment2== "10")
SM_N10_MC <- subset(SM_MC, Treatment2== "10")
AG_N10_MC <- subset(AG_MC, Treatment2== "10")
SN_N10_MC <- subset(SN_MC, Treatment2== "10")





#### Subset out Change MC data for each species by treatment
DO_Ch_MC <- subset(DO_MC, Experiment == "Change")
AP_Ch_MC <- subset(AP_MC, Experiment == "Change")
SM_Ch_MC <- subset(SM_MC, Experiment == "Change")
AG_Ch_MC <- subset(AG_MC, Experiment == "Change")
SN_Ch_MC <- subset(SN_MC, Experiment == "Change")

# subset out data for boxplots 
DO_Ch_N0_MC <- subset(DO_Ch_MC, Treatment2== "0")
AP_Ch_N0_MC <- subset(AP_Ch_MC, Treatment2== "0")
SM_Ch_N0_MC <- subset(SM_Ch_MC, Treatment2== "0")
AG_Ch_N0_MC <- subset(AG_Ch_MC, Treatment2== "0")
SN_Ch_N0_MC <- subset(SN_Ch_MC, Treatment2== "0")

DO_Ch_N2_MC <- subset(DO_Ch_MC, Treatment2== "2.5")
AP_Ch_N2_MC <- subset(AP_Ch_MC, Treatment2== "2.5")
SM_Ch_N2_MC <- subset(SM_Ch_MC, Treatment2== "2.5")
AG_Ch_N2_MC <- subset(AG_Ch_MC, Treatment2== "2.5")
SN_Ch_N2_MC <- subset(SN_Ch_MC, Treatment2== "2.5")

DO_Ch_N10_MC <- subset(DO_Ch_MC, Treatment2== "10")
AP_Ch_N10_MC <- subset(AP_Ch_MC, Treatment2== "10")
SM_Ch_N10_MC <- subset(SM_Ch_MC, Treatment2== "10")
AG_Ch_N10_MC <- subset(AG_Ch_MC, Treatment2== "10")
SN_Ch_N10_MC <- subset(SN_Ch_MC, Treatment2== "10")

DO_Ch_N20_MC <- subset(DO_Ch_MC, Treatment2== "20")
AP_Ch_N20_MC <- subset(AP_Ch_MC, Treatment2== "20")
SM_Ch_N20_MC <- subset(SM_Ch_MC, Treatment2== "20")
AG_Ch_N20_MC <- subset(AG_Ch_MC, Treatment2== "20")
SN_Ch_N20_MC <- subset(SN_Ch_MC, Treatment2== "20")


################# DATA CLEANING #####################


# I had removed NAs
# Checked outliers with histograms 
# Did this in stage1: Gora_MC-Project_Analysis_Prelim_BIO709.R 



# Variables 
# Aboveground Biomass 
# Perc_MC_Colonized


# Stats Table for Aboveground Biomass 
# dropped range bc that doesnt tell me much more than the means 
# re-run means on updated FINAL dataset

###### Get Means for N0N10  ######

### DO 
summary(DO_N0_MC$Aboveground_Biomass)
# N0 mean: 0.2950
summary(DO_N10_MC$Aboveground_Biomass)
# N10 mean: 0.2855

### AP 
summary(AP_N0_MC$Aboveground_Biomass)
# N0 mean: 0.2055
summary(AP_N10_MC$Aboveground_Biomass)
# N10 mean: 0.9855

### SM
summary(SM_N0_MC$Aboveground_Biomass)
# N0 mean: 2.187
summary(SM_N10_MC$Aboveground_Biomass)
# N10 mean: 2.153

### AG
summary(AG_N0_MC$Aboveground_Biomass)
# N0 mean: 0.8717
summary(AG_N10_MC$Aboveground_Biomass)
# N10 mean: 1.659

### SN
summary(SN_N0_MC$Aboveground_Biomass)
# N0 mean: 0.5133
summary(SN_N10_MC$Aboveground_Biomass)
# N10 mean: 0.8308


###### Get AbG Means for Change  ######

### DO 
summary(DO_Ch_N0_MC$Aboveground_Biomass)
# N0 mean: 0.226
summary(DO_Ch_N2_MC$Aboveground_Biomass)
# N2.5 mean: 0.1433
summary(DO_Ch_N10_MC$Aboveground_Biomass)
# N10 mean: 0.162
summary(DO_Ch_N20_MC$Aboveground_Biomass)
# N20 mean: 0.5600

### AP 
summary(AP_Ch_N0_MC$Aboveground_Biomass)
# N0 mean: 0.2583
summary(AP_Ch_N2_MC$Aboveground_Biomass)
# N2.5 mean: 0.5383
summary(AP_Ch_N10_MC$Aboveground_Biomass)
# N10 mean: 0.816
summary(AP_Ch_N20_MC$Aboveground_Biomass)
# N20 mean: 1.272

### SM
summary(SM_Ch_N0_MC$Aboveground_Biomass)
# N0 mean: 2.888
summary(SM_Ch_N2_MC$Aboveground_Biomass)
# N2.5 mean: 4.128
summary(SM_Ch_N10_MC$Aboveground_Biomass)
# N10 mean: 2.872
summary(SM_Ch_N20_MC$Aboveground_Biomass)
# N20 mean: 4.055

### AG
summary(AG_Ch_N0_MC$Aboveground_Biomass)
# N0 mean: 0.8683
summary(AG_Ch_N2_MC$Aboveground_Biomass)
# N2.5 mean: 0.6633
summary(AG_Ch_N10_MC$Aboveground_Biomass)
# N10 mean: 0.986
summary(AG_Ch_N20_MC$Aboveground_Biomass)
# N20 mean: 1.0733

### SN
summary(SN_Ch_N0_MC$Aboveground_Biomass)
# N0 mean: 0.5250
summary(SN_Ch_N2_MC$Aboveground_Biomass)
# N2.5 mean: 0.6067
summary(SN_Ch_N10_MC$Aboveground_Biomass)
# N10 mean: 0.590
summary(SN_Ch_N20_MC$Aboveground_Biomass)
# N20 mean: 1.116




# Stats Table for %MC 
# dropped range bc that doesnt tell me much more than the means 

###### Get %MC Means for N0N10  ######

### DO 
summary(DO_N0_MC$Perc_MC_Colonized)
# N0 mean: 47.77 
summary(DO_N10_MC$Perc_MC_Colonized)
# N10 mean:  46.47

### AP 
summary(AP_N0_MC$Perc_MC_Colonized)
# N0 mean:  23.238
summary(AP_N10_MC$Perc_MC_Colonized)
# N10 mean:  30.115

### SM
summary(SM_N0_MC$Perc_MC_Colonized)
# N0 mean:  45.61
summary(SM_N10_MC$Perc_MC_Colonized)
# N10 mean:  53.70

### AG
summary(AG_N0_MC$Perc_MC_Colonized)
# N0 mean:  30.64
summary(AG_N10_MC$Perc_MC_Colonized)
# N10 mean:  27.792

### SN
summary(SN_N0_MC$Perc_MC_Colonized)
# N0 mean:  32.80
summary(SN_N10_MC$Perc_MC_Colonized)
# N10 mean:  42.94
 

###### Get %MC Means for Change  ######

### DO 
summary(DO_Ch_N0_MC$Perc_MC_Colonized)
# N0 mean: 51.79
summary(DO_Ch_N2_MC$Perc_MC_Colonized)
# N2.5 mean: 57.24
summary(DO_Ch_N10_MC$Perc_MC_Colonized)
# N10 mean: 61.06
summary(DO_Ch_N20_MC$Perc_MC_Colonized)
# N20 mean: 67.11

### AP 
summary(AP_Ch_N0_MC$Perc_MC_Colonized)
# N0 mean: 14.989
summary(AP_Ch_N2_MC$Perc_MC_Colonized)
# N2.5 mean:  21.003
summary(AP_Ch_N10_MC$Perc_MC_Colonized)
# N10 mean:  26.770
summary(AP_Ch_N20_MC$Perc_MC_Colonized)
# N20 mean: 18.237

### SM
summary(SM_Ch_N0_MC$Perc_MC_Colonized)
# N0 mean: 41.92
summary(SM_Ch_N2_MC$Perc_MC_Colonized)
# N2.5 mean:  57.52
summary(SM_Ch_N10_MC$Perc_MC_Colonized)
# N10 mean: 53.11
summary(SM_Ch_N20_MC$Perc_MC_Colonized)
# N20 mean:  14.989                           **** hmmmm 

### AG
summary(AG_Ch_N0_MC$Perc_MC_Colonized)
# N0 mean: 24.31
summary(AG_Ch_N2_MC$Perc_MC_Colonized)
# N2.5 mean:  30.36
summary(AG_Ch_N10_MC$Perc_MC_Colonized)
# N10 mean:  23.134
summary(AG_Ch_N20_MC$Perc_MC_Colonized)
# N20 mean:  26.818

### SN
summary(SN_Ch_N0_MC$Perc_MC_Colonized)
# N0 mean:  35.67
summary(SN_Ch_N2_MC$Perc_MC_Colonized)
# N2.5 mean:  46.66
summary(SN_Ch_N10_MC$Perc_MC_Colonized)
# N10 mean:  43.35
summary(SN_Ch_N20_MC$Perc_MC_Colonized)
# N20 mean:  45.00





# Run MIXED-MODEL ANOVAs... by Species for AbG Biomass & Perc_MC_Colonized... 

### Trait: Perc_MC_Colonized ###

# DO, N0N10
aov_DO_MCvTrt <- aov(Perc_MC_Colonized~Treatment2+(1/Experiment)+(1/Plot), data=DO_N0N10_MC)
summary(aov_DO_MCvTrt)
# p= 0.870, not significant


# AP, N0N10
aov_AP_MCvTrt <- aov(Perc_MC_Colonized~Treatment2+(1/Experiment)+(1/Plot), data=AP_N0N10_MC)
summary(aov_AP_MCvTrt)
# p= 0.433, not significant 

# SM, N0N10
aov_SM_MCvTrt <- aov(Perc_MC_Colonized~Treatment2+(1/Experiment)+(1/Plot), data=SM_N0N10_MC)
summary(aov_SM_MCvTrt)
# p= 0.409, not significant  ** changed in final

# AG, N0N10 ----- NOT ENOUGH DATA YET for Change --- put in later
aov_AG_MCvTrt <- aov(Perc_MC_Colonized~Treatment2+(1/Experiment)+(1/Plot), data=AG_N0N10_MC)
summary(aov_AG_MCvTrt)
# p= 0.662, not significant     ** changed in final

# SN, N0N10
aov_SN_MCvTrt <- aov(Perc_MC_Colonized~Treatment2+(1/Experiment)+(1/Plot), data=SN_N0N10_MC)
summary(aov_SN_MCvTrt)
# p= 0.137, not significant       ** changed


### FINAL N0N10 RESULTS: 
# no response in %MC to Nitrogen additions (10g)



######## Change Data

# Run mixed model ANOVAs... by species for %MC in N gradient

# DO, Change
aov_DO_Ch_MCvTrt <- aov(Perc_MC_Colonized~Treatment2+(1/Plot)+(1/Block), data=DO_Ch_MC)
summary(aov_DO_Ch_MCvTrt)
# p= 0.012, ** significant
model.tables(aov_DO_Ch_MCvTrt, "means")
TukeyHSD(aov_DO_Ch_MCvTrt) 
# Control: 53.80        *** changed in final
# 2.5g N: 55.52
# 10g N: 60.65
# 20g N: 67.50 ** p=0.04 ** significant

# AP,  Change
aov_AP_Ch_MCvTrt <- aov(Perc_MC_Colonized~Treatment2+(1/Plot)+(1/Block), data=AP_Ch_MC)
summary(aov_AP_Ch_MCvTrt)
# p= 0.389, not significant  ** changed in final
model.tables(aov_AP_Ch_MCvTrt, "means")
# Control: 19.144
# 2.5g N: 19.400
# 10g N: 20.167
# 20g N: 21.189
summary(AP_Ch_MC$Perc_MC_Colonized)


# SM,  Change
aov_SM_Ch_MCvTrt <- aov(Perc_MC_Colonized~Treatment2+(1/Plot)+(1/Block), data=SM_Ch_MC)
summary(aov_SM_Ch_MCvTrt)
# p= 0.492, not significant     * changed in final
model.tables(aov_SM_Ch_MCvTrt, "means")
# Control: 48.74
# 2.5g N: 49.80
# 10g N: 52.97
# 20g N: 57.20
summary(SM_Ch_MC$Perc_MC_Colonized)



# AG,  Change
aov_AG_Ch_MCvTrt <- aov(Perc_MC_Colonized~Treatment2+(1/Plot)+(1/Block), data=AG_Ch_MC)
summary(aov_AG_Ch_MCvTrt)
# p= 0.952, not significant         *** changed in final
model.tables(aov_AG_Ch_MCvTrt, "means")
# Control: 26.469
# 2.5g N: 26.413
# 10g N: 26.243
# 20g N: 26.016
summary(AG_Ch_MC$Perc_MC_Colonized)


# SN,  Change
aov_SN_Ch_MCvTrt <- aov(Perc_MC_Colonized~Treatment2+(1/Plot)+(1/Block), data=SN_Ch_MC)
summary(aov_SN_Ch_MCvTrt)
# p= 0.587, not significant 
model.tables(aov_SN_Ch_MCvTrt, "means")
# Control: 40.52
# 2.5g N: 41.19
# 2.5g N: 43.21
# 2.5g N: 46.90            *** changed in final
summary(SN_Ch_MC$Perc_MC_Colonized)



################################################################
############# FINAL RESULTS for %MC: ##########################
# %MC in Dican was the only species to respond to N 
# DO only responsive with 20g Nitrogen additions
# shown as an increase in %MC  with more N added
###############################################################
###############################################################






# Run mixed model ANOVAs... by Species for AbG Biomass ... 
### Trait: Aboveground_Biomass ###

DO_N0N10_MC$Treatment2 <- as.factor(DO_N0N10_MC$Treatment2)
AP_N0N10_MC$Treatment2 <- as.factor(AP_N0N10_MC$Treatment2)
SM_N0N10_MC$Treatment2 <- as.factor(SM_N0N10_MC$Treatment2)
AG_N0N10_MC$Treatment2 <- as.factor(AG_N0N10_MC$Treatment2)
SN_N0N10_MC$Treatment2 <- as.factor(SN_N0N10_MC$Treatment2)

# DO, N0N10
aov_DO_AbGvTrt <- aov(Aboveground_Biomass~Treatment2+(1/Experiment)+(1/Plot), data=DO_N0N10_MC)
summary(aov_DO_AbGvTrt)
# p= 0.579, not significant    
model.tables(aov_DO_AbGvTrt, "means")
# Control: 0.24273
# 10g N: 0.28545
summary(DO_N0N10_MC$Aboveground_Biomass)


# AP, N0N10
aov_AP_AbGvTrt <- aov(Aboveground_Biomass~Treatment2+(1/Experiment)+(1/Plot), data=AP_N0N10_MC)
summary(aov_AP_AbGvTrt)
# p<0.001, *** significant
model.tables(aov_AP_AbGvTrt, "means")
# Control: 0.2055
# 10g N: 0.9855
summary(AP_N0N10_MC$Aboveground_Biomass)


# SM, N0N10
aov_SM_AbGvTrt <- aov(Aboveground_Biomass~Treatment2+(1/Experiment)+(1/Plot), data=SM_N0N10_MC)
summary(aov_SM_AbGvTrt)
# p= 0.540, not significant 
model.tables(aov_SM_AbGvTrt, "means")
# Control: 1.8233
# 10g N: 2.1942
summary(SM_N0N10_MC$Aboveground_Biomass)


# AG, N0N10 
aov_AG_AbGvTrt <- aov(Aboveground_Biomass~Treatment2+(1/Plot), data=AG_N0N10_MC)
summary(aov_AG_AbGvTrt)
# p= 0.001, ** Significant
model.tables(aov_AG_AbGvTrt, "means")
# Control: 0.728
# 10g N: 2.452
summary(AG_N0N10_MC$Aboveground_Biomass)


# SN, N0N10
aov_SN_AbGvTrt <- aov(Aboveground_Biomass~Treatment2+(1/Experiment)+(1/Plot), data=SN_N0N10_MC)
summary(aov_SN_AbGvTrt)
# p= 0.0262, ** significant 
model.tables(aov_SN_AbGvTrt, "means")
# Control: 0.513
# 10g N: 0.831
summary(SN_N0N10_MC$Aboveground_Biomass)


### FINAL N0N10 RESULTS: Aboveground Biomass
# AG, AP, SN had a significant response in 
# Aboveground Biomass to 10g Nitrogen additions

# 3/5 species responsive in Aboveground biomass w 10N additions




# Change Data

# Run mixed model ANOVAs... by species for AbG in N gradient

DO_Ch_MC$Treatment2 <- as.factor(DO_Ch_MC$Treatment2)
AP_Ch_MC$Treatment2 <- as.factor(AP_Ch_MC$Treatment2)
SM_Ch_MC$Treatment2 <- as.factor(SM_Ch_MC$Treatment2)
AG_Ch_MC$Treatment2 <- as.factor(AG_Ch_MC$Treatment2)
SN_Ch_MC$Treatment2 <- as.factor(SN_Ch_MC$Treatment2)


# DO, Change
aov_DO_Ch_AbGvTrt <- aov(Aboveground_Biomass~Treatment+(1/Plot)+(1/Block), data=DO_Ch_MC)
summary(aov_DO_Ch_AbGvTrt)
# p= 0.0628, not significant     **changed in final
model.tables(aov_DO_Ch_AbGvTrt, "means")
TukeyHSD(aov_DO_Ch_AbGvTrt) 
# Control: 0.226
#  2.5g N: 0.1433
#   10g N: 0.162
#   20g N: 0.56
summary(DO_Ch_MC$Aboveground_Biomass)



# AP,  Change
aov_AP_Ch_AbGvTrt <- aov(Aboveground_Biomass~Treatment+(1/Plot)+(1/Block), data=AP_Ch_MC)
summary(aov_AP_Ch_AbGvTrt)
# p= 0.0042, *** significant 
model.tables(aov_AP_Ch_AbGvTrt, "means")
TukeyHSD(aov_AP_Ch_AbGvTrt)       # changed in final
# Control: 0.2583
# 2.5g N: 0.5383
#  10g N: 0.816
#  20g N: 1.272   ** significant 

### 20g N: adj-p= 0.035, *** Significant
summary(AP_Ch_MC$Aboveground_Biomass)


# SM,  Change
aov_SM_Ch_AbGvTrt <- aov(Aboveground_Biomass~Treatment2+(1/Plot)+(1/Block), data=SM_Ch_MC)
summary(aov_SM_Ch_AbGvTrt)
# p= 0.636, not significant 
model.tables(aov_SM_Ch_AbGvTrt, "means")
TukeyHSD(aov_SM_Ch_AbGvTrt)
# Control: 3.277 
# 2.5g N: 3.349
#  10g N: 3.568
#  20g N: 3.859
summary(SM_Ch_MC$Aboveground_Biomass)



# AG,  Change 
aov_AG_Ch_AbGvTrt <- aov(Aboveground_Biomass~Treatment2+(1/Plot)+(1/Block), data=AG_Ch_MC)
summary(aov_AG_Ch_MCvTrt)
# p= 0.621, not significant
model.tables(aov_AG_Ch_AbGvTrt, "means")
TukeyHSD(aov_AG_Ch_AbGvTrt)
# Control: 0.7449
# 2.5g N: 0.7816
# 10g N: 0.8917
# 20g N: 1.0385
summary(AG_Ch_MC$Aboveground_Biomass)


# SN,  Change
aov_SN_Ch_AbGvTrt <- aov(Aboveground_Biomass~Treatment+(1/Plot)+(1/Block), data=SN_Ch_MC)
summary(aov_SN_Ch_AbGvTrt)
# p= 0.155, not significant 
model.tables(aov_SN_Ch_AbGvTrt, "means")
TukeyHSD(aov_SN_Ch_AbGvTrt)
# Control: 0.5087
# 2.5g N: 0.5583
# 2.5g N: 0.7072
# 2.5g N: 0.9058
summary(SN_Ch_MC$Aboveground_Biomass)



############## FINAL RESULTS: Aboveground Biomass #################
#  AG, AP, SN  had a significant response in AbG Biomass to 10g N additions

#  AP  had a significant response in AbG in 20g N as well 
####################################################################






############ RAMF #########################3

devtools::install_github("mchiapello/Ramf")

# error 

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("BiocStyle")

devtools::install_github("mchiapello/Ramf")


## Load library
library(Ramf)

## Read data in
f <- dir(system.file("extdata", package = "Ramf"), full.names = TRUE, pattern = "grid.csv")
x <- readData(f, type = "grid")


## Summary of the data
am_summary(x)

## Plot
am_barplot(x)
am_boxplot(x)
am_dotplot(x)

# Plot with different display
am_barplot2(x)
am_boxplot2(x)
am_dotplot2(x)


## Statistics
am_stat(x)
am_stat(x, methods = "BH")


## Plot with statistics
# the original code had a typo
am_barplot(x, annot = "asterisks")

## Plot with statistics
am_barplot(x, annot = "letters")


## Save summary data
am_save(am_summary(x), "My_data") # 2 files will be save: "My_data_per_Sample.csv" and "My_data_per_Replicate.csv"

View(My_data)

## Save plot data
am_save(am_dotplot(x), "RPlot.jpg")
am_save(am_dotplot(x), "RPlot.pdf", unit = "cm", width = 20, height = 20, dpi = 300) # set image unit, dimention and quality
am_save(am_dotplot(x), "RPlot.png", width = 7, height = 7)


?Ramf()

install.packages("remotes")
remotes::install_github("mchiapello/InteRamf")

## Load library
library(InteRamf)

?InteRamf()

### what abt linear regressions? 
### what abt relationship betweem %MC and Aboveground Biomass?



## use regressions 
# 



# biomass on y 
# %MC on X 

# See if they are related by Treatment by species 
# put them all on the same graph- different colors for different treatments
# 5 graphs - 1 graph per species 


# above-ground_biomass continuous variable predictor


# DO
# N0 
lm_DO_N0_AbGvMC <- lmer(Perc_MC_Colonized~Aboveground_Biomass + (Aboveground_Biomass| Experiment), data=DO_N0_MC)
summary(lm_DO_N0_AbGvMC)

# plot 
pred_DO_N0_MC <- predict(lm_DO_N0_AbGvMC, newdata = DO_N0_MC, type = "response") # create a vector of predicted values for Chicks
pred_DO_N0_MC_df <- data.frame(DO_N0_MC, pred_DO_N0_MC) #bind those predictions to the original data frame

ggplot(data = pred_DO_N0_MC_df, aes(Aboveground_Biomass, Perc_MC_Colonized)) + 
  geom_point() +
  geom_line(aes(Aboveground_Biomass, pred_DO_N0_MC), col = "dodgerblue") + # set new aesthetic for the predicted Y
  facet_wrap(vars(Experiment))+ 
  labs(title="General Linear Model for %MC vs Aboveground Biomass with Experiment and Plot as Radndom Effects",
       x ="Aboveground_Biomass", y = "Perc_MC_Colonized")


#### try 2 #### 

install.packages("lmerTest")
library(lmerTest)

### All control Plots ###
lmer_N0_AbGvMC <- lmerTest::lmer(Perc_MC_Colonized~Aboveground_Biomass +(Aboveground_Biomass | Plot)+(Aboveground_Biomass | Experiment), data=N0_MC,
                                    control=lmerControl(check.nobs.vs.nlev = "ignore",
                                                        check.nobs.vs.rankZ = "ignore",
                                                        check.nobs.vs.nRE="ignore"))

summary(lmer_N0_AbGvMC)
# Correlation of Fixed Effects:
# Abvgrnd_Bms  -0.609

# Catepillar Plot
lm_N0_AbGvMC_ranef <- ranef(lmer_N0_AbGvMC)
head(lm_N0_AbGvMC_ranef)
dotplot(lm_N0_AbGvMC_ranef)
# almost no variation within plots for Control plots


lm_DO_N2_AbGvMC <- lmer(Perc_MC_Colonized~Aboveground_Biomass + (Block | Plot ), data=DO_Ch_N2_MC)
lm_DO_N10_AbGvMC <- lmer(Perc_MC_Colonized~Aboveground_Biomass + (Aboveground_Biomass| Experiment), data=DO_N10_MC)
lm_DO_N20_AbGvMC <- lmer(Perc_MC_Colonized~Aboveground_Biomass + (Aboveground_Biomass| Experiment), data=DO_Ch_N20_MC)




### DO N0 Plots ###
lmer_DO_N0_AbGvMC <- lmerTest::lmer(Perc_MC_Colonized~Aboveground_Biomass +(Aboveground_Biomass | Plot)+(Aboveground_Biomass | Experiment), data=DO_N0_MC,
                                    control=lmerControl(check.nobs.vs.nlev = "ignore",
                                                        check.nobs.vs.rankZ = "ignore",
                                                        check.nobs.vs.nRE="ignore"))

summary(lmer_DO_N0_AbGvMC)
# Correlation of Fixed Effects:
# Abvgrnd_Bms  -0.974

# Catepillar Plot
lm_DO_N0_AbGvMC_ranef <- ranef(lmer_DO_N0_AbGvMC)
head(lm_DO_N0_AbGvMC_ranef)
dotplot(lm_DO_N0_AbGvMC_ranef)
# variation within plots for Control plots for DO


### DO N2.5 ###
lmer_DO_N2_AbGvMC <- lmerTest::lmer(Perc_MC_Colonized~Aboveground_Biomass +(Aboveground_Biomass | Plot)+(Aboveground_Biomass | Block), data=DO_Ch_N2_MC,
                                    control=lmerControl(check.nobs.vs.nlev = "ignore",
                                                        check.nobs.vs.rankZ = "ignore",
                                                        check.nobs.vs.nRE="ignore"))
summary(lmer_DO_N2_AbGvMC)
#  -0.998

# catepillar plot 
lmer_DO_N2_AbGvMC_ranef <- ranef(lmer_DO_N2_AbGvMC)
head(lmer_DO_N2_AbGvMC_ranef)
dotplot(lmer_DO_N2_AbGvMC_ranef)
#variation within Blocks 




### all DO N10 Plots ####
lmer_DO_N10_AbGvMC <- lmerTest::lmer(Perc_MC_Colonized~Aboveground_Biomass +(Aboveground_Biomass | Plot)+(Aboveground_Biomass | Experiment), data=DO_N10_MC,
                                    control=lmerControl(check.nobs.vs.nlev = "ignore",
                                                        check.nobs.vs.rankZ = "ignore",
                                                        check.nobs.vs.nRE="ignore"))

summary(lmer_DO_N10_AbGvMC)
# correlation of fixed effects

# Catepillar Plot
lmer_DO_N10_AbGvMC_ranef <- ranef(lmer_DO_N10_AbGvMC)
head(lmer_DO_N10_AbGvMC_ranef)
dotplot(lmer_DO_N10_AbGvMC_ranef)
# variation within 10g N plots for 




### DO N20 ###
lmer_DO_N20_AbGvMC <- lmerTest::lmer(Perc_MC_Colonized~Aboveground_Biomass +(Aboveground_Biomass | Plot)+(Aboveground_Biomass | Block), data=DO_Ch_N20_MC,
                                    control=lmerControl(check.nobs.vs.nlev = "ignore",
                                                        check.nobs.vs.rankZ = "ignore",
                                                        check.nobs.vs.nRE="ignore"))
summary(lmer_DO_N20_AbGvMC)
#  -0.945

# catepillar plot 
lmer_DO_N20_AbGvMC_ranef <- ranef(lmer_DO_N20_AbGvMC)
head(lmer_DO_N20_AbGvMC_ranef)
dotplot(lmer_DO_N20_AbGvMC_ranef)
#variation within Blocks 



### AP Cat Plots ### 

### AP N0 Plots ###
lmer_AP_N0_AbGvMC <- lmerTest::lmer(Perc_MC_Colonized~Aboveground_Biomass +(Aboveground_Biomass | Plot)+(Aboveground_Biomass | Experiment), data=AP_N0_MC,
                                    control=lmerControl(check.nobs.vs.nlev = "ignore",
                                                        check.nobs.vs.rankZ = "ignore",
                                                        check.nobs.vs.nRE="ignore"))

summary(lmer_AP_N0_AbGvMC)
# Correlation of Fixed Effects:
# Abvgrnd_Bms   0.075

# Catepillar Plot
lm_AP_N0_AbGvMC_ranef <- ranef(lmer_AP_N0_AbGvMC)
head(lm_AP_N0_AbGvMC_ranef)
dotplot(lm_AP_N0_AbGvMC_ranef)
# variation within plots for Control plots for AP


### AP N2.5 ###
lmer_AP_N2_AbGvMC <- lmerTest::lmer(Perc_MC_Colonized~Aboveground_Biomass +(Aboveground_Biomass | Plot)+(Aboveground_Biomass | Block), data=AP_Ch_N2_MC,
                                    control=lmerControl(check.nobs.vs.nlev = "ignore",
                                                        check.nobs.vs.rankZ = "ignore",
                                                        check.nobs.vs.nRE="ignore"))
summary(lmer_AP_N2_AbGvMC)
#  -0.711

# catepillar plot 
lmer_AP_N2_AbGvMC_ranef <- ranef(lmer_AP_N2_AbGvMC)
head(lmer_AP_N2_AbGvMC_ranef)
dotplot(lmer_AP_N2_AbGvMC_ranef)
#variation within Blocks 




### all AP N10 Plots ####
lmer_AP_N10_AbGvMC <- lmerTest::lmer(Perc_MC_Colonized~Aboveground_Biomass +(Aboveground_Biomass | Plot)+(Aboveground_Biomass | Experiment), data=AP_N10_MC,
                                     control=lmerControl(check.nobs.vs.nlev = "ignore",
                                                         check.nobs.vs.rankZ = "ignore",
                                                         check.nobs.vs.nRE="ignore"))

summary(lmer_AP_N10_AbGvMC)
# correlation of fixed effects
#   -0.958

# Catepillar Plot
lmer_AP_N10_AbGvMC_ranef <- ranef(lmer_AP_N10_AbGvMC)
head(lmer_AP_N10_AbGvMC_ranef)
dotplot(lmer_AP_N10_AbGvMC_ranef)
# variation within 10g N plots for 




### AP N20 ###
lmer_AP_N20_AbGvMC <- lmerTest::lmer(Perc_MC_Colonized~Aboveground_Biomass +(Aboveground_Biomass | Plot)+(Aboveground_Biomass | Block), data=AP_Ch_N20_MC,
                                     control=lmerControl(check.nobs.vs.nlev = "ignore",
                                                         check.nobs.vs.rankZ = "ignore",
                                                         check.nobs.vs.nRE="ignore"))
summary(lmer_AP_N20_AbGvMC)
#  -0.909

# catepillar plot 
lmer_AP_N20_AbGvMC_ranef <- ranef(lmer_AP_N20_AbGvMC)
head(lmer_AP_N20_AbGvMC_ranef)
dotplot(lmer_AP_N20_AbGvMC_ranef)
#variation within Blocks 




### SM Cat Plots ### 

### SM N0 Plots ###
lmer_SM_N0_AbGvMC <- lmerTest::lmer(Perc_MC_Colonized~Aboveground_Biomass +(Aboveground_Biomass | Plot)+(Aboveground_Biomass | Experiment), data=SM_N0_MC,
                                    control=lmerControl(check.nobs.vs.nlev = "ignore",
                                                        check.nobs.vs.rankZ = "ignore",
                                                        check.nobs.vs.nRE="ignore"))

summary(lmer_SM_N0_AbGvMC)
# Correlation of Fixed Effects:
# Abvgrnd_Bms  -0.884

# Catepillar Plot
lm_SM_N0_AbGvMC_ranef <- ranef(lmer_SM_N0_AbGvMC)
head(lm_SM_N0_AbGvMC_ranef)
dotplot(lm_SM_N0_AbGvMC_ranef)
# variation within plots for Control plots for AP


### SM N2.5 ###
lmer_SM_N2_AbGvMC <- lmerTest::lmer(Perc_MC_Colonized~Aboveground_Biomass +(Aboveground_Biomass | Plot)+(Aboveground_Biomass | Block), data=SM_Ch_N2_MC,
                                    control=lmerControl(check.nobs.vs.nlev = "ignore",
                                                        check.nobs.vs.rankZ = "ignore",
                                                        check.nobs.vs.nRE="ignore"))
summary(lmer_SM_N2_AbGvMC)
#  -0.994

# catepillar plot 
lmer_SM_N2_AbGvMC_ranef <- ranef(lmer_SM_N2_AbGvMC)
head(lmer_SM_N2_AbGvMC_ranef)
dotplot(lmer_SM_N2_AbGvMC_ranef)
#variation within Blocks 




### all SM N10 Plots ####
lmer_SM_N10_AbGvMC <- lmerTest::lmer(Perc_MC_Colonized~Aboveground_Biomass +(Aboveground_Biomass | Plot)+(Aboveground_Biomass | Experiment), data=SM_N10_MC,
                                     control=lmerControl(check.nobs.vs.nlev = "ignore",
                                                         check.nobs.vs.rankZ = "ignore",
                                                         check.nobs.vs.nRE="ignore"))

summary(lmer_SM_N10_AbGvMC)
# correlation of fixed effects
#   -0.925

# Catepillar Plot
lmer_SM_N10_AbGvMC_ranef <- ranef(lmer_SM_N10_AbGvMC)
head(lmer_SM_N10_AbGvMC_ranef)
dotplot(lmer_SM_N10_AbGvMC_ranef)
# variation within 10g N plots for 



### SM N20 ###
lmer_SM_N20_AbGvMC <- lmerTest::lmer(Perc_MC_Colonized~Aboveground_Biomass +(Aboveground_Biomass | Plot)+(Aboveground_Biomass | Block), data=SM_Ch_N20_MC,
                                     control=lmerControl(check.nobs.vs.nlev = "ignore",
                                                         check.nobs.vs.rankZ = "ignore",
                                                         check.nobs.vs.nRE="ignore"))
summary(lmer_SM_N20_AbGvMC)
#  -0.924

# catepillar plot 
lmer_SM_N20_AbGvMC_ranef <- ranef(lmer_SM_N20_AbGvMC)
head(lmer_SM_N20_AbGvMC_ranef)
dotplot(lmer_SM_N20_AbGvMC_ranef)
#variation within Blocks 




### AG Cat Plots ### 

### AG N0 Plots ###
lmer_AG_N0_AbGvMC <- lmerTest::lmer(Perc_MC_Colonized~Aboveground_Biomass +(Aboveground_Biomass | Plot)+(Aboveground_Biomass | Experiment), data=AG_N0_MC,
                                    control=lmerControl(check.nobs.vs.nlev = "ignore",
                                                        check.nobs.vs.rankZ = "ignore",
                                                        check.nobs.vs.nRE="ignore"))

summary(lmer_AG_N0_AbGvMC)
# Correlation of Fixed Effects:
# Abvgrnd_Bms  -0.979

# Catepillar Plot
lm_AG_N0_AbGvMC_ranef <- ranef(lmer_AG_N0_AbGvMC)
head(lm_AG_N0_AbGvMC_ranef)
dotplot(lm_AG_N0_AbGvMC_ranef)
# variation within plots for Control plots for AP


### AG N2.5 ###
lmer_AG_N2_AbGvMC <- lmerTest::lmer(Perc_MC_Colonized~Aboveground_Biomass +(Aboveground_Biomass | Plot)+(Aboveground_Biomass | Block), data=AG_Ch_N2_MC,
                                    control=lmerControl(check.nobs.vs.nlev = "ignore",
                                                        check.nobs.vs.rankZ = "ignore",
                                                        check.nobs.vs.nRE="ignore"))
summary(lmer_AG_N2_AbGvMC)
#  -0.976

# catepillar plot 
lmer_AG_N2_AbGvMC_ranef <- ranef(lmer_AG_N2_AbGvMC)
head(lmer_AG_N2_AbGvMC_ranef)
dotplot(lmer_AG_N2_AbGvMC_ranef)
#variation within Blocks 



### all AG N10 Plots ####
lmer_AG_N10_AbGvMC <- lmerTest::lmer(Perc_MC_Colonized~Aboveground_Biomass +(Aboveground_Biomass | Plot)+(Aboveground_Biomass | Experiment), data=AG_N10_MC,
                                     control=lmerControl(check.nobs.vs.nlev = "ignore",
                                                         check.nobs.vs.rankZ = "ignore",
                                                         check.nobs.vs.nRE="ignore"))

summary(lmer_AG_N10_AbGvMC)
# correlation of fixed effects
#   -0.890

# Catepillar Plot
lmer_AG_N10_AbGvMC_ranef <- ranef(lmer_AG_N10_AbGvMC)
head(lmer_AG_N10_AbGvMC_ranef)
dotplot(lmer_AG_N10_AbGvMC_ranef)
# variation within 10g N plots for AG



### AG N20 ###
lmer_AG_N20_AbGvMC <- lmerTest::lmer(Perc_MC_Colonized~Aboveground_Biomass +(Aboveground_Biomass | Plot)+(Aboveground_Biomass | Block), data=AG_Ch_N20_MC,
                                     control=lmerControl(check.nobs.vs.nlev = "ignore",
                                                         check.nobs.vs.rankZ = "ignore",
                                                         check.nobs.vs.nRE="ignore"))
summary(lmer_AG_N20_AbGvMC)
#  -0.978

# catepillar plot 
lmer_AG_N20_AbGvMC_ranef <- ranef(lmer_AG_N20_AbGvMC)
head(lmer_AG_N20_AbGvMC_ranef)
dotplot(lmer_AG_N20_AbGvMC_ranef)
#variation within Blocks 




### SN Cat Plots ### 

### SN N0 Plots ###
lmer_SN_N0_AbGvMC <- lmerTest::lmer(Perc_MC_Colonized~Aboveground_Biomass +(Aboveground_Biomass | Plot)+(Aboveground_Biomass | Experiment), data=SN_N0_MC,
                                    control=lmerControl(check.nobs.vs.nlev = "ignore",
                                                        check.nobs.vs.rankZ = "ignore",
                                                        check.nobs.vs.nRE="ignore"))

summary(lmer_SN_N0_AbGvMC)
# Correlation of Fixed Effects:
# Abvgrnd_Bms  -0.955

# Catepillar Plot
lm_SN_N0_AbGvMC_ranef <- ranef(lmer_SN_N0_AbGvMC)
head(lm_SN_N0_AbGvMC_ranef)
dotplot(lm_SN_N0_AbGvMC_ranef)
# variation within plots for Control plots for SN


### SN N2.5 ###
lmer_SN_N2_AbGvMC <- lmerTest::lmer(Perc_MC_Colonized~Aboveground_Biomass +(Aboveground_Biomass | Plot)+(Aboveground_Biomass | Block), data=SN_Ch_N2_MC,
                                    control=lmerControl(check.nobs.vs.nlev = "ignore",
                                                        check.nobs.vs.rankZ = "ignore",
                                                        check.nobs.vs.nRE="ignore"))
summary(lmer_SN_N2_AbGvMC)
#  -1.000

# catepillar plot 
lmer_SN_N2_AbGvMC_ranef <- ranef(lmer_SN_N2_AbGvMC)
head(lmer_SN_N2_AbGvMC_ranef)
dotplot(lmer_SN_N2_AbGvMC_ranef)
#variation within Blocks 



### all SN N10 Plots ####
lmer_SN_N10_AbGvMC <- lmerTest::lmer(Perc_MC_Colonized~Aboveground_Biomass +(Aboveground_Biomass | Plot)+(Aboveground_Biomass | Experiment), data=SN_N10_MC,
                                     control=lmerControl(check.nobs.vs.nlev = "ignore",
                                                         check.nobs.vs.rankZ = "ignore",
                                                         check.nobs.vs.nRE="ignore"))

summary(lmer_SN_N10_AbGvMC)
# correlation of fixed effects
#   -0.920

# Catepillar Plot
lmer_SN_N10_AbGvMC_ranef <- ranef(lmer_SN_N10_AbGvMC)
head(lmer_SN_N10_AbGvMC_ranef)
dotplot(lmer_SN_N10_AbGvMC_ranef)
# variation within 10g N plots for SN



### SN N20 ###
lmer_SN_N20_AbGvMC <- lmerTest::lmer(Perc_MC_Colonized~Aboveground_Biomass +(Aboveground_Biomass | Plot)+(Aboveground_Biomass | Block), data=SN_Ch_N20_MC,
                                     control=lmerControl(check.nobs.vs.nlev = "ignore",
                                                         check.nobs.vs.rankZ = "ignore",
                                                         check.nobs.vs.nRE="ignore"))
summary(lmer_SN_N20_AbGvMC)
#  -0.837

# catepillar plot 
lmer_SN_N20_AbGvMC_ranef <- ranef(lmer_SN_N20_AbGvMC)
head(lmer_SN_N20_AbGvMC_ranef)
dotplot(lmer_SN_N20_AbGvMC_ranef)
#variation within Blocks 













# DO
lm_DO_AbGvMC <- lmer(Perc_MC_Colonized~Treatment*Aboveground_Biomass + (1| Experiment), data=DO_MC)

# plot 
pred_DO_MC <- predict(lm_DO_AbGvMC, newdata = DO_MC, type = "response") # create a vector of predicted values for Chicks
pred_DO_MC_df <- data.frame(DO_MC, pred_DO_MC) #bind those predictions to the original data frame

ggplot(data = pred_DO_MC_df, aes(Aboveground_Biomass, Perc_MC_Colonized)) + 
  geom_point() +
  geom_line(aes(Aboveground_Biomass, pred_DO_MC), col = "dodgerblue") + # set new aesthetic for the predicted Y
  facet_wrap(vars(Treatment))+ 
  labs(title="General Linear Model for %MC vs Aboveground Biomass with Experiment and Plot as Random Effects",
       x ="Aboveground_Biomass", y = "Perc_MC_Colonized")




plot(Perc_MC_Colonized~Aboveground_Biomass, data= DO_MC)
abline(lm(lm_DO_N0_AbGvMC), lwd=4, lty=1, col="#6E687E")
abline(lm(lm_DO_N2_AbGvMC), lwd=4, lty=1, col= "#F1C646")
abline(lm(lm_DO_N10_AbGvMC), lwd=4, lty=1, col="#769370")
abline(lm(lm_DO_N20_AbGvMC), lwd=4, lty=1, col="#BDB2A7")








ggplot(data  = MC_Data,
       aes(x = Aboveground_Biomass,
           y = Perc_MC_Colonized))+
  geom_point(size = 1.2,
             alpha = .8,
             position = "jitter")+# to add some random noise for plotting purposes
  theme_minimal()+
  labs(title = "Perc MC vs. Aboveground biomass")




View(MC_Data)





### Transformed data 

ggplot(data  = MC_Data,
       aes(x = Log_Aboveground_Biomass,
           y = Log_Perc_MC_Colonized))+
  geom_point(size = 1.2,
             alpha = 0.8,
             position = "jitter")+      # to add some random noise for plotting purposes
  theme_minimal()+
  labs(title = "Perc MC vs. Aboveground biomass")


### nested effects, multilevel 

ggplot(data    = DO_MC,
       aes(x   = Log_Aboveground_Biomass,
           y   = Log_Perc_MC_Colonized,
           col = Treatment2))+   #to add the colours for different classes
  geom_point(size     = 1.2,
             position = "jitter")+ #to add some random noise for plotting purposes
  theme_minimal()+
  theme(legend.position = "none")+
  scale_color_gradientn(colours = cols4)+
  labs(title    = "Perc MC vs. Aboveground biomass",
       subtitle = "Dicanthelium oligosanthes")




# Now we can draw different regression lines for the 4 different treatments in the data

ggplot(data      = DO_MC,
       aes(x     = Log_Aboveground_Biomass,
           y     = Log_Perc_MC_Colonized,
           col = as.factor(Treatment2),
           group = Treatment2))+   #to add the colours for different classes
  geom_point(size     = 1.5,
             position = "jitter")+ #to add some random noise for plotting purposes
  theme_minimal()+
  scale_color_manual(values=c("0"    = "#6E687E",
                              "2.5" = "#BDB2A7",
                              "10"  = "#769370",
                          "20"   = "#F1C646"))+
  geom_smooth(method = lm,
              se     = FALSE,
              size   = 1.2)+        # to add regression line
  labs(title    = "Log of Perc MC vs. Log of Aboveground Biomass",
       subtitle = "Dicanthelium oligosanthes")


#### DO NOT TRANSFORMED
ggplot(data      = DO_MC,
       aes(x     = Aboveground_Biomass,
           y     = Perc_MC_Colonized,
           col = as.factor(Treatment2),
           group = Treatment2))+   #to add the colours for different classes
  geom_point(size     = 1.5,
             position = "jitter")+ #to add some random noise for plotting purposes
  theme_minimal()+
  scale_color_manual(values=c("0"    = "#6E687E",
                              "2.5" = "#BDB2A7",
                              "10"  = "#769370",
                              "20"   = "#F1C646"))+
  geom_smooth(method = lm,
              se     = FALSE,
              size   = 1.2)+        # to add regression line
  labs(title    = "Perc MC vs. Aboveground Biomass",
       subtitle = "Dicanthelium oligosanthes")







### plots 

#### Combine plots ### 
install.packages("cowplot")
library(cowplot)


# DO Transformed Data
plotdo <- ggplot(data      = DO_MC,
       aes(x     = Log_Perc_MC_Colonized,
           y     = Log_Aboveground_Biomass,
           col = as.factor(Treatment2),
           group = Treatment2))+   #to add the colours for different classes
  geom_point(size     = 1.5,
             position = "jitter")+ #to add some random noise for plotting purposes
  theme_minimal()+
  theme(legend.position = "none")+
  scale_color_manual(values=c("0"    = "#6E687E",
                              "2.5" = "#BDB2A7",
                              "10"  = "#769370",
                              "20"   = "#F1C646"))+
  geom_smooth(method = lm,
              se     = FALSE,
              size   = 1.2)+        # to add regression line
  labs(title    = "D. oligosanthes")
plotdo 

# AP Transformed Data
plotap <- ggplot(data      = AP_MC,
       aes(x     = Log_Perc_MC_Colonized,
           y     = Log_Aboveground_Biomass,
           col = as.factor(Treatment2),
           group = Treatment2))+   #to add the colours for different classes
  geom_point(size     = 1.5,
             position = "jitter")+ #to add some random noise for plotting purposes
  theme_minimal()+
  theme(legend.position = "none")+
  scale_color_manual(values=c("0"    = "#6E687E",
                              "2.5" = "#BDB2A7",
                              "10"  = "#769370",
                              "20"   = "#F1C646"))+
  geom_smooth(method = lm,
              se     = FALSE,
              size   = 1.2)+        # to add regression line
  labs(title    = "A. psilostachya")
plotap

# SM Transformed Data
plotsm <- ggplot(data      = SM_MC,
       aes(x     = Log_Perc_MC_Colonized,
           y     = Log_Aboveground_Biomass,
           col = as.factor(Treatment2),
           group = Treatment2))+   #to add the colours for different classes
  geom_point(size     = 1.5,
             position = "jitter")+ #to add some random noise for plotting purposes
  theme_minimal()+
  theme(legend.position = "none")+
  scale_color_manual(values=c("0"    = "#6E687E",
                              "2.5" = "#BDB2A7",
                              "10"  = "#769370",
                              "20"   = "#F1C646"))+
  geom_smooth(method = lm,
              se     = FALSE,
              size   = 1.2)+        # to add regression line
  labs(title    = "S. missouriensis")
plotsm

# AG Transformed Data
plotag <- ggplot(data      = AG_MC,
       aes(x     = Log_Perc_MC_Colonized,
           y     = Log_Aboveground_Biomass,
           col = as.factor(Treatment2),
           group = Treatment2))+   #to add the colours for different classes
  geom_point(size     = 1.5,
             position = "jitter")+ #to add some random noise for plotting purposes
  theme_minimal()+
  theme(legend.position = "none")+
  scale_color_manual(values=c("0"    = "#6E687E",
                              "2.5" = "#BDB2A7",
                              "10"  = "#769370",
                              "20"   = "#F1C646"))+
  geom_smooth(method = lm,
              se     = FALSE,
              size   = 1.2)+        # to add regression line
  labs(title    = "A. gerardii")
plotag 

# SN Transformed Data
plotsn <- ggplot(data      = SN_MC,
       aes(x     = Log_Perc_MC_Colonized,
           y     = Log_Aboveground_Biomass,
           col = as.factor(Treatment2),
           group = Treatment2))+   #to add the colours for different classes
  geom_point(size     = 1.5,
             position = "jitter")+ #to add some random noise for plotting purposes
  theme_minimal()+
  theme(legend.position = "none")+
  scale_color_manual(values=c("0"    = "#6E687E",
                              "2.5" = "#BDB2A7",
                              "10"  = "#769370",
                              "20"   = "#F1C646"))+
  geom_smooth(method = lm,
              se     = FALSE,
              size   = 1.2)+        # to add regression line
  labs(title    = "S. nutans")
plotsn 

install.packages("patchwork")
library(patchwork)

plotdo + plotap + plotsm + plotag + plotsn

text(locator(), labels = c("p=0.240"), col = "#CD0BBC")






######  Regressions by treatment #######

######### %MC For all Species ####
reg_MCvTrt <- lm(Perc_MC_Colonized~Treatment2+(1/Plot), data = MC_Data)
summary(reg_MCvTrt)
AIC(reg_MCvTrt)
# p= 0.2404, not significant
# R2= 0.002

reg_DO_MCvTrt <- lm(Perc_MC_Colonized~Treatment2+(1/Plot), data = DO_MC)
summary(reg_DO_MCvTrt)
# p=0.101

reg_AP_MCvTrt <- lm(Perc_MC_Colonized~Treatment2+(1/Plot), data = AP_MC)
summary(reg_AP_MCvTrt)
# p=0.898

reg_SM_MCvTrt <- lm(Perc_MC_Colonized~Treatment2+(1/Plot), data = SM_MC)
summary(reg_SM_MCvTrt)
# p=0.482

reg_AG_MCvTrt <- lm(Perc_MC_Colonized~Treatment2+(1/Plot), data = AG_MC)
summary(reg_AG_MCvTrt)
# p=0.552

reg_SN_MCvTrt <- lm(Perc_MC_Colonized~Treatment2+(1/Plot), data = SN_MC)
summary(reg_SN_MCvTrt)
# p=0.224

# plot
plot(MC_Data$Perc_MC_Colonized~MC_Data$Treatment2,
     pch=20,
     ylab="Mycorrhizal Colonization (%)",
     xlab="Nitrogen Treatment (g)",
     main= "Mycorrhizal Colonization by N Treatment across Species")
#abline(lm(reg_MCvTrt), lwd=3, lty= 2, col = "black")
abline(lm(reg_DO_MCvTrt), lwd=3, lty= 2, col = "#CD0BBC")
abline(lm(reg_AP_MCvTrt), lwd=3, lty= 2, col = "#61D04F")
abline(lm(reg_SM_MCvTrt), lwd=3, lty= 2, col = "#2297E6")
abline(lm(reg_AG_MCvTrt), lwd=3, lty= 2, col = "brown")
abline(lm(reg_SN_MCvTrt), lwd=3, lty= 2, col = "#F5C710")

# p-values
text(locator(), labels = c("p=0.240"), col = "#CD0BBC")
text(locator(), labels = c("p=0.898"), col = "#61D04F")
text(locator(), labels = c("p=0.482"), col = "#2297E6")
text(locator(), labels = c("p=0.552"), col = "brown")
text(locator(), labels = c("p=0.224"), col = "#F5C710")


######### %MC For all Species ####
reg_LAbGvTrt <- lm(Log_Aboveground_Biomass~Treatment2+(1/Plot), data = MC_Data)
summary(reg_LAbGvTrt)
# p= 0.003, ** significant
# R2= 0.068

reg_DO_AbGvTrt <- lm(Aboveground_Biomass~Treatment2+(1/Plot), data = DO_MC)
summary(reg_DO_AbGvTrt)
# p=0.007, ** significant
# R2= 0.179

reg_AP_AbGvTrt <- lm(Aboveground_Biomass~Treatment2+(1/Plot), data = AP_MC)
summary(reg_AP_AbGvTrt)
# p<0.001, ** significant
# R2=0.349

reg_SM_AbGvTrt <- lm(Aboveground_Biomass~Treatment2+(1/Plot), data = SM_MC)
summary(reg_SM_AbGvTrt)
# p=0.394

reg_AG_AbGvTrt <- lm(Aboveground_Biomass~Treatment2+(1/Plot), data = AG_MC)
summary(reg_AG_AbGvTrt)
# p=0.163

reg_SN_AbGvTrt <- lm(Aboveground_Biomass~Treatment2+(1/Plot), data = SN_MC)
summary(reg_SN_AbGvTrt)
# p=0.009, ** significant
# R2=0.161

# plot
plot(MC_Data$Aboveground_Biomass~MC_Data$Treatment2,
     pch=20,
     ylim=c(0.0,2.0),
     ylab="Aboveground Biomass (g)",
     xlab="Nitrogen Treatment (g)",
     main= "Aboveground Biomass by N Treatment across Species")
#abline(lm(reg_LAbGvTrt), lwd=3, col = "black")
abline(lm(reg_DO_AbGvTrt), lwd=4.5, col = "#CD0BBC")
abline(lm(reg_AP_AbGvTrt), lwd=4.5, col = "#61D04F")
abline(lm(reg_SM_LAbGvTrt), lwd=3, lty= 2, col ="#2297E6")
abline(lm(reg_AG_AbGvTrt), lwd=3, lty= 2, col = "brown")
abline(lm(reg_SN_AbGvTrt), lwd=4.5, col = "#F5C710")

# p-values
text(locator(), labels = c("p=0.007 *"), font=2, col = "#CD0BBC")
text(locator(), labels = c("p<0.001 *"), font=2, col = "#61D04F")
text(locator(), labels = c("p=0.394"), col = "#2297E6")
text(locator(), labels = c("p=0.163"), col = "brown")
text(locator(), labels = c("p=0.009 *"), font=2, col = "#F5C710")



# Legend
plot(MC_Data$Aboveground_Biomass~MC_Data$Treatment2)
legend("topright", inset = 0.10, 
       legend = c("D. oligosanthes", "A. psilostachya", "S. missouriensis", "A.gerardii","S. nutans" ), 
       fill = c("#CD0BBC", "#61D04F", "#2297E6", "brown", "#F5C710"), 
       bg = "white", 
       title = "Species")



######################################################################
### Results of AbG Biomass Regressions: Ambrosia has the most significant slope here 



###### Get Means for MC in N0N10  ######

### DO 
summary(DO_N0_MC$MC)
# N0 mean: 
summary(DO_N10_MC$MC)
# N10 mean: 


