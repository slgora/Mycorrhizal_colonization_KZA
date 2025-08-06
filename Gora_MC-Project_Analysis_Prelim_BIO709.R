#### BioStats MC Project- Prelim Data Analysis ####
###  Gora_MC-Project_Analysis_Prelim_BIO709.R
###  by Sarah Gora
###  Date created: March 23, 2022

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



# Load data ***** THIS IS PRELIM
library(readr)
MC_Data_Prelim <- read_csv("~/Desktop/Biostats_2/Gora_MC_Project/Gora_MC_Datasheet_Prelim2.csv")
MC_Data_Prelim3 <- read_csv("~/Desktop/Biostats_2/Gora_MC_Project/Gora_MC_Datasheet_Prelim3.csv")


head(MC_Data_Prelim)

N0_MC <- subset(MC_Data, Treatment2== "0")

#### Subset out MC Data by Species
DO_MC <- subset(MC_Data_Prelim, Species=="Dichanthelium_oligosanthes")
AP_MC <- subset(MC_Data_Prelim, Species=="Ambrosia_psilostachya")
SM_MC <- subset(MC_Data_Prelim, Species=="Solidago_missouriensis")
AG_MC <- subset(MC_Data_Prelim, Species=="Andropogon_gerardii")
SN_MC <- subset(MC_Data_Prelim, Species=="Sorghastrum_nutans")


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




#### Subset out Change MC data for each species 
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

par(mfrow=c(1, 2))

# Visualize the data distribution
hist(MC_Data_Prelim$Perc_MC_Colonized)

# slightly right skewed
# log transform .... ?

hist(MC_Data_Prelim$Log_Perc_MC_Colonized)
# not great 

hist(MC_Data_Prelim$Sqrt_Perc_MC_Colonized)
# better 

head(MC_Data_Prelim)


### GRAPHED ######
par(mfrow=c(1, 2))
g = MC_Data_Prelim3$Perc_MC_Colonized
m<-mean(g)
std<-sqrt(var(g))
hist(g, prob=TRUE, 
     xlab="Mycorrhizal Colonization (%)", ylim=c(0, 0.02), 
     main="Non-Transformed")
curve(dnorm(x, mean=m, sd=std), 
      col="red", lwd=2, add=TRUE, yaxt="n")

g = MC_Data_Prelim3$Sqrt_Perc_MC_Colonized
m<-mean(g)
std<-sqrt(var(g))
hist(g, prob=TRUE, 
     xlab="Sqrt of Mycorrhizal Colonization (%)", ylim=c(0, 0.25), 
     main="Square-Root Transformed")
curve(dnorm(x, mean=m, sd=std), 
      col="red", lwd=2, add=TRUE, yaxt="n")







# this provides values of all four *quartiles*, plus the min and max
summary(MC_Data_Prelim$Perc_MC_Colonized) 
#    Min. 1st Qu.  Median  Mean    3rd Qu.   Max.    NA's 
#   0.00   21.04   41.56   39.96   58.01    82.35   22 

# View as %MC by Treatments
boxplot(Perc_MC_Colonized ~ Treatment2, MC_Data_Prelim)
# 0N mean: 36.25
# 2.5N mean: 43.52
# 10N mean: 39.77 
# 20N mean: 43.197

N0_MC <- subset(MC_Data_Prelim, Treatment2=="0")
N2_MC <- subset(MC_Data_Prelim, Treatment2=="2.5")
N10_MC <- subset(MC_Data_Prelim, Treatment2=="10")
N20_MC <- subset(MC_Data_Prelim, Treatment2=="20")

summary(N0_MC$Perc_MC_Colonized)
summary(N2_MC$Perc_MC_Colonized)
summary(N10_MC$Perc_MC_Colonized)
summary(N20_MC$Perc_MC_Colonized)





# Have to look at Aboveground Biomass by Species because they vary by species
par(mfrow=c(1, 2))

hist(MC_Data_Prelim$Aboveground_Biomass)
# really left skewed, but I think this is bc of species differences as well 
# a few really small values on the tail 
View(MC_Data_Prelim)


hist(MC_Data_Prelim$Log_Aboveground_Biomass)
# Looks GREAT !!!!!



# Non-Transformed Data 
par(mfcol = c(1, 2))
g = MC_Data_Prelim$Aboveground_Biomass
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=20, breaks=20, prob=TRUE, 
     xlab="Aboveground Biomass (g)", ylim=c(0, 0.8), 
     main="Non-Transformed")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

# Log Transformed Aboveground Biomass 
g = MC_Data_Prelim$Log_Aboveground_Biomass
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=20, breaks=20, prob=TRUE, 
     xlab="Log of Aboveground Biomass (g)", ylim=c(0, 1.25), 
     main="Log-Transformed")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")






########## DO ##########
par(mfrow=c(1, 2))
hist(DO_MC$Aboveground_Biomass)
# Left Skewed

hist(DO_MC$Log_Aboveground_Biomass)
# looks good

par(mfrow=c(1, 1))

# Means, Range and Outliers
summary(DO_MC$Aboveground_Biomass) 
# Min.    1st Qu.  Median    Mean    3rd Qu.    Max. 
# 0.0100  0.1100  0.2100     0.2871  0.4175     1.3500 

# Means, Range and Outliers
summary(DO_MC$Log_Aboveground_Biomass) 
# Min.    1st Qu.  Median    Mean    3rd Qu.    Max. 
#-2.0000 -0.9586  -0.6778   -0.7244  -0.3820    0.1303

View(DO_MC)
# Check DO_Change_C18- 1.35 g, this is massive
# checked, actual

########## AP ##########
par(mfrow=c(1, 2))
hist(AP_MC$Aboveground_Biomass)
# Left Skewed
hist(AP_MC$Log_Aboveground_Biomass)
# not the greatest

# Means, Range and Outliers
summary(AP_MC$Aboveground_Biomass) 
# Min.     1st Qu.  Median    Mean     3rd Qu.    Max.    
# 0.0200   0.1850   0.5600    0.7306   1.0550     2.0300  

# Means, Range and Outliers
summary(AP_MC$Log_Aboveground_Biomass)
# Min.      1st Qu.   Median    Mean      3rd Qu.    Max.
# -1.69897  -0.74074  -0.25181  -0.42238  0.02325    0.30750

View(AP_MC)
# Check AP_Change_E40- missing 
# checked


########## SM ##########
hist(SM_MC$Aboveground_Biomass)
# Left skewed
hist(SM_MC$Log_Aboveground_Biomass)


# Means, Range and Outliers
summary(SM_MC$Aboveground_Biomass)
# Min.    1st Qu.  Median    Mean    3rd Qu.   Max.   
# 0.410   1.250    1.870     2.703   3.890     11.630    

View(SM_MC)
# Check SM_Change_A2, 11.63 g - this huge 
# Check SM_Change_F45 - missing 
# Check SM_Change_F48 - missing
# checked 


########## AG ##########
hist(AG_MC$Aboveground_Biomass)
# Odd skew
hist(AG_MC$Log_Aboveground_Biomass)
# better 

# Means, Range and Outliers
summary(AG_MC$Aboveground_Biomass)
# Min.    1st Qu.  Median  Mean    3rd Qu.  Max. 
# 0.1800  0.4925  0.7900  1.0919  1.4900  3.4300 

View(AG_MC)
# nothing to check



########## SN ##########
par(mfcol = c(1, 1))
hist(SN_MC$Aboveground_Biomass)
lines(density(SN_MC$Aboveground_Biomass))

# Left skew
hist(SN_MC$Log_Aboveground_Biomass)
lines(frequency(SN_MC$Log_Aboveground_Biomass))
# better

# Non-Transformed Data 
par(mfcol = c(1, 2))
g = SN_MC$Aboveground_Biomass
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=20, breaks=20, prob=TRUE, 
     xlab="Aboveground Biomass (g)", ylim=c(0, 2.9), 
     main="Non-Transformed")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

# Log Transformed Aboveground Biomass 
g = SN_MC$Log_Aboveground_Biomass
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=20, breaks=20, prob=TRUE, 
     xlab="Aboveground Biomass (g)", ylim=c(0, 2.9), 
     main="Log-Transformed")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")





summary(SN_MC$Aboveground_Biomass)
# average: 0.739 g
# Min.    1st Qu.  Median   Mean    3rd Qu.  Max.   
# 0.1700  0.4050  0.5450    0.7089  0.9025   2.8800 

View(SN_MC)
# Check SN_Change_F41- 2.88 g , really large
# Check SN_Change_D25- missing


# MC Percent Colonized Data

########## DO ##########
hist(DO_MC$Perc_MC_Colonized)
# Right Skewed
hist(DO_MC$Sqrt_Perc_MC_Colonized)
# maybe.... 

# Means, Range and Outliers
summary(DO_MC$Perc_MC_Colonized) 
# Min.    1st Qu. Median  Mean    3rd Qu. Max. 
# 10.71   48.76   56.62   52.43   64.04   75.76

View(DO_MC)
# Check DO_Pplots_F1, first sample - 13.9%
# Check DO_Pplots_B7, small sample- 10.7%
#        ^^^ maybe throw out
# checked


########## AP ##########
hist(AP_MC$Perc_MC_Colonized)
# Left Skewed, but odd
hist(AP_MC$Sqrt_Perc_MC_Colonized)
# better... 

# Means, Range and Outliers
summary(AP_MC$Perc_MC_Colonized) 
# Min.    1st Qu.  Median   Mean    3rd Qu.  Max.    NA's   
# 0.000   8.562    14.583   24.186  41.924   64.706     1

View(AP_MC)
# Check AP_Change_D25 - 0% colonized? what?
# checked

########## SM ##########
hist(SM_MC$Perc_MC_Colonized)
# Right skewed
hist(SM_MC$Sqrt_Perc_MC_Colonized)
# better

# Means, Range and Outliers
summary(SM_MC$Perc_MC_Colonized)
# Min.   1st Qu. Median  Mean    3rd Qu.  Max.   NAs
# 0.00   37.32   52.16   48.57   69.87    82.35   4 

View(SM_MC)
# Check SM_Change_C18- 6.2% colonized
#          ^^^^ maybe throw this out, the next % colonized is 28%
# checked

########## AG ##########
hist(AG_MC$Perc_MC_Colonized)
# Slightly right skew
hist(AG_MC$Sqrt_Perc_MC_Colonized)
# better 

# Means, Range and Outliers
summary(AG_MC$Perc_MC_Colonized)
# Min.    1st Qu.  Median    Mean    3rd Qu.   Max.    NA's 
#  7.692  15.800   33.957    32.295  44.787    57.447    12

View(AG_MC)
# Check AG_Pplots_E5- 10.2% 
# Check AG_Pplots_F1 - 14.8% 
# checked

########## SN ##########
hist(SN_MC$Perc_MC_Colonized)
# Left skew and odd 
hist(SN_MC$Sqrt_Perc_MC_Colonized)
# not great at all

summary(SN_MC$Perc_MC_Colonized)
# average: 0.739 g
# Min.   1st Qu.  Median    Mean    3rd Qu.   Max.    NA's 
# 14.81   28.32   34.63     40.58   58.09    74.32       4 
   

View(SN_MC)
# Nothing to check 



########### Data Transformation ##########


# log transform works for Aboveground Biomass

# need to find what transformation works for %MC 






################ Linear Mixed Model ################

# % MC - Continuous variable

# FIXED effect- Treatment (N treatments)
# no interaction terms 
# RANDOM effect - Experiment (aka "site", Change and Pplots)
# RANDOM effect - Plot (Plots are all different in space)
# RANDOM effect - Block (Change has blocks that are somewhat spread out)

# NESTED RANDOM effect: Plots within Blocks within Experiments
# CROSSED RANDOM effect: Blocks are only within Change experiment


##### NOTES: 

# Construct a GLMM 
# relating % MC to predictor: Treatment (categorical variable)

# Construct a GLMM Blcok
# relating % MC to two predictors: Treatment (categorical variable) & Aboveground Biomass (continuous variable)



# model run on ALL SPECIES first

glmm_model_1 <- lmer(Perc_MC_Colonized ~ Treatment2 + (1 | Species) + (1| Block/ Experiment), 
                     data = MC_Data_Prelim)
summary(glmm_model_1)

# how to include plot as random effect?  
# Error: number of levels of each grouping factor must be < number of observations (problems: Plot)





###### DO in N0N10 data
glmm_model_DO_N0N10 <- lmer(Perc_MC_Colonized ~ Treatment2 + (1| Block/ Experiment),
                     data = DO_N0N10_MC)
summary(glmm_model_DO_N0N10)

# Has some kind of weird error when adding "Block" to the model:
# Boundary (singular) fit: see help('isSingular')


###### DO in Change data
glmm_model_DO_Ch <- lmer(Perc_MC_Colonized ~ Treatment2 + (1| Block/ Experiment),
                     data = DO_Ch_MC)
summary(glmm_model_DO_Ch)

# Has some kind of weird error when adding "Block" to the model:
# Boundary (singular) fit: see help('isSingular')






# Include Aboveground Biomass as an interaction term .... ??? 
# in Model 2

glmm_model_2 <- lmer(Perc_MC_Colonized ~ Treatment2*Aboveground_Biomass + (1| Species) + (1| Block/ Experiment), 
                     data = DO_MC)








########## Mixed-Model ANOVAS ############

# All data, %MC 
aov_MCvTrt <- aov(Perc_MC_Colonized~Treatment2+(1/Experiment)+(1/Plot), data=MC_Data_Prelim)
summary(aov_MCvTrt)
# p= 0.324, not significant 
model.tables(aov_MCvTrt, "means")
#   0g N: 38.22
# 2.5g N: 38.82
#  10g N: 40.63
#  20g N: 43.05





# Run mixed model ANOVAs... by Species ... 

### Trait: Perc_MC_Colonized ###

# DO, N0N10
aov_DO_MCvTrt <- aov(Perc_MC_Colonized~Treatment2+(1/Experiment)+(1/Plot), data=DO_N0N10_MC)
summary(aov_DO_MCvTrt)
# p= 0.870, not significant 
model.tables(aov_DO_MCvTrt, "means")
# Control: 47.77
# 10g N: 46.47
summary(DO_N0N10_MC$Perc_MC_Colonized)

# AP, N0N10
aov_AP_MCvTrt <- aov(Perc_MC_Colonized~Treatment2+(1/Experiment)+(1/Plot), data=AP_N0N10_MC)
summary(aov_AP_MCvTrt)
# p= 0.433, not significant 
model.tables(aov_AP_MCvTrt, "means")
# Control: 23.238
# 10g N: 30.115
summary(AP_N0N10_MC$Perc_MC_Colonized)


# SM, N0N10
aov_SM_MCvTrt <- aov(Perc_MC_Colonized~Treatment2+(1/Experiment)+(1/Plot), data=SM_N0N10_MC)
summary(aov_SM_MCvTrt)
# p= 0.671, not significant 
model.tables(aov_SM_MCvTrt, "means")
# Control: 41.05
# 10g N: 46.16
summary(SM_N0N10_MC$Perc_MC_Colonized)


# AG, N0N10 ----- NOT ENOUGH DATA YET for Change --- put in later
aov_AG_MCvTrt <- aov(Perc_MC_Colonized~Treatment2+(1/Plot), data=AG_N0N10_MC)
summary(aov_AG_MCvTrt)
# p= 0.536
model.tables(aov_AG_MCvTrt, "means")
# Control: 36.68
# 10g N: 30.75
summary(AG_N0N10_MC$Perc_MC_Colonized)



# SN, N0N10
aov_SN_MCvTrt <- aov(Perc_MC_Colonized~Treatment2+(1/Experiment)+(1/Plot), data=SN_N0N10_MC)
summary(aov_SN_MCvTrt)
# p= 0.209, not significant 
model.tables(aov_SN_MCvTrt, "means")
# Control: 33.11
# 10g N: 42.94
summary(SN_N0N10_MC$Perc_MC_Colonized)



### PRELIM RESULTS: 
# no response in %MC to 10g Nitrogen additions






# Change Data

# Run mixed model ANOVAs... by species in N gradient

# DO, Change
aov_DO_Ch_MCvTrt <- aov(Perc_MC_Colonized~Treatment2+(1/Plot)+(1/Block), data=DO_Ch_MC)
summary(aov_DO_Ch_MCvTrt)
# p= 0.012, ** significant
model.tables(aov_DO_Ch_MCvTrt, "means")
TukeyHSD(aov_DO_Ch_MCvTrt) 
# Control: 51.79
# 2.5g N: 57.24
# 10g N: 61.06
# 20g N: 67.50 ** p=0.04 ** significant

summary(DO_Ch_MC$Perc_MC_Colonized)



# AP,  Change
aov_AP_Ch_MCvTrt <- aov(Perc_MC_Colonized~Treatment2+(1/Plot)+(1/Block), data=AP_Ch_MC)
summary(aov_AP_Ch_MCvTrt)
# p= 0.833, not significant 
model.tables(aov_AP_Ch_MCvTrt, "means")
# Control: 19.144
# 2.5g N: 19.400
# 10g N: 20.167
# 20g N: 21.189
summary(AP_Ch_MC$Perc_MC_Colonized)


# SM,  Change
aov_SM_Ch_MCvTrt <- aov(Perc_MC_Colonized~Treatment2+(1/Plot)+(1/Block), data=SM_Ch_MC)
summary(aov_SM_Ch_MCvTrt)
# p= 0.644, not significant 
model.tables(aov_SM_Ch_MCvTrt, "means")
# Control: 46.41
# 2.5g N: 47.23
# 10g N: 49.67
# 20g N: 52.94
summary(SM_Ch_MC$Perc_MC_Colonized)



# AG,  Change ----- NOT ENOUGH DATA YET for Change --- put in later
aov_AG_Ch_MCvTrt <- aov(Perc_MC_Colonized~Treatment2+(1/Plot)+(1/Block), data=AG_Ch_MC)
summary(aov_AG_Ch_MCvTrt)
# p= 0.621
model.tables(aov_AG_Ch_MCvTrt, "means")
# Control: 31.98
# 2.5g N: 31.32
# 10g N: 29.33
# 20g N: 26.68
summary(AG_Ch_MC$Perc_MC_Colonized)



# SN,  Change
aov_SN_Ch_MCvTrt <- aov(Perc_MC_Colonized~Treatment2+(1/Plot)+(1/Block), data=SN_Ch_MC)
summary(aov_SN_Ch_MCvTrt)
# p= 0.587, not significant 
model.tables(aov_SN_Ch_MCvTrt, "means")
# Control: 40.64
# 2.5g N: 41.33
# 2.5g N: 43.41
# 2.5g N: 46.18
summary(SN_Ch_MC$Perc_MC_Colonized)



################################################################
############# PRELIM RESULTS for %MC: ##########################
# %MC for Dican was the only species to respond to N 
# DO only responsive with 20g Nitrogen additions
# increased %MC colonization
###############################################################
###############################################################






# Run mixed model ANOVAs... by Species ... 
### Trait: Aboveground_Biomass ###

DO_N0N10_MC$Treatment2 <- as.factor(DO_N0N10_MC$Treatment2)
AP_N0N10_MC$Treatment2 <- as.factor(AP_N0N10_MC$Treatment2)
SM_N0N10_MC$Treatment2 <- as.factor(SM_N0N10_MC$Treatment2)
AG_N0N10_MC$Treatment2 <- as.factor(AG_N0N10_MC$Treatment2)
SN_N0N10_MC$Treatment2 <- as.factor(SN_N0N10_MC$Treatment2)

# DO, N0N10
aov_DO_AbGvTrt <- aov(Aboveground_Biomass~Treatment2+(1/Experiment)+(1/Plot), data=DO_N0N10_MC)
summary(aov_DO_AbGvTrt)
# p= 0.422, not significant 
model.tables(aov_DO_AbGvTrt, "means")
# Control: 0.22417
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


### PRELIM RESULTS: Aboveground Biomass
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
# p= 0.0146, ** significant 
model.tables(aov_DO_Ch_AbGvTrt, "means")
TukeyHSD(aov_DO_Ch_AbGvTrt) 
# Control: 0.1148
#  2.5g N: 0.1626 
#   10g N: 0.3061
#   20g N: 0.4975

# not significant in control vs N groups 
summary(DO_Ch_MC$Aboveground_Biomass)



# AP,  Change
aov_AP_Ch_AbGvTrt <- aov(Aboveground_Biomass~Treatment+(1/Plot)+(1/Block), data=AP_Ch_MC)
summary(aov_AP_Ch_AbGvTrt)
# p= 0.0042, *** significant 
model.tables(aov_AP_Ch_AbGvTrt, "means")
TukeyHSD(aov_AP_Ch_AbGvTrt)
# Control: 0.3350
# 2.5g N: 0.4537
#  10g N: 0.8099
#  20g N: 1.2848

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
# Control: 0.5087
# 2.5g N: 0.5583
# 2.5g N: 0.7072
# 2.5g N: 0.9058
summary(SN_Ch_MC$Aboveground_Biomass)



############## PRELIM RESULTS: Aboveground Biomass #################
#  AG, AP, SN  had a significant response in AbG Biomass to 10g N additions

# AP  had a significant response in AbG in 20g N as well 
####################################################################





























par(mfrow=c(1, 1))

### MC Count Boxplots ###

pal <- park_palette("Redwoods", 5)
# gray= control, green= 10N

pal2 <- c("#BDB2A7","#769370")

cat3 <- c("          DO", "", "          AP", "", "          SM", "", "          AG", "", "          SN", "")
boxplot(DO_N0_MC$Perc_MC_Colonized, DO_N10_MC$Perc_MC_Colonized,
        AP_N0_MC$Perc_MC_Colonized, AP_N10_MC$Perc_MC_Colonized,
        SM_N0_MC$Perc_MC_Colonized, SM_N10_MC$Perc_MC_Colonized,
        AG_N0_MC$Perc_MC_Colonized, AG_N10_MC$Perc_MC_Colonized,
        SN_N0_MC$Perc_MC_Colonized, SN_N10_MC$Perc_MC_Colonized,
        at = c(1.15, 2, 4.15, 5, 7.15, 8, 10.15, 11, 13.15, 14),
        names= cat3,
        ylim=c(0, 100),
        xlab="Species", 
        ylab="Mycorrhizal Colonization (%)", 
        main= "Root Mycorrhizal Colonization in 0g vs 10g Nitrogen Addition", 
        pch=20,
        col= pal2)
abline(v = c(3, 6, 9, 12), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")

# put in p-values 
text(locator(), labels = c("p=0.870"))
text(locator(), labels = c("p=0.433"))
text(locator(), labels = c("p=0.409"))
text(locator(), labels = c("p=0.662"))
text(locator(), labels = c("p=0.137"))


### Trait: Aboveground_Biomass ###

pal2 <- c("#BDB2A7","#769370")
cat3 <- c("          DO", "", "          AP", "", "          SM", "", "          AG", "", "          SN", "")

boxplot(DO_N0_MC$Aboveground_Biomass, DO_N10_MC$Aboveground_Biomass,
        AP_N0_MC$Aboveground_Biomass, AP_N10_MC$Aboveground_Biomass,
        SM_N0_MC$Aboveground_Biomass, SM_N10_MC$Aboveground_Biomass,
        AG_N0_MC$Aboveground_Biomass, AG_N10_MC$Aboveground_Biomass,
        SN_N0_MC$Aboveground_Biomass, SN_N10_MC$Aboveground_Biomass,
        at = c(1.15, 2, 4.15, 5, 7.15, 8, 10.15, 11, 13.15, 14),
        names= cat3,
        ylim=c(0, 5.2),
        xlab="Species", 
        ylab="Aboveground Biomass (g)", 
        main= "Aboveground Biomass in 0g vs 10g Nitrogen Addition", 
        pch=20,
        col= pal2)
abline(v = c(3, 6, 9, 12), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")

# put in p-values 
text(locator(), labels = c("p= 0.579"))
text(locator(), labels = c("p<0.001 *"), font=2)
text(locator(), labels = c("*"), font=2)
text(locator(), labels = c("p=0.540"))
text(locator(), labels = c("p=0.001 *"), font=2)
text(locator(), labels = c("*"), font=2)
text(locator(), labels = c("p=0.026 *"), font=2)
text(locator(), labels = c("*"), font=2)










### MC Count Boxplots: n0n10 ###
par(mfrow=c(1, 1))

pal <- park_palette("Redwoods", 5)
# gray= control, green= 10N

pal2 <- c("#BDB2A7","#769370")

cat3 <- c("          DO", "", "          AP", "", "          SM", "", "          AG", "", "          SN", "")
boxplot(DO_N0_MC$Perc_MC_Colonized, DO_N10_MC$Perc_MC_Colonized,
        AP_N0_MC$Perc_MC_Colonized, AP_N10_MC$Perc_MC_Colonized,
        SM_N0_MC$Perc_MC_Colonized, SM_N10_MC$Perc_MC_Colonized,
        AG_N0_MC$Perc_MC_Colonized, AG_N10_MC$Perc_MC_Colonized,
        SN_N0_MC$Perc_MC_Colonized, SN_N10_MC$Perc_MC_Colonized,
        at = c(1.15, 2, 4.15, 5, 7.15, 8, 10.15, 11, 13.15, 14),
        names= cat3,
        ylim=c(0, 100),
        xlab="Species", 
        ylab="Mycorrhizal Colonization (%)", 
        main= "Root Mycorrhizal Colonization in 0g vs 10g Nitrogen Addition", 
        pch=20,
        col= pal2)
abline(v = c(3, 6, 9, 12), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")

# put in p-values 
text(locator(), labels = c("p="))
text(locator(), labels = c("p="))
text(locator(), labels = c("p="))
text(locator(), labels = c("p="))
text(locator(), labels = c("p="))




##### Change Boxplots for % MC ########## 

trtcol <- c("#BDB2A7", "#6E687E", "#769370", "#F1C646")
spnames <- c(" ", "   DO", " ", " ", " ", "  AP", " ", " ", " ", "  SM", " ", " ", " ", "    AG", " ", " ", " ", "      SN", " ", " ")


boxplot(DO_Ch_N0_MC$Perc_MC_Colonized, DO_Ch_N2_MC$Perc_MC_Colonized, DO_Ch_N10_MC$Perc_MC_Colonized, DO_Ch_N20_MC$Perc_MC_Colonized,
        AP_Ch_N0_MC$Perc_MC_Colonized, AP_Ch_N2_MC$Perc_MC_Colonized, AP_Ch_N10_MC$Perc_MC_Colonized, AP_Ch_N20_MC$Perc_MC_Colonized,
        SM_Ch_N0_MC$Perc_MC_Colonized, SM_Ch_N2_MC$Perc_MC_Colonized, SM_Ch_N10_MC$Perc_MC_Colonized, SM_Ch_N20_MC$Perc_MC_Colonized,
        AG_Ch_N0_MC$Perc_MC_Colonized, AG_Ch_N2_MC$Perc_MC_Colonized, AG_Ch_N10_MC$Perc_MC_Colonized, AG_Ch_N20_MC$Perc_MC_Colonized,
        SN_Ch_N0_MC$Perc_MC_Colonized, SN_Ch_N2_MC$Perc_MC_Colonized, SN_Ch_N10_MC$Perc_MC_Colonized, SN_Ch_N20_MC$Perc_MC_Colonized,
        names= spnames,
        at = c(1.5, 2.5, 3.5, 4.5, 6.5, 7.5, 8.5, 9.5, 11.5, 12.5, 13.5, 14.5, 16.5, 17.5, 18.5, 19.5, 21.5, 22.5, 23.5, 24.5),
        col= trtcol,
        ylim=c(0, 100),
        pch=20, 
        ylab="Mycorrhizal Colonization (%)", 
        xlab="Species",       
        main= "Root Mycorrhizal Colonization in N Treatments")
abline(v = c(5.5, 10.5, 15.5, 20.5), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")
text(locator(), labels = c("p=0.012 *"), font=2)
text(locator(), labels = c("*"), font=2)
text(locator(), labels = c("p=0.389"))
text(locator(), labels = c("p=0.492"))
text(locator(), labels = c("p=0.952"))
text(locator(), labels = c("p=0.587"))


##### Change Boxplots for Aboveground Biomass ########## 

trtcol <- c("#BDB2A7", "#6E687E", "#769370", "#F1C646")
spnames <- c(" ", "   DO", " ", " ", " ", "  AP", " ", " ", " ", "  SM", " ", " ", " ", "    AG", " ", " ", " ", "      SN", " ", " ")


boxplot(DO_N0_MC$Aboveground_Biomass, DO_Ch_N2_MC$Aboveground_Biomass, DO_N10_MC$Aboveground_Biomass, DO_Ch_N20_MC$Aboveground_Biomass,
        AP_N0_MC$Aboveground_Biomass, AP_Ch_N2_MC$Aboveground_Biomass, AP_N10_MC$Aboveground_Biomass, AP_Ch_N20_MC$Aboveground_Biomass,
        SM_N0_MC$Aboveground_Biomass, SM_Ch_N2_MC$Aboveground_Biomass, SM_N10_MC$Aboveground_Biomass, SM_Ch_N20_MC$Aboveground_Biomass,
        AG_N0_MC$Aboveground_Biomass, AG_Ch_N2_MC$Aboveground_Biomass, AG_N10_MC$Aboveground_Biomass, AG_Ch_N20_MC$Aboveground_Biomass,
        SN_N0_MC$Aboveground_Biomass, SN_Ch_N2_MC$Aboveground_Biomass, SN_N10_MC$Aboveground_Biomass, SN_Ch_N20_MC$Aboveground_Biomass,
        names= spnames,
        at = c(1.5, 2.5, 3.5, 4.5, 6.5, 7.5, 8.5, 9.5, 11.5, 12.5, 13.5, 14.5, 16.5, 17.5, 18.5, 19.5, 21.5, 22.5, 23.5, 24.5),
        col= trtcol,
        ylim=c(0, 6.05),
        pch=20, 
        ylab="Aboveground Biomass (g)", 
        xlab="Species",       
        main= "Aboveground Biomass in N Treatments")
abline(v = c(5.5, 10.5, 15.5, 20.5), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")
text(locator(), labels = c("p=0.063 "))
text(locator(), labels = c("p=0.004 * "), font=2)
text(locator(), labels = c("*"), font=2)  #n=10
text(locator(), labels = c("*"), font=2)  #n=20
text(locator(), labels = c("p=0.636 "))
text(locator(), labels = c("p=0.001 *"), font=2)
text(locator(), labels = c("*"), font=2)  #n=10
text(locator(), labels = c("p=0.026 *"), font=2)
text(locator(), labels = c("*"), font=2)  #n=10








#### Could try to Visualize with a stacked Barchart 
# percent of the whole? 








######### ONLY for Change ######

# DO 
# run linear model 
reg_DO_Ch_MCvTrt <- lm(Perc_MC_Colonized~Treatment2+(1/Block)+(1/Plot), data = DO_Ch_MC)
summary(reg_DO_Ch_MCvTrt)
AIC(reg_DO_Ch_MCvTrt)
# AIC: 164.2012  ** LOWEST

# run second order Linear model 
reg_DO_Ch_MCvTrt2 <- glm(Perc_MC_Colonized ~ poly(Treatment2, degree = 2, raw = T), data=DO_Ch_MC, family = gaussian()) 
AIC(reg_DO_Ch_MCvTrt2)
# AIC: 166.0388

# predict based on best AIC: 
# fakeY_DOLeafTh_poly2 <- predict(reg_DO_Ch_LeafThvTrt2, list(Treatment2= fakeX_DOTrt2,
                                                            type="response", se.fit=F))

# plot
plot(DO_Ch_MC$Perc_MC_Colonized~DO_Ch_MC$Treatment2,
     pch=20,
     ylab="Mycorrhizal Colonization (%)",
     xlab="Nitrogen Treatment (g)",
     main= "D. oligosantheses")
abline(lm(reg_DO_Ch_MCvTrt), lwd=3, col = "blue")
# lines(fakeY_DOLeafTh_poly2~fakeX_DOTrt2, lwd = 3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.012 *"), font=2)
text(locator(), labels = c("R2=0.238"), font=2)





######### %MC For all Species ####

Ch_MC <- subset(MC_Data_Prelim, Experiment=="Change")

reg_Ch_MCvTrt <- lm(Perc_MC_Colonized~Treatment2+(1/Block)+(1/Plot), data = Ch_MC)
summary(reg_Ch_MCvTrt)
AIC(reg_Ch_MCvTrt)
# p= 0.403, not significant

# plot
plot(Ch_MC$Perc_MC_Colonized~Ch_MC$Treatment2,
     pch=20,
     ylab="Mycorrhizal Colonization (%)",
     xlab="Nitrogen Treatment (g)",
     main= "All Species")
abline(lm(reg_Ch_MCvTrt), lwd=3, lty= 2, col = "black")
text(locator(), labels = c("p=403"))
text(locator(), labels = c("R2=-0.003"))


######## AbG , for all Species #####

reg_Ch_AbGvTrt <- lm(Aboveground_Biomass~Treatment2+(1/Block)+(1/Plot), data = Ch_MC)
summary(reg_Ch_AbGvTrt)
AIC(reg_Ch_AbGvTrt)
# p= 0.1667, not significant
# AIC: 452.385

# run second order Linear model 
reg_Ch_AbGvTrt2 <- glm(Aboveground_Biomass ~ poly(Treatment2, degree = 2, raw = T), data=Ch_MC, family = gaussian()) 
summary(reg_Ch_AbGvTrt2)
AIC(reg_Ch_AbGvTrt2)
# AIC: 454.29

# plot
plot(Ch_MC$Aboveground_Biomass~Ch_MC$Treatment2,
     pch=20,
     ylab="Aboveground Biomass (g)",
     xlab="Nitrogen Treatment (g)",
     main= "All Species")
abline(lm(reg_Ch_AbGvTrt), lwd=3, col = "blue")




