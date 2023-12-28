#_______________________________________________________________________________
#
# AQUACOSM-Plus: Statistics for Speed
# 21 February 2022
# Author: Anika Happe
#
#_______________________________________________________________________________

#HOUSEHOLD

rm(list=ls())
setwd("/Users/Anika/Library/Mobile Documents/com~apple~CloudDocs/PhD/AquaCosm/SupOpTrons_2022/4 NP Incubations/8 R Scripts")

#Load libraries,
library(readxl)
library(tidyverse)
library(MASS)
library(nlme)
library(reshape2)
library(sjPlot)
library(lattice)
library(mgcv)
library(lme4)
library(tidyverse)
library(psych)
library(ggplot2)
library(effects)
library(dplyr)
library(car)
library(performance)
library(modelsummary)
library(multcomp)
library(rstatix) #for tukey_hsd()

#Read data and prepare for usage for r and N:P
Data_Growth <- read_excel("Excel Files for Stats/STATISTICS_TABLE.xlsx")
Data_Growth$Temp <- as.factor(Data_Growth$Temp)
Data_Growth$Run <- as.factor(Data_Growth$Run)
Data_Growth$T_Treat <- as.factor(Data_Growth$T_Treat)
Data_Growth$T_ID <- as.factor(Data_Growth$T_ID)
Data_Growth <- mutate(Data_Growth, Treat_Unique = paste(T_ID, Run, sep = "_"))

Data_Growth <- mutate(Data_Growth, Balanced = "Balanced")
Data_Growth$Balanced[Data_Growth$Final_Ratio >= 42] <- "P-Limited"
Data_Growth$Balanced[Data_Growth$Final_Ratio <= 11] <- "N-Limited"

Data_Growth <- mutate(Data_Growth, growth_rate_2 = Growth_Lin+1)
Data_Growth <- filter(Data_Growth, Run == 1)
Data_Growth <- filter(Data_Growth, T_ID != "Constant_6")

Data_Growth$Temp <- as.factor(Data_Growth$Temp)
Data_Growth$Run <- as.factor(Data_Growth$Run)
Data_Growth$T_Treat <- as.factor(Data_Growth$T_Treat)
Data_Growth$T_ID <- as.factor(Data_Growth$T_ID)
Data_Growth$Balanced <- as.factor(Data_Growth$Balanced)

Data_NP <- filter(Data_Growth, Rep == 1)

#Read data and prepare for usage for LRR 
#(based on the calculated LRR in script 1) -> Take data from there
Data_LRR <- read_excel("Excel Files for Stats/SupOpTrons_LRR_Speed_2.xlsx")
Data_LRR$Temp <- as.factor(Data_LRR$Temp)
Data_LRR <- mutate(Data_LRR, Mean_Ratio = (Final_Ratio_Run1+Final_Ratio_Run2)/2)
Data_LRR <- mutate(Data_LRR, Balanced = "Balanced")
Data_LRR$Balanced[Data_LRR$Mean_Ratio >= 42] <- "P-Limited"
Data_LRR$Balanced[Data_LRR$Mean_Ratio <= 11] <- "N-Limited"

#_______________________________________________________________________________

#### TABLE 2 GLM: GROWTH ~ TEMP * RATIO * SPEED ####

# 1) Explore your data

#Data_NP <- filter(Data_Growth, Rep == 1)

# 2) Check for outliers

Data_Growth <- Data_Growth[-11, ]

par(mfrow = c(1, 3)) #Three figures in one graph
boxplot(Data_Growth$Growth_Lin) #Does it look normally distributed?
hist(Data_Growth$Growth_Lin) #Does it look normally distributed?
dotchart(Data_Growth$Growth_Lin) #Points very far away are outliers

# 6) Building the model

#Temp + Run

#T*N Interaction with all levels
glm_r_speed <- glm(Growth_Lin ~ Temp * Balanced * T_Treat, family = gaussian, data = Data_Growth)

plot(glm_r_speed) #Check the assumptions of the model!
hist(resid(glm_r_speed)) #Does the data look normally distributed?
summary(glm_r_speed)
summary(aov(glm_r_speed))
#tukey_results <- tukey_hsd(glm_r_speed)
#print(tukey_results, n = Inf)

#_______________________________________________________________________________

#### TABLE 2 GLM: LRR ~ TEMP * RATIO ####

# 2) Check for outliers

par(mfrow = c(1, 3)) #Three figures in one graph
boxplot(Data_LRR$LRR) #Does it look normally distributed?
hist(Data_LRR$LRR) #Does it look normally distributed?
dotchart(Data_LRR$LRR) #Points very far away are outliers

# 6) Building the model

#Temp + Run

#T*N Interaction with all levels
glm_lrr_speed <- glm(LRR ~ Temp * Balanced, family = gaussian, data = Data_LRR)
plot(glm_lrr_speed) #Check the assumptions of the model!
hist(resid(glm_lrr_speed)) #Does the data look normally distributed?
summary(aov(glm_lrr_speed))

#_______________________________________________________________________________

#### TABLE 2 GLM: N:P ~ TEMP * RATIO * SPEED ####

# 2) Check for outliers

Data_NP <- Data_NP[-1, ]

par(mfrow = c(1, 3)) #Three figures in one graph
boxplot(Data_NP$NP_Ratio) #Does it look normally distributed?
hist(Data_NP$NP_Ratio) #Does it look normally distributed?
dotchart(Data_NP$NP_Ratio) #Points very far away are outliers

# 6) Building the model

#Temp + Run

#T*N Interaction with all levels
glm_NP_speed <- glm(NP_Ratio ~ Temp * Balanced * T_Treat, family = gaussian, data = Data_NP)
plot(glm_NP_speed) #Check the assumptions of the model!
hist(resid(glm_NP_speed)) #Does the data look normally distributed?
summary(aov(glm_NP_speed))
#tukey_hsd(glm_NP_speed)

#_______________________________________________________________________________
