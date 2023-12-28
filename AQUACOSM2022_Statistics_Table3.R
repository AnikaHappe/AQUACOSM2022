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
Data_Growth <- filter(Data_Growth, Treat_Unique != "Constant_12_1")
Data_Growth <- filter(Data_Growth, Treat_Unique != "Constant_18_1")

#Read data and prepare for usage for LRR
Data_LRR <- read_excel("Excel Files for Stats/SupOpTrons_LRR_NutAv.xlsx")
Data_LRR$Temp <- as.factor(Data_LRR$Temp)
Data_LRR <- mutate(Data_LRR, Mean_Ratio = (Final_Ratio_Run1+Final_Ratio_Run2)/2)
Data_LRR <- mutate(Data_LRR, Balanced = "Balanced")
Data_LRR$Balanced[Data_LRR$Mean_Ratio >= 42] <- "P-Limited"
Data_LRR$Balanced[Data_LRR$Mean_Ratio <= 11] <- "N-Limited"

#_______________________________________________________________________________

#### TABLE 3 GLM: GROWTH ~ TEMP * RATIO * NUTAV ####

#Here the data has been box-cox transformed due to a strong right-shift of the
#data. Afterwards, four outliers have been removed based on the assumption plots.

# 1) Explore your data

Data_Growth_H2 <- mutate(Data_Growth, Growth_Lin_3 = Growth_Lin+1) #Because box-cox transformation does not support negative values

# 2) Check for outliers

#Maybe transform the data since the distribution is right-shifted?

Data_Growth_H2 <- Data_Growth_H2[-171, ]
Data_Growth_H2 <- Data_Growth_H2[-153, ]
Data_Growth_H2 <- Data_Growth_H2[-171, ]
Data_Growth_H2 <- Data_Growth_H2[-240, ]
par(mfrow = c(1, 3)) #Three figures in one graph
boxplot((Data_Growth_H2$Growth_Lin_3)^3) #Does it look normally distributed?
hist((Data_Growth_H2$Growth_Lin_3)^3) #Does it look normally distributed?
dotchart((Data_Growth_H2$Growth_Lin_3)^3) #Points very far away are outliers

# 6) Building the model

#T*N Interaction with all levels
glm.growth.TNP <- glm((Growth_Lin_3)^3 ~ Temp * Balanced * Run, data = Data_Growth_H2, family = gaussian)
plot(glm.growth.TNP) #Check the assumptions of the model!
hist(resid(glm.growth.TNP)) #Does the data look normally distributed?
#summary(glm.growth.TNP)
summary(aov(glm.growth.TNP))

#This is to test which exponent works best!
bc = boxcox(glm.growth.TNP, lambda = seq(-4,4))
best.lam <- bc$x[which(bc$y==max(bc$y))]
best.lam #This gives you the best exponent!

#_______________________________________________________________________________

#### TABLE 3 GLM: LRR ~ TEMP * RATIO ####

# 2) Check for outliers

Data_LRR <- Data_LRR[-1, ]
Data_LRR <- Data_LRR[-70, ]
Data_LRR <- Data_LRR[-69, ]
Data_LRR <- Data_LRR[-1, ]

par(mfrow = c(1, 3)) #Three figures in one graph
boxplot(Data_LRR$LRR) #Does it look normally distributed?
hist(Data_LRR$LRR) #Does it look normally distributed?
dotchart(Data_LRR$LRR) #Points very far away are outliers

# 6) Building the model

#Temp + Run

#T*N Interaction with all levels
glm_LRR <- glm(LRR ~ (Temp * Balanced), family = gaussian, data = Data_LRR)
plot(glm_LRR) #Check the assumptions of the model!
hist(resid(glm_LRR)) #Does the data look normally distributed?
summary(aov(glm_LRR))

#_______________________________________________________________________________

#### TABLE 3 GLM: N:P ~ TEMP * RATIO * NUTAV ####

Data_NP_2 <- Data_Growth

# 2) Check for outliers

Data_NP_2 <- Data_NP_2[-126, ]
Data_NP_2 <- Data_NP_2[-14, ]
Data_NP_2 <- Data_NP_2[-76, ]
Data_NP_2 <- Data_NP_2[-75, ]
Data_NP_2 <- Data_NP_2[-147, ]

par(mfrow = c(1, 3)) #Three figures in one graph
boxplot(Data_NP_2$NP_Ratio) #Does it look normally distributed?
hist(Data_NP_2$NP_Ratio) #Does it look normally distributed?
dotchart(Data_NP_2$NP_Ratio) #Points very far away are outliers

# 6) Building the model

#Temp + Run

#T*N Interaction with all levels
glm_3 <- glm(log(NP_Ratio) ~ (Temp * Balanced * Run), family = gaussian, data = Data_NP_2)
plot(glm_3) #Check the assumptions of the model!
hist(resid(glm_3)) #Does the data look normally distributed?
summary(aov(glm_3))

#_______________________________________________________________________________


