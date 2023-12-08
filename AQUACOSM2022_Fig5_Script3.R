#-------------------------------------------------------------------------------
#
# AQUACOSM-Plus: NP Incubations
# 10 November 2022
# Author: Anika Happe
#
#-------------------------------------------------------------------------------

#HOUSEHOLD

rm(list=ls())
setwd("/Users/Anika/Library/Mobile Documents/com~apple~CloudDocs/PhD/AquaCosm/SupOpTrons_2022/4 NP Incubations/8 R Scripts")

#Load libraries,
library(readxl)
library(tidyverse)
library(dplyr)
library(tidyr)

H1_Data <- read_excel("Excel Files for Stats/STATISTICS_TABLE.xlsx")
H1_Data <- filter(H1_Data, Run == 1)
#H1_Data <- filter(H1_Data, Temp != 6)

H1_Data <- mutate(H1_Data, Balanced = "Balanced")
#H1_Data$Balanced[H1_Data$Final_Ratio > 19] <- "Unbalanced"
#H1_Data$Balanced[H1_Data$Final_Ratio < 13] <- "Unbalanced"
H1_Data$Balanced[H1_Data$Final_Ratio > 43] <- "Limited"

H1_Data_6 <- filter(H1_Data, Temp == 6)
H1_Data_6$T_Treat <- "Ramp"

H1_Data_Full <- rbind(H1_Data, H1_Data_6)
H1_Data_Full_Mean <- dplyr::group_by(H1_Data_Full, Temp, T_Treat, Balanced)
H1_Data_Full_Mean <- dplyr::mutate(H1_Data_Full_Mean, Mean_r = mean(Growth_Lin), Sd_r = sd(Growth_Lin))
H1_Data_Full_Mean <- 

p <- ggplot(H1_Data_Full, aes(Temp, Growth_Lin, color = Balanced, shape = T_Treat)) +
  geom_jitter(width = 0.5, alpha = 0.6) +
  geom_smooth(data = subset(H1_Data_Full, T_Treat != "Ramp"), span = 0.8) +
  geom_smooth(data = subset(H1_Data_Full, T_Treat == "Ramp"), linetype = "dashed", span = 0.8) +
  ylim(0.15, 0.5)+
  scale_x_continuous(breaks = c(6, 12, 18))+
  theme_light()+
  theme(text = element_text(family = "Times"))
p

p <- ggplot(H1_Data_Full, aes(Temp, Growth_Lin, color = T_Treat, shape = Balanced)) +
  geom_jitter(width = 0.5, alpha = 0.6, size = 1) +
  geom_smooth(data = subset(H1_Data_Full, Balanced != "Limited"), span = 0.8) +
  geom_smooth(data = subset(H1_Data_Full, Balanced == "Limited"), linetype = "dashed", span = 0.8) +
  ylim(0.15, 0.5)+
  scale_x_continuous(breaks = c(6, 12, 18))+
  theme_light()+
  theme(text = element_text(family = "Times"))
p



ggplot(H1_Data_Full_Mean, aes(Temp, Mean_r, color = Balanced, shape = T_Treat))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=Mean_r-Sd_r, ymax=Mean_r+Sd_r), width=0.3, size=0.5,alpha=0.8)
  