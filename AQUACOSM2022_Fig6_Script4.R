#-------------------------------------------------------------------------------
#
# AQUACOSM-Plus: SupOpTrons 2022
# NP Incubations - Stoichiometry
# 20 September 2022
# Anika Happe
#
#-------------------------------------------------------------------------------

#DATA HANDLING

rm(list=ls())
setwd("/Users/Anika/Library/Mobile Documents/com~apple~CloudDocs/PhD/AquaCosm/SupOpTrons_2022/4 NP Incubations/8 R Scripts")

#Load packages
library(plotly)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(withr)
library(colorRamps)
library(colorspace)

#Read data
data <- read_excel("Table_NP_End_SupOpTrons_2022.xlsx")
data <- mutate(data, Maren_P_mol_L = Maren_P_µmol_L/1000000)
data <- mutate(data, CN_Ratio = C_mol_L/N_mol_L, CP_Ratio = C_mol_L/Maren_P_mol_L, NP_Ratio = N_mol_L/Maren_P_mol_L)
data <- mutate(data, P_Back = P_Supply+0.31)
data <- mutate(data, N_Back = N_Supply+18.07)
data <- mutate(data, Ratio_Back = N_Back/P_Back)
data <- filter(data, Temp_Treat != "Start")
data <- mutate(data, T_ID = paste(Temp_Treat, Temp_C, sep="_"))

data_1 <- data
data_1$Temp_Treat[data_1$Run == 2] <- "Ramp"
data_1$Temp_Treat[data_1$Temp_C == 6] <- "Ambient"

#_______________________________________________________________________________

part1 <- ggplot(data_1, aes(log(Ratio_Back), NP_Ratio, color = Temp_Treat, shape = as.factor(Run), linetype = factor(Run))) +
  geom_point(alpha=0.4) +
  geom_smooth(method="gam") +
  theme_light()+
  scale_color_manual(values = c("grey28", "#EC7D10", "deepskyblue3"))+
  facet_grid(. ~ Temp_C)+
  theme(legend.position = "bottom")

data_2 <- data_1
data_2 <- filter(data_2, Ratio_Back < 40)
part2 <- ggplot(data_2, aes(Ratio_Back, NP_Ratio, color = Temp_Treat, shape = as.factor(Run), linetype = factor(Run))) +
  geom_point(alpha = 0.4) +
  geom_smooth(method="gam") +
  theme_light()+
  scale_color_manual(values = c("grey28", "#EC7D10", "deepskyblue3"))+
  facet_grid(. ~ Temp_C)+
  theme(legend.position = "bottom")

ggsave("supoptrons_fig5_part1.eps", part1, device = "eps", unit = "cm", height = 7.5, width = 15, dpi = 300)
ggsave("supoptrons_fig5_part2.eps", part2, device = "eps", unit = "cm", height = 7.5, width = 15, dpi = 300)

  
