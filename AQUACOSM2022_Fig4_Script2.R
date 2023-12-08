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
H1_Data$Balanced[H1_Data$Final_Ratio >= 42] <- "P-Limited"
H1_Data$Balanced[H1_Data$Final_Ratio <= 11] <- "N-Limited"

H1_Data_6 <- filter(H1_Data, Temp == 6)
H1_Data_6$T_Treat <- "Ramp"

H1_Data_Full <- rbind(H1_Data, H1_Data_6)
H1_Data_Full_Mean <- dplyr::group_by(H1_Data_Full, Temp, T_Treat, Balanced)
H1_Data_Full_Mean <- dplyr::mutate(H1_Data_Full_Mean, Mean_r = mean(Growth_Lin), Sd_r = sd(Growth_Lin))
 

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

?geom_smooth

ggplot(H1_Data_Full_Mean, aes(Temp, Mean_r, color = Balanced, shape = T_Treat))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=Mean_r-Sd_r, ymax=Mean_r+Sd_r), width=0.3, size=0.5,alpha=0.8)
  
#_______________________________________________________________________________

#Single plots

Data_Balanced <- filter(H1_Data_Full, Balanced == "Balanced")
p1 <- ggplot(Data_Balanced, aes(Temp, Growth_Lin, color = T_Treat, shape = Balanced)) +
  geom_jitter(width = 0.5, alpha = 0.6, size = 1) +
  geom_smooth()+
  #geom_smooth(data = subset(Data_Balanced, Balanced != "Limited"), span = 0.8) +
  #geom_smooth(data = subset(Data_Balanced, Balanced == "Limited"), linetype = "dashed", span = 0.8) +
  ylim(0.15, 0.5)+
  scale_x_continuous(breaks = c(6, 12, 18))+
  theme_light()+
  theme(text = element_text(family = "Times"))+
  ggtitle("Balanced Nutrients")
p1

p1_V2 <- ggplot(Data_Balanced, aes(x = factor(Temp), y = Growth_Lin, fill = T_Treat, shape = Balanced)) +
  geom_smooth(aes(group = T_Treat, color = T_Treat), method = "loess", se = TRUE) +
  geom_boxplot(position = position_dodge(width = 0.2), width = 0.2, show.legend = FALSE) +
  #geom_point(position = position_dodge(width = 0.6), size = 3, alpha = 0.6) +
  ylim(0.15, 0.5) +
  scale_x_discrete(labels = c("6", "12", "18")) +
  scale_color_discrete() +
  theme_light() +
  theme(text = element_text(family = "Times")) +
  ggtitle("Balanced Nutrients")
p1_V2

Data_Nlim <- filter(H1_Data_Full, Balanced == "N-Limited")
p2 <- ggplot(Data_Nlim, aes(Temp, Growth_Lin, color = T_Treat, shape = Balanced)) +
  geom_jitter(width = 0.5, alpha = 0.6, size = 1) +
  geom_smooth()+
  #geom_smooth(data = subset(Data_Balanced, Balanced != "Limited"), span = 0.8) +
  #geom_smooth(data = subset(Data_Balanced, Balanced == "Limited"), linetype = "dashed", span = 0.8) +
  ylim(0.15, 0.5)+
  scale_x_continuous(breaks = c(6, 12, 18))+
  theme_light()+
  theme(text = element_text(family = "Times"))+
  ggtitle("N-limited Nutrients")
p2

p2_V2 <- ggplot(Data_Nlim, aes(x = factor(Temp), y = Growth_Lin, fill = T_Treat, shape = Balanced)) +
  geom_smooth(aes(group = T_Treat, color = T_Treat), method = "loess", se = TRUE) +
  geom_boxplot(position = position_dodge(width = 0.2), width = 0.2, show.legend = FALSE) +
  #geom_point(position = position_dodge(width = 0.6), size = 3, alpha = 0.6) +
  ylim(0.15, 0.5) +
  scale_x_discrete(labels = c("6", "12", "18")) +
  scale_color_discrete() +
  theme_light() +
  theme(text = element_text(family = "Times")) +
  ggtitle("N-limited Nutrients")
p2_V2

Data_Plim <- filter(H1_Data_Full, Balanced == "P-Limited")
p3 <- ggplot(Data_Plim, aes(Temp, Growth_Lin, color = T_Treat, shape = Balanced)) +
  geom_jitter(width = 0.5, alpha = 0.6, size = 1) +
  geom_smooth()+
  #geom_smooth(data = subset(Data_Balanced, Balanced != "Limited"), span = 0.8) +
  #geom_smooth(data = subset(Data_Balanced, Balanced == "Limited"), linetype = "dashed", span = 0.8) +
  ylim(0.15, 0.5)+
  scale_x_continuous(breaks = c(6, 12, 18))+
  theme_light()+
  theme(text = element_text(family = "Times"))+
  ggtitle("P-limited Nutrients")
p3

#_______________________________________________________________________________

Data_Ramp <- filter(H1_Data_Full, T_Treat == "Ramp")
p2 <- ggplot(Data_Ramp, aes(Temp, Growth_Lin, color = Balanced)) +
  geom_jitter(width = 0.5, alpha = 0.6, size = 1) +
  geom_smooth()+
  #geom_smooth(data = subset(Data_Balanced, Balanced != "Limited"), span = 0.8) +
  #geom_smooth(data = subset(Data_Balanced, Balanced == "Limited"), linetype = "dashed", span = 0.8) +
  scale_color_manual(values = c("darkgreen", "cornflowerblue", "lightskyblue"))+
  ylim(0.15, 0.5)+
  scale_x_continuous(breaks = c(6, 12, 18))+
  theme_light()+
  #theme(text = element_text(family = "Times"))+
  ggtitle("Ramp")+
  xlab("Temperature (°C)")+
  ylab("Growth Rate (1/day)")
p2

p2 <- ggplot(Data_Ramp, aes(Temp, Growth_Lin, color = Balanced)) +
  geom_jitter(width = 0.5, size = 1) +
  geom_smooth(alpha = 1)+
  #geom_smooth(data = subset(Data_Balanced, Balanced != "Limited"), span = 0.8) +
  #geom_smooth(data = subset(Data_Balanced, Balanced == "Limited"), linetype = "dashed", span = 0.8) +
  scale_color_manual(values = c("darkgreen", "cornflowerblue", "lightskyblue"))+
  ylim(0.15, 0.5)+
  scale_x_continuous(breaks = c(6, 12, 18))+
  theme_light()+
  #theme(text = element_text(family = "Times"))+
  ggtitle("Ramp")+
  xlab("Temperature (°C)")+
  ylab("Growth Rate (1/day)")
p2

ggsave("supoptrons_fig6.eps", p2, device = "eps", unit = "cm", height = 7.5, width = 11, dpi = 300)


p2_Ramp_V2 <- ggplot(Data_Ramp, aes(x = factor(Temp), y = Growth_Lin, fill = Balanced, color = Balanced)) +
  geom_smooth(aes(group = Balanced, color = Balanced), method = "loess", se = TRUE) +
  geom_boxplot(position = position_dodge(width = 0.2), width = 0.2, show.legend = FALSE) +
  #geom_point(position = position_dodge(width = 0.6), size = 3, alpha = 0.6) +
  scale_color_manual(values = c("darkgreen", "cornflowerblue", "lightskyblue"))+
  ylim(0.15, 0.5)+
  #scale_x_continuous(breaks = c(6, 12, 18))+
  theme_light()+
  theme(text = element_text(family = "Times"))+
  ggtitle("Ramp")+
  xlab("Temperature (°C)")+
  ylab("Growth Rate (1/day)")
p2_Ramp_V2

Data_Constant <- filter(H1_Data_Full, T_Treat == "Constant")
Data_Constant_mean <- dplyr::group_by(Data_Constant, Temp, Balanced)
Data_Constant_mean <- dplyr::mutate(Data_Constant_mean, Mean_r = mean(Growth_Lin), Sd_r = sd(Growth_Lin))
p3 <- ggplot(Data_Constant, aes(Temp, Growth_Lin, color = Balanced)) +
  geom_jitter(width = 0.5, alpha = 0.6, size = 1) +
  geom_smooth()+
  #geom_smooth(data = subset(Data_Balanced, Balanced != "Limited"), span = 0.8) +
  #geom_smooth(data = subset(Data_Balanced, Balanced == "Limited"), linetype = "dashed", span = 0.8) +
  ylim(0.15, 0.5)+
  scale_color_manual(values = c("darkgreen", "cornflowerblue", "lightskyblue"))+
  scale_x_continuous(breaks = c(6, 12, 18))+
  theme_light()+
  theme(text = element_text(family = "Times"))+
  ggtitle("Constant")+
  xlab("Temperature (°C)")+
  ylab("Growth Rate (1/day)")
p3

p3 <- ggplot(Data_Constant, aes(Temp, Growth_Lin, color = Balanced)) +
  geom_jitter(width = 0.5, size = 1) +
  geom_smooth(alpha=1)+
  #geom_smooth(data = subset(Data_Balanced, Balanced != "Limited"), span = 0.8) +
  #geom_smooth(data = subset(Data_Balanced, Balanced == "Limited"), linetype = "dashed", span = 0.8) +
  ylim(0.15, 0.5)+
  scale_color_manual(values = c("darkgreen", "cornflowerblue", "lightskyblue"))+
  scale_x_continuous(breaks = c(6, 12, 18))+
  theme_light()+
  #theme(text = element_text(family = "Times"))+
  ggtitle("Constant")+
  xlab("Temperature (°C)")+
  ylab("Growth Rate (1/day)")
p3

ggsave("supoptrons_fig6_part2.eps", p3, device = "eps", unit = "cm", height = 7.5, width = 11, dpi = 300)


p3 <- ggplot(Data_Constant_mean, aes(Temp, Mean_r, color = Balanced)) +
  #geom_jitter(width = 0.5, alpha = 0.6, size = 1) +
  geom_smooth()+
  geom_errorbar(aes(ymin=Mean_r-Sd_r,ymax=Mean_r+Sd_r), width=0.3, linewidth=0.5,alpha=0.6)+
  #geom_smooth(data = subset(Data_Balanced, Balanced != "Limited"), span = 0.8) +
  #geom_smooth(data = subset(Data_Balanced, Balanced == "Limited"), linetype = "dashed", span = 0.8) +
  ylim(0.15, 0.5)+
  scale_x_continuous(breaks = c(6, 12, 18))+
  theme_light()+
  theme(text = element_text(family = "Times"))+
  ggtitle("Constant")
p3

