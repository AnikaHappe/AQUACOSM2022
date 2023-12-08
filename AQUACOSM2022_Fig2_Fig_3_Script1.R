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
library(fields) #for tps plots
library(viridis)
library(reshape2)
library(MBA)

#Read data and prepare for usage
data_run1 <- read_excel("NP_incubations_run1.xlsx")
data_run2 <- read_excel("NP_incubations_run2.xlsx")
data_run1 <- dplyr::select(data_run1, Timepassed, T_ID, T_Treat, Temp, N, P, Ratio, Rep, Chl)
data_run2 <- dplyr::select(data_run2, Timepassed, T_ID, T_Treat, Temp, N, P, Ratio, Rep, Chl)
data_run1 <- mutate(data_run1, Run = 1)
data_run2 <- mutate(data_run2, Run = 2)

#-------------------------------------------------------------------------------

#CALCULATING LINEAR GROWTH RATES

#Filter for acclimation type since they grew with a time-delay (need a different
#selection of end days), then make the data table wide (one column for each sampling
#day) which makes it possible to choose the days for calculating the growth rate.
#Then calculate the mean growth rate (for each treatment) and filter for one replicate
#only (they should have the same value for the mean, check!)

#Temperature Ramp Treatments, Run 1
data_run1_ramp <- filter(data_run1, T_Treat == "Ramp")
data_run1_ramp <- pivot_wider(data_run1_ramp, names_from = Timepassed, names_prefix = "T", values_from = Chl)
data_run1_ramp <- mutate(data_run1_ramp, growth_rate = (log(T10)-log(T0))/(10-0))
data_run1_ramp <- group_by(data_run1_ramp, Temp, N, P)
data_run1_ramp <- mutate(data_run1_ramp, growth_mean = mean(growth_rate))
data_run1_ramp <- filter(data_run1_ramp, Rep == 1)
data_run1_ramp <- dplyr::select(data_run1_ramp, T_ID, T_Treat, Temp, N, P, Ratio, Run, growth_rate, growth_mean)

#Constant Temperature Treatments, Run 1
data_run1_const <- filter(data_run1, T_Treat == "Constant")
data_run1_const <- pivot_wider(data_run1_const, names_from = Timepassed, names_prefix = "T", values_from = Chl)
data_run1_const <- mutate(data_run1_const, growth_rate = (log(T8)-log(T0))/(8-0))
data_run1_const <- group_by(data_run1_const, Temp, N, P)
data_run1_const <- mutate(data_run1_const, growth_mean = mean(growth_rate))
data_run1_const <- filter(data_run1_const, Rep == 1)
data_run1_const <- dplyr::select(data_run1_const, T_ID, T_Treat, Temp, N, P, Ratio, Run, growth_rate, growth_mean)

#Constant Temperature Treatments, Run 2
data_run2_const <- filter(data_run2, T_Treat == "Constant")
data_run2_const <- tidyr::pivot_wider(data_run2_const, names_from = Timepassed, names_prefix = "T", values_from = Chl)
data_run2_const$T6[data_run2_const$T6==0] <- 1
data_run2_const <- mutate(data_run2_const, growth_rate = (log(T6)-log(T0))/(6-0))
data_run2_const <- group_by(data_run2_const, Temp, N, P)
data_run2_const <- mutate(data_run2_const, growth_mean = mean(growth_rate))
data_run2_const <- filter(data_run2_const, Rep == 1)
data_run2_const <- dplyr::select(data_run2_const, T_ID, T_Treat, Temp, N, P, Ratio, Run, growth_rate, growth_mean)

#Create a data table to put everything together
data_growth <- rbind(data_run1_ramp, data_run1_const, data_run2_const)

#library(writexl)
#write_xlsx(data_growth, "NP_incubations_growth_rates_linear.xlsx")

#-------------------------------------------------------------------------------

#PLOTTING THE GROWTH RATES

#These are just normal response surface plots. For plot 1, we mostly see the 
#phosphorus limitation and then a diffus pattern. For plot 2 and 3, we see a
#diverging pattern: While the constant temperatures in run 1 have lowest growth
#at 6°C, they have highest growth in run 2. Still rather diffus across the 
#nutrient levels. Here a LRR would be interesting!

#Plot 0: All data together
data_growth_2 <- data_growth
data_growth_2$T_Treat[data_growth_2$Run == 2] <- "Ramp"
data_growth_2$T_ID[data_growth_2$T_ID == "Constant-12" & data_growth_2$Run == 2] <- "Slow-to-12"
data_growth_2$T_ID[data_growth_2$T_ID == "Constant-18" & data_growth_2$Run == 2] <- "Slow-to-18"
data_growth_2$T_ID[data_growth_2$T_ID == "Constant-6"] <- "Slow-to-6"
data_growth_2$T_ID[data_growth_2$T_ID == "Ramp-to-12" & data_growth_2$Run == 1] <- "Slow-to-12"
data_growth_2$T_ID[data_growth_2$T_ID == "Ramp-to-18" & data_growth_2$Run == 1] <- "Slow-to-18"
data_growth_2$T_ID[data_growth_2$T_ID == "Constant-12" & data_growth_2$Run == 1] <- "Fast-to-12"
data_growth_2$T_ID[data_growth_2$T_ID == "Constant-18" & data_growth_2$Run == 1] <- "Fast-to-18"

data_growth_X <- filter(data_growth_2, Run == 1)
data_growth_X <- filter(data_growth_X, Temp != 6)
data_growth_X <- group_by(data_growth_X, T_Treat, Temp)
data_growth_X <- summarize(data_growth_X, mean = mean(growth_mean))

#data_growth_2$growth_mean[data_growth_2$growth_mean < 0] <- 0
data_growth_2$T_ID = factor(data_growth_2$T_ID, levels=c("Slow-to-6","Slow-to-12","Slow-to-18","Fast-to-12","Fast-to-18"))
ggplot(data = data_growth_2, aes(x=as.factor(N), y=as.factor(P), fill=growth_mean))+
  geom_raster(interpolate = TRUE)+
  geom_abline(intercept = 0, slope = 1, colour="black", alpha = 0.5, linetype=3)+
  scale_shape_manual(values=c(8), guide = 'none')+
  scale_fill_distiller(palette = "Spectral")+
  theme(text=element_text(size=14,  family="Times New Roman"))+
  ggtitle("Linear Growth Rate between two time points")+
  xlab("Nitrogen")+
  ylab("Phosphorus")+
  #theme(legend.position = "bottom", legend.direction = "horizontal")+
  facet_grid(Run~T_ID)

#_______________________________________________________________________________

#Response Surface as Interpolation (script from Jakob)

#Prepare data sets

#Interpolate the data (no.X or Y refers to the resolution you want to interpolate)
#The columns that mba.surf() will interpolate are the X, Y, Z values in that order

Dat1 <- filter(data_growth_2, Run == 1, T_ID == "Slow-to-6")
Dat1_mba <- mba.surf(Dat1[c("N", "P", "growth_mean")], no.X = 15, no.Y = 15, extend = T)
dimnames(Dat1_mba$xyz.est$z) <- list(Dat1_mba$xyz.est$x, Dat1_mba$xyz.est$y)
Dat1_mba <- melt(Dat1_mba$xyz.est$z, varnames = c('N', 'P'), value.name = 'growth_mean') #hier musst du halt die  x y z namen aendern 
Dat1_mba <- mutate(Dat1_mba, T_ID = "Slow-to-6", Run = 1)

Dat2 <- filter(data_growth_2, Run == 1, T_ID == "Slow-to-12")
Dat2_mba <- mba.surf(Dat2[c("N", "P", "growth_mean")], no.X = 15, no.Y = 15, extend = T)
dimnames(Dat2_mba$xyz.est$z) <- list(Dat2_mba$xyz.est$x, Dat2_mba$xyz.est$y)
Dat2_mba <- melt(Dat2_mba$xyz.est$z, varnames = c('N', 'P'), value.name = 'growth_mean') #hier musst du halt die  x y z namen aendern 
Dat2_mba <- mutate(Dat2_mba, T_ID = "Slow-to-12", Run = 1)

Dat3 <- filter(data_growth_2, Run == 1, T_ID == "Slow-to-18")
Dat3_mba <- mba.surf(Dat3[c("N", "P", "growth_mean")], no.X = 15, no.Y = 15, extend = T)
dimnames(Dat3_mba$xyz.est$z) <- list(Dat3_mba$xyz.est$x, Dat3_mba$xyz.est$y)
Dat3_mba <- melt(Dat3_mba$xyz.est$z, varnames = c('N', 'P'), value.name = 'growth_mean') #hier musst du halt die  x y z namen aendern 
Dat3_mba <- mutate(Dat3_mba, T_ID = "Slow-to-18", Run = 1)

Dat4 <- filter(data_growth_2, Run == 1, T_ID == "Fast-to-12")
Dat4_mba <- mba.surf(Dat4[c("N", "P", "growth_mean")], no.X = 15, no.Y = 15, extend = T)
dimnames(Dat4_mba$xyz.est$z) <- list(Dat4_mba$xyz.est$x, Dat4_mba$xyz.est$y)
Dat4_mba <- melt(Dat4_mba$xyz.est$z, varnames = c('N', 'P'), value.name = 'growth_mean') #hier musst du halt die  x y z namen aendern 
Dat4_mba <- mutate(Dat4_mba, T_ID = "Fast-to-12", Run = 1)

Dat5 <- filter(data_growth_2, Run == 1, T_ID == "Fast-to-18")
Dat5_mba <- mba.surf(Dat5[c("N", "P", "growth_mean")], no.X = 15, no.Y = 15, extend = T)
dimnames(Dat5_mba$xyz.est$z) <- list(Dat5_mba$xyz.est$x, Dat5_mba$xyz.est$y)
Dat5_mba <- melt(Dat5_mba$xyz.est$z, varnames = c('N', 'P'), value.name = 'growth_mean') #hier musst du halt die  x y z namen aendern 
Dat5_mba <- mutate(Dat5_mba, T_ID = "Fast-to-18", Run = 1)

Dat6 <- filter(data_growth_2, Run == 2, T_ID == "Slow-to-6")
Dat6_mba <- mba.surf(Dat6[c("N", "P", "growth_mean")], no.X = 15, no.Y = 15, extend = T)
dimnames(Dat6_mba$xyz.est$z) <- list(Dat6_mba$xyz.est$x, Dat6_mba$xyz.est$y)
Dat6_mba <- melt(Dat6_mba$xyz.est$z, varnames = c('N', 'P'), value.name = 'growth_mean') #hier musst du halt die  x y z namen aendern 
Dat6_mba <- mutate(Dat6_mba, T_ID = "Slow-to-6", Run = 2)

Dat7 <- filter(data_growth_2, Run == 2, T_ID == "Slow-to-12")
Dat7_mba <- mba.surf(Dat7[c("N", "P", "growth_mean")], no.X = 15, no.Y = 15, extend = T)
dimnames(Dat7_mba$xyz.est$z) <- list(Dat7_mba$xyz.est$x, Dat7_mba$xyz.est$y)
Dat7_mba <- melt(Dat7_mba$xyz.est$z, varnames = c('N', 'P'), value.name = 'growth_mean') #hier musst du halt die  x y z namen aendern 
Dat7_mba <- mutate(Dat7_mba, T_ID = "Slow-to-12", Run = 2)

Dat8 <- filter(data_growth_2, Run == 2, T_ID == "Slow-to-18")
Dat8_mba <- mba.surf(Dat8[c("N", "P", "growth_mean")], no.X = 15, no.Y = 15, extend = T)
dimnames(Dat8_mba$xyz.est$z) <- list(Dat8_mba$xyz.est$x, Dat8_mba$xyz.est$y)
Dat8_mba <- melt(Dat8_mba$xyz.est$z, varnames = c('N', 'P'), value.name = 'growth_mean') #hier musst du halt die  x y z namen aendern 
Dat8_mba <- mutate(Dat8_mba, T_ID = "Slow-to-18", Run = 2)

AllGrowth_mba <- rbind(Dat1_mba, Dat2_mba, Dat3_mba, Dat4_mba, Dat5_mba, Dat6_mba, Dat7_mba, Dat8_mba)
AllGrowth_mba_0 <- AllGrowth_mba
AllGrowth_mba_0$growth_mean[AllGrowth_mba_0$growth_mean < 0] <- 0
AllGrowth_mba_0$T_ID = factor(AllGrowth_mba_0$T_ID, levels=c("Slow-to-6","Slow-to-12","Slow-to-18","Fast-to-12","Fast-to-18"))
data_growth_2$T_ID = factor(data_growth_2$T_ID, levels=c("Slow-to-6","Slow-to-12","Slow-to-18","Fast-to-12","Fast-to-18"))
ggplot(data = AllGrowth_mba_0, aes(x = N, y = P)) +
  geom_raster(aes(fill = growth_mean)) +
  #geom_contour(aes(z = growth_mean), binwidth = 0.05, size = 0.2, colour = "black") +
  scale_fill_viridis_c(option="viridis",direction=1, limits = c(0, 0.55))+
  coord_cartesian(expand = F)+
  #scale_fill_gradient2(low="#F8766D", mid="gray90", high="royalblue3", 
                       #limits = c(0, 0.6), oob = scales::squish)+
  scale_fill_distiller(palette = "Spectral")+
  geom_point(data = data_growth_2, aes(x= N, y= P), size= 0.5, alpha = 0.7)+
  #scale_y_continuous(breaks=c(0,4,16,24))+
  #scale_x_continuous(breaks=c(4,9,13.5))+
  theme_light()+
  labs(x="Nitrogen (µmol/L)",y="Phosphorus (µmol/L)",fill="r")+
  #theme(legend.direction = "horizontal", legend.position = "bottom")+
  facet_grid(Run ~ T_ID)

#_______________________________________________________________________________

#### LRR ####

data_lrr <- data_growth_2
data_lrr_speed <- filter(data_lrr, Run == 1)
data_lrr_speed <- ungroup(data_lrr_speed)
data_lrr_speed <- dplyr::select(data_lrr_speed, "T_ID", "N", "P", "growth_mean")
data_lrr_speed_slow12 <- filter(data_lrr_speed, T_ID == "Slow-to-12" | T_ID == "Slow-to-6")
data_lrr_speed_slow12 <- spread(data_lrr_speed_slow12, key = T_ID, value = growth_mean)
data_lrr_speed_slow12 <- mutate(data_lrr_speed_slow12, Slow12 = data_lrr_speed_slow12$`Slow-to-12`/data_lrr_speed_slow12$`Slow-to-6`)
data_lrr_speed_slow12 <- mutate(data_lrr_speed_slow12, Slow12 = log10(Slow12))
data_lrr_speed_slow12 <- mutate(data_lrr_speed_slow12, Log_ID = "Slow12", Temp = 12, T_ID = "Slow")
data_lrr_speed_slow12 <- rename(data_lrr_speed_slow12, "LRR" = "Slow12")
data_lrr_speed_slow12 <- select(data_lrr_speed_slow12, T_ID, Temp, N, P, Log_ID, LRR)

data_lrr_speed_fast12 <- filter(data_lrr_speed, T_ID == "Fast-to-12" | T_ID == "Slow-to-6")
data_lrr_speed_fast12 <- spread(data_lrr_speed_fast12, key = T_ID, value = growth_mean)
data_lrr_speed_fast12 <- mutate(data_lrr_speed_fast12, fast12 = data_lrr_speed_fast12$`Fast-to-12`/data_lrr_speed_fast12$`Slow-to-6`)
data_lrr_speed_fast12 <- mutate(data_lrr_speed_fast12, fast12 = log10(fast12))
data_lrr_speed_fast12 <- mutate(data_lrr_speed_fast12, Log_ID = "Fast12", Temp = 12, T_ID = "Fast")
data_lrr_speed_fast12 <- rename(data_lrr_speed_fast12, "LRR" = "fast12")
data_lrr_speed_fast12 <- select(data_lrr_speed_fast12, T_ID, Temp, N, P, Log_ID, LRR)

data_lrr_speed_slow18 <- filter(data_lrr_speed, T_ID == "Slow-to-18" | T_ID == "Slow-to-6")
data_lrr_speed_slow18 <- spread(data_lrr_speed_slow18, key = T_ID, value = growth_mean)
data_lrr_speed_slow18 <- mutate(data_lrr_speed_slow18, Slow18 = data_lrr_speed_slow18$`Slow-to-18`/data_lrr_speed_slow18$`Slow-to-6`)
data_lrr_speed_slow18 <- mutate(data_lrr_speed_slow18, Slow18 = log10(Slow18))
data_lrr_speed_slow18 <- mutate(data_lrr_speed_slow18, Log_ID = "Slow18", Temp = 18, T_ID = "Slow")
data_lrr_speed_slow18 <- rename(data_lrr_speed_slow18, "LRR" = "Slow18")
data_lrr_speed_slow18 <- select(data_lrr_speed_slow18, T_ID, Temp, N, P, Log_ID, LRR)

data_lrr_speed_fast18 <- filter(data_lrr_speed, T_ID == "Fast-to-18" | T_ID == "Slow-to-6")
data_lrr_speed_fast18 <- spread(data_lrr_speed_fast18, key = T_ID, value = growth_mean)
data_lrr_speed_fast18 <- mutate(data_lrr_speed_fast18, Fast18 = data_lrr_speed_fast18$`Fast-to-18`/data_lrr_speed_fast18$`Slow-to-6`)
data_lrr_speed_fast18 <- mutate(data_lrr_speed_fast18, Fast18 = log10(Fast18))
data_lrr_speed_fast18 <- mutate(data_lrr_speed_fast18, Log_ID = "Fast18", Temp = 18, T_ID = "Fast")
data_lrr_speed_fast18 <- rename(data_lrr_speed_fast18, "LRR" = "Fast18")
data_lrr_speed_fast18 <- select(data_lrr_speed_fast18, T_ID, Temp, N, P, Log_ID, LRR)

data_lrr_speed_full <- rbind(data_lrr_speed_slow12, data_lrr_speed_fast12, data_lrr_speed_slow18, data_lrr_speed_fast18)
#library(writexl)
#write_xlsx(data_lrr_speed_full, "SupOpTrons_LRR_TSpeed.xlsx")

ggplot(data = data_lrr_speed_full, aes(x=as.factor(N), y=as.factor(P), fill=LRR))+
  geom_raster(interpolate = TRUE)+
  geom_abline(intercept = 0, slope = 1, colour="black", alpha = 0.5, linetype=3)+
  scale_shape_manual(values=c(8), guide = 'none')+
  #scale_fill_distiller(palette = "Spectral")+
  scale_fill_viridis_c(limits= c(-0.16, 0.26), breaks = c(-0.1, 0, 0.1, 0.2))+
  theme(text=element_text(size=14,  family="Times New Roman"))+
  ggtitle("LRR: Treatment / Control")+
  xlab("Nitrogen")+
  ylab("Phosphorus")+
  #theme(legend.position = "bottom", legend.direction = "horizontal")
  facet_grid(.~Log_ID)

#_______________________________________________________________________________

data_lrr_nut <- data_growth_2
#data_lrr_nut <- filter(data_lrr, Temp != 6)
data_lrr_nut <- filter(data_lrr_nut, T_ID != "Fast-to-12")
data_lrr_nut <- filter(data_lrr_nut, T_ID != "Fast-to-18")
data_lrr_nut <- dplyr::select(data_lrr_nut, "Run", "Temp", "N", "P", "growth_mean")
data_lrr_nut$growth_mean <- ifelse(data_lrr_nut$growth_mean < 0, 0,data_lrr_nut$growth_mean)
data_lrr_nut$growth_mean <- data_lrr_nut$growth_mean +1 
data_lrr_nut$Run <- as.factor(data_lrr_nut$Run)
data_lrr_nut$Run <- ifelse(data_lrr_nut$Run == "1", "Run1", "Run2")
data_lrr_nut <- spread(data_lrr_nut, key = Run, value = growth_mean)
data_lrr_nut <- mutate(data_lrr_nut, ratio = Run1/Run2)
data_lrr_nut <- mutate(data_lrr_nut, logratio = log10(ratio))

#library(writexl)
#write_xlsx(data_lrr_nut, "SupOpTrons_LRR_NutAv.xlsx")

ggplot(data = data_lrr_nut, aes(x=as.factor(N), y=as.factor(P), fill=logratio))+
  geom_raster(interpolate = TRUE)+
  geom_abline(intercept = 0, slope = 1, colour="black", alpha = 0.5, linetype=3)+
  scale_shape_manual(values=c(8), guide = 'none')+
  #scale_fill_distiller(palette = "Spectral")+
  scale_fill_viridis_c(limits= c(-0.15, 0.25), breaks = c(-0.1, 0, 0.1, 0.2))+
  theme(text=element_text(size=14,  family="Times New Roman"))+
  ggtitle("LRR: Run addition / Run depletion")+
  xlab("Nitrogen")+
  ylab("Phosphorus")+
  #theme(legend.position = "bottom", legend.direction = "horizontal")+
  facet_grid(.~Temp)

#_______________________________________________________________________________

data_lrr <- data_growth_2
data_lrr_speed <- filter(data_lrr, Run == 1)
data_lrr_speed <- ungroup(data_lrr_speed)
data_lrr_speed <- dplyr::select(data_lrr_speed, "T_ID", "N", "P", "growth_mean")
data_lrr_speed_T12 <- filter(data_lrr_speed, T_ID == "Slow-to-12" | T_ID == "Fast-to-12")
data_lrr_speed_T12 <- spread(data_lrr_speed_T12, key = T_ID, value = growth_mean)
data_lrr_speed_T12 <- mutate(data_lrr_speed_T12, Temp12 = data_lrr_speed_T12$`Slow-to-12`/data_lrr_speed_T12$`Fast-to-12`)
data_lrr_speed_T12 <- mutate(data_lrr_speed_T12, Temp12 = log10(Temp12))
data_lrr_speed_T12 <- mutate(data_lrr_speed_T12, Log_ID = "Temp12", Temp = 12)
data_lrr_speed_T12 <- rename(data_lrr_speed_T12, "LRR" = "Temp12")
data_lrr_speed_T12 <- select(data_lrr_speed_T12, Temp, N, P, Log_ID, LRR)

data_lrr_speed_T18 <- filter(data_lrr_speed, T_ID == "Slow-to-18" | T_ID == "Fast-to-18")
data_lrr_speed_T18 <- spread(data_lrr_speed_T18, key = T_ID, value = growth_mean)
data_lrr_speed_T18 <- mutate(data_lrr_speed_T18, Temp18 = data_lrr_speed_T18$`Slow-to-18`/data_lrr_speed_T18$`Fast-to-18`)
data_lrr_speed_T18 <- mutate(data_lrr_speed_T18, Temp18 = log10(Temp18))
data_lrr_speed_T18 <- mutate(data_lrr_speed_T18, Log_ID = "Temp18", Temp = 18)
data_lrr_speed_T18 <- rename(data_lrr_speed_T18, "LRR" = "Temp18")
data_lrr_speed_T18 <- select(data_lrr_speed_T18, Temp, N, P, Log_ID, LRR)

data_lrr_speed_full_2 <- rbind(data_lrr_speed_T12, data_lrr_speed_T18)
library(writexl)
write_xlsx(data_lrr_speed_full_2, "SupOpTrons_LRR_Speed_2.xlsx")

Dat_LRR1 <- data_lrr_speed_T12
Dat_LRR1 <- mba.surf(Dat_LRR1[c("N", "P", "LRR")], no.X = 15, no.Y = 15, extend = T)
dimnames(Dat_LRR1$xyz.est$z) <- list(Dat_LRR1$xyz.est$x, Dat_LRR1$xyz.est$y)
Dat_LRR1 <- melt(Dat_LRR1$xyz.est$z, varnames = c('N', 'P'), value.name = 'LRR') #hier musst du halt die  x y z namen aendern 
Dat_LRR1 <- mutate(Dat_LRR1, T_ID = "Temp12")

Dat_LRR2 <- data_lrr_speed_T18
Dat_LRR2 <- mba.surf(Dat_LRR2[c("N", "P", "LRR")], no.X = 15, no.Y = 15, extend = T)
dimnames(Dat_LRR2$xyz.est$z) <- list(Dat_LRR2$xyz.est$x, Dat_LRR2$xyz.est$y)
Dat_LRR2 <- melt(Dat_LRR2$xyz.est$z, varnames = c('N', 'P'), value.name = 'LRR') #hier musst du halt die  x y z namen aendern 
Dat_LRR2 <- mutate(Dat_LRR2, T_ID = "Temp18")

Dat_LRR_Nut_6 <- filter(data_lrr_nut, Temp == 6)
Dat_LRR_Nut_6 <- mba.surf(Dat_LRR_Nut_6[c("N", "P", "logratio")], no.X = 15, no.Y = 15, extend = T)
dimnames(Dat_LRR_Nut_6$xyz.est$z) <- list(Dat_LRR_Nut_6$xyz.est$x, Dat_LRR_Nut_6$xyz.est$y)
Dat_LRR_Nut_6 <- melt(Dat_LRR_Nut_6$xyz.est$z, varnames = c('N', 'P'), value.name = 'logratio') #hier musst du halt die  x y z namen aendern 
Dat_LRR_Nut_6 <- mutate(Dat_LRR_Nut_6, T_ID = "Temp6_Nut")
Dat_LRR_Nut_6 <- Dat_LRR_Nut_6 %>% rename(LRR = logratio)

Dat_LRR_Nut_12 <- filter(data_lrr_nut, Temp == 12)
Dat_LRR_Nut_12 <- mba.surf(Dat_LRR_Nut_12[c("N", "P", "logratio")], no.X = 15, no.Y = 15, extend = T)
dimnames(Dat_LRR_Nut_12$xyz.est$z) <- list(Dat_LRR_Nut_12$xyz.est$x, Dat_LRR_Nut_12$xyz.est$y)
Dat_LRR_Nut_12 <- melt(Dat_LRR_Nut_12$xyz.est$z, varnames = c('N', 'P'), value.name = 'logratio') #hier musst du halt die  x y z namen aendern 
Dat_LRR_Nut_12 <- mutate(Dat_LRR_Nut_12, T_ID = "Temp12_Nut")
Dat_LRR_Nut_12 <- Dat_LRR_Nut_12 %>% rename(LRR = logratio)

Dat_LRR_Nut_18 <- filter(data_lrr_nut, Temp == 18)
Dat_LRR_Nut_18 <- mba.surf(Dat_LRR_Nut_18[c("N", "P", "logratio")], no.X = 15, no.Y = 15, extend = T)
dimnames(Dat_LRR_Nut_18$xyz.est$z) <- list(Dat_LRR_Nut_18$xyz.est$x, Dat_LRR_Nut_18$xyz.est$y)
Dat_LRR_Nut_18 <- melt(Dat_LRR_Nut_18$xyz.est$z, varnames = c('N', 'P'), value.name = 'logratio') #hier musst du halt die  x y z namen aendern 
Dat_LRR_Nut_18 <- mutate(Dat_LRR_Nut_18, T_ID = "Temp18_Nut")
Dat_LRR_Nut_18 <- Dat_LRR_Nut_18 %>% rename(LRR = logratio)

LRR_ALL <- rbind(Dat_LRR1, Dat_LRR2, Dat_LRR_Nut_6, Dat_LRR_Nut_12, Dat_LRR_Nut_18)

data_lrr_nut1 <- data_lrr_nut %>% mutate(T_ID = "Temp6_Nut") %>% select(N, P, Temp, T_ID, "LRR" = "logratio")
data_lrr_nut1$T_ID[data_lrr_nut1$Temp == 12] <- "Temp12_Nut"
data_lrr_nut1$T_ID[data_lrr_nut1$Temp == 18] <- "Temp18_Nut"

data_lrr_speed_T12_1 <- data_lrr_speed_T12 %>% mutate(T_ID = Log_ID)
data_lrr_speed_T12_1 <- select(data_lrr_speed_T12_1, -Log_ID)
data_lrr_speed_T18_1 <- data_lrr_speed_T18 %>% mutate(T_ID = Log_ID)
data_lrr_speed_T18_1 <- select(data_lrr_speed_T18_1, -Log_ID)

LRR_points <- rbind(data_lrr_nut1, data_lrr_speed_T12_1, data_lrr_speed_T18_1)

library(scales)

# Create a custom color scale
custom_colors <- colorRampPalette(c("blue", "royalblue3", "lightgrey", "#F8766D", "darkred"))(100)
custom_colors2 <- colorRampPalette(c("#D96624", "#EA915E", "white", "#9EBD6E", "#169873"))(100)
custom_colors3 <- colorRampPalette(c("#81457A", "#C57EBD", "#E7C6E3", "white", "#9EBD6E", "#169873", "#103E31"))(100)
custom_colors4 <- colorRampPalette(c("#E35807","#F79256", "#FCE762", "#F9F0B2", "white", "#9EBD6E", "#169873", "#103E31", "#0E311D"))(100)

LRR_ALL$T_ID <- as.factor(LRR_ALL$T_ID)
LRR_ALL$T_ID <- factor(LRR_ALL$T_ID, levels=c("Temp6_Nut","Temp12_Nut","Temp18_Nut","Temp12","Temp18"))
LRR_points$T_ID <- factor(LRR_points$T_ID, levels=c("Temp6_Nut","Temp12_Nut","Temp18_Nut","Temp12","Temp18"))
ggplot(data = LRR_ALL, aes(x = N, y = P)) +
  geom_raster(aes(fill = LRR)) +
  #geom_contour(aes(z = LRR), binwidth = 0.05, size = 0.2, colour = "black") +
  #scale_fill_viridis_c(option="viridis",direction=1, limits = c(-0.15, 0.11))+
  scale_fill_gradientn(colors = custom_colors4, limits = c(-0.15, 0.15)) +
  coord_cartesian(expand = F)+
  #scale_fill_gradient2(low="#F8766D", mid="gray90", high="royalblue3", 
  #limits = c(0, 0.6), oob = scales::squish)+
  #scale_fill_distiller(palette = "Spectral")+
  geom_point(data = LRR_points, aes(x= N, y= P), size= 0.5, alpha = 0.7)+
  #scale_y_continuous(breaks=c(0,4,16,24))+
  #scale_x_continuous(breaks=c(4,9,13.5))+
  theme_light()+
  labs(x="Nitrogen (µmol/L)",y="Phosphorus (µmol/L)",fill="r")+
  theme(legend.direction = "horizontal", legend.position = "bottom")+
  facet_grid(.~ T_ID)

ggplot(data = Data_LRR_T12_T18, aes(x=as.factor(N), y=as.factor(P), fill=LRR))+
  geom_raster(interpolate = TRUE)+
  geom_abline(intercept = 0, slope = 1, colour="black", alpha = 0.5, linetype=3)+
  scale_shape_manual(values=c(8), guide = 'none')+
  #scale_fill_distiller(palette = "Spectral")+
  scale_fill_viridis_c(limits= c(-0.16, 0.26), breaks = c(-0.1, 0, 0.1, 0.2))+
  theme(text=element_text(size=14,  family="Times New Roman"))+
  ggtitle("LRR: Treatment / Control")+
  xlab("Nitrogen")+
  ylab("Phosphorus")+
  #theme(legend.position = "bottom", legend.direction = "horizontal")
  facet_grid(.~Log_ID)

H1_data <- filter(data_run1, T_ID != "Constant-6")

