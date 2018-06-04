#Exploring the relationship between lobster and sea scallop abundances on Georges Bank
library(nlme)
library(ggplot2)
library(dplyr)

setwd('z:/shardison/neiea') #EcoAp drive
pd <- read.csv("Lobster_sea_scallop_biomass_GB.csv")

#Process data
lobster_f <- pd[pd$Prop.table.SVSPP==301 & pd$Prop.table.season=='fall',]$Prop.table.Species.biomass
lobster_s <- pd[pd$Prop.table.SVSPP==301 & pd$Prop.table.season=='spring',]$Prop.table.Species.biomass

scallop_f <- pd[pd$Prop.table.SVSPP==401 & pd$Prop.table.season=='fall',]$Prop.table.Species.biomass
scallop_s <- pd[pd$Prop.table.SVSPP==401 & pd$Prop.table.season=='spring',]$Prop.table.Species.biomass

#--------------------Fall model---------------------------#
pd_f <- data.frame(scallop_f = scallop_f, lobster_f = lobster_f)

constant <- gls(scallop_f ~ 1, data = pd_f,correlation = corAR1()) #null model for comparison
fall_mod_linear <- gls(scallop_f ~ lobster_f, #first order linear
                       data = pd_f,
                       correlation = corAR1()) #including autocorrelated error structure

pd_f$lob2 <- pd_f$lobster_f^2
fall_mod <- gls(scallop_f ~ lobster_f + lob2, #quadratric
                data = pd_f,
                correlation = corAR1())

#Find best model
anova(update(constant, method = "ML"),
      update(fall_mod_linear, method = "ML"))
anova(update(constant, method = "ML"),
      update(fall_mod, method = "ML"))
anova(update(fall_mod_linear, method = "ML"),
      update(fall_mod, method = "ML"))
#Best model is linear

#Plot relationship 
g <- ggplot(data = pd_f, aes(y = scallop_f, x = lobster_f)) + geom_point(col = 'blue',size = 2)
g + geom_abline(slope = fall_mod$coefficients[2],
                intercept = fall_mod$coefficients[1], col = 'red') +
  labs(x = expression(paste("Lobster Biomass (kg tow"^"-1",")")),
       y = expression(paste("Scallop Biomass (kg tow"^"-1",")"))) +
  theme(axis.text=element_text(size=20),
        text = element_text(size = 20))

#---------------------Spring model---------------------------#
pd_s <- data.frame(scallop_s = scallop_s, lobster_s = lobster_s)

constant <- gls(scallop_s ~ 1, data = pd_s,correlation = corAR1())
pd_s$lob2 <- pd_s$lobster_s^2
spring_mod_linear <- gls(scallop_s ~ lobster_s, #first order linear
                       data = pd_s,
                       correlation = corAR1()) #including autocorrelated error structure

spring_mod <- gls(scallop_s ~ lobster_s + lob2, #quadratic model
                  data = pd_s,
                  correlation = corAR1())

anova(update(constant, method = "ML"), #no significant relationship
      update(spring_mod_linear, method = "ML"))$`p-value`[2]

