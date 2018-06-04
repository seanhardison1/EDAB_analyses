#Exploring the relationship between lobster and sea scallop abundances on Georges Bank
library(nlme)
library(ggplot2)
library(dplyr)
library(pracma)
library(AICcmodavg)
library(cowplot)

setwd('z:/shardison/neiea') #EcoAp drive
pd <- read.csv("Lobster_sea_scallop_biomass_GB.csv")

#Process and detrend data
pd <- pd[pd$Prop.table.SVSPP == 301 | pd$Prop.table.SVSPP == 401,]

lobster_f <- detrend(pd[pd$Prop.table.SVSPP==301 &
                  pd$Prop.table.season=='fall',]$Prop.table.Species.biomass)
lobster_s <- detrend(pd[pd$Prop.table.SVSPP==301 & 
                          pd$Prop.table.season=='spring',]$Prop.table.Species.biomass)

scallop_f <- detrend(pd[pd$Prop.table.SVSPP==401 &
                          pd$Prop.table.season=='fall',]$Prop.table.Species.biomass)
scallop_s <- detrend(pd[pd$Prop.table.SVSPP==401 &
                          pd$Prop.table.season=='spring',]$Prop.table.Species.biomass)


#--------------------Fall model---------------------------#
pd_f <- data.frame(scallop_f = scallop_f, lobster_f = lobster_f)

constant <- gls(lobster_f ~ 1, data = pd_f) #null model for comparison

fall_mod_linear<- gls(lobster_f ~ scallop_f, #first order linear
                      data = pd_f)

fall_mod_linear_ar <- gls(lobster_f ~ scallop_f, 
                       data = pd_f,
                       correlation = corAR1()) #including autocorrelated error structure

pd_f$scal2 <- pd_f$scallop_f^2

fall_mod_quad <- gls(lobster_f ~ scallop_f + scal2, #quadratric
       data = pd_f)

fall_mod_quad_ar <- gls(lobster_f ~ scallop_f + scal2,
                        data = pd_f,
                        correlation = corAR1()) #including autocorrelated error

#Find best model
aic_result <- data.frame(aicc = c(AICc(fall_mod_linear),
                         AICc(fall_mod_linear_ar),
                         AICc(fall_mod_quad),
                          AICc(fall_mod_quad_ar)),
                         model = c("linear", "linear_ar",
                                   "quad","quad_ar"))
best <- dplyr::filter(aic_result,aicc == min(aicc))

#compare best model to null model
anova(update(constant, method = "ML"), #no significant relationship
      update(fall_mod_linear_ar, method = "ML"))$`p-value`[2]

#Plot relationship 
fall <- ggplot(data = pd_f, aes(y = lobster_f, x = scallop_f)) + geom_point(col = 'blue',size = 2)
fall <- fall + geom_abline(slope = fall_mod_linear_ar$coefficients[2],
                intercept = fall_mod_linear_ar$coefficients[1], col = 'red') +
  labs(x = expression(paste("Scallop Biomass (detrended)")),
       y = expression(paste("Lobster Biomass (detrended)"))) +
  theme(axis.text=element_text(size=20),
        text = element_text(size = 20)) +
  annotate("text",x = 7, y = 2.5, label = "p = 0.02")+
  ylim(-1,3)

#---------------------Spring model---------------------------#
pd_s <- data.frame(scallop_s = scallop_s, lobster_s = lobster_s)

constant <- gls(lobster_s ~ 1, data = pd_s) #null model for comparison

spring_mod_linear <- gls(lobster_s ~ scallop_s, #first order linear
                      data = pd_s)

spring_mod_linear_ar <- gls(lobster_s ~ scallop_s, 
                          data = pd_s,
                          correlation = corAR1()) #including autocorrelated error structure

pd_s$scal2 <- pd_s$scallop_s^2

spring_mod_quad <- gls(lobster_s ~ scallop_s + scal2, #quadratric
                     data = pd_s)

spring_mod_quad_ar <- gls(lobster_s ~ scallop_s + scal2,
                        data = pd_s,
                        correlation = corAR1()) #including autocorrelated error

#Find best model
aic_result_spring <- data.frame(aicc = c(AICc(spring_mod_linear),
                                  AICc(spring_mod_linear_ar),
                                  AICc(spring_mod_quad),
                                  AICc(spring_mod_quad_ar)),
                         model = c("linear", "linear_ar",
                                   "quad","quad_ar"))
best_spring <- dplyr::filter(aic_result_spring,aicc == min(aicc))

#compare best model to null model
anova(update(constant, method = "ML"),
      update(spring_mod_linear, method = "ML"))$`p-value`[2]

#Plot relationship 
spring <- ggplot(data = pd_s, aes(y = lobster_s, x = scallop_s)) + geom_point(col = 'blue',size = 2)
spring <- spring + geom_abline(slope = spring_mod_linear$coefficients[2],
                intercept = spring_mod_linear$coefficients[1], col = 'red') +
  labs(x = expression(paste("Scallop Biomass (detrended)")),
       y = expression(paste("Lobster Biomass (detrended)"))) +
  theme(axis.text=element_text(size=20),
        text = element_text(size = 20)) + 
  annotate("text", x = 1.25,y = 1.5, label = "p = 0.06")+
  ylim(-1,2)

plot_grid(fall, spring, 
          labels = c("Fall","Spring"), label_x = c(.165,.13),
          label_size = 18, label_fontface = "plain")

