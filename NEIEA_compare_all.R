#A routine to find correlations between all species in NEFSC survey biomass data using a GLS model selection approach.

library(dplyr)
library(nlme)
library(pracma)
library(AICcmodavg)
rm(list = ls())

#Working directory - EcoAp
setwd('z:/shardison/neiea')
load("Surv_biomass_by_species.Rdata")
species <- na.omit(unique(survey_biomass$comname))
corr_results <- NULL

for (epu in c("GOM","GB","MAB")){
  
  survey <- filter(survey_biomass,EPU == epu)
  
  for (seas in c("spring","fall")){
    
    survey <- filter(survey, season == seas, !is.na(comname))
    
    for (i in 1:length(species)){
      
      #-----------------Get predictor variable--------------------------# 
      ind <- as.numeric(na.omit(survey[survey$comname == species[i],]$kg.per.tow))
      time <- as.numeric(na.omit(survey[survey$comname == species[i],]$YEAR))
      ind_df <- data.frame(ind = ind,
                           time = time)
      if (length(ind) < 20){
        next
      }
    
      for (j in 1:length(species)){
        
        #-----------------Get response variable--------------------------# 
        
        if (species[j]==species[i] | length(resp) < 20){ #Exclude data < 20 years
          next
        }
        resp <- as.numeric(na.omit(survey[survey$comname == species[j],]$kg.per.tow))
        time <- as.numeric(na.omit(survey[survey$comname == species[j],]$YEAR))
        resp_df <- data.frame(resp = resp,
                              time = time)
        
        #-----------------Merge predictor and response variable data by year--------------------------# 
        model_df <- merge(ind_df, resp_df, by = "time")
        model_df$ind <- detrend(model_df$ind)
        model_df$resp <- detrend(model_df$resp)
       
        #----------------------Build models--------------------------# 
        constant <- gls(resp ~ 1, data = model_df) #null model for comparison
        
        linear_mod <- tryCatch(gls(resp ~ ind, #first order linear
                              data = model_df),
        error = function(e)NA)
        
        linear_mod_ar <- tryCatch(gls(resp ~ ind, 
                                  data = model_df,
                                  correlation = corAR1()),
        error = function(e)NA) #including autocorrelated error structure
        
        model_df$ind2 <- model_df$ind^2
        
        quad_mod <- tryCatch(gls(resp ~ ind + ind2, #quadratric
                             data = model_df),
                             error = function(e)NA)
        
        quad_mod_ar <- tryCatch(gls(resp ~ ind + ind2,
                                data = model_df,
                                correlation = corAR1()),
                                error = function(e) NA) #including autocorrelated error structure
        
        #--------------------Find best model--------------------------# 
        aic_result <- data.frame(aicc = c(if(any(is.na(linear_mod))){
                                                  linear_mod}
                                          else {
                                            AICc(linear_mod)  
                                          },
                                          
                                          if(any(is.na(linear_mod_ar))){
                                            linear_mod_ar}
                                          else {
                                            AICc(linear_mod_ar)  
                                          },
                                          
                                          if(any(is.na(quad_mod))){
                                            quad_mod}
                                          else {
                                            AICc(quad_mod)  
                                          },
                                          
                                          if(any(is.na(quad_mod_ar))){
                                            quad_mod_ar}
                                          else {
                                            AICc(quad_mod_ar)  
                                          }),
                                 
                                 model = c("linear_mod", "linear_mod_ar",
                                           "quad_mod","quad_mod_ar"))
        best <- aic_result %>% dplyr::filter(!is.na(aicc),
                                             aicc == min(aicc))
        best <- aic_result %>% filter(!is.na(aicc))
        best <- best  %>% filter(aicc == min(aicc))
        
        #-----------------ANOVA--------------------------# 
        p <- anova(update(constant, method = "ML"), #no significant relationship
              update(get(paste(best$model)), method = "ML"))$`p-value`[2]
        
        #-----------------Collect results--------------------------# 
        ind_species <- species[i]
        resp_species <- species[j]
        
        result <- data.frame(best = best, p = p,
                             ind_species = ind_species,
                             resp_species = resp_species,
                             season = seas,
                             epu = epu,
                             n = length(model_df$time))
        
        assign("corr_results",rbind(corr_results, result))
        
      }
    }
  }
}


