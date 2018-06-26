#Source functions for NEIEA app
#Working directory - EcoAp - setwd('z:/shardison/neiea')


#libraries
library(dygraphs);library(AICcmodavg)
library(nlme);library(dplyr)
library(ggplot2);library(plotly);
library(Hmisc)

#Load disaggregated survey biomass data. Mapped to NEIEA groupings
load("Surv_biomass_by_species.Rdata")
#load("NEIEA_landings.Rdata") #old groupings  
load("NEIEA_comm_FI_update.Rdata") #new groupings with updated fished inverts
load("Survey_abundance_proportions.Rdata")
load("Comm_prop.Rdata")
load("SOE_data_2018.Rdata")
neiea_comm_landings <- filter(neiea_comm_landings, YEAR >1965)

#GLS model selection
fit_lm <- function(dat) {
  constant_norm <-
    nlme::gls(series ~ 1, 
              data = dat)
  constant_ar1 <-
    try(nlme::gls(series ~ 1,
                  data = dat,
                  correlation = nlme::corAR1(form = ~time)))
  if (class(constant_ar1) == "try-error"){
    return(best_lm <- data.frame(model = NA,aicc  = NA,
                                 coefs..Intercept = NA,coefs.time = NA,
                                 coefs.time2 = NA,pval = NA)) 
  } 
  # Linear model with normal error
  linear_norm <- 
    nlme::gls(series ~ time, 
              data = dat)
  # Linear model with AR1 error
  linear_ar1 <- 
    try(nlme::gls(series ~ time, 
                  data = dat,
                  correlation = nlme::corAR1(form = ~time)))
  if (class(linear_ar1) == "try-error"){
    return(best_lm <- data.frame(model = NA,aicc  = NA,
                                 coefs..Intercept = NA,coefs.time = NA,
                                 coefs.time2 = NA,pval = NA))
  }
  # Polynomial model with normal error
  dat$time2 <- dat$time^2
  poly_norm <- 
    nlme::gls(series ~ time + time2, 
              data = dat)
  # Polynomial model with AR1 error
  poly_ar1 <- 
    try(nlme::gls(series ~ time + time2, 
                  data = dat,
                  correlation = nlme::corAR1(form = ~time)))
  if (class(poly_ar1) == "try-error"){
    return(best_lm <- data.frame(model = NA,aicc  = NA,
                                 coefs..Intercept = NA,coefs.time = NA,
                                 coefs.time2 = NA,pval = NA))
  }
  # Calculate AICs for all models
  df_aicc <-
    data.frame(model = c("poly_norm","poly_ar1",
                         "linear_norm","linear_ar1"),
               aicc  = c(AICc(poly_norm),AICc(poly_ar1),
                         AICc(linear_norm),AICc(linear_ar1)),
               coefs = rbind(coef(poly_norm),coef(poly_ar1),
                             c(coef(linear_norm), NA),c(coef(linear_ar1),  NA)),
               # Calculate overall signifiance (need to use
               # ML not REML for this)
               pval = c(anova(update(constant_norm, method = "ML"),
                              update(poly_norm, method = "ML"))$`p-value`[2],
                        anova(update(constant_ar1, method = "ML"),
                              update(poly_ar1, method = "ML"))$`p-value`[2],
                        anova(update(constant_norm, method = "ML"),
                              update(linear_norm, method = "ML"))$`p-value`[2],
                        anova(update(constant_ar1, method = "ML"),
                              update(linear_ar1, method = "ML"))$`p-value`[2]))
  
  best_lm <-
    df_aicc %>%
    dplyr::filter(aicc == min(aicc))
  if (best_lm$model == "poly_norm") {
    model <- poly_norm
  } else if (best_lm$model == "poly_ar1") {
    model <- poly_ar1
  } else if (best_lm$model == "linear_norm") {
    model <- linear_norm
  } else if (best_lm$model == "linear_ar1") {
    model <- linear_ar1
  }
  return(list(p = best_lm$pval,
              model = model))
}

#Function to visualize either aggregated or species-level survey data
neiea_vis <- function(agg = NULL, dat, var, surv_season, epu, comm = F, USD = F){
  dat <- dat
  
  
  if (agg){
    
    if (comm & USD){
      series_df <- dat %>% group_by(EPU, group, YEAR) %>%
        dplyr::summarise(agg = sum(landingsUSD)) %>%
        filter(EPU == epu, group ==  var)
      
    } else if (comm){
      series_df <- dat %>% group_by(EPU, group, YEAR) %>%
        dplyr::summarise(agg = sum(landingsMT)) %>%
        filter(EPU == epu, group ==  var)
      
    } else {
      series_df <- dat %>% group_by(EPU, group, YEAR) %>% 
        filter(season == surv_season) %>%
        dplyr::summarise(agg = sum(kg.per.tow)) %>%
        filter(EPU == epu, group == var) %>% arrange(YEAR)
    }
    
    series <- series_df$agg
    time <- series_df$YEAR
    
  } else if (!agg){
    if (comm & USD){
      
      series_df <- dat %>% filter(EPU == epu, comname ==  var)
      name <- unique(series_df$comname)
      series <- series_df$landingsUSD
      time <- series_df$YEAR
      
    } else if (comm){
      
      series_df <- dat %>% filter(EPU == epu, comname ==  var)
      name <- unique(series_df$comname)
      series <- series_df$landingsMT
      time <- series_df$YEAR
      
    } else {
      
      series_df <- dat %>% filter(season == surv_season, EPU == paste(epu),
                            comname == var)
      series <- series_df$kg.per.tow
      time <- series_df$YEAR
      name <- unique(series_df$comname)


    }
  }
  
  model_df <- data.frame(time = time,series = series)
  
  
  oldw <- getOption("warn")
  options(warn = -1)
  
  out <- tryCatch(fit_lm(dat = model_df),
                  error = function(e)NA)
  
  options(warn = oldw)
  
  if (any(is.na(out))){
    p <- .99
  } else {
    p <- out$p
  }
  
  if (p < 0.05){
    newtime <- seq(min(model_df$time), max(model_df$time), length.out=length(model_df$time))
    newdata <- data.frame(time = newtime,
                          time2 = newtime^2)
    lm_pred <- AICcmodavg::predictSE(out$model, 
                                     newdata = newdata,
                                     se.fit = TRUE)
    lm_pred <- lm_pred$fit
  } else {
    lm_pred <- rep(NA,nrow(model_df))
  }
  
  dat <- cbind(model_df, lm_pred)
  
  if (agg){
    if (comm & USD){
      ylab <- paste(var, 'landings (USD)')
    } else if (comm){
      ylab <- paste(var, 'landings (MT)')
    } else if (!comm){
      ylab <- paste(var, 'aggregate biomass (kg per tow)')
    }
    if (p < 0.05){
      
      dygraph(dat) %>%
        dyOptions(drawPoints = T, pointSize = 2) %>%
        dyAxis("y", label = ylab) %>%
        dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>%
        dySeries('series', label = var) %>%
        dySeries('lm_pred','trend', drawPoints = F) %>%
        dyAnnotation(paste(max(model_df$time) - 5),text = paste0("p = ",round(p,4)), width = 100, height = 25)  
    } else {
      dat <- select(dat,-c(lm_pred))
      dygraph(dat) %>%
        dyOptions(drawPoints = T, pointSize = 2) %>%
        dyAxis("y", label = ylab) %>%
        dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>%
        dySeries('series', label = var)
    }
    
  } else if (!agg){
    
    if (comm & USD){
      ylab <- paste(name, 'landings (USD)')
    } else if (comm){
      ylab <- paste(name, 'landings (MT)')
    } else if (!comm){
      ylab <- paste(name, 'aggregate biomass (kg per tow)')
    }
    if (p < 0.05){
      dygraph(dat) %>%
        dyOptions(drawPoints = T, pointSize = 2) %>%
        dyAxis("y", label = ylab) %>%
        dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>%
        dySeries('series', label = paste(name)) %>%
        dySeries('lm_pred',label = 'trend', drawPoints = F)%>%
        dyAnnotation(paste(max(model_df$time) - 5),text = paste0("p = ",round(p,4)), width = 100, height = 25)  
      
    } else {
      dat <- select(dat,-c(lm_pred))
      dygraph(dat) %>%
        dyOptions(drawPoints = T, pointSize = 2) %>%
        dyAxis("y", label = ylab) %>%
        dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>%
        dySeries('series', label = paste(name)) 
    }
  }
}

species_summary_plot <- function(dat, var, epu, surv_season){
  dat <- dat
  
  series <- dat %>% filter(comname == var,
                         EPU == epu,
                         season == surv_season) %>%
                         filter (! duplicated(YEAR))
    
  time <- series$YEAR
  
  model_df <- data.frame(time = time,
                          series = series$prop)
  model_df <- model_df %>% filter(!is.na(series))
  out <- tryCatch(fit_lm(dat = model_df),
                  error = function(e)NA)
  if (any(is.na(out))){
    p <- .99
  } else {
    p <- out$p
  }
    
  if (p < 0.05){
    newtime <- seq(min(model_df$time), max(model_df$time), length.out=length(model_df$time))
    newdata <- data.frame(time = newtime,
                          time2 = newtime^2)
    lm_pred <- AICcmodavg::predictSE(out$model, 
                                     newdata = newdata,
                                     se.fit = TRUE)
    lm_pred <- lm_pred$fit
  } else {
    lm_pred <- rep(NA,nrow(model_df))
  }
  
  ylab <- unique(series$comname)
  
  if (p < 0.05){
    model_df$lm_pred <- lm_pred
    dygraph(model_df) %>%
      dyOptions(drawPoints = T, pointSize = 2) %>%
      dyAxis("y", label = paste(ylab, "Proportion of Group Biomass")) %>%
      dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>%
      dySeries('series', label = paste(ylab)) %>%
      dySeries('lm_pred',label = 'trend', drawPoints = F)%>%
      dyAnnotation(paste(max(model_df$time) - 5),text = paste0("p = ",round(p,4)),
                   width = 100, height = 25)  
    
  } else {
    dygraph(model_df) %>%
      dyOptions(drawPoints = T, pointSize = 2) %>%
      dyAxis("y", label =  paste(ylab, "Proportion of Group Biomass")) %>%
      dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>%
      dySeries('series', label = paste(ylab)) 
  }
  
  
}

group_summary_plot <- function(dat, epu, var, surv_season, year){
  dat <- dat
  
  filt_dat <- dat %>% filter(group == var,
                           EPU == epu,
                           season == surv_season,
                           YEAR == year) %>%
    arrange(desc(prop))
  
  p <- ggplot(data = filt_dat, aes(x = Prop.table.comname, y = Prop.table.proportion)) +
    geom_bar(stat = "identity",position = "dodge") +
    ylab("Proportion of Survey Abundance") +
    xlab("Species")
  ggplotly(p)
}

ts_summary_plot <- function(dat, epu, var, surv_season){
  dat <- dat

  filt_dat <- dat %>% filter(group == var,
                             EPU == epu,
                             season == surv_season) %>%
    filter(!duplicated(kg.per.tow))


  species <- unique(filt_dat$comname)
    
  out <- list()
  
  for (i in 1:length(species)){
    time <- filt_dat %>% filter(comname == species[i]) %>% select(YEAR)
    series <- filt_dat %>% filter(comname == species[i]) %>% select(prop)
    
    out2 <- data.frame(time = time,
                      series = series)
    names(out2)[2] <- paste(as.character(species[i]))

    null_df <- data.frame(YEAR = min(filt_dat$YEAR):max(filt_dat$YEAR),
                          series = NA)
    out2 <- merge(out2, null_df, by = "YEAR", all = T)
    out[[i]] <- out2
  }
  oldw <- getOption("warn")
  options(warn = -1)
  
  plot_df <- Reduce(function(...) merge(..., by="YEAR", all=T), out)
  
  options(warn = oldw)
  
  emptycols <- colSums(is.na(plot_df)) == nrow(plot_df)
  plot_df <- plot_df[!emptycols]
  
  dygraph(plot_df) %>%
    dyOptions(drawPoints = T, pointSize = 2) %>%
    dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>%
    #dyOptions(stackedGraph = TRUE) %>%
    dyAxis("y",valueRange = c(0,max(plot_df))) %>%
    dyAxis("y", label =  paste(capitalize(var), "biomass contributions,",epu)) %>%
    dyLegend(width = 600)
}

comm_vis <- function(dat, epu, var, usd){
  dat <- dat
  
  if (usd){
    series_df <- dat %>% filter(group == var,
                                EPU == epu) %>%
      filter (! duplicated(landingsUSD)) %>%
      group_by(EPU, YEAR) %>%
      arrange(desc(YEAR),desc(propUSD))  %>%
      top_n(n = 5, wt = propUSD) %>%
      as.data.frame()
  } else if (!usd){
    series_df <- dat %>% filter(group == var,
                                EPU == epu) %>%
      filter (! duplicated(landingsMT)) %>%
      group_by(EPU, YEAR) %>%
      arrange(desc(YEAR),desc(propMT)) %>%
      top_n(n = 5, wt = propMT) %>%
      as.data.frame()
  }
  
  out <- list()
  species <- unique(series_df$comname)
  
  for (i in 1:length(species)){
    
    if (!usd){
      series <- series_df %>% filter(comname == species[i]) %>% select(propMT, YEAR) 
    } else {
      series <- series_df %>% filter(comname == species[i]) %>% select(propUSD, YEAR) 
    }
    time <- series$YEAR
    
    out2 <- data.frame(time = time,
                       series = series)
    names(out2)[2] <- paste(as.character(species[i]))
    
    null_df <- data.frame(time = min(series_df$YEAR):max(series_df$YEAR),
                          series = NA)
    out2 <- out2 %>% select(-series.YEAR)
    out2 <- merge(out2, null_df, by = "time", all = T)
    out[[i]] <- out2
  }
  oldw <- getOption("warn")
  options(warn = -1)
  
  plot_df <- Reduce(function(...) merge(..., by="time", all=T), out)
  
  options(warn = oldw)
  
  emptycols <- colSums(is.na(plot_df)) == nrow(plot_df)
  plot_df <- plot_df[!emptycols]
  
  if (usd){
    ylab <- "USD"
  } else {
    ylab <- "metric tons (MT"
  }
  
  dygraph(plot_df) %>%
    dyOptions(drawPoints = T, pointSize = 2) %>%
    dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>%
    #dyOptions(stackedGraph = TRUE) %>%
    dyAxis("y",valueRange = c(0,max(plot_df))) %>%
    dyAxis("y", label =  paste(epu,capitalize(var),"commercial contributions (",ylab,")")) %>%
    dyLegend(width = 700) %>%
    dyCSS('legend.css')
  
  
}
survey_fields <- sort(as.factor(na.omit(unique(survey_biomass$comname))))
agg_survey_fields <- sort(as.factor(unique(survey_biomass$group)))
soe_fields <- sort(as.factor(unique(SOE.data.2018$Var)))

soe_plot <- function(dat, var, summ = F){
  dat <- dat
  
  dat <- dat[dat$Var == var,]

  
  dat <- dat %>% arrange(Time)
  series <- dat$Value
  time <- dat$Time
  unit <- unique(dat$Units)
  name <- unique(dat$Var)
  
  model_df <- data.frame(time = time,series = series)
  
  
  oldw <- getOption("warn")
  options(warn = -1)
  
  out <- tryCatch(fit_lm(dat = model_df),
                  error = function(e)NA)
  
  options(warn = oldw)
  
  if (any(is.na(out))){
    p <- .99
  } else {
    p <- out$p
  }
  
  if (p < 0.05 & nrow(model_df) > 20){
    newtime <- seq(min(model_df$time), max(model_df$time), length.out=length(model_df$time))
    newdata <- data.frame(time = newtime,
                          time2 = newtime^2)
    lm_pred <- AICcmodavg::predictSE(out$model, 
                                     newdata = newdata,
                                     se.fit = TRUE)
    lm_pred <- lm_pred$fit
  } else {
    lm_pred <- rep(NA,nrow(model_df))
  }
  
  dat <- cbind(model_df, lm_pred)
  ylab  <- paste(name, ",",unit)
  
  if (!summ){
    if (p < 0.05){
      
      dygraph(dat) %>%
        dyOptions(drawPoints = T, pointSize = 2) %>%
        dyAxis("y", label = ylab) %>%
        dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>%
        dySeries('series', label = var) %>%
        dySeries('lm_pred','trend', drawPoints = F) %>%
        dyAnnotation(paste(max(model_df$time) - 5),text = paste0("p = ",round(p,4)), width = 100, height = 25)  
    } else {
      dat <- select(dat,-c(lm_pred))
      dygraph(dat) %>%
        dyOptions(drawPoints = T, pointSize = 2) %>%
        dyAxis("y", label = ylab) %>%
        dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>%
        dySeries('series', label = var)
    }
    
  } else if (summ){
    summ <- data.frame(n = nrow(model_df),
               mean = round(mean(model_df$series),3),
               sd = round(sd(model_df$series),3),
               start = model_df$time[1],
               end = model_df$time[nrow(model_df)])
    return(summ)
  }
  
  
  
  
}


#neiea_vis(agg = F, dat = survey_biomass, var = "BLUEFISH",surv_season = "fall",epu = "MAB")
