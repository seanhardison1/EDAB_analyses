data.dir <- '~/soe_compile'
library(dplyr)
library(plotly)
library(AICcmodavg)

Sys.setenv("plotly_username"="seanhardison")
Sys.setenv("plotly_api_key"="BKjJCXeK4Oq7d9WY4TcM")

load(file.path(data.dir, "SOE_data_2018.Rdata"))

SOE.data.2018[SOE.data.2018$Value == 'NaN',]$Value <- NA

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
  
  # Calculate AICs for all models
  df_aicc <-
    data.frame(model = c(
      "linear_norm","linear_ar1"),
      aicc  = c(
        AICc(linear_norm),AICc(linear_ar1)),
      coefs = rbind(
        c(coef(linear_norm), NA),c(coef(linear_ar1),  NA)),
      # Calculate overall signifiance (need to use
      # ML not REML for this)
      pval = c(
        anova(update(constant_norm, method = "ML"),
              update(linear_norm, method = "ML"))$`p-value`[2],
        anova(update(constant_ar1, method = "ML"),
              update(linear_ar1, method = "ML"))$`p-value`[2]))
  
  best_lm <-
    df_aicc %>%
    dplyr::filter(aicc == min(aicc))
  if (best_lm$model == "linear_norm") {
    model <- linear_norm
  } else if (best_lm$model == "linear_ar1") {
    model <- linear_ar1
  }
  return(list(p = best_lm$pval,
              model = model))
}

SOE <- SOE.data.2018 %>% group_by(Var) %>% filter(n()>= 20) %>% ungroup()


df <- NULL
for (i in unique(SOE$Var)){
  var <- SOE %>% filter(Var == i) %>% arrange(Time)
  var$Value <- var$Value/max(var$Value, na.rm = T)
  data <- data.frame(time = 1:length(var$Time),series = var$Value)
  out <- tryCatch(fit_lm(dat = data),
                  error = function(e)NA)  
  
  if(is.na(out[1])){

    print(nrow(var))
    print(unique(var$Var))
    rmse <- NA
    b <- NA
    status <- NA
  } else {
    if (out[1] < 0.05){
      print(paste('No issues',unique(var$Var)))
      rmse <- sqrt(mean(out$model$residuals^2))
      b <- out$model$coefficients[2]
      status <- sqrt(mean(out$model$residuals[(nrow(data)-9):nrow(data)]^2))
    } else {
      print('No trend')
      rmse <- NA
      b <- NA
      status <- NA
    }
    
  }
  
  
  
  fin <- data.frame(rmse = rmse, 
                    b = b,
                    status = status,
                    variable = unique(var$Var))
  
  assign('df',rbind(df, fin))
}

#Plotly graphic
f <- list(
  family = "Times New Roman",
  size = 18
)
x <- list(
  title = intToUtf8(0x03B2L),
  titlefont = f
)
y <- list(
  title = "RMSE (full series)",
  titlefont = f
)
p <- plot_ly(type = "scatter",
             df, x = df$b, y = df$rmse, 
             text = paste("Variable: ", df$variable, "\nSlope:", round(df$b,2), 
                          "\nRMSE:",round(df$rmse,2)),
             mode = "markers", color = df$status,
             marker=list(colorbar=list(
               title='RMSE\n (past decade)')
             )) %>%
  layout(xaxis = x, yaxis = y)
p

chart_link = api_create(p, filename = "indicator_vis")

