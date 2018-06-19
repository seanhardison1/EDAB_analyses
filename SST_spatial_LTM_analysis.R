rm(list = ls())

setwd("z:/shardison/neiea")

sub_region <- raster("Fall_anomaly_1971_2000.nc")

setwd('c:/users/sean.hardison/documents/sst_data')

#libraries
library(ncdf4);library(dplyr)
library(readr);library(tidyr)
library(sp);library(rgdal)
library(raster);library(stringr)

fall <- stack()
winter <- stack()
spring <- stack()
summer <- stack()


for (dir. in 1){
  #Loop through directories
  setwd(paste0('c:/users/sean.hardison/documents/sst_data/',dir.))
  print(getwd())
  for (f in 1:length(list.files())){
    
    if (!str_detect(list.files()[f],".nc")){
      print(paste(list.files()[f],"is not a raster")) #Based on file type
      next
    }
    
    for (s in c("winter","spring","summer","fall")){
  
      y <- as.numeric(str_extract(list.files()[f],"[0-9]+")) #Pull out file year for identifying leap years
      print(paste('Year:',y,"Season:",s))
      leap_year <- seq(1984,2016,4)
      
      #Pick out leap years
      if (y %in% leap_year){
          leap <- T
      } else {
        leap <- F
      }
      if (s == "winter")
        if (!leap){
          season <- 1:90 
        } else {
          season <- 1:91
        }
      else if (s == "spring"){
        if (!leap){
          season <- 91:181 
        } else {
          season <- 92:182
        } 
      }
      else if (s == "summer"){
        if (!leap){
          season <- 182:273
        } else {
          season <- 183:274
        } 
      }
      else if (s == "fall"){
        if (!leap){
          season <- 274:365
        } else {
          season <- 275:366
        } 
      }
      
      if(leap){
        print(paste(s,y,"leap"))
      }
      
        
        for (b in season){
          r <- raster(list.files()[f],b) #Import raster bands within a given season

          if (b == season[1]){
            print(list.files()[f])
            print(length(season))
            print(r@file@nbands) #Print file name
          }
          
          #Rotate and crop
          r <- rotate(r) 
          r <- crop(r, extent(sub_region))
          #print(r@file@nbands)
          if (b == season[50]){
            print("Over halfway there")
          }
          
          #Add bands to raster stack for each season. Seaonal bands for each year are bound together.
          assign(s,stack(get(s),r))
          
        }
      print(paste("Done stacking",nlayers(get(s)),"days"))
    } 
  }
}




#---------------------------------------------------------------#
setwd("c:/users/sean.hardison/documents/sst_data/grouped")


fall_1 <- stack("fall_1")
fall_2 <- stack("fall_2")
fall_3 <- stack("fall_3")


spring_1 <- stack("spring_1")
spring_2 <- stack("spring_2")
spring_3 <- stack("spring_3")


winter_1 <- stack("winter_1")
winter_2 <- stack("winter_2")
winter_3 <- stack("winter_3")


summer_1 <- stack("summer_1")
summer_2 <- stack("summer_2")
summer_3 <- stack("summer_3")


winter <- stack(winter_1,winter_2,winter_3)
rm(winter_1,winter_2,winter_3)
fall <- stack(fall_1,fall_2,fall_3)
rm(fall_1,fall_2,fall_3)
spring <- stack(spring_1,spring_2,spring_3)
rm(spring_1,spring_2,spring_3)
summer <- stack(summer_1,summer_2,summer_3)
rm(summer_1,summer_2,summer_3)


#Find seasonal means across all days
winter_mean <- stackApply(winter, indices = rep(1,nlayers(winter)),mean)
spring_mean <- stackApply(spring, indices = rep(1,nlayers(spring)),mean)
summer_mean <- stackApply(summer, indices = rep(1,nlayers(summer)),mean)
fall_mean <- stackApply(fall, indices = rep(1,nlayers(fall)),mean)


rm(winter, spring, summer, fall)

#Get 2017 daily mean data
setwd("z:/shardison/neiea")
daily_mean_2017 <- stack("sst.day.mean.2017.nc")
sub_region <- raster("Fall_anomaly_1971_2000.nc") #get an old file to crop with


daily_mean_2017 <- rotate(daily_mean_2017) #rotate
daily_mean_2017 <- crop(daily_mean_2017, extent(sub_region)) #crop

#Find daily mean across seasons
seasonal_sst_2017 <- stackApply(daily_mean_2017, indices = c(rep(1,90),rep(2,91),
                                                             rep(3, 92), rep(4, 92)), mean)


#Get old 1971-2000 anomaly data for comparison
setwd('c:/users/sean.hardison/documents')

#Old anomaly
old_anom <- stack('sst.day.anom.2017.v2.nc')
old_anom <- stackApply(old_anom, indices = c(rep(1,90),rep(2,91),
                                             rep(3, 92), rep(4, 92)), mean)
old_anom <- rotate(old_anom)
old_anom <- crop(old_anom, extent(sub_region))

#Old long-term mean
old_ltm <- stack('sst.day.mean.ltm.1971-2000.nc')
old_ltm <- stackApply(old_ltm, indices = c(rep(1,90),rep(2,91),
                                           rep(3, 92), rep(4, 92)), mean)
old_ltm <- rotate(old_ltm)
old_ltm <- crop(old_ltm, extent(sub_region))

#Get NEW anomaly
winter_anom <- seasonal_sst_2017[[1]] - winter_mean
spring_anom <- seasonal_sst_2017[[2]] - spring_mean
summer_anom <- seasonal_sst_2017[[3]] - summer_mean
fall_anom <- seasonal_sst_2017[[4]] - fall_mean


#compare old and new anomalies
plot(winter_anom)
plot(old_anom[[1]])

plot(spring_anom)
plot(old_anom[[2]])

plot(summer_anom)
plot(old_anom[[3]])

plot(fall_anom)
plot(old_anom[[4]])

#2017 fluctuations as time series data
setwd('c:/users/sean.hardison/documents/soe_compile')
library(stringr)
library(data.table)

load("1982_2012_lt_mean.Rdata")
load("SOE_data_2018.Rdata")

#Filter out old data
SOE <- SOE.data.2018 %>% filter(!str_detect(Var, "sst mean 2017")) %>%
  filter(!str_detect(Var, "sst mean long term")) %>% 
  filter(!str_detect(Var, "sst sd long term"))


#Get new long-term mean
mab <- agg_sst_EPU[str_detect(agg_sst_EPU$EPU, "MAB"),]$sst_mean
gb <- agg_sst_EPU[str_detect(agg_sst_EPU$EPU, "GB"),]$sst_mean
gom <- agg_sst_EPU[str_detect(agg_sst_EPU$EPU, "GOM"),]$sst_mean

#New SD
mab_sd <- agg_sst_EPU[str_detect(agg_sst_EPU$EPU, "MAB"),]$sst_sd
gb_sd <- agg_sst_EPU[str_detect(agg_sst_EPU$EPU, "GB"),]$sst_sd
gom_sd <- agg_sst_EPU[str_detect(agg_sst_EPU$EPU, "GOM"),]$sst_sd


new_data <- data.frame(Time = rep(1:365,9),
                       Value = c(mab, gb, gom,
                               current[current$region == "MAB",]$value,
                               current[current$region == "GBK",]$value,
                               current[current$region == "GOM",]$value,
                               mab_sd, gb_sd, gom_sd),
                       Units = "degrees C",
                       EPU = rep(c("MAB","GB","GOM"),3,each = 365),
                       Var = c(     rep(c("sst mean long term MAB",
                                     "sst mean long term GB",
                                     "sst mean long term GOM"),each = 365),
                                    rep(c("sst mean 2017 MAB",
                                          "sst mean 2017 GB",
                                          "sst mean 2017 GOM"),each = 365),
                                    rep(c("sst sd long term MAB",
                                          "sst sd long term GB",
                                          "sst sd long term GOM"),each = 365)))


View(new_data)



#Make sure data copied correctly
current <- read.csv("currentyr_soe.csv")

plot(mab)
points(current[current$region == "MAB",]$value)
points(new_data[str_detect(new_data$Var, "sst mean long term MAB"),]$Value, col = "red")
points(new_data[str_detect(new_data$Var, "sst mean 2017 MAB"),]$Value, col = "red")


plot(gb)
points(current[current$region == "GBK",]$value)
points(new_data[str_detect(new_data$Var, "sst mean long term GB"),]$Value, col = "red")
points(new_data[str_detect(new_data$Var, "sst mean 2017 GB"),]$Value, col = "red")

plot(gom)
points(current[current$region == "GOM",]$value)
points(new_data[str_detect(new_data$Var, "sst mean long term GOM"),]$Value, col = "red")
points(new_data[str_detect(new_data$Var, "sst mean 2017 GOM"),]$Value, col = "red")


#Replace

#SOE.data.2018 <- rbind(SOE, as.data.table(new_data))

#save(SOE.data.2018, file = "SOE_data_2018.Rdata")
