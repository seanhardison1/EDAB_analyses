rm(list = ls())
library(dplyr)

#Narrowing conceptual model groupings to pick out commercially vs. non-commercially valuable species
setwd('z:/shardison/neiea') #EcoAp
load("Species_codes.Rdata") #Old species codes
load("NEIEA_comm_FI_update.Rdata") #Current IEA groupings

neiea <- neiea_comm_landings %>% select(c(NESPP3,group)) %>% distinct() #Pull out just species codes and groups

neiea[neiea$group == 'fishedInvert',]$group <- 'benthos' #Turn subset groups back to larger overarching group structures
neiea[neiea$group == 'mab.groundfish',]$group <- 'groundfish'


load("comland_meatwt_deflated.Rdata") #Commercial landings data (comland)

comagg <- comland %>%  filter(YEAR > 1988) %>% #Get revenue and landings mean for last 30 years within EPU and species
  group_by(EPU, NESPP3) %>%
  dplyr::summarise(meanMT = mean(SPPLIVMT),
                   meanUSD = mean(SPPVALUE)) %>%
  filter(EPU != "SS")
  
comagg <- left_join(comagg, spp, by = "NESPP3") #Add species table

commerge1 <- left_join(comagg, neiea, by = c("NESPP3")) #Add conceptual model groupings
commerge1$group <- as.character(commerge1$group)
commerge1[is.na(commerge1$group),]$group <- "other.na" #Turn NA to other.na; species not specified in earlier groupings

#Update all species groupings at once

com <- commerge1 %>% 
  group_by(EPU, group) %>% 
   mutate(propMT = meanMT/max(meanMT), propUSD = meanUSD/max(meanUSD))


# fished_group <- NULL
# for (i in 1:nrow(com)){
#   
#   if (com[i,]$propMT > 0.05 & com[i,]$propUSD > 0.05){
#     fished_group[i] <- paste0("fished.",com [i,]$group)
#   } else {
#     fished_group[i] <- as.character(com[i,]$group)
#   }
#   
# }
  
#com$fished.group <- fished_group

#Update benthic species only
com_benthos <- commerge1 %>% 
  group_by(EPU, group) %>% 
  mutate(propMT = meanMT/max(meanMT), propUSD = meanUSD/max(meanUSD)) %>%
  filter(group == 'benthos') %>% filter(EPU != "OTHER") %>%
  filter(propMT > 0.01, propUSD > 0.01)
fished_inverts <- unique(com_benthos$COMNAME)


neiea[neiea$NESPP3 %in% fished_inverts,]$group <- 'fishedInvert'


#write.csv(spp, file = "Species_groupings_IEA.csv")

