library(tidyr)
library(dplyr)

load('~/git/soe_simulations/data.dir/SOE_data_2017.RData')

fields <- c('GOM Benthos Landings','GOM Mesoplanktivore Landings',
            'GOM Macroplanktivore Landings','GOM Macrozoo-piscivore Landings',
            'GOM Benthivore Landings','GOM Piscivore Landings',
            'GOM Benthos Revenue','GOM Mesoplanktivore Revenue',
            'GOM Macroplanktivore Revenue','GOM Macrozoo-piscivore Revenue',
            'GOM Benthivore Revenue','GOM Piscivore Revenue','GOM Benthos Fall',
            'GOM Mesoplanktivore Fall','GOM Macroplanktivore Fall',
            'GOM Macrozoo-piscivore Fall','GOM Benthivore Fall',
            'GOM Piscivore Fall','GOM Benthos Spring',
            'GOM Mesoplanktivore Spring','GOM Macroplanktivore Spring',
            'GOM Macrozoo-piscivore Spring','GOM Benthivore Spring',
            'GOM Piscivore Spring','FALL GOM ESn',
            'SPRING GOM ESn','Right whale population',
            'GOM PPD','GOM Yearly Calanus Anomaly',
            'GOM Yearly Zooplankton Biovolume','GOM Yearly small copeopods anomaly',
            'MAB Benthos Landings','MAB Mesoplanktivore Landings',
            'MAB Macroplanktivore Landings', 'MAB Macrozoo-piscivore Landings',
            'MAB Benthivore Landings','MAB Piscivore Landings',
            'MAB Benthos Revenue','MAB Mesoplanktivore Revenue',
            'MAB Macroplanktivore Revenue', 'MAB Macrozoo-piscivore Revenue',
            'MAB Benthivore Revenue','MAB Piscivore Revenue','MAB Benthos Fall',
            'MAB Mesoplanktivore Fall','MAB Macroplanktivore Fall',
            'MAB Macrozoo-piscivore Fall','MAB Benthivore Fall',
            'MAB Piscivore Fall','MAB Benthos Spring',
            'MAB Mesoplanktivore Spring','MAB Macroplanktivore Spring',
            'MAB Macrozoo-piscivore Spring','MAB Benthivore Spring',
            'MAB Piscivore Spring','FALL MAB ESn',
            'SPRING MAB ESn',
            'MAB PPD','MAB Yearly Calanus Anomaly',
            'MAB Yearly Zooplankton Biovolume','MAB Yearly small copeopods anomaly',
            'Sea Surface Temperature','Gulf Stream Index',
            'Surface temp GB','Bottom temp GB','Stratification (0-50m) GB core',
            'Surface Salinity GB','Bottom salinity GB',
            'Surface temp GOM','Bottom temp GOM',
            'Stratification (0-50m) GOM core',
            'Bottom salinity GOM',
            'New England fleet count','New England average fleet diversity',
            'New England commercial species diversity','North Atlantic Rec participation',
            'North Atlantic angler trips','Mid-Atlantic fleet count',
            'Mid-Atlantic average fleet diversity','Mid-Atlantic commercial species diversity',
            'Mid-Atlantic Rec participation','Mid-Atlantic angler trips',
            'aquaculture VA hard clams sold','aquaculture VA oysters sold',
            'aquaculture VA hard clams planted','aquaculture VA oysters planted',
            'aquaculture ME Atlantic salmon harvest weight','aquaculture ME trout harvest weight',
            'aquaculture ME blue mussels harvest weight','aquaculture ME oysters harvest weight',
            'aquaculture RI total value value','aquaculture RI oysters sold',
            'GB Benthos Landings','GB Mesoplanktivore Landings',
            'GB Macroplanktivore Landings','GB Macrozoo-piscivore Landings',
            'GB Benthivore Landings','GB Piscivore Landings',
            'GB Benthos Revenue','GB Mesoplanktivore Revenue',
            'GB Macroplanktivore Revenue','GB Macrozoo-piscivore Revenue',
            'GB Benthivore Revenue','GB Piscivore Revenue',
            'GB Piscivore Fall', 'GB Benthivore Fall',
            'GB Macrozoo-piscivore Fall','GB Macroplanktivore Fall',
            'GB Mesoplanktivore Fall','GB Benthos Fall',
            'GB Piscivore Spring', 'GB Benthivore Spring',
            'GB Macrozoo-piscivore Spring','GB Macroplanktivore Spring',
            'GB Mesoplanktivore Spring','GB Benthos Spring',
            'FALL GB ESn','SPRING GB ESn','GBK PPD',
            'GBK Yearly Calanus Anomaly','GBK Yearly Zooplankton Biovolume',
            'GBK Yearly small copeopods anomaly')

#Function to normalize data
get_dat <- function(field,norm = NULL){
  time <- SOE.data[SOE.data$Var == field,]$Time
  end = max(time)
  time = time[1:which(time == end)]
  
  var <- SOE.data[SOE.data$Var == field,]$Value
  var <- var[1:length(time)]
  
  
  if(norm == TRUE){
    var = (var-mean(var))/sd(var)
  } else {
    var = var
  }
  
  out <- data.frame(var = field,
                    value = var,
                    time = time)
  return(out)
  
}

#Normalize data
norm_dat <- NULL
for (i in 1:length(fields)){
  assign("norm_dat",rbind(get_dat(fields[[i]], norm = TRUE), norm_dat))
}

#Filter to make data same length

l <- norm_dat %>% filter(time > 1985) %>% group_by(var) %>% filter(n() > 30) %>%
  arrange() %>% ungroup() %>%
  as.data.frame() 
equal_dat <- l %>% group_by(var) %>% dplyr::summarise(n = n()) %>% ungroup() %>% as.data.frame()

#Long to wide format
wide_out <- l %>% tidyr::spread(key = var, value = value, fill = NA)
wide_out <- wide_out %>% select(-time)

#examine pc1 and pc2
soe_pca <- prcomp(wide_out, scale. = F)
biplot(soe_pca, scale = 0)

#compute standard deviation of each principal component
sdev <- soe_pca$sdev

#variance
soe_var <- sdev^2

#proportion of variance explained by principal components
variance_explained <- soe_var/sum(soe_var)

#scree plot
plot(variance_explained, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")

#cumulative scree plot
plot(cumsum(variance_explained), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")

#References
#https://mvuorre.github.io/toolbox/reshape.html
