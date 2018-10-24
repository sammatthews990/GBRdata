library(dplyr)
# install.packages("gstat")
setwd("C:/Users/jc312264/Documents/GitHub/GBRdata")
# load environmental data and bleaching aerial surveys.
load("RawData_bleaching.Rdata")

# Inverse distance weighting interpolation
bl98.idw = as.data.frame(gstat::idw(BleachingCategory~1, bl98, XYZ, maxdist = 1, nmin= 3))
bl02.idw = as.data.frame(gstat::idw(BleachingCategory~1, bl02, XYZ, maxdist = 1, nmin= 3))
bl16.idw = as.data.frame(gstat::idw(BleachingCategory~1, bl16, XYZ, maxdist = 1, nmin= 3))

# join results to pixel identifiers
bleach = as.data.frame(XYZ) %>% 
  inner_join(bl98.idw[1:3], by=c("lon", "lat")) %>%
  inner_join(bl02.idw[1:3], by=c("lon", "lat")) %>% 
  inner_join(bl16.idw[1:3], by=c("lon", "lat")) 
bleach[5:7] = round(bleach[5:7])
colnames(bleach)[5:7] = paste0("bleach_", c("1998", "2002", "2016"))

# write.csv(bleach, "Bleaching_data_98_02_16.csv", row.names = F)
