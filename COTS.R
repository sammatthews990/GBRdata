# 1. Import Manta data
# 2. Average CoTS count per manta tow for each year
# 3. Find the nearest GBRMPA site to LMTP
# 4. Add CoTS data to XYZ
install.packages("tidyverse")
install.packages("rgeos")
install.packages("sp")
library(tidyverse)
library(sp)
library(rgeos)
library(gstat)

# 1. LOad Data ----
setwd("C:/Users/jc312264/OneDrive - James Cook University/PhDThymes/Modelling/Chapter1_DisturbanceRegimes")
manta = read.csv("Data/ltmp/manta.csv") #LTMP Manta Tow Data
XYZ = read.csv("Data/FinalDataFiles/Environmental_data.csv") #GBRMPA 0.01 degree grid
# benthos <- read.csv("ltmp/benthos.csv")

# 2. Average COTS/Manta Per Site ----
colnames(manta)[6] = "LONG"

# need to create a calender year column
manta$CALENDER_YEAR = substring(as.character(manta$SDATE),8,11)

# create average COTS/manta tow for each year
manta_year = manta %>% group_by(REEF_NAME, LAT, LONG, CALENDER_YEAR) %>%
                        dplyr::summarise(mean_COTS = mean(COT_COUNT)) %>% 
                        spread(key = CALENDER_YEAR, value = mean_COTS)


# 3. Attribute to locast grid cell ----

# I have mean CoTS count for each year, now I need to attibute each 
# LTMP site to the closest on the grid 
manta.sp = manta_year
coordinates(manta.sp) = ~LONG+LAT # define the coords to creat Spatial Points data frame
XYZ.sp = XYZ
coordinates(XYZ.sp) <- ~lon+lat # same as above
Gdist <- rgeos::gDistance(manta.sp, XYZ.sp, byid = T) #calculate the geographic distance between every point


#find which row has the min distance 
#creates vector of the minimum distances which we use to index the cords of XYZ
min.d <- apply(Gdist, 2, which.min) #index
min.gd <- apply(Gdist, 2, min) # minimum distance

# add the minimum geographic distance to the dataframe to remove those that have been displaced greater than 0.01 degrees
manta_year$min.gd = min.gd 

# Double check that the matches are ok
coord.matches <- cbind(manta_year[1:3],XYZ[min.d,1:3], min.gd)

# Replace the Coordinates with the closest coordinates from our grid
manta_year[,2:3] <- XYZ[min.d,2:3]
colnames(manta_year)[2:3] <- c("LONG", "LAT") # coords were in different order
colnames(XYZ)[2:3] <- c("LONG", "LAT")

# Add Grid identifiers
XYZ_COTS <- inner_join(XYZ, manta_year, by=c("LAT", "LONG")) %>% 
  #join if the distance that the LTMP site has been moved is below a 
  #certain threshold and keeps NA's
            filter(min.gd < 0.02 | is.na(min.gd)) 