# 1. Import Manta data
# 2. Average CoTS count per manta tow for each year
# 3. Load preprocessed Data
# 4. Interpolate


# install.packages("tidyverse")
# install.packages("rgeos")
# install.packages("sp")
library(tidyverse)
library(sp)
library(rgeos)
library(gstat)

# 1. Load Data and PreProcessing ----
#setwd("C:/Users/jc312264/OneDrive - James Cook University/PhDThymes/Modelling/Chapter1_DisturbanceRegimes")
manta = read.csv("Data/ltmp/manta.csv") #LTMP Manta Tow Data
XYZ = read.csv("Data/FinalDataFiles/Environmental_data.csv")[1:5] #GBRMPA 0.01 degree grid
# benthos <- read.csv("ltmp/benthos.csv")

# 2. Average COTS/Manta Per Site ----
colnames(manta)[6] = "LONG"

# need to create a calender year column
manta$CALENDER_YEAR = substring(as.character(manta$SDATE),8,11)

# create average COTS/manta tow for each year
manta_year = manta %>% group_by(REEF_NAME, LAT, LONG, CALENDER_YEAR) %>%
                        dplyr::summarise(mean_COTS = mean(COT_COUNT)) %>% 
                        spread(key = CALENDER_YEAR, value = mean_COTS)

save(file = "RawData_COTS.Rdata", list = c("manta_year", "XYZ"))

# 3. Load Processed Data (Start here for Github users) ----

# setwd() # set working directory to load data files
load("RawData_COTS.Rdata")

# 4. Interpolate to XYZ grid ----

# set up dataframe for results
XYZ_COTS = as.data.frame(XYZ)

for (Year in 1985:2017) {## returns the object as a list of dataframes
  #first we need to subset the manta_year data set
  Obs = na.omit(data.frame(manta_year[,2:3],manta_year[,Year-1983+4])) #create observations of CoTS for year i
  colnames(Obs)[3] = "mean.COTS"
  coordinates(Obs) = ~LONG+LAT 
  COTS.idw = as.data.frame(idw(mean.COTS~1, Obs, XYZ, maxdist = 1, nmin= 3))
  XYZ_COTS = XYZ_COTS %>% bind_cols(newdat = round(COTS.idw$var1.pred,4))
  colnames(XYZ_COTS)[Year-1983+4] = paste0("COTS_", Year)
}

#write.csv(XYZ_COTS, "CoTS_data.csv", row.names = F)


