#######################################################################################
################# (2a) Subsets Records into Month and Adds Env Data ###################
# Code adapted from Liu et al. 2022

library(raster)
library(rgeos)
library(rgdal)
library(maptools) 
data(wrld_simpl)
library(maps)

## Subsetting the data into months, with a separate file for records in each month

setwd("/Users/Andy/Documents/Uni Work/4th Year/Project/Data/")
file_dir <- "/Users/Andy/Documents/Uni Work/4th Year/Project/Data/"
file_prefix <- "FILE.ValueCheck"

b_flies <- read.csv(paste0(file_prefix,".csv"), header = T, stringsAsFactors = F)

# using subset function (part 2)
# Change number after month for each month and do this 12 times, one for every month
Mon_records <- b_flies[b_flies$month == 12, ]

write.csv(Mon_records, "Dec.ValueCheck.csv"), row.names=F, quote=F)


# Select which mmonth file you want to add the climate data to
setwd("/Users/Andy/Documents/Uni Work/4th Year/Project/Data/")
file_dir <- "/Users/Andy/Documents/Uni Work/4th Year/Project/Data/"
file_prefix <- "Dec.ValueCheck"

records <- read.csv(paste0(file_prefix,".csv"), header = T, stringsAsFactors = F)


## Extracts annual climatic data (add your own filepath to these files) 
## Change th number after tavg_ etc. in the filepath to the month that matches your records (the 12 here means the data is from Dec)
# Source: https://www.worldclim.org/data/worldclim21.html
temp_av <- raster("/Users/Andy/Documents/Uni Work/4th Year/Project/Data/WorldClim Data/Full/wc2.1_2.5m_tavg_12.tif")
rad_av <- raster("/Users/Andy/Documents/Uni Work/4th Year/Project/Data/WorldClim Data/Full/wc2.1_2.5m_srad_12.tif")
pre_av <- raster("/Users/Andy/Documents/Uni Work/4th Year/Project/Data/WorldClim Data/Full/wc2.1_2.5m_prec_12.tif")


#Adds environmental variables to the records
DataPoints1 <- records[c(14:13)] # defines long and lat of each record

records$temp_av <- extract(temp_av, DataPoints1)
records$rad_av <- extract(rad_av, DataPoints1)
records$pre_av <- extract(pre_av, DataPoints1)


write.csv(records, paste0(file_prefix,".MnthAv.csv"), row.names=F, quote=F)
