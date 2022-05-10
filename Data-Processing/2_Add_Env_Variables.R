#######################################################################################
################# (2) Add Environmental Variables to Butterfly records #################
# Code adapted from Liu et al. 2022

library(raster)
library(rgeos)
library(rgdal)
library(maptools) 
data(wrld_simpl)
library(maps)

# Setting workplace
setwd("/Users/Andy/Documents/Uni Work/4th Year/Project/Data")

##Extracts annual climatic data (add your own filepath to these files)
# Source: https://www.climond.org/BioclimData.aspx

Bio01 <- raster("/Users/Andy/Documents/Uni Work/4th Year/Project/Data/Climond Data/CM10_1975H_Bio01_V1.2.asc")
Bio12 <- raster("/Users/Andy/Documents/Uni Work/4th Year/Project/Data/Climond Data/CM10_1975H_Bio12_V1.2.asc")
Bio20 <- raster("/Users/Andy/Documents/Uni Work/4th Year/Project/Data/Climond Data/CM10_1975H_Bio20_V1.2.asc")
Bio28 <- raster("/Users/Andy/Documents/Uni Work/4th Year/Project/Data/Climond Data/CM10_1975H_Bio28_V1.2.asc")
Bio08 <- raster("/Users/Andy/Documents/Uni Work/4th Year/Project/Data/Climond Data/CM10_1975H_Bio04_V1.2.asc")
Bio09 <- raster("/Users/Andy/Documents/Uni Work/4th Year/Project/Data/Climond Data/CM10_1975H_Bio15_V1.2.asc")
Bio10 <- raster("/Users/Andy/Documents/Uni Work/4th Year/Project/Data/Climond Data/CM10_1975H_Bio23_V1.2.asc")
Bio11 <- raster("/Users/Andy/Documents/Uni Work/4th Year/Project/Data/Climond Data/CM10_1975H_Bio31_V1.2.asc")

## Extracts ground cover data (add your own filepath to these files)
# Source: https://www.earthenv.org/landcover

shrub <- raster("/Users/Andy/Documents/Uni Work/4th Year/Project/Data/Ground Cover Data/consensus_full_class_5.tif")
herb <- raster("/Users/Andy/Documents/Uni Work/4th Year/Project/Data/Ground Cover Data/consensus_full_class_6.tif")
cult <- raster("/Users/Andy/Documents/Uni Work/4th Year/Project/Data/Ground Cover Data/consensus_full_class_7.tif")
barren <- raster("/Users/Andy/Documents/Uni Work/4th Year/Project/Data/Ground Cover Data/consensus_full_class_11.tif")

# Loads butterfly records (add your own filepath to this file)
setwd("/Users/Andy/Documents/Uni Work/4th Year/Project/Data/")
file_dir <- "/Users/Andy/Documents/Uni Work/4th Year/Project/Data/"
file_prefix <- "FILE NAME" #Add your file name here

records <- read.csv(paste0(file_dir, file_prefix,".csv"), header = T, stringsAsFactors = F)

#Adds environmental variables to the records
DataPoints1 <- records[c(14:13)] # defines long and lat of each record

records$Bio01 <- extract(Bio01, DataPoints1)
records$Bio12 <- extract(Bio12, DataPoints1)
records$Bio20 <- extract(Bio20, DataPoints1)
records$Bio28 <- extract(Bio28, DataPoints1)

records$Bio08 <- extract(Bio08, DataPoints1)
records$Bio09 <- extract(Bio09, DataPoints1)
records$Bio10 <- extract(Bio10, DataPoints1)
records$Bio11 <- extract(Bio11, DataPoints1)


records$shrub <- extract(shrub, DataPoints1)
records$herb <- extract(herb, DataPoints1)
records$cult <- extract(cult, DataPoints1)
records$barren <- extract(barren, DataPoints1)

write.csv(records, paste0(file_prefix,".ValueCheck.csv"), row.names=F, quote=F)
