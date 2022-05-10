#######################################################################################
################# (2a) Subsets Records into Month and Adds Env Data ###################
# Code adapted from Liu et al. 2022

# Divides the globe into 4x4 degree blocks
define_all_blocks <- function(blocksize){
  longitude_level_names <- levels(cut(0, breaks = seq(-180, 180, blocksize)))
  latitude_level_names <- levels(cut(0, breaks = seq(-90, 90, blocksize)))
  
  midpoint_long <- seq(-180+blocksize/2, 180, blocksize)
  midpoint_lat <- seq(-90+blocksize/2, 90, blocksize)
  
  names(midpoint_long) <- levels(longitude_level_names)
  names(midpoint_lat) <- levels(latitude_level_names)
  
  blocks <- expand.grid(longitude_level_names, latitude_level_names)
  
  # Add a column into the data with the midpoint location of each block
  names(blocks) <- c("block_name_long", "block_name_lat")
  
  blocks$block_midpoint_long <- midpoint_long[blocks$block_name_long]
  blocks$block_midpoint_lat <- midpoint_lat[blocks$block_name_lat]
  
  blocks
}

# Assigns each record to its block
group_records_into_blocks <- function(longitude, latitude, blocksize, block_data){
 
  block_assignment_long <- cut(longitude, breaks = seq(-180, 180, blocksize))
  block_assignment_lat <- cut(latitude, breaks = seq(-90, 90, blocksize))
  
  lapply(1:nrow(block_data), function(i) which(block_assignment_long == block_data[i,"block_name_long"] &
                                                 block_assignment_lat == block_data[i,"block_name_lat"]))
}



# input your file from script (2)
setwd("/Users/Andy/Documents/Uni Work/4th Year/Project/Data/")
file_dir <- "/Users/Andy/Documents/Uni Work/4th Year/Project/Data/"
file_prefix <- "FILENAME.ValueCheck"

records <- read.csv(paste0(file_prefix,".csv"), header = T, stringsAsFactors = F)


block_data <- define_all_blocks(blocksize=4)

# Identifies records which fall in each block
rows_by_block <- group_records_into_blocks(records$decimalLongitude, records$decimalLatitude,
                                           blocksize=4, block_data=block_data)

# Sums and averages each of the traits for each block

traits = c("Total", "male", "female", "aa", "A.", "bb", "B.", "cc", "C.")

counts_by_block <- t(sapply(rows_by_block, function(rows) apply(records[rows, traits], 2, sum, na.rm=T)))

block_data <- cbind(block_data, counts_by_block)

# Averages each environmental variable for each block

env_variables <- c("Bio01", "Bio12", "Bio20", "Bio28", "Bio08","Bio09","Bio10","Bio11","shrub","herb","cult","barren")

for (env_variables in env_variables){
  env_var_block_means <- sapply(rows_by_block, function(rows) mean(records[rows, env_variables], na.rm=T))
  block_data[,env_variables] <- env_var_block_means
}


write.csv(block_data[,-c(1,2)],paste0(file_prefix,".4DegreeBlocks.csv"), row.names=F, quote=F)