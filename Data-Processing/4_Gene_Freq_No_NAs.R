#######################################################################################
######## (4) Calculates the genotype frequencies in each block and removes NAs #########
# Code adapted from Liu et al. 2022


setwd("/Users/Andy/Documents/Uni Work/4th Year/Project/Data/")
file_dir <- "/Users/Andy/Documents/Uni Work/4th Year/Project/Data/"
file_prefix <- "FILENAME.ValueCheckNew.4DegreeBlocks"

records <- read.csv(paste0(file_prefix,".csv"), header = T, stringsAsFactors = F)

# Calculates trait frequency
records$hindwingWhite_aa <- round(records$aa/(records$aa+records$A.), 3)
records$forewingBand_cc <- round(records$cc/(records$cc+records$C.), 3)
records$groundColour_bb <- round(records$bb/(records$bb+records$B.), 3)

# Calculates allele frequency
records$hindwingWhite_a <- round(sqrt(records$aa/(records$aa+records$A.)), 3)
records$forewingBand_c <- round(sqrt(records$cc/(records$cc+records$C.)), 3)
records$groundColour_b <- round(sqrt(records$bb/(records$bb+records$B.)), 3)
records$groundColour_B <- round(1 - records$groundColour_b, 3)


# Calculates sex frequency
records$Female_ZW <- round(sqrt(records$female/(records$female+records$male)), 3)
records$Male_ZZ <- round(sqrt(records$male/(records$female+records$male)), 3)

# Calculates number of recordings
records$hindwingWhite_n <- round(records$aa+records$A., 3)
records$forewingBand_n <- round(records$cc+records$C., 3)
records$groundColour_n <- round(records$bb+records$B., 3)
records$sex_n  <- round(records$female+records$male, 3)


#removes rows with NAs in them
records_clean <- na.omit(records)


write.csv(records_clean, paste0(file_prefix,".frequency_processed.csv"), row.names=F, quote=F)

# Reads the above file and creates mean allele frequencies for use in cline analysis and outputs another file

setwd("/Users/Andy/Documents/Uni Work/4th Year/Project/Data/")
file_dir <- "/Users/Andy/Documents/Uni Work/4th Year/Project/Data/"
file_prefix <- "FILENAME.ValueCheck.4DegreeBlocks.Clean.frequency_processed"

records <- read.csv(paste0(file_prefix,".csv"), header = T, stringsAsFactors = F)

# Renames current columns
records$hindwingWhite_mean <- round(records$hindwingWhite_aa, 3)
records$forewingBand_mean <- round(records$forewingBand_cc, 3)

# Calculates new categories
records$groundColour_mean <- round(records$groundColour_B, 3)
records$sex_mean <- round(records$male/(records$sex_n), 3)

write.csv(records, paste0(file_prefix,".means.csv"), row.names=F, quote=F)