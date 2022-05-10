#######################################################################################
################# (1) Scripts to Clean-up and Check Raw Records #######################
# Code adapted from Liu et al. 2022

setwd("/Users/Andy/Documents/Uni Work/4th Year/Project/Data/")
file_dir <- "/Users/Andy/Documents/Uni Work/4th Year/Project/Data/"
file_prefix <- "FILE NAME" #Add input file name here

butterflies <- read.csv(paste0(file_prefix,".csv"), header = T, stringsAsFactors = F)

#Replace blanks and unknowns with NA and corrects common spelling mistakes
butterflies$sex[which(butterflies$sex == "")] <- NA
butterflies$sex[which(butterflies$sex == "UNKNOWN")] <- NA
butterflies$sex[which(butterflies$sex == "Unknown")] <- NA

butterflies$hindwingWhite[which(butterflies$hindwingWhite == "")] <- NA
butterflies$hindwingWhite[which(butterflies$hindwingWhite == "UNKNOWN")] <- NA
butterflies$hindwingWhite[which(butterflies$hindwingWhite == "Unknown")] <- NA
butterflies$hindwingWhite[which(butterflies$hindwingWhite == "Absent")] <- "absent"
butterflies$hindwingWhite[which(butterflies$hindwingWhite == "ABsent")] <- "absent"
butterflies$hindwingWhite[which(butterflies$hindwingWhite == "Partial")] <- "partial"
butterflies$hindwingWhite[which(butterflies$hindwingWhite == "Present")] <- "present"

butterflies$forewingTip[which(butterflies$forewingTip == "")] <- NA
butterflies$forewingTip[which(butterflies$forewingTip == "UNKNOWN")] <- NA
butterflies$forewingTip[which(butterflies$forewingTip == "unknown")] <- NA
butterflies$forewingTip[which(butterflies$forewingTip == "Unknown")] <- NA
butterflies$forewingTip[which(butterflies$forewingTip == "Absent")] <- "absent"
butterflies$forewingTip[which(butterflies$forewingTip == "Partial")] <- "partial"
butterflies$forewingTip[which(butterflies$forewingTip == "Present")] <- 'present'

butterflies$groundColour[which(butterflies$groundColour == "")] <- NA
butterflies$groundColour[which(butterflies$groundColour == "UNKNOWN")] <- NA
butterflies$groundColour[which(butterflies$groundColour == "Unknown")] <- NA
butterflies$groundColour[which(butterflies$groundColour == "Dark")] <- "dark"
butterflies$groundColour[which(butterflies$groundColour == "Light")] <- "light"

butterflies$month[which(butterflies$month == "")] <- NA
butterflies$month[which(butterflies$month == "UNKNOWN")] <- NA
butterflies$month[which(butterflies$month == "Unknown")] <- NA

butterflies$year[which(butterflies$year == "")] <- NA
butterflies$year[which(butterflies$year == "UNKNOWN")] <- NA
butterflies$year[which(butterflies$year == "Unknown")] <- NA

#Sorts out records which do not contain required info

butterflies <- subset(butterflies, is.na(hindwingWhite)==FALSE | is.na(forewingTip)==FALSE | is.na(groundColour)==FALSE | is.na(sex)==FALSE | is.na(month)==FALSE | is.na(year)==FALSE)


# Outputs values for each category - to check for weird spellings/typos

table(butterflies$sex)

table(butterflies$month)
table(butterflies$year)

table(butterflies$hindwingWhite)
table(butterflies$forewingTip)
table(butterflies$groundColour)

#Exports cleaned file

write.csv(butterflies, paste0(file_prefix,".subset.csv"), row.names=F, quote=F)


#######################################################################################
#####################  Processes records from the GBIF Dataset ########################


setwd("/Users/Andy/Documents/Uni Work/4th Year/Project/Data")
file_dir <- "/Users/Andy/Documents/Uni Work/4th Year/Project/Data"
file_prefix <- "FILE NAME_subset" #Add input file name here

records <- read.csv(paste0(file_prefix,".csv"), header = T, stringsAsFactors = F)

# Replaces blanks and unknowns with NAs
records$hindwingWhite[which(records$hindwingWhite == "")] <- NA
records$hindwingWhite[which(records$hindwingWhite == "UNKNOWN")] <- NA

records$forewingTip[which(records$forewingTip == "")] <- NA
records$forewingTip[which(records$forewingTip == "UNKNOWN")] <- NA

records$groundColour[which(records$groundColour == "")] <- NA
records$groundColour[which(records$groundColour == "UNKNOWN")] <- NA

# Removes rows with no data at all
records <- subset(records, is.na(hindwingWhite)==FALSE | is.na(forewingTip)==FALSE | is.na(groundColour)==FALSE)

# Outputs values for each category - to check for weird spellings/typos, to be changed manually
table(records$sex)
table(records$hindwingWhite)
table(records$forewingTip)
table(records$groundColour)


#converts phenotypes to numeric values
records$aa <- ifelse(records$hindwingWhite == "present", 1, 0)
records$A. <- ifelse(records$hindwingWhite == "absent" | records$hindwingWhite == "partial", 1, 0)

records$bb <- ifelse(records$groundColour == "light", 1, 0)
records$B. <- ifelse(records$groundColour == "dark" | records$groundColour == "intermediate", 1, 0)

records$cc <- ifelse(records$forewingTip == "present", 1, 0)
records$C. <- ifelse(records$forewingTip == "absent" | records$forewingTip == "partial", 1, 0)

records$male <- ifelse(records$sex == "male", 1, 0)
records$female <- ifelse(records$sex == "female", 1, 0)

records$Total <- 1

#Exports cleaned file
write.csv(records, paste0(file_prefix,".processed.csv"), row.names=F, quote=F)

####### Subsets data and outputs files with only male records or only females

# Only females
female_records <- records[records$sex == "female", ]

# Exports file of only female records
write.csv(female_records, "Only_Females.processed"), row.names=F, quote=F)

# Only males
male_records <- records[records$sex == "male", ]

# Exports file of only female records
write.csv(male_records, "Only_Males.processed"), row.names=F, quote=F)
