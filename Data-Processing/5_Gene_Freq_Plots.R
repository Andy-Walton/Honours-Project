#######################################################################################
############################# (5) Gene frequency plots ################################
# Code adapted from Liu et al. 2022


library(ggplot2)
library(maps)
library(mapproj)
library(geosphere)
library(png)
library(showtext)

# Allows the font Helvetica to be used in the plots
font_add("Helvetica", regular =  "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-01.ttf", italic = "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-LightOblique-06.ttf", bolditalic = "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-BoldOblique-04.ttf", bold = "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-Bold-02.ttf")


showtext_auto()

# Outputs plots as png files in R workspace with the below parameters
png(width=1500, height=1000, res=200, family="Helvetica", filename="Freq_Map%d.png",pointsize=8)

cap <- function(x, max) ifelse(x <= max, x, max)


# Add own filepath and file name
setwd("/Users/Andy/Documents/Uni Work/4th Year/Project/Data/")
file_dir <- "/Users/Andy/Documents/Uni Work/4th Year/Project/Data/"
file_prefix <- "FILE NAME.4DegreeBlocks.frequency_processed"

records_4deg <- read.csv(paste0(file_prefix,".csv"), header = T, stringsAsFactors = F)

# regions to include on map
regions = c("Afghanistan"  , "Angola"  , "Albania"  , "Finland"  , "Andorra"  , "United Arab Emirates"  , "Armenia"  , "American Samoa"  , "Australia"  , "French Southern and Antarctic Lands" , "Austria"  , "AzerbaiNov"  , "Burundi"  , "Belgium"  , "Benin"  , "Burkina Faso"  , "Bangladesh"  , "Bulgaria"  , "Bahrain"  , "Bosnia and Herzegovina"  , "Belarus"  , "Brunei"  , "Bhutan"  , "Botswana"  , "Central African Republic"  , "Switzerland"  , "China"  , "Ivory Coast"  , "Cameroon"  , "Democratic Republic of the Congo"  , "Republic of Congo"  , "Cook Islands"  , "Comoros"  , "Cape Verde"  , "Cyprus"  , "Czech Republic"  , "Germany"  , "Djibouti"  , "DenNovk"  , "Algeria"  , "Egypt"  , "Eritrea"  , "Canary Islands"  , "Spain"  , "Estonia"  , "Ethiopia"  , "Fiji"  , "Reunion"  , "Novotte"  , "France"  , "Faroe Islands"  , "Micronesia"  , "Gabon"  , "UK"  , "Georgia"  , "Guernsey"  , "Ghana"  , "Guinea"  , "Gambia"  , "Guinea-Bissau"  , "Equatorial Guinea"  , "Greece"  , "Heard Island"  , "Croatia"  , "Hungary"  , "Indonesia"  , "Isle of Man"  , "India"  , "Cocos Islands"  , "Christmas Island"  , "Chagos Archipelago"  , "Ireland"  , "Iran"  , "Iraq"  , "Iceland"  , "Israel"  , "Italy"  , "San Novino"  , "Jersey"  , "Jordan"  , "Japan"  , "Siachen Glacier"  , "Kazakhstan"  , "Kenya"  , "Kyrgyzstan"  , "Cambodia"  , "Kiribati"  , "South Korea"  , "Kosovo"  , "Kuwait"  , "Laos"  , "Lebanon"  , "Liberia"  , "Libya"  , "Liechtenstein"  , "Sri Lanka"  , "Lesotho"  , "Lithuania"  , "Luxembourg"  , "Latvia"  , "Morocco"  , "Monaco"  , "Moldova"  , "Madagascar"  , "Maldives"  , "Novshall Islands"  , "Macedonia"  , "Mali"  , "Malta"  , "MyanNov"  , "Montenegro"  , "Mongolia"  , "Northern Noviana Islands"  , "Mozambique"  , "Mauritania"  , "Mauritius"  , "Malawi"  , "Malaysia"  , "Namibia"  , "New Caledonia"  , "Niger"  , "Norfolk Island"  , "Nigeria"  , "Niue"  , "Bonaire"  , "Netherlands"  , "Norway"  , "Nepal"  , "Nauru"  , "New Zealand"  , "Oman"  , "Pakistan"  , "Panama"  , "Pitcairn Islands"  , "Philippines"  , "Palau"  , "Papua New Guinea"  , "Poland"  , "North Korea"  , "Madeira Islands"  , "Azores"  , "Portugal"  , "Palestine"  , "French Polynesia"  , "Qatar"  , "Romania"  , "Russia"  , "Rwanda"  , "Western Sahara"  , "Saudi Arabia"  , "Sudan"  , "South Sudan"  , "Senegal"  , "Singapore"  , "South Sandwich Islands"  , "South Georgia"  , "Saint Helena"  , "Ascension Island"  , "Solomon Islands"  , "Sierra Leone"  , "El Salvador"  , "Somalia"  , "Serbia"  , "Slovakia"  , "Slovenia"  , "Sweden"  , "Swaziland"  , "Seychelles"  , "Syria"  , "Turks and Caicos Islands"  , "Chad"  , "Togo"  , "Thailand"  , "Tajikistan"  , "Turkmenistan"  , "Timor-Leste"  , "Tonga"  , "Trinidad"  , "Tobago"  , "Tunisia"  , "Turkey"  , "Taiwan"  , "Tanzania"  , "Uganda"  , "Ukraine"  , "Uzbekistan"  , "Vatican"  , "Vietnam"  , "Vanuatu"  , "Wallis and Futuna"  , "Samoa"  , "Yemen"  , "South Africa"  , "Zambia"  , "Zimbabwe")

map <- map_data("world", regions=regions)

cols = c("#6b2e06", "#9d5100", "#cd7800")

# PLots the world map
plot_map <- ggplot(map, aes(x = long, y = lat, group=group)) +
  geom_polygon(fill="gray95", colour = "gray60") +
  labs(x = "", y = "") +
  theme_bw()

# Defines the boundaries of the plot on the globe
whole_range <- coord_map("mercator", xlim = c(-30,160), ylim = c(-50,50))


# Hindwing white frequency map
records_4deg_Hind <- subset(records_4deg, is.na(hindwingWhite_aa)==FALSE)
trait <- records_4deg_Hind$hindwingWhite_aa
n <- cap(records_4deg_Hind$hindwingWhite_n, 12)
trait_name <- "Hindwing White freq. - 
Nov"

plot_map + whole_range +
  geom_tile(data = records_4deg_Hind, 
            aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
  scale_fill_viridis_c() + labs(fill = trait_name) 


# Forewing band frequency map
records_4deg_Fore <- subset(records_4deg, is.na(forewingBand_cc)==FALSE)
trait <- records_4deg_Fore$forewingBand_cc
n <- cap(records_4deg_Fore$forewingBand_n, 12)
trait_name <- "Forewing Tip freq. - 
Nov"

plot_map + whole_range +
  geom_tile(data = records_4deg_Fore, 
            aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
  scale_fill_viridis_c() + labs(fill = trait_name)


# Ground Colour frequency map
records_4deg_Ground <- subset(records_4deg, is.na(groundColour_bb)==FALSE)
trait <- records_4deg_Ground$groundColour_bb
n <- cap(records_4deg_Ground$groundColour_n, 12)
trait_name <- "Orange Ground Colour freq. -
Nov"

plot_map + whole_range +
  geom_tile(data = records_4deg_Ground, 
            aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
  scale_fill_gradientn(colours = cols) + labs(fill = trait_name) 

dev.off()