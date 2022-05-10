#######################################################################################
######################## (6) Create transects for cline analysis ######################
# Code adapted from Liu et al. 2022

# Do this for the only male and then only female files

library(ggplot2)
library(maps)
library(mapproj)
library(geosphere)
library(showtext)

font_add("Helvetica", regular =  "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-01.ttf", italic = "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-LightOblique-06.ttf", bolditalic = "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-BoldOblique-04.ttf", bold = "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-Bold-02.ttf")

showtext_auto()


cap <- function(x, max) ifelse(x <= max, x, max)


setwd("/Users/Andy/Documents/Uni Work/4th Year/Project/Data/")
file_dir <- "/Users/Andy/Documents/Uni Work/4th Year/Project/Data/"
file_prefix <- "FILE NAME.4DegreeBlocks.frequency_processed.means"


records_4deg <- read.csv(paste0(file_prefix,".csv"), header = T, stringsAsFactors = F)




# regions to include on map
regions = c("Afghanistan"  , "Angola"  , "Albania"  , "Finland"  , "Andorra"  , "United Arab Emirates"  , "Armenia"  , "American Samoa"  , "Australia"  , "French Southern and Antarctic Lands" , "Austria"  , "Azerbaijan"  , "Burundi"  , "Belgium"  , "Benin"  , "Burkina Faso"  , "Bangladesh"  , "Bulgaria"  , "Bahrain"  , "Bosnia and Herzegovina"  , "Belarus"  , "Brunei"  , "Bhutan"  , "Botswana"  , "Central African Republic"  , "Switzerland"  , "China"  , "Ivory Coast"  , "Cameroon"  , "Democratic Republic of the Congo"  , "Republic of Congo"  , "Cook Islands"  , "Comoros"  , "Cape Verde"  , "Cyprus"  , "Czech Republic"  , "Germany"  , "Djibouti"  , "Denmark"  , "Algeria"  , "Egypt"  , "Eritrea"  , "Canary Islands"  , "Spain"  , "Estonia"  , "Ethiopia"  , "Fiji"  , "Reunion"  , "Mayotte"  , "France"  , "Faroe Islands"  , "Micronesia"  , "Gabon"  , "UK"  , "Georgia"  , "Guernsey"  , "Ghana"  , "Guinea"  , "Gambia"  , "Guinea-Bissau"  , "Equatorial Guinea"  , "Greece"  , "Heard Island"  , "Croatia"  , "Hungary"  , "Indonesia"  , "Isle of Man"  , "India"  , "Cocos Islands"  , "Christmas Island"  , "Chagos Archipelago"  , "Ireland"  , "Iran"  , "Iraq"  , "Iceland"  , "Israel"  , "Italy"  , "San Marino"  , "Jersey"  , "Jordan"  , "Japan"  , "Siachen Glacier"  , "Kazakhstan"  , "Kenya"  , "Kyrgyzstan"  , "Cambodia"  , "Kiribati"  , "South Korea"  , "Kosovo"  , "Kuwait"  , "Laos"  , "Lebanon"  , "Liberia"  , "Libya"  , "Liechtenstein"  , "Sri Lanka"  , "Lesotho"  , "Lithuania"  , "Luxembourg"  , "Latvia"  , "Morocco"  , "Monaco"  , "Moldova"  , "Madagascar"  , "Maldives"  , "Marshall Islands"  , "Macedonia"  , "Mali"  , "Malta"  , "Myanmar"  , "Montenegro"  , "Mongolia"  , "Northern Mariana Islands"  , "Mozambique"  , "Mauritania"  , "Mauritius"  , "Malawi"  , "Malaysia"  , "Namibia"  , "New Caledonia"  , "Niger"  , "Norfolk Island"  , "Nigeria"  , "Niue"  , "Bonaire"  , "Netherlands"  , "Norway"  , "Nepal"  , "Nauru"  , "New Zealand"  , "Oman"  , "Pakistan"  , "Panama"  , "Pitcairn Islands"  , "Philippines"  , "Palau"  , "Papua New Guinea"  , "Poland"  , "North Korea"  , "Madeira Islands"  , "Azores"  , "Portugal"  , "Palestine"  , "French Polynesia"  , "Qatar"  , "Romania"  , "Russia"  , "Rwanda"  , "Western Sahara"  , "Saudi Arabia"  , "Sudan"  , "South Sudan"  , "Senegal"  , "Singapore"  , "South Sandwich Islands"  , "South Georgia"  , "Saint Helena"  , "Ascension Island"  , "Solomon Islands"  , "Sierra Leone"  , "El Salvador"  , "Somalia"  , "Serbia"  , "Slovakia"  , "Slovenia"  , "Sweden"  , "Swaziland"  , "Seychelles"  , "Syria"  , "Turks and Caicos Islands"  , "Chad"  , "Togo"  , "Thailand"  , "Tajikistan"  , "Turkmenistan"  , "Timor-Leste"  , "Tonga"  , "Trinidad"  , "Tobago"  , "Tunisia"  , "Turkey"  , "Taiwan"  , "Tanzania"  , "Uganda"  , "Ukraine"  , "Uzbekistan"  , "Vatican"  , "Vietnam"  , "Vanuatu"  , "Wallis and Futuna"  , "Samoa"  , "Yemen"  , "South Africa"  , "Zambia"  , "Zimbabwe")

map <- map_data("world", regions=regions)

# creates background map
plot_map <- ggplot(map, aes(x = long, y = lat, group=group)) +
  geom_polygon(fill="gray95", colour = "gray60") +
  labs(x = "", y = "") +
  theme_bw()

# Defines boundary of map plot
whole_range <- coord_map("mercator", xlim = c(-30,160), ylim = c(-50,50))

cols = c("black", "red")

# Set ouput location for the allele frequency maps
setwd("/Users/Andy/Documents/Uni Work/4th Year/Project/Results/")

## Allele frequency plots which are used to identify potential transect locations
trait <- 1-records_4deg$hindwingWhite_a
n <- cap(records_4deg$hindwingWhite_n, 12)
trait_name <- "Hindwing White freq."

plot1 <- plot_map + whole_range +
  geom_tile(data = records_4deg, 
            aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait, alpha=n), inherit.aes = FALSE) +
  scale_fill_gradientn(colours = cols) + labs(fill = trait_name) + labs(alpha = "No. of records") +
  theme(legend.title = element_text(family = "Helvetica", size=15),
        legend.text = element_text(family = "Helvetica", size = 12),
        axis.text.x = element_text(family = "Helvetica", size = 10),
        axis.text.y = element_text(family = "Helvetica", size = 10))

pdf(file='HindWhiteDistro.pdf')
plot1
dev.off()


trait <- records_4deg$forewingBand_c
n <- cap(records_4deg$forewingBand_n, 12)
trait_name <- "Forewing Tip freq."

plot2 <- plot_map + whole_range +
  geom_tile(data = records_4deg, 
            aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait, alpha=n), inherit.aes = FALSE) +
  scale_fill_gradientn(colours = cols) + labs(fill = trait_name) + labs(alpha = "No. of records") +
  theme(legend.title = element_text(family = "Helvetica", size=15),
        legend.text = element_text(family = "Helvetica", size = 12),
        axis.text.x = element_text(family = "Helvetica", size = 10),
        axis.text.y = element_text(family = "Helvetica", size = 10))

pdf(file='ForewingDistro.pdf')
plot2
dev.off()


trait <- 1-records_4deg$groundColour_B
n <- cap(records_4deg$groundColour_n, 12)
trait_name <- "Orange Ground Colour freq."

plot3 <- plot_map + whole_range +
  geom_tile(data = records_4deg, 
            aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait, alpha=n), inherit.aes = FALSE) +
  scale_fill_gradientn(colours = cols) + labs(fill = trait_name) + labs(alpha = "No. of records") +
  theme(legend.title = element_text(family = "Helvetica", size=15),
        legend.text = element_text(family = "Helvetica", size = 12),
        axis.text.x = element_text(family = "Helvetica", size = 10),
        axis.text.y = element_text(family = "Helvetica", size = 10))

pdf(file='GCDistro.pdf')
plot3
dev.off()


## Once you have identified possible transect locations using the above maps, input them below

library(geosphere)

# Defines start and end point of each transect
transects <- as.data.frame(rbind(c(lon_start=-14,   lat_start=16,  lon_end=38,  lat_end=-8), #Central Africa
                                 c(lon_start=26,  lat_start=-32, lon_end=42,  lat_end=8),  #South East Africa
                                 c(lon_start=10,  lat_start=12,   lon_end=18,  lat_end=-28),#South West Africa
                                 c(lon_start=-18, lat_start=12,  lon_end=2, lat_end=44), #North West Africa
                                 ))


# Defines maximum perpendictular distance midpoint of the 4x4 block can be from the transect to be included
max_dist = 450


records_4deg_transects <- list()


for (i in 1:nrow(transects)){
  dist_from_transect <- dist2gc(p1=c(transects[i,"lon_start"], transects[i,"lat_start"]),
                                p2=c(transects[i,"lon_end"], transects[i,"lat_end"]),
                                p3=records_4deg[,c("block_midpoint_long","block_midpoint_lat")])/1000
  
  transect_pos <- alongTrackDistance(p1=c(transects[i,"lon_start"], transects[i,"lat_start"]),
                                     p2=c(transects[i,"lon_end"], transects[i,"lat_end"]),
                                     p3=records_4deg[,c("block_midpoint_long","block_midpoint_lat")])/1000
  
  # Identifies blocks along the transect
  transect_blocks <- which(dist_from_transect <= max_dist &
                             records_4deg$block_midpoint_long >= min(c(transects$lon_start[i], c(transects$lon_end[i]))) &
                             records_4deg$block_midpoint_long <= max(c(transects$lon_start[i], c(transects$lon_end[i]))) &
                             records_4deg$block_midpoint_lat >= min(c(transects$lat_start[i], c(transects$lat_end[i]))) &
                             records_4deg$block_midpoint_lat <= max(c(transects$lat_start[i], c(transects$lat_end[i]))))
  
  records_4deg_transects[[i]] <- records_4deg[transect_blocks,]
  
  records_4deg_transects[[i]]$transect_pos <- transect_pos[transect_blocks]
  
}


# Exports records located along each transect
for (i in 1:nrow(transects)){
  write.csv(records_4deg_transects[[i]], file=paste0(file_prefix, ".processed.4DegreeBlocks.transect", i, ".csv"),
            row.names=FALSE, quote=FALSE)
}


########################  plotting transects  ############################


# For transect plot, sets boundaries for plot

Central_Africa <- coord_map("mercator", xlim = c(-20,60), ylim = c(-20,30))

Southern_Africa <- coord_map("mercator", xlim = c(-5,42), ylim = c(-35,20))

Northwest_Africa <- coord_map("mercator", xlim = c(-20,10), ylim = c(5,45))

Southeast_Asia <- coord_map("mercator", xlim = c(90,130), ylim = c(-15,25))


transect_col = "blue"

# Plots Transect 1 and blocks included for the two loci included

i=1

trait <- 1-records_4deg_transects[[i]]$hindwingWhite_a
n <- cap(records_4deg_transects[[i]]$hindwingWhite_n, 12)
trait_name <- "Hindwing white freq."

Plot4 <- plot_map + Central_Africa +
  geom_tile(data = records_4deg_transects[[i]], 
            aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait, alpha=n), inherit.aes = FALSE) +
  scale_fill_gradientn(colours = cols) + labs(fill = trait_name) + labs(alpha = "No. of records") +
  
  annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
           y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2) +
  theme(legend.title = element_text(family = "Helvetica", size=15),
        legend.text = element_text(family = "Helvetica", size = 12),
        axis.text.x = element_text(family = "Helvetica", size = 10),
        axis.text.y = element_text(family = "Helvetica", size = 10))

pdf(file='Transect1_1.pdf')
Plot4
dev.off()


trait <- records_4deg_transects[[i]]$forewingBand_c
n <- cap(records_4deg_transects[[i]]$forewingBand_n, 12)
trait_name <- "Forewing tip freq."

Plot5 <- plot_map + Central_Africa +
  geom_tile(data = records_4deg_transects[[i]], 
            aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait, alpha=n), inherit.aes = FALSE) +
  scale_fill_gradientn(colours = cols) + labs(fill = trait_name) + labs(alpha = "No. of records") +
  
  annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
           y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2) +
  theme(legend.title = element_text(family = "Helvetica", size=15),
        legend.text = element_text(family = "Helvetica", size = 12),
        axis.text.x = element_text(family = "Helvetica", size = 10),
        axis.text.y = element_text(family = "Helvetica", size = 10))

pdf(file='Transect1_2.pdf')
Plot5
dev.off()



# Plots Transect 2 and blocks included for the two loci included

i=2

trait <- records_4deg_transects[[i]]$forewingBand_c
n <- cap(records_4deg_transects[[i]]$forewingBand_n, 12)
trait_name <- "Forewing tip freq."

Plot6 <- plot_map + Southern_Africa +
  geom_tile(data = records_4deg_transects[[i]], 
            aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait, alpha=n), inherit.aes = FALSE) +
  scale_fill_gradientn(colours = cols) + labs(fill = trait_name) + labs(alpha = "No. of records") +
  
  annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
           y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2) +
  theme(legend.title = element_text(family = "Helvetica", size=15),
        legend.text = element_text(family = "Helvetica", size = 12),
        axis.text.x = element_text(family = "Helvetica", size = 10),
        axis.text.y = element_text(family = "Helvetica", size = 10))

pdf(file='Transect2_1.pdf')
Plot6
dev.off()


trait <- 1-records_4deg_transects[[i]]$groundColour_B
n <- cap(records_4deg_transects[[i]]$groundColour_n, 12)
trait_name <- "Orange ground colour freq."

Plot7 <- plot_map + Southern_Africa +
  geom_tile(data = records_4deg_transects[[i]], 
            aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait, alpha=n), inherit.aes = FALSE) +
  scale_fill_gradientn(colours = cols) + labs(fill = trait_name) + labs(alpha = "No. of records") +
  
  annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
           y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2) +
  theme(legend.title = element_text(family = "Helvetica", size=15),
        legend.text = element_text(family = "Helvetica", size = 12),
        axis.text.x = element_text(family = "Helvetica", size = 10),
        axis.text.y = element_text(family = "Helvetica", size = 10))

pdf(file='Transect2_2.pdf')
Plot7
dev.off()


# Plots Transect 3 and blocks included for the two loci included

i=3

trait <- 1-records_4deg_transects[[i]]$hindwingWhite_a
n <- cap(records_4deg_transects[[i]]$hindwingWhite_n, 12)
trait_name <- "Hindwing white freq."

Plot8 <- plot_map + Southern_Africa +
  geom_tile(data = records_4deg_transects[[i]], 
            aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait, alpha=n), inherit.aes = FALSE) +
  scale_fill_gradientn(colours = cols) + labs(fill = trait_name) + labs(alpha = "No. of records") +
  
  annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
           y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2) +
  theme(legend.title = element_text(family = "Helvetica", size=15),
        legend.text = element_text(family = "Helvetica", size = 12),
        axis.text.x = element_text(family = "Helvetica", size = 10),
        axis.text.y = element_text(family = "Helvetica", size = 10))

pdf(file='Transect3_1.pdf')
Plot8
dev.off()


trait <- 1-records_4deg_transects[[i]]$groundColour_B
n <- cap(records_4deg_transects[[i]]$groundColour_n, 12)
trait_name <- "Orange ground colour freq."

Plot9 <- plot_map + Southern_Africa +
  geom_tile(data = records_4deg_transects[[i]], 
            aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait, alpha=n), inherit.aes = FALSE) +
  scale_fill_gradientn(colours = cols) + labs(fill = trait_name) + labs(alpha = "No. of records") +
  
  annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
           y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2) +
  theme(legend.title = element_text(family = "Helvetica", size=15),
        legend.text = element_text(family = "Helvetica", size = 12),
        axis.text.x = element_text(family = "Helvetica", size = 10),
        axis.text.y = element_text(family = "Helvetica", size = 10))

pdf(file='Transect3_2.pdf')
Plot9
dev.off()


# Plots Transect 4 and blocks included for the two loci included

i=4

trait <- 1-records_4deg_transects[[i]]$hindwingWhite_a
n <- cap(records_4deg_transects[[i]]$hindwingWhite_n, 12)
trait_name <- "Hindwing white freq."

Plot10 <- plot_map + Northwest_Africa +
  geom_tile(data = records_4deg_transects[[i]], 
            aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait, alpha=n), inherit.aes = FALSE) +
  scale_fill_gradientn(colours = cols) + labs(fill = trait_name) + labs(alpha = "No. of records") +
  
  annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
           y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2) +
  theme(legend.title = element_text(family = "Helvetica", size=15),
        legend.text = element_text(family = "Helvetica", size = 12),
        axis.text.x = element_text(family = "Helvetica", size = 10),
        axis.text.y = element_text(family = "Helvetica", size = 10))

pdf(file='Transect4_1.pdf')
Plot10
dev.off()


trait <- 1-records_4deg_transects[[i]]$groundColour_B
n <- cap(records_4deg_transects[[i]]$groundColour_n, 12)
trait_name <- "Orange ground colour freq."

Plot11 <- plot_map + Northwest_Africa +
  geom_tile(data = records_4deg_transects[[i]], 
            aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait, alpha=n), inherit.aes = FALSE) +
  scale_fill_gradientn(colours = cols) + labs(fill = trait_name) + labs(alpha = "No. of records") +
  
  annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
           y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2) +
  theme(legend.title = element_text(family = "Helvetica", size=15),
        legend.text = element_text(family = "Helvetica", size = 12),
        axis.text.x = element_text(family = "Helvetica", size = 10),
        axis.text.y = element_text(family = "Helvetica", size = 10))

pdf(file='Transect4_2.pdf')
Plot11
dev.off()

#### Plots locations of all transects on one combined plot

# Sets boundaries of the plot
Africa_range <- coord_map("mercator", xlim = c(-30,65), ylim = c(-42,50))

# Input the start/end point of each transect
mydf <- data.frame(id = 1:4, 
                   lat_1 = c(16, -32, 12, 12), 
                   lon_1 = c(-14, 26, 10, -18), 
                   lat_2 = c(-8, 8, -28, 44), 
                   lon_2 = c(38, 42, 18, 2))


plot_map + Africa_range +
  geom_segment(data = mydf, 
               aes(x = lon_1, y = lat_1, xend = lon_2, yend = lat_2), inherit.aes = F, 
               color = "blue", size = 2, alpha = 0.8, lineend = "round")

dev.off()