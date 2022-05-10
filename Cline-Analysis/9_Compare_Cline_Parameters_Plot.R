#######################################################################################
############################## (8) Full cline analysis ################################

library(ggplot2)
library(tidyverse)
library(colorspace)
library(viridis)
library(RColorBrewer)
library(showtext)

# Allows the font helvetica to be used in the plots
font_add("Helvetica", regular =  "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-01.ttf", italic = "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-LightOblique-06.ttf", bolditalic = "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-BoldOblique-04.ttf", bold = "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-Bold-02.ttf")


showtext_auto()


setwd("/Users/Andy/Documents/Uni Work/4th Year/Project/Results/")

# Outputs plots as png files in R workspace with the below parameters
png(width=1500, height=1000, res=200, family="Helvetica", 
    filename="Cline_Analysis_Parameters.png",
    pointsize=8)

# Create csv file of cline parameters with columns called "Transect", "Width", "Sex", "LowerCI", "HigherCI" from the results of the HZAR best-fit cline analysis

Width_data <- read.csv(file.choose(), header = T, stringsAsFactors = F)

# Creates a paired barplot of the cline analysis paramters
plot <- ggplot(Width_data, aes(fill=Sex, y=Width, x=Transect)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(x = "Transect No.", y = "Cline Width (Km)") +
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 12, family="Helvetica"), legend.text = element_text(size = 12, family="Helvetica"), legend.title = element_text(size = 12, family="Helvetica")) +
  geom_errorbar(aes(ymin=LowerCI, ymax=HigherCI), width=.5,
                position=position_dodge(.9)) +
  scale_fill_manual(values = c("#E31B23", "#005CAB")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


plot

dev.off()