#######################################################################################
###### (12) GLS Process Part 3 - Plot linear correlation and Pearson Correlation ######
# Code adapted from Liu et al. 2022

library(ggplot2)
library(scales)
library(gridExtra)
library(ggpubr)
library(showtext)

# Allows the font Helvetica to be used in the plots
font_add("Helvetica", regular =  "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-01.ttf", italic = "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-LightOblique-06.ttf", bolditalic = "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-BoldOblique-04.ttf", bold = "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-Bold-02.ttf")

showtext_auto()

# Where to output plots
setwd("/Users/Andy/Documents/Uni Work/4th Year/Project/Results/")

perfal <- read.csv(file.choose(), header = T, stringsAsFactors = F)


### Separate plot for each trait, change which trait and locus you want to plot

groundColour.plot <- ggplot(aes(x=shrub, y=groundColour_bb), data=perfal) +
  geom_jitter(shape=16, alpha=0.2, position=position_jitter(0.2))+
  labs(x = expression(paste("shrub (C of V)")), y = "Ground Colour bb Freq (Score)", col=expression(paste("shrub (C of V)"))) +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x.top = element_line(color = "black"),
        axis.line.x.bottom = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.title.x = element_text(size = 20, family = "Helvetica"),
        axis.text = element_text(size=16, family = "Helvetica"),
        axis.title.y = element_text(size = 20, family = "Helvetica"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
groundColour.plot <- groundColour.plot + geom_smooth(method="lm", fill="lightblue", formula = y ~ x) + stat_cor(method = "pearson", size=6, family = "Helvetica")
groundColour.plot

pdf(file='Scatterplots_shrub_03.pdf')
groundColour.plot
dev.off()

########################

groundColour.plot <- ggplot(aes(x=herb, y=groundColour_bb), data=perfal) +
  geom_jitter(shape=16, alpha=0.2, position=position_jitter(0.2))+
  labs(x = expression(paste("herb (C of V)")), y = "Ground Colour bb Freq (Score)", col=expression(paste("herb (C of V)"))) +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x.top = element_line(color = "black"),
        axis.line.x.bottom = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.title.x = element_text(size = 20, family = "Helvetica"),
        axis.text = element_text(size=16, family = "Helvetica"),
        axis.title.y = element_text(size = 20, family = "Helvetica"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
groundColour.plot <- groundColour.plot + geom_smooth(method="lm", fill="lightblue", formula = y ~ x) + stat_cor(method = "pearson", size=6, family = "Helvetica")
groundColour.plot

pdf(file='Scatterplots_herb_03.pdf')
groundColour.plot
dev.off()

########################

groundColour.plot <- ggplot(aes(x=cult, y=groundColour_bb), data=perfal) +
  geom_jitter(shape=16, alpha=0.2, position=position_jitter(0.2))+
  labs(x = expression(paste("cult (C of V)")), y = "Ground Colour bb Freq (Score)", col=expression(paste("cult (C of V)"))) +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x.top = element_line(color = "black"),
        axis.line.x.bottom = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.title.x = element_text(size = 20, family = "Helvetica"),
        axis.text = element_text(size=16, family = "Helvetica"),
        axis.title.y = element_text(size = 20, family = "Helvetica"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
groundColour.plot <- groundColour.plot + geom_smooth(method="lm", fill="lightblue", formula = y ~ x) + stat_cor(method = "pearson", size=6, family = "Helvetica")
groundColour.plot

pdf(file='Scatterplots_cult_03.pdf')
groundColour.plot
dev.off()

########################

groundColour.plot <- ggplot(aes(x=barren, y=groundColour_bb), data=perfal) +
  geom_jitter(shape=16, alpha=0.2, position=position_jitter(0.2))+
  labs(x = expression(paste("barren (C of V)")), y = "Ground Colour bb Freq (Score)", col=expression(paste("barren (C of V)"))) +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x.top = element_line(color = "black"),
        axis.line.x.bottom = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.title.x = element_text(size = 20, family = "Helvetica"),
        axis.text = element_text(size=16, family = "Helvetica"),
        axis.title.y = element_text(size = 20, family = "Helvetica"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
groundColour.plot <- groundColour.plot + geom_smooth(method="lm", fill="lightblue", formula = y ~ x) + stat_cor(method = "pearson", size=6, family = "Helvetica")
groundColour.plot

pdf(file='Scatterplots_barren_03.pdf')
groundColour.plot
dev.off()

########################
