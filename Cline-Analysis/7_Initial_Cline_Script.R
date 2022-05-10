#######################################################################################
########################### (7) Initial Cline Analysis ################################
# Code adapted from Liu et al. 2022

library(ggplot2)
library(hzar)
library(showtext)

# Allows the font Helvetica to be used in the plots
font_add("Helvetica", regular =  "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-01.ttf", italic = "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-LightOblique-06.ttf", bolditalic = "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-BoldOblique-04.ttf", bold = "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-Bold-02.ttf")

showtext_auto()

setwd("/Users/Andy/Documents/Uni Work/4th Year/Project/Data/")
file_dir <- "/Users/Andy/Documents/Uni Work/4th Year/Project/Data/"


### hindwingWhite transect1_2_1


# Input file for each transect
file_prefix <- "FILE NAME.transect1"

transect_data <- read.csv(paste0(file_prefix,".csv"), header = T, as.is=T)



hwdata <- hzar.doMolecularData1DPops(transect_data$transect_pos,
                                     transect_data$hindwingWhite_mean,
                                     transect_data$hindwingWhite_n)

hzar.doMolecularData1DPops(transect_data$transect_pos)

hzar.plot.obsData(hwdata)

gwmodel <- hzar.makeCline1DFreq(hwdata, scaling="fixed",tails="none")

gwmodel <- hzar.model.addBoxReq(gwmodel,-100,7000)

gwmodelFitR <- hzar.first.fitRequest.old.ML(model=gwmodel, hwdata, verbose=FALSE)

gwmodelFitR$mcmcParam$chainLength <- 5e4

gwmodelFitR$mcmcParam$burnin <- 1e3

gwmodelFit <- hzar.doFit(gwmodelFitR)

plot(hzar.mcmc.bindLL(gwmodelFit))

gwmodelData <- hzar.dataGroup.add(gwmodelFit)

print(hzar.getLLCutParam(gwmodelData,c("center","width")))

hzar.plot.fzCline(gwmodelData, pch=NA)
hzar.plot.obsData(gwmodelData, pch=19, cex=0.1 + log10(gwmodelData$obsData$frame$n), add=T)
abline(v=hzar.getLLCutParam(gwmodelData,"center"))

png("hindwingWhite_transect1CentralAfrica_2_1c.png", width = 2000, height=1000, res=300, bg=NA)
par(mar=c(4,4,1,1))

hzar.plot.fzCline(gwmodelData, pch=NA)
hzar.plot.obsData(gwmodelData, pch=19, cex=0.1 + log10(gwmodelData$obsData$frame$n), add=T)
abline(v=hzar.getLLCutParam(gwmodelData,"center"))


dev.off()