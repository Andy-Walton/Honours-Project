#######################################################################################
######################## (10) GLS Process Part 1 - Linear Models ######################
# Code adapted from Liu et al. 2022

library(AICcmodavg)
library(ape)
library(ggplot2)
library(gridExtra)
library(lme4)
library(nlme)
library(PerformanceAnalytics)
library(raster)
library(RColorBrewer)
library(rworldxtra)
library(rworldmap)
library(scales)
library(viridis)
library(maptools)
library(corrplot)
library(fmsb)
library(showtext)
library(rtf)

# Allows the font Helvetica to be used in the plots
font_add("Helvetica", regular =  "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-01.ttf", italic = "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-LightOblique-06.ttf", bolditalic = "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-BoldOblique-04.ttf", bold = "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-Bold-02.ttf")


showtext_auto()

# Where to output plots
setwd("/Users/Andy/Documents/Uni Work/4th Year/Project/Results/")



setwd("/Users/Andy/Documents/Uni Work/4th Year/Project/Results/")


###################################################
##### Reads in the data and extracts the scores ###
###################################################

perfal <- read.csv(file.choose(), header = T, stringsAsFactors = F)

# Change to where your environment variables are
dat <- perfal[c(12:25)]

# Change to where your phenotype data are
scores <- perfal[c(24:26)]


### Tests for correlations in the variables and outputs them as a csv
# Environmental variables
round(cor(dat),
      digits = 2 # rounded to 2 decimals
)

jnk=round(cor(na.omit(dat)),digits = 2)
write.csv(jnk, file = "Correlation_4v.csv")

# Phenotypes
as.scoresa.frame(na.omit(scores))
cor(na.omit(scores))
round(cor(na.omit(scores)),
      digits = 2 # rounded to 2 decimals
)
jnk=round(cor(na.omit(scores)),digits = 2)
write.csv(jnk, file = "Correlation_traitsv2.csv")




### Plots correlations between the variables

library(PerformanceAnalytics)

chart.Correlation <-
  function (R, histogram = TRUE, method=c("pearson", "kendall", "spearman"), ...)
  { 
    
    x = checkData(R, method="matrix")
    
    if(missing(method)) method=method[1] #only use one
    cormeth <- method
    
    # Published at http://addictedtor.free.fr/graphiques/sources/source_137.R
    panel.cor <- function(x, y, digits=2, prefix="", use="pairwise.complete.obs", method=cormeth, cex.cor, ...)
    {
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r <- cor(x, y, use=use, method=method) # MG: remove abs here
      txt <- format(c(r, 0.123456789), digits=digits)[1]
      txt <- paste(prefix, txt, sep="")
      if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
      
      test <- cor.test(as.numeric(x),as.numeric(y), method=method)
      # add abs here and also include a 30% buffer for small numbers
      text(0.5, 0.5, txt, cex = cex * (abs(r) + .3) / 1.3)
    }
    f <- function(t) {
      dnorm(t, mean=mean(x), sd=sd.xts(x) )
    }
    
    dotargs <- list(...)
    dotargs$method <- NULL
    rm(method)
    
    hist.panel = function (x, ...=NULL ) {
      par(new = TRUE)
      hist(x,
           col = "light gray",
           probability = TRUE,
           axes = FALSE,
           main = "",
           breaks = "FD")
      rug(x)
    }
    
    # Plots the histogram
    if(histogram)
      pairs(x, gap=0, lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=hist.panel)
    else
      pairs(x, gap=0, lower.panel=panel.smooth, upper.panel=panel.cor) 
  }



# Correlations between predictor variables
png(width=1000, height=1000, res=200, family="Helvetica", filename="AbioticCorr.png",pointsize=12)
chart.Correlation(dat)
dev.off()
png(width=1000, height=1000, res=200, family="Helvetica", filename="GeneCorr.png",pointsize=12)

# Correlations between malar stripe scores
chart.Correlation(scores) 

dev.off()

#################################################
####### Checks for spatial autocorrelation ######
#################################################


## Standardizing the variables

# Environmetal variables, change to which ones you are including in these models
perfal$herb.x <- scale(perfal$herb)
perfal$Bio01.x <- scale(perfal$Bio01)
perfal$cult.x <- scale(perfal$cult)
perfal$Bio12.x <- scale(perfal$Bio12)


# Phenotypes
perfal$groundColour_bb.x <- scale(perfal$groundColour_bb)
groundColour_n <- perfal$groundColour_n
groundColour_n

### Plots linear models of pairwise correlation
# Change gene and abiotic data included to desired

png(width=1000, height=1000, res=200, family="Helvetica", filename="LMs1.png",pointsize=12)

my_log <- file("lm.txt")
sink(my_log, append = TRUE, type = "output")

#herb
groundColour.global <- lm(groundColour_bb.x ~ herb.x, data=perfal, weights = groundColour_n)
summary(groundColour.global)


# Check the residuals of these basic models for spatial autocorrelation
library(ape) 
perfal$groundColour.model.residuals <- groundColour.global$residuals
Latitude <- perfal$block_midpoint_lat
Longitude <- perfal$block_midpoint_long
perfal = cbind(perfal, Latitude&Longitude) # combines the model residual for each 
dists <- as.matrix(dist(cbind(perfal$block_midpoint_lat, perfal$block_midpoint_long)))
dists.inv <- (1/(dists + 0.000000000000000001)) # add a small constant so not dividing by zero
diag(dists.inv) <- 0
Moran.I(perfal$groundColour.model.residuals, dists.inv, scaled=T, na.rm=T)


plot(perfal$groundColour_bb.x ~ perfal$herb.x) + abline(groundColour.global)

dev.off()

png(width=1000, height=1000, res=200, family="Helvetica", filename="LMs2.png",pointsize=12)

#Bio01
groundColour.global <- lm(groundColour_bb.x ~ Bio01.x, data=perfal, weights = groundColour_n)
summary(groundColour.global)


perfal$groundColour.model.residuals <- groundColour.global$residuals
Latitude <- perfal$block_midpoint_lat
Longitude <- perfal$block_midpoint_long
perfal = cbind(perfal, Latitude&Longitude) # combine the model residual for each
dists <- as.matrix(dist(cbind(perfal$block_midpoint_lat, perfal$block_midpoint_long)))
dists.inv <- (1/(dists + 0.000000000000000001)) # add a small constant so not dividing by zero
diag(dists.inv) <- 0
Moran.I(perfal$groundColour.model.residuals, dists.inv, scaled=T, na.rm=T)

plot(perfal$groundColour_bb.x ~ perfal$Bio01.x) + abline(groundColour.global)

dev.off()

png(width=1000, height=1000, res=200, family="Helvetica", filename="LMs3.png",pointsize=12)

#cult
groundColour.global <- lm(groundColour_bb.x ~ cult.x, data=perfal, weights = groundColour_n)
summary(groundColour.global)

perfal$groundColour.model.residuals <- groundColour.global$residuals
Latitude <- perfal$block_midpoint_lat
Longitude <- perfal$block_midpoint_long
perfal = cbind(perfal, Latitude&Longitude) # combine the model residual for each 
dists <- as.matrix(dist(cbind(perfal$block_midpoint_lat, perfal$block_midpoint_long)))
dists.inv <- (1/(dists + 0.000000000000000001)) # add a small constant so not dividing by zero
diag(dists.inv) <- 0
Moran.I(perfal$groundColour.model.residuals, dists.inv, scaled=T, na.rm=T)

plot(perfal$groundColour_bb.x ~ perfal$cult.x) + abline(groundColour.global)

dev.off()

png(width=1000, height=1000, res=200, family="Helvetica", filename="LMs4.png",pointsize=12)


#Bio12
groundColour.global <- lm(groundColour_bb.x ~ Bio12.x, data=perfal, weights = groundColour_n)
summary(groundColour.global)


perfal$groundColour.model.residuals <- groundColour.global$residuals
Latitude <- perfal$block_midpoint_lat
Longitude <- perfal$block_midpoint_long
perfal = cbind(perfal, Latitude&Longitude) # combine the model residual for each 
dists <- as.matrix(dist(cbind(perfal$block_midpoint_lat, perfal$block_midpoint_long)))
dists.inv <- (1/(dists + 0.000000000000000001)) # add a small constant so not dividing by zero
diag(dists.inv) <- 0
Moran.I(perfal$groundColour.model.residuals, dists.inv, scaled=T, na.rm=T)


plot(perfal$groundColour_bb.x ~ perfal$cult.x) + abline(groundColour.global)

dev.off()

png(width=1000, height=1000, res=200, family="Helvetica", filename="LMs4.png",pointsize=12)


#herb+Bio01+cult+Bio12
groundColour.global <- lm(groundColour_bb.x ~ herb.x + Bio01.x + cult.x + Bio12.x, data=perfal, weights = groundColour_n)
summary(groundColour.global)

perfal$groundColour.model.residuals <- groundColour.global$residuals
Latitude <- perfal$block_midpoint_lat
Longitude <- perfal$block_midpoint_long
perfal = cbind(perfal, Latitude&Longitude) # combine the model residual for each 
dists <- as.matrix(dist(cbind(perfal$block_midpoint_lat, perfal$block_midpoint_long)))
dists.inv <- (1/(dists + 0.000000000000000001)) # add a small constant so not dividing by zero
diag(dists.inv) <- 0
Moran.I(perfal$groundColour.model.residuals, dists.inv, scaled=T, na.rm=T)

dev.off()

closeAllConnections()
