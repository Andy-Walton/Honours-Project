#######################################################################################
######################## (10) GLS Process Part 2 - Run GLS Models #####################
# Code adapted from Liu et al. 2022

library(nlme)
library(lme4)
library(AICcmodavg)
library(ape)
library(PerformanceAnalytics)
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
library(MuMIn)
library(ggpubr)
library(showtext)
library(rtf)

# Allows the font Helvetica to be used in the plots
font_add("Helvetica", regular =  "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-01.ttf", italic = "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-LightOblique-06.ttf", bolditalic = "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-BoldOblique-04.ttf", bold = "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-Bold-02.ttf")

showtext_auto()

# Where to output plots
setwd("/Users/Andy/Documents/Uni Work/4th Year/Project/Results/GLS/Ground Colour/14.1/")

perfal <- read.csv(file.choose(), header = T, stringsAsFactors = F)


# Set the models you want to run, here ground colour and Bio01/Bio12/herb/cult's combinations are being run. If you have less abiotic variables, less models will be needed
my_log <- file("GLS.txt")
sink(my_log, append = TRUE, type = "output")

Cand.mod.groundColour_bb <- list()

Cand.mod.groundColour_bb[[1]] <- gls(groundColour_bb ~ 1, method = "ML", data=perfal,
                                     correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[1]])

Cand.mod.groundColour_bb[[2]] <- gls(groundColour_bb ~ Bio01.x, method = "ML", data=perfal,
                                     correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[2]])
Cand.mod.groundColour_bb[[3]] <- gls(groundColour_bb ~ Bio12.x, method = "ML", data=perfal,
                                     correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[3]])
Cand.mod.groundColour_bb[[4]] <- gls(groundColour_bb ~ herb.x, method = "ML", data=perfal,
                                     correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[4]])
Cand.mod.groundColour_bb[[5]] <- gls(groundColour_bb ~ cult.x, method = "ML", data=perfal,
                                     correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[5]])
Cand.mod.groundColour_bb[[6]] <- gls(groundColour_bb ~ Bio01.x + Bio12.x, method = "ML", data=perfal,
                                     correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[6]])
Cand.mod.groundColour_bb[[7]] <- gls(groundColour_bb ~ Bio01.x + herb.x, method = "ML", data=perfal,
                                     correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[7]])
Cand.mod.groundColour_bb[[8]] <- gls(groundColour_bb ~ Bio01.x + cult.x, method = "ML", data=perfal,
                                     correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[8]])
Cand.mod.groundColour_bb[[9]] <- gls(groundColour_bb ~ Bio12.x + herb.x, method = "ML", data=perfal,
                                     correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[9]])
Cand.mod.groundColour_bb[[10]] <- gls(groundColour_bb ~ Bio12.x + cult.x, method = "ML", data=perfal,
                                      correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[10]])
Cand.mod.groundColour_bb[[11]] <- gls(groundColour_bb ~ herb.x + cult.x, method = "ML", data=perfal,
                                      correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[11]])
Cand.mod.groundColour_bb[[12]] <- gls(groundColour_bb ~ Bio01.x + Bio12.x + herb.x, method = "ML", data=perfal,
                                      correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[12]])
Cand.mod.groundColour_bb[[13]] <- gls(groundColour_bb ~ Bio01.x + Bio12.x + cult.x, method = "ML", data=perfal,
                                      correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[13]])
Cand.mod.groundColour_bb[[14]] <- gls(groundColour_bb ~ Bio01.x + herb.x + cult.x, method = "ML", data=perfal,
                                      correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[14]])
Cand.mod.groundColour_bb[[15]] <- gls(groundColour_bb ~ Bio12.x + herb.x + cult.x, method = "ML", data=perfal,
                                      correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[15]])
Cand.mod.groundColour_bb[[16]] <- gls(groundColour_bb ~ Bio01.x + Bio12.x + herb.x + cult.x, method = "ML", data=perfal,
                                      correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[16]])


closeAllConnections()


### Outputs the fit of each model
# Rename these if your variables, number of your variables are different

Model.names <- c("Null", "Bio01.x", "Bio12.x", "herb.x", "cult.x", "Bio01.x + Bio12.x", "Bio01.x + herb.x", "Bio01.x + cult.x", "Bio12.x + herb.x", "Bio12.x + cult.x", "herb.x + cult.x", "Bio01.x + Bio12.x + herb.x", "Bio01.x + Bio12.x + cult.x", "Bio01.x + herb.x + cult.x", "Bio12.x + herb.x + cult.x", "Bio01.x + Bio12.x + herb.x + cult.x")

aictabSUM <- aictab(cand.set = Cand.mod.groundColour_bb, modnames = Model.names)

write.csv(aictabSUM, file = "AIC02_weight.csv")





### Creates Forrest plots for the best-fit model

# Extract model-averaged estimates and confidence intervals
#Top seven variables

groundColour.Bio01 <- modavg(cand.set = Cand.mod.groundColour_bb, modnames = Model.names, parm = "Bio01.x", conf.level = 0.95)
# If it does not work, to annotate the upper "Model-averaged estimates"

groundColour.Bio12 <- modavg(cand.set = Cand.mod.groundColour_bb, modnames = Model.names, parm = "Bio12.x", conf.level = 0.95)

groundColour.herb <- modavg(cand.set = Cand.mod.groundColour_bb, modnames = Model.names, parm = "herb.x", conf.level = 0.95)

groundColour.cult <- modavg(cand.set = Cand.mod.groundColour_bb, modnames = Model.names, parm = "cult.x", conf.level = 0.95)


groundColour.means <- c(groundColour.Bio01$Mod.avg.beta, groundColour.Bio12$Mod.avg.beta, groundColour.herb$Mod.avg.beta, groundColour.cult$Mod.avg.beta)

groundColour.mins <- c(groundColour.Bio01$Lower.CL, groundColour.Bio12$Lower.CL, groundColour.herb$Lower.CL, groundColour.cult$Lower.CL)

groundColour.maxes <- c(groundColour.Bio01$Upper.CL, groundColour.Bio12$Upper.CL, groundColour.herb$Upper.CL, groundColour.cult$Upper.CL)

predictors <- c("Bio01", "Bio12", "herb", "cult")
var.name <- c("groundColour", "groundColour", "groundColour", "groundColour")

groundColour.avgs <- data.frame(Variable = var.name, Mean=groundColour.means, Upper=groundColour.maxes, Lower=groundColour.mins,
                                Predictor=predictors)

perfal.avgs <- rbind(groundColour.avgs)


perfal.avgs$Variable <- factor(perfal.avgs$Variable, levels = c("groundColour"))

# Create forest plot - first four variables
forest.plot.main=ggplot(perfal.avgs, aes(y = Predictor, x = Mean, xmin=Lower, xmax=Upper))+
  geom_point(color = 'black', size=4)+
  geom_errorbarh(aes(xmin=Lower, xmax=Upper, height=0.2))+
  scale_x_continuous(limits=c(-0.6,0.6), name='Estimate')+
  ylab('')+
  geom_vline(xintercept=0, color='black', linetype='dashed')+
  facet_wrap(~Variable,strip.position="right",nrow=4,scales = "free_y") +
  theme()+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.line=element_line(),
        text=element_text(family = "Helvetica",size=24,face="bold"),
        legend.position='none')
axis.ticks.y=element_blank()
axis.text.x=element_text(family = "Helvetica",face="bold")
axis.title=element_text(family = "Helvetica", size=24,face="bold")
strip.text.y = element_text(hjust=0,vjust = 1,angle=270, family = "Helvatica",face="bold")
axis.text.y=element_text(hjust=0,vjust = 1, family = "Helvetica", angle=0)
forest.plot.main

pdf(file='forestplot_seven_v_06.pdf')
forest.plot.main
dev.off()


# Create forest plot
forest.plot.main=ggplot(perfal.avgs, aes(y = Predictor, x = Mean, xmin=Lower, xmax=Upper))+
  geom_point(color = 'black', size=4)+
  geom_errorbarh(aes(xmin=Lower, xmax=Upper, height=0.2))+
  scale_x_continuous(limits=c(-0.5,0.5), name='Estimate')+
  ylab('')+
  geom_vline(xintercept=0, color='black', linetype='dashed')+
  facet_wrap(~Variable,strip.position="right",nrow=4,scales = "free_y") +
  theme()+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.line=element_line(),
        text=element_text(family = "Helvetica",size=24,face="bold"),
        legend.position='none')
axis.ticks.y=element_blank()
axis.text.x=element_text(family = "Helvetica",face="bold")
axis.title=element_text(family = "Helvetica", size=24,face="bold")
strip.text.y = element_text(hjust=0,vjust = 1,angle=270, family = "Helvatica",face="bold")
axis.text.y=element_text(hjust=0,vjust = 1, family = "Helvetica", angle=0)
forest.plot.main

pdf(file='forestplot_seven_v_05.pdf')
forest.plot.main
dev.off()


# Create forest plot
forest.plot.main=ggplot(perfal.avgs, aes(y = Predictor, x = Mean, xmin=Lower, xmax=Upper))+
  geom_point(color = 'black', size=4)+
  geom_errorbarh(aes(xmin=Lower, xmax=Upper, height=0.2))+
  scale_x_continuous(limits=c(-0.4,0.4), name='Estimate')+
  ylab('')+
  geom_vline(xintercept=0, color='black', linetype='dashed')+
  facet_wrap(~Variable,strip.position="right",nrow=4,scales = "free_y") +
  theme()+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.line=element_line(),
        text=element_text(family = "Helvetica",size=24,face="bold"),
        legend.position='none')
axis.ticks.y=element_blank()
axis.text.x=element_text(family = "Helvetica",face="bold")
axis.title=element_text(family = "Helvetica", size=24,face="bold")
strip.text.y = element_text(hjust=0,vjust = 1,angle=270, family = "Helvatica",face="bold")
axis.text.y=element_text(hjust=0,vjust = 1, family = "Helvetica", angle=0)
forest.plot.main

pdf(file='forestplot_seven_v_04.pdf')
forest.plot.main
dev.off()