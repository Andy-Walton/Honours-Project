#######################################################################################
############################## (8) Full cline analysis ################################
# Code adapted from Liu et al. 2022

# Do this once for all the male transect files and once for all the female transect files

library(hzar)
library(doMC)
library(png)
library(rtf)
library(showtext)

font_add("Helvetica", regular =  "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-01.ttf", italic = "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-LightOblique-06.ttf", bolditalic = "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-BoldOblique-04.ttf", bold = "/Users/Andy/Documents/Uni Work/4th Year/Project/Thesis Writeup/Final Plots/Font/Helvetica-Bold-02.ttf")

showtext_auto()

# Outputs plots as png files in R workspace with the below parameters
png(width=900, height=900, res=200, family="Helvetica", filename="Fem_Tran1_For%d.png",pointsize=8)


# Default chain length setting in HZAR
chainLength=1e5;
# chainLength=5e4;                       

# Sets each model to run off a separate seed
mainSeed=
  list(A=c(596,528,124,978,544,99),
       B=c(528,124,978,544,99,596),
       C=c(124,978,544,99,596,528),
       D=c(596,528,124,978,544,99),
       E=c(528,124,978,544,99,596),
       F=c(124,978,544,99,596,528))


if(require(doMC)){
  registerDoMC()
} else {
  registerDoSEQ();
}


if(length(apropos("^mkn$",ignore.case=FALSE)) == 0 ||
   !is.list(mkn) ) mkn <- list()
# Space to hold allele data
mkn$AdaA <- list();
# Space to hold observed data
mkn$AdaA$obs <- list();
# Space to hold models to fit
mkn$AdaA$models <- list();
# Space to holdcompiled fit requests
mkn$AdaA$fitRs <- list();
# Space to hold output data chains
mkn$AdaA$runs <- list();
# Space to hold analysed data
mkn$AdaA$analysis <- list();

# Input transect file
setwd("/Users/Andy/Documents/Uni Work/4th Year/Project/Data/")
file_dir <- "/Users/Andy/Documents/Uni Work/4th Year/Project/Data/"
file_prefix <- "FILE NAME.frequency_processed.means.processed.4DegreeBlocks.transect1"

transect_data_F <- read.csv(paste0(file_prefix,".csv"), as.is=T);

# Defines locus interested in, change to ground colour or hindwing band if needed for that transect
mkn$AdaA$obs <- hzar.doMolecularData1DPops(transect_data_F$transect_pos,
                                           transect_data_F$forewingBand_mean,
                                           transect_data_F$forewingBand_n);

# Plots graph of observed data
hzar.plot.obsData(mkn$AdaA$obs);

mkn.loadAdaAmodel <- function(scaling,tails,
                              id=paste(scaling,tails,sep="."))
  mkn$AdaA$models[[id]] <<- hzar.makeCline1DFreq(mkn$AdaA$obs, scaling, tails)

# Defines models run and their parameters
mkn.loadAdaAmodel("none","none","model01");
mkn.loadAdaAmodel("none" ,"both","model02");
mkn.loadAdaAmodel("free","none","model03");
mkn.loadAdaAmodel("free" ,"both","model04");
mkn.loadAdaAmodel("fixed","none","model05");
mkn.loadAdaAmodel("fixed" ,"both","model06");

## Prints the default settings
print(mkn$AdaA$models)

# Define start and end point of transect (change second number here to length of transect)
mkn$AdaA$models <- sapply(mkn$AdaA$models,
                          hzar.model.addBoxReq,
                          0, 5530,
                          simplify=FALSE)


# Prints the updated settings
print(mkn$AdaA$models)

# Compile each of the models to prepare for fitting
mkn$AdaA$fitRs$init <- sapply(mkn$AdaA$models,
                              hzar.first.fitRequest.old.ML,
                              obsData=mkn$AdaA$obs,
                              verbose=FALSE,
                              simplify=FALSE)


mkn$AdaA$fitRs$init$model01$mcmcParam$chainLength <-
  chainLength;                          #1e5
mkn$AdaA$fitRs$init$model01$mcmcParam$burnin <-
  chainLength %/% 10;                   #1e4
mkn$AdaA$fitRs$init$model01$mcmcParam$seed[[1]] <-
  mainSeed$A

mkn$AdaA$fitRs$init$model02$mcmcParam$chainLength <-
  chainLength;                          #1e5
mkn$AdaA$fitRs$init$model02$mcmcParam$burnin <-
  chainLength %/% 10;                   #1e4
mkn$AdaA$fitRs$init$model02$mcmcParam$seed[[1]] <-
  mainSeed$B 

mkn$AdaA$fitRs$init$model03$mcmcParam$chainLength <-
  chainLength;                          #1e5
mkn$AdaA$fitRs$init$model03$mcmcParam$burnin <-
  chainLength %/% 10;                   #1e4
mkn$AdaA$fitRs$init$model03$mcmcParam$seed[[1]] <-
  mainSeed$C 

mkn$AdaA$fitRs$init$model04$mcmcParam$chainLength <-
  chainLength;                          #1e5
mkn$AdaA$fitRs$init$model04$mcmcParam$burnin <-
  chainLength %/% 10;                   #1e4
mkn$AdaA$fitRs$init$model04$mcmcParam$seed[[1]] <-
  mainSeed$D

mkn$AdaA$fitRs$init$model05$mcmcParam$chainLength <-
  chainLength;                          #1e5
mkn$AdaA$fitRs$init$model05$mcmcParam$burnin <-
  chainLength %/% 10;                   #1e4
mkn$AdaA$fitRs$init$model05$mcmcParam$seed[[1]] <-
  mainSeed$E 

mkn$AdaA$fitRs$init$model06$mcmcParam$chainLength <-
  chainLength;                          #1e5
mkn$AdaA$fitRs$init$model06$mcmcParam$burnin <-
  chainLength %/% 10;                   #1e4
mkn$AdaA$fitRs$init$model06$mcmcParam$seed[[1]] <-
  mainSeed$F   

# Prints the fit request settings
print(mkn$AdaA$fitRs$init)


# Run each of the models for an initial chain and plots the trace
mkn$AdaA$runs$init <- list()

mkn$AdaA$runs$init$model01 <-
  hzar.doFit(mkn$AdaA$fitRs$init$model01)

plot(hzar.mcmc.bindLL(mkn$AdaA$runs$init$model01))


mkn$AdaA$runs$init$model02 <-
  hzar.doFit(mkn$AdaA$fitRs$init$model02)

plot(hzar.mcmc.bindLL(mkn$AdaA$runs$init$model02))



mkn$AdaA$runs$init$model03 <-
  hzar.doFit(mkn$AdaA$fitRs$init$model03)

plot(hzar.mcmc.bindLL(mkn$AdaA$runs$init$model03))



mkn$AdaA$runs$init$model04 <-
  hzar.doFit(mkn$AdaA$fitRs$init$model04)

plot(hzar.mcmc.bindLL(mkn$AdaA$runs$init$model04))



mkn$AdaA$runs$init$model05 <-
  hzar.doFit(mkn$AdaA$fitRs$init$model05)

plot(hzar.mcmc.bindLL(mkn$AdaA$runs$init$model05))



mkn$AdaA$runs$init$model06 <-
  hzar.doFit(mkn$AdaA$fitRs$init$model06)

plot(hzar.mcmc.bindLL(mkn$AdaA$runs$init$model06))



# Compile a new set of fit requests using the initial chains 
mkn$AdaA$fitRs$chains <-
  lapply(mkn$AdaA$runs$init,
         hzar.next.fitRequest)

# Replicate each fit request 3 times, keeping the original seeds while switching to a new seed channel.
mkn$AdaA$fitRs$chains <-
  hzar.multiFitRequest(mkn$AdaA$fitRs$chains,
                       each=3,
                       baseSeed=NULL)


## Randomize the initial value for each fit, using the runif() function to generate the random values


mkn$AdaA$fitRs$chains[[1]]$modelParam$init["center"]= 4321.0792
mkn$AdaA$fitRs$chains[[2]]$modelParam$init["center"]= 660.5222
mkn$AdaA$fitRs$chains[[3]]$modelParam$init["center"]= 855.9821
mkn$AdaA$fitRs$chains[[4]]$modelParam$init["center"]= 4740.9078
mkn$AdaA$fitRs$chains[[5]]$modelParam$init["center"]= 3509.5428
mkn$AdaA$fitRs$chains[[6]]$modelParam$init["center"]= 337.7526
mkn$AdaA$fitRs$chains[[7]]$modelParam$init["center"]= 1608.5543
mkn$AdaA$fitRs$chains[[8]]$modelParam$init["center"]= 4128.5353
mkn$AdaA$fitRs$chains[[9]]$modelParam$init["center"]= 3867.6668
mkn$AdaA$fitRs$chains[[10]]$modelParam$init["center"]= 1692.2470
mkn$AdaA$fitRs$chains[[11]]$modelParam$init["center"]= 218.9130
mkn$AdaA$fitRs$chains[[12]]$modelParam$init["center"]= 2804.1826
mkn$AdaA$fitRs$chains[[13]]$modelParam$init["center"]= 4425.7489
mkn$AdaA$fitRs$chains[[14]]$modelParam$init["center"]= 2120.3820
mkn$AdaA$fitRs$chains[[15]]$modelParam$init["center"]= 2605.2191
mkn$AdaA$fitRs$chains[[16]]$modelParam$init["center"]= 3942.9046
mkn$AdaA$fitRs$chains[[17]]$modelParam$init["center"]= 163.3335
mkn$AdaA$fitRs$chains[[18]]$modelParam$init["center"]= 1477.0913


mkn$AdaA$fitRs$chains[[1]]$modelParam$init["width"]= 2065.11330
mkn$AdaA$fitRs$chains[[2]]$modelParam$init["width"]= 2053.21684
mkn$AdaA$fitRs$chains[[3]]$modelParam$init["width"]= 2685.68547
mkn$AdaA$fitRs$chains[[4]]$modelParam$init["width"]= 2669.97817
mkn$AdaA$fitRs$chains[[5]]$modelParam$init["width"]= 1293.35245
mkn$AdaA$fitRs$chains[[6]]$modelParam$init["width"]= 2005.43569
mkn$AdaA$fitRs$chains[[7]]$modelParam$init["width"]= 3663.01978
mkn$AdaA$fitRs$chains[[8]]$modelParam$init["width"]= 2313.32831
mkn$AdaA$fitRs$chains[[9]]$modelParam$init["width"]= 1559.05990
mkn$AdaA$fitRs$chains[[10]]$modelParam$init["width"]= 497.22087
mkn$AdaA$fitRs$chains[[11]]$modelParam$init["width"]= 3901.50765
mkn$AdaA$fitRs$chains[[12]]$modelParam$init["width"]= 2757.94308
mkn$AdaA$fitRs$chains[[13]]$modelParam$init["width"]= 4620.24683
mkn$AdaA$fitRs$chains[[14]]$modelParam$init["width"]= 74.55348
mkn$AdaA$fitRs$chains[[15]]$modelParam$init["width"]= 2386.26991
mkn$AdaA$fitRs$chains[[16]]$modelParam$init["width"]= 161.75335
mkn$AdaA$fitRs$chains[[17]]$modelParam$init["width"]= 3532.25941
mkn$AdaA$fitRs$chains[[18]]$modelParam$init["width"]= 2458.46647



# Runs a chain of 3 runs for every fit request
mkn$AdaA$runs$chains <-  hzar.doChain.multi(mkn$AdaA$fitRs$chains,
                                            doPar=TRUE,
                                            inOrder=FALSE,
                                            count=3)

# Did model x converge?
summary(do.call(mcmc.list,
                lapply(mkn$AdaA$runs$chains[1:3],
                       function(x) hzar.mcmc.bindLL(x[[3]]) )) )
summary(do.call(mcmc.list,
                lapply(mkn$AdaA$runs$chains[4:6],
                       function(x) hzar.mcmc.bindLL(x[[3]]) )) )
summary(do.call(mcmc.list,
                lapply(mkn$AdaA$runs$chains[7:9],
                       function(x) hzar.mcmc.bindLL(x[[3]]) )) )
summary(do.call(mcmc.list,
                lapply(mkn$AdaA$runs$chains[10:12],
                       function(x) hzar.mcmc.bindLL(x[[3]]) )) )
summary(do.call(mcmc.list,
                lapply(mkn$AdaA$runs$chains[13:15],
                       function(x) hzar.mcmc.bindLL(x[[3]]) )) )
summary(do.call(mcmc.list,
                lapply(mkn$AdaA$runs$chains[16:18],
                       function(x) hzar.mcmc.bindLL(x[[3]]) )) )


# Create a model data group for the null model to include in analysis.
mkn$AdaA$analysis$initDGs <- list(
  nullModel =  hzar.dataGroup.null(mkn$AdaA$obs))

# Create a model data group (hzar.dataGroup object) for each model from the initial runs.
mkn$AdaA$analysis$initDGs$model01 <-
  hzar.dataGroup.add(mkn$AdaA$runs$init$model01)
mkn$AdaA$analysis$initDGs$model02 <-
  hzar.dataGroup.add(mkn$AdaA$runs$init$model02)
mkn$AdaA$analysis$initDGs$model03 <-
  hzar.dataGroup.add(mkn$AdaA$runs$init$model03)
mkn$AdaA$analysis$initDGs$model04 <-
  hzar.dataGroup.add(mkn$AdaA$runs$init$model04)
mkn$AdaA$analysis$initDGs$model05 <-
  hzar.dataGroup.add(mkn$AdaA$runs$init$model05)
mkn$AdaA$analysis$initDGs$model06 <-
  hzar.dataGroup.add(mkn$AdaA$runs$init$model06)


# Create a hzar.obsDataGroup object from the four hzar.dataGroup just created, copying the naming scheme (nullModel, modelI, modelII, modelIII).
mkn$AdaA$analysis$oDG <-
  hzar.make.obsDataGroup(mkn$AdaA$analysis$initDGs)
mkn$AdaA$analysis$oDG <-
  hzar.copyModelLabels(mkn$AdaA$analysis$initDGs,
                       mkn$AdaA$analysis$oDG)

# Convert all 27 runs to hzar.dataGroup objects, adding them to the hzar.obsDataGroup object.
mkn$AdaA$analysis$oDG <-
  hzar.make.obsDataGroup(lapply(mkn$AdaA$runs$chains,
                                hzar.dataGroup.add),
                         mkn$AdaA$analysis$oDG);


## Check to make sure that there are only four hzar.dataGroup objects named nullModel, modelI, modelII, and modelIII in the hzar.obsDataGroup object.
print(summary(mkn$AdaA$analysis$oDG$data.groups))

## Compare the 3 cline models to the null model graphically
hzar.plot.cline(mkn$AdaA$analysis$oDG);


## Find best-fit model based on the AICc scores
print(mkn$AdaA$analysis$AICcTable <-
        hzar.AICc.hzar.obsDataGroup(mkn$AdaA$analysis$oDG));

Fem_Tran1_For_AIC <- mkn$AdaA$analysis$AICcTable <-
  hzar.AICc.hzar.obsDataGroup(mkn$AdaA$analysis$oDG)

# Outputs AICc scores to txt file
write.table(Fem_Tran1_For_AIC, 'Fem_Tran1_For_AIC')

# Prints out the model with the minimum AICc score "best-fit model"
print(mkn$AdaA$analysis$model.name <-
        rownames(mkn$AdaA$analysis$AICcTable
        )[[ which.min(mkn$AdaA$analysis$AICcTable$AICc )]])

# Output best-fit model to txt file
Fem_Tran1_For_Model_Chosen <- mkn$AdaA$analysis$model.name <-
  rownames(mkn$AdaA$analysis$AICcTable
  )[[ which.min(mkn$AdaA$analysis$AICcTable$AICc )]]

write.table(Fem_Tran1_For_Model_Chosen, 'Fem_Tran1_For_Model_Chosen')


# Extracts the hzar.dataGroup object for best-fit model
mkn$AdaA$analysis$model.selected <-
  mkn$AdaA$analysis$oDG$data.groups[[mkn$AdaA$analysis$model.name]]


# Look at the variation in parameters for best-fit model
print(hzar.getLLCutParam(mkn$AdaA$analysis$model.selected,
                         names(mkn$AdaA$analysis$model.selected$data.param)));

Fem_Tran1_Forr_CIs <- hzar.getLLCutParam(mkn$AdaA$analysis$model.selected,
                                         names(mkn$AdaA$analysis$model.selected$data.param))

# Outputs variation in parameters for best-fit model
write.table(Fem_Tran1_For_CIs, 'Fem_Tran1_For_CIs')


# Prints the maximum likelihood cline for best-fit model
print(hzar.get.ML.cline(mkn$AdaA$analysis$model.selected))

Fem_Tran1_For_CentreWidth <- hzar.get.ML.cline(mkn$AdaA$analysis$model.selected)

# Outputs parameters for best-fit model
write.table(Fem_Tran1_For_CentreWidth$param.free, 'Fem_Tran1_For_CentreWidth')


# Plots the maximum likelihood cline for best-fit model
hzar.plot.cline(mkn$AdaA$analysis$model.selected);


# Plots the 95% credible cline region for best-fit model
hzar.plot.fzCline(mkn$AdaA$analysis$model.selected, pch=NA);
hzar.plot.obsData(mkn$AdaA$analysis$model.selected, pch=19, cex=0.1 + log10(mkn$AdaA$analysis$model.selected$obsData$frame$n), add=T);
abline(v=hzar.getLLCutParam(mkn$AdaA$analysis$model.selected,"center"));


# Plots the 95% credible cline region for the best-fit model
hzar.plot.fzCline(mkn$AdaA$analysis$model.selected);
abline(v=hzar.getLLCutParam(mkn$AdaA$analysis$model.selected,"center"))


dev.off()