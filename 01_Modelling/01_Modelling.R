# Script for running maxent models on historic climate data, and saving 
# modelling stats
# Updated October 26 2015

# Clear workspace
rm(list=ls())
start=Sys.time()

#-----------------#
# Set directories #
#-----------------#
Computer <- "HP"#---

if(Computer == "EOS") {
  wdir <- 'bien/Naia/'
  cdir <- paste0(wdir, 'BCM/CA_2014/Summary/')
} else if (Computer == "HP") {
  wdir <- 'C:/Users/Naia Morueta Holme/Documents/Documents_share/Projects/101_TBC3_modelling/Lead-trail_R-project/'
  cdir <- 'D:/BCM/CA_2014/Summary/HST/Normals_30years/'
  spdir <- 'D:/Phylo_modelling/Data/Species/Processed2/'
}

spdir2 <- paste0(wdir,'Data/Species/')
bgfile <- 'D:/Phylo_modelling/Data/Species/Processed/10000_CA_plants_bg.rdata'
odir <- paste0(wdir, 'ModelResults/Maxent/V4/')



#----------------#
# Load libraries #
#----------------#
options(java.parameters = "-Xmx1g" )
require(dismo)
require(rJava)
require(raster) 

#-------------------------#
# Parameters for modeling #
#-------------------------#
# Set the environmental file names (right now set to California BCM layers)
env.files <- list.files(path=cdir, pattern='.Rdata', full.names=FALSE)
climnames <- sort(c("cwd","djf","jja","ppt")) #use sort to ensure correct names used for predictors
mxModelType <- paste(climnames,collapse="-")

# Arguments for maxent models
mxArgs = c("-a", "-z", "outputformat=raw", "maximumbackground=30000", 
           "nothreshold", "nohinge")

# What species?

allSpecies <- sort(unique(read.csv(paste0(spdir2, 'Species_CLN_matching_v2.csv'), as.is=T)[,'Scientific_name']))
#allSpecies <- c("Quercus garryana", "Quercus douglasii")

#####################
# RUN MAXENT MODELS #
#####################
files <- env.files[which(substr(env.files,9,11)%in%climnames)]
predictors <- stack(lapply(files,function(x) readRDS(paste(cdir,x,sep=""))))
names(predictors) = climnames

orig.project <- '+proj=longlat +ellps=WGS84'
bg <- readRDS(bgfile)

#library(doParallel)
#cl <- makeCluster(7)
#registerDoParallel(cl)

# Run full models
#results <- foreach(i=1:length(allSpecies)) {
for (i in 1:length(allSpecies)) {
  mySpecies <- allSpecies[i]
  
  writeLines(mySpecies)
  Sys.time()
  
  # Read in species occurrence data
  pres <- readRDS(paste(spdir, mySpecies, ".rdata", sep=""))
  coordinates(pres) <- ~longitude + latitude
  projection(pres) <- CRS(orig.project)
  occur <- spTransform(pres, projection(predictors))
  
  occur <- occur[!is.na(extract(predictors[[1]], occur)),]
  if(nrow(occur)==0){
    writeLines("           no occurrences overlapping climate data")
    next()}
  
  
  # Directory to write files to
  mx.dir = paste(odir, mySpecies, "/", mxModelType, "/fullmodel", sep="")
  if(file.exists(mx.dir) == F) {dir.create(mx.dir, recursive=T)} 
    
  # Run the model!
  mx <- try(maxent(predictors, p=occur, a=bg ,progress='text', path=mx.dir, args=mxArgs))
  saveRDS(mx, file = paste(mx.dir, 'ModelObject.rdata', sep='/'))
  writeLines('Full model done... running evaluation')
  
  # Rerun models with 80% random training data and evaluate on remaining 20%
  # 20% sample for testing
  fold <- kfold(occur, k=5)
  occtest <- occur[fold ==1,]
  occtrain <- occur[fold !=1,]
  
  # Directory to write files to
  mx.dir = paste(odir, mySpecies, "/", mxModelType, "/randomtesting", sep="")
  if(file.exists(mx.dir) == F) {dir.create(mx.dir, recursive=T)} 
  
  # Run the model!
  mx <- maxent(predictors, occtrain, progress='text', path=mx.dir, args=mxArgs)
  
  saveRDS(mx, file = paste(mx.dir, 'ModelObject.rdata', sep='/'))
  
  # Evaluate the model
  # background data
  e1 <- evaluate(mx, p=occtest, a=bg, x=predictors)
  
  saveRDS(e1, file = paste(mx.dir, 'EvaluateObject.rdata', sep='/'))

}

print(Sys.time()-start)

