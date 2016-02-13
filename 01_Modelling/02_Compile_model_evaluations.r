# Compile the results of model evaluations

# Clear workspace
rm(list=ls())

#-----------------#
# Set directories #
#-----------------#
Computer <- "HP"
Group <- 'Species'

if(Computer == "EOS") {
  wdir <- 'bien/Naia/'
} else if (Computer == "HP") {
  wdir <- 'C:/Users/Naia Morueta Holme/Documents/Documents_share/Projects/101_TBC3_modelling/Lead-trail_R-project/' 
  spdir2 <- paste0(wdir,'Data/Species/')
}

if(Group == 'Species') {
  idir <- paste0(wdir, 'ModelResults/Maxent/V4/')
  ofile <- 'Evaluation_results_spp_v4.csv'
  # What species?
  allSpecies <- sort(unique(read.csv(paste0(spdir2, 'Species_CLN_matching_v2.csv'), as.is=T)[,'Scientific_name']))
  
  
} else if(Group == 'Vegetation') {
  idir = paste0(wdir, 'ModelResults/Maxent/Veg4/')
  ofile <- 'Evaluation_results_veg_v4.csv'
  # What species?
  lk <- read.csv(paste0(spdir2, 'CalVeg_CLN_matching_v2.csv'),as.is=T)
  allSpecies <- sort(unique(lk[which(lk$CalVeg_Code!='-'),'Vegtype_naming']))
  allSpecies <- allSpecies[which(!allSpecies%in%c("Monterey Cypress Forest", "Santa Cruz Cypress"))]
  
}
odir <- idir

climnames <- sort(c("cwd","djf","jja","ppt"))
mxModelType <- paste(climnames,collapse="-")


#----------------------#
# Assemble evaluations #
#----------------------#


res <- data.frame(matrix(nrow=length(allSpecies),ncol=8))
colnames(res) <- c("Species", "Predictors", "Train_pres", "Train_bg",
                   "Train_AUC", "Test_pres", "Test_bg", "Test_AUC")

for (i in 1:length(allSpecies)) {
  mySpecies <- allSpecies[i]
  print(mySpecies)
  mxdir <- paste(idir, mySpecies, "/", mxModelType, "/fullmodel", sep="")
  edir <- paste(idir, mySpecies, "/", mxModelType, "/randomtesting", sep="")
  
  e1 <- readRDS(paste(edir, 'EvaluateObject.rdata',sep='/'))
  mx <- readRDS(paste(mxdir, 'ModelObject.rdata', sep='/'))
  
  res[i,"Species"] <- mySpecies
  res[i,"Predictors"] <- paste(colnames(mx@presence),collapse="-")
  res[i,"Train_pres"] <- mx@results["X.Training.samples",]
  res[i,"Train_bg"] <- mx@results["X.Background.points",]
  res[i,"Train_AUC"] <- mx@results["Training.AUC",]
  res[i,"Test_pres"] <- length(e1@presence)
  res[i,"Test_bg"] <- length(e1@absence)
  res[i,"Test_AUC"] <- e1@auc 
  
}


write.csv(res, paste0(odir,ofile))
