# Scripts for plotting suitability maps for historic climate
# Updated October 28 2015


# Clear workspace
rm(list=ls())

Computer <- "HP"

#-----------------#
# Set directories #
#-----------------#
if (Computer == "HP") {
  wdir <- 'C:/Users/Naia Morueta Holme/Documents/Documents_share/Projects/'
  idir <- 'D:/Lead-trail/Projections/V4/'
  bgdir <- paste0(wdir,'100_Postdoc/Data/Background_layers/PROCESSED/')
  spdir2 <- paste0(wdir,'101_TBC3_modelling/Lead-trail_R-project/Data/Species/')
  fdir <- 'D:/Lead-trail/Projections/Figures_V4/CA_HST_suitabilities/'
  spdir <- 'D:/Phylo_modelling/Data/Species/Processed2/'
}


#----------------#
# Load libraries #
#----------------#
require(raster)

#-----------------------#
# Parameters for script #
#-----------------------#
# What species?
allSpecies <- sort(unique(read.csv(paste0(spdir2, 'Species_CLN_matching_v2.csv'), as.is=T)[,'Scientific_name']))

# Which model type
mxModelType = "cwd-djf-jja-ppt"

# Background CA map
bg <- readRDS(paste(bgdir, "GADM_California_proj.rdata",sep=""))

orig.project <- '+proj=longlat +ellps=WGS84'

#---------------------#
# Plot continuous map #
#---------------------#

i=1

for(i in 1:length(allSpecies)) {
  mySpecies <- allSpecies[i]
  writeLines(mySpecies)
  #load projection
  hfile <- paste0(idir,mxModelType,'/',mySpecies,'/','1951-1980_suitability_CA_HST.tif')
  hst <- raster(hfile)
  
  
  #load occurrences
  pres <- readRDS(paste(spdir, mySpecies, ".rdata", sep=""))
  coordinates(pres) <- ~longitude + latitude
  projection(pres) <- CRS(orig.project)
  
  figfile <- paste0(fdir, mySpecies,'_CA_suitability.png')
  png(figfile, width=1000, height=1000, pointsize = 36)
  
  par(mar=c(0.1,0.1,0.1,2))
    
  plot(hst,axes=F,zlim=c(0,1),legend=T,col=grey.colors(20,start=1,end=0))

  # add CA outline
  plot(bg,add=T)
  # add occurrence points
  occur <- spTransform(pres, projection(hst))
  points(occur,pch=4,col='red',cex=0.1)

  dev.off()
  
}

