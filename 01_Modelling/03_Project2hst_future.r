# Script to load in model and project to historic or future climate layers
# Updated October 27 2015

# Clear workspace
rm(list=ls())
Start=Sys.time()

#-----------------#
# Set directories #
#-----------------#
Computer <- "HP"

if(Computer == "EOS") {
  #wdir <- 'bien/Naia/'
  #cdir <- paste(wdir, 'BCM/CA_2014/Summary/', sep='')
} else if (Computer == "HP") {
  wdir <- 'C:/Users/Naia Morueta Holme/Documents/Documents_share/Projects/'
  mdir <- paste0(wdir, '101_TBC3_modelling/Lead-trail_R-project/ModelResults/Maxent/V4/')
  hdir <- 'E:/BCM/CA_2014/Summary/HST/Normals_30years/'
  fdir <- 'E:/BCM/CA_2014/Summary/Futures/Normals_30years/'
  bgdir <- paste0(wdir,'100_Postdoc/Data/Background_layers/PROCESSED/')
  spdir2 <- paste0(wdir,'101_TBC3_modelling/Lead-trail_R-project/Data/Species/')
  odir <- 'E:/Lead-trail/Projections/V4/'
}

# What species?
allSpecies <- sort(unique(read.csv(paste0(spdir2, 'Species_CLN_matching_v2.csv'), as.is=T)[,'Scientific_name']))
#allSpecies <- allSpecies[1:2]

# Set climate layer names (right now for California BCM layers)
climnames <- sort(c("cwd","djf","jja","ppt")) #use sort to ensure correct names used for predictors
mxModelType <- paste(climnames,collapse="-")

# CC scenarios
allScenarios <- c("HST", "GFDL_B1","GFDL_A2","PCM_A2","CNRM_rcp85","CCSM4_rcp85","MIROC_rcp85", 
                  "PCM_B1","MIROC3_2_A2","csiro_A1B","GISS_AOM_A1B","MIROC5_rcp26","MIROC_rcp45",
                  "MIROC_rcp60","GISS_rcp26","MRI_rcp26","MPI_rcp45","IPSL_rcp85","Fgoals_rcp85")
myrange=c(9,10,18,19)
#myrange=c(1:8,11:17)

#Time period
hstyrs <- '1951-1980'
futyrs <- '2070-2099'

clip.file <- paste0(bgdir,"GADM_BayArea_proj.rdata")
bay <- readRDS(clip.file)


#-----------#
# Libraries #
#-----------#
require(raster)
require(dismo)
require(rJava)

#-----------------#
# Project models  #
#-----------------#

j=2
for(j in myrange) {
  mod <- allScenarios[j]
  writeLines(mod)
  print(Sys.time())
  
  if(mod == 'HST') {
    period <- hstyrs
    files <- paste0("BCM2014_",climnames,period,"_wy_ave_",mod,".Rdata")
    predictors <- stack(lapply(files,function(x) readRDS(paste0(hdir,x))))
  } else {
    period <- futyrs
    files <- paste0("BCM2014_",climnames,period,"_wy_ave_",mod,".Rdata")
    predictors <- stack(lapply(files,function(x) readRDS(paste0(fdir,x))))
  }
  names(predictors) <- climnames
  
  pclip <- mask(crop(predictors, bay),bay)
  
  i=5
  for (i in 1:length(allSpecies)) {
    mySpecies <- allSpecies[i]
    writeLines(mySpecies)
    # Create directory to save projections
    proj.dir <- paste0(odir, mxModelType, "/", mySpecies, "/")
    if(!file.exists(proj.dir)) {dir.create(proj.dir, recursive=T)}
    
    # Load mx model
    mx.dir <- paste(mdir, mySpecies, mxModelType, 'fullmodel', sep='/')
    mx <- readRDS(paste(mx.dir, 'ModelObject.rdata', sep='/'))
    
    res <- predict(mx, pclip)
    
    fpath <- paste0(proj.dir, period,'_suitability_Bay_', mod,'.tif')
    writeRaster(res, filename=fpath,format='GTiff')
    
    if(mod == 'HST') {
      res2 <- predict(mx, predictors)
      fpath2 <- paste0(proj.dir, period,'_suitability_CA_', mod,'.tif')
      writeRaster(res2, filename=fpath2,format='GTiff')
      
    }

   }
}

print(Sys.time()-Start)
