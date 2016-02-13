
# Scripts for plotting suitability maps, vegetation, future projections
# and changes (leading/trailing edged) as well as summary plots for landscape units

# Clear workspace
rm(list=ls())

source("Scripts/00_Functions_trailing-edge.r")

Computer <- "HP"

#-----------------#
# Set directories #
#-----------------#
if (Computer == "HP") {
  wdir <- 'C:/Users/Naia Morueta Holme/Documents/Documents_share/Projects/'
  idir <- 'D:/Lead-trail/Projections/V4/'
  vdir <- 'D:/Vegetation/PROCESSED/'
  bgdir <- paste0(wdir,'100_Postdoc/Data/Background_layers/PROCESSED/')
  spdir2 <- paste0(wdir,'101_TBC3_modelling/Lead-trail_R-project/Data/Species/')
  fdir <- 'D:/Lead-trail/Projections/Figures_V4/Four_squares/'
  spdir <- 'D:/Phylo_modelling/Data/Species/Processed2/'
  mdir <- paste0(wdir, '101_TBC3_modelling/Lead-trail_R-project/ModelResults/Maxent/V4/')
}

#----------------#
# Load libraries #
#----------------#
require(raster)
# require(rgdal)
# require(fields)

#-----------------------#
# Parameters for script #
#-----------------------#
# What species?
allSpecies <- sort(unique(read.csv(paste0(spdir2, 'Species_CLN_matching_v2.csv'), as.is=T)[,'Scientific_name']))

# CC scenarios
allScenarios <- c("HST", "GFDL_B1","GFDL_A2","PCM_A2","CNRM_rcp85","CCSM4_rcp85","MIROC_rcp85", 
                  "PCM_B1","MIROC3_2_A2","csiro_A1B","GISS_AOM_A1B","MIROC5_rcp26","MIROC_rcp45",
                  "MIROC_rcp60","GISS_rcp26","MRI_rcp26","MPI_rcp45","IPSL_rcp85","Fgoals_rcp85")
myrange=c(2:8,11:17)
#myrange=13

# Which model type
mxModelType = "cwd-djf-jja-ppt"

# Landscape units
lu <- readRDS(paste(bgdir, "tbc3_landscape_units_p.rdata",sep=""))
lunames <- as.character(lu@data$Name[which(!is.na(lu@data$Name))])

# Load urban-ag mask
noveg <- readRDS(paste0(vdir,'CLN_Non-veg_30m.rdata'))
noveg270 <- aggregate(noveg, 9, fun=mean, na.rm=T)
rm(noveg)

#Define scenarios to lump:
wawet = c("PCM_B1","GISS_AOM_A1B","MPI_rcp45","MRI_rcp26","GISS_rcp26")
howet = c("CCSM4_rcp85","CNRM_rcp85","PCM_A2")
wadry = c("GFDL_B1", "MIROC5_rcp26","MIROC_rcp45")
hodry = c("GFDL_A2","MIROC_rcp85","MIROC_rcp60")
Gr = list(wawet,howet,wadry,hodry)
rm(wawet,howet,wadry,hodry)

#---------------#
# Set variables #
#---------------#
i=1
b=2


for(i in 2:length(allSpecies)) {
  mySpecies <- allSpecies[i]
  writeLines(mySpecies)


  #load historic and future suitabilities
  hfile <- paste0(idir,mxModelType,'/',mySpecies,'/','1951-1980_suitability_Bay_HST.tif')
  ffiles <- lapply(allScenarios[myrange],function(x) paste0(idir,mxModelType,'/',mySpecies,'/','2070-2099_suitability_Bay_',x,'.tif'))
  
  hst <- raster(hfile)
  fut <- stack(ffiles)
  rm(hfile,ffiles)
  
  #load vegetation mask
  spname <- sub(' ','_',mySpecies)
  
  mask1file <- paste0(vdir,'CLN_mask1/CLN_mask1_',spname,'.rdata')
  if(file.exists(mask1file)) {
    mask1 <- readRDS(mask1file)
    projection(mask1) <- projection(hst)
  }
  
  mask2 <- readRDS(paste0(vdir,'CLN_mask2/CLN_mask2_',spname,'.rdata'))
  projection(mask2) <- projection(hst)
  
  # Presence/absence threshold from maxent models
  mx.dir <- paste(mdir, mySpecies, mxModelType, 'fullmodel', sep='/')
  mx <- readRDS(paste(mx.dir, 'ModelObject.rdata', sep='/'))
  Threshold = 'Equal.training.sensitivity.and.specificity.logistic.threshold'
  mx.th = as.numeric(mx@results[Threshold,])
  rm(mx.dir, mx, Threshold)
  
  for(b in 1:length(lunames)) {
    
    luName <- lunames[b]
    writeLines(luName)
    
    fdir2 <- paste0(fdir, luName,'/')
    if(file.exists(fdir2)==F) {dir.create(fdir2, recursive=F)}
    ffile <- paste0(fdir2, mySpecies,'_4square.png')
    png(ffile, width=600, height=200)
    
    par(mfrow=c(1,3),mar=c(rep(0.1,4)),oma=rep(0,4))
    
    
    slu <- subset(lu,Name==luName)
    smask1 <- mask(crop(mask1,slu),slu)
    smask2 <- mask(crop(mask2,slu),slu)
    
    p0 <- mask(crop(hst,slu),slu)
    f0 <- mask(crop(fut,slu),slu)
    
  
    for(z in 1:3) {
      if(z==1) {
        p <- p0
        f <- f0
      } else if(z==2 & file.exists(mask1file)) {
        p <- mask(p0, smask1)
        f <- mask(f0, smask1)
      } else if(z==3) {
        p <- mask(p0, smask2)
        f <- mask(f0, smask2)
      }
      
      if(z==2 & !file.exists(mask1file)) {
        plot(0,0)
      } else {
        
        psuit = mean(getValues(p),na.rm=T)
        fsuits = c()
        for(i in 1:nlayers(f)) {
          res = mean(getValues(f[[i]]),na.rm=T)
          fsuits=c(fsuits,res)
        }
  
  
        Means = sapply(Gr,function(x) {mean(fsuits[which(allScenarios[myrange]%in%x)])})
        
        # Plot colored summaries of change
        
        myCols = sapply(Means,function(x) {pickCol(psuit=psuit,fsuit=x)})
        
        plot(-1,xlim=c(0.5,3.5),ylim=c(0.5,3.5),axes=F,xlab="",ylab="")
        rect(xleft=c(1,2,1,2),ybottom=c(2,2,1,1),xright=c(2,3,2,3),ytop=c(3,3,2,2), col=myCols)
      }
    }
    dev.off()
  }
}

