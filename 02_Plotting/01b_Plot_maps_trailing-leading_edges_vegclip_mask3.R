# Scripts for plotting maps of trailing and leading edges
# Updated February 13 2016


# Clear workspace
rm(list=ls())

Computer <- "HP"

#-----------------#
# Set directories #
#-----------------#
if (Computer == "HP") {
  wdir <- 'C:/Users/Naia Morueta Holme/Documents/Documents_share/Projects/'
  idir <- 'E:/Lead-trail/Projections/V4/'
  vdir <- 'E:/Vegetation/PROCESSED/'
  bgdir <- paste0(wdir,'100_Postdoc/Data/Background_layers/PROCESSED/')
  spdir2 <- paste0(wdir,'101_TBC3_modelling/Lead-trail_R-project/Data/Species/')
  fdir <- 'E:/Lead-trail/Projections/Figures_V5/'
  spdir <- 'E:/Phylo_modelling/Data/Species/Processed2/'
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

# CC scenarios
allScenarios <- c("HST", "GFDL_B1","GFDL_A2","PCM_A2","CNRM_rcp85","CCSM4_rcp85","MIROC_rcp85", 
                  "PCM_B1","MIROC3_2_A2","csiro_A1B","GISS_AOM_A1B","MIROC5_rcp26","MIROC_rcp45",
                  "MIROC_rcp60","GISS_rcp26","MRI_rcp26","MPI_rcp45","IPSL_rcp85","Fgoals_rcp85")
myrange=c(2:19)

# Which model type
mxModelType = "cwd-djf-jja-ppt"


# Landscape units
lu <- readRDS(paste(bgdir, "tbc3_landscape_units_p.rdata",sep=""))

# Load urban-ag mask
noveg <- readRDS(paste0(vdir,'CLN_Non-veg_30m.rdata'))
noveg270 <- aggregate(noveg, 9, fun=mean, na.rm=T)

# Background Bay Area map
bg <- readRDS(paste(bgdir, "GADM_BayArea_proj.rdata",sep=""))

orig.project <- '+proj=longlat +ellps=WGS84'

#--------------------------------#
# Plot continuous lead-trail map #
#--------------------------------#

i=1
a=7

for(i in 1:length(allSpecies)) {
  mySpecies <- allSpecies[i]
  writeLines(mySpecies)
  #load vegetation mask
  spname <- sub(' ','_',mySpecies)
  mask3 <- readRDS(paste0(vdir,'CLN_mask3/CLN_mask3_',spname,'.rdata'))
  
  #load occurrences
  pres <- readRDS(paste(spdir, mySpecies, ".rdata", sep=""))
  coordinates(pres) <- ~longitude + latitude
  projection(pres) <- CRS(orig.project)
  
  
  for(a in myrange) {
    
    myScenario <- allScenarios[a]
    writeLines(myScenario)
    figfile <- paste0(fdir, 'Bay_lead-trail/',mySpecies,'_',myScenario,'.png')
    if(file.exists(figfile)) {
      print(paste('Skipping',myScenario,'...already plotted'))
    } else {
      
    
  
      hfile <- paste0(idir,mxModelType,'/',mySpecies,'/','1951-1980_suitability_Bay_HST.tif')
      ffile <- paste0(idir,mxModelType,'/',mySpecies,'/','2070-2099_suitability_Bay_',myScenario,'.tif')
      
      hst <- raster(hfile)
      fut <- raster(ffile)
      
  
      # Plot Bay Area map with noveg and mask3
      png(figfile, width=3000, height=1500, pointsize = 36)
      
      par(mfrow=c(1,2),mar=c(0.1,0.1,2,0.1))
      
      ## First plot only clipping noveg
      plot(mask(fut-hst,noveg270,inverse=T),axes=F,zlim=c(-0.5,0.5),legend=F,
           col=colorRampPalette(c("red", "white", "blue"))(20))
      plot(mask(hst,noveg270,inverse=T),axes=F,zlim=c(0,1),legend=F,col=grey.colors(20,start=1,end=0),alpha=0.5,add=T)
      title(mySpecies)
      
        # add landscape units and occurrences
        lup <- spTransform(lu,projection(hst))
        plot(lup,add=T)    
        # add occurrence points
        occur <- spTransform(pres, projection(hst))
        points(occur[lup,],pch=4)
      
  
      ## Second plot masking to current veg type mask 3
      if(file.exists(paste0(vdir,'CLN_mask3/CLN_mask3_',spname,'.rdata'))) {
        mask3p <- projectRaster(mask3,hst,method='ngb')
      
        plot(mask(fut-hst,mask1p,inverse=T),axes=F,zlim=c(-0.5,0.5),legend=F,
             col=colorRampPalette(c("red", "white", "blue"))(20))
        plot(mask(hst,mask3p,inverse=T),axes=F,zlim=c(0,1),legend=F,col=grey.colors(20,start=1,end=0),alpha=0.5,add=T)
        title('mask3')
        
          # add landscape units and occurrences
          lup <- spTransform(lu,projection(hst))
          plot(lup,add=T)    
          # add occurrence points
          occur <- spTransform(pres, projection(hst))
          points(occur[lup,],pch=4)
      } else {
        plot(0,0,main='no mask available')
      }
        
      dev.off()
      
      
      
      # Legend
      if(i==1 & a==2) {
        lfile <- paste0(fdir, 'Bay_lead-trail/00_Legend.png')
        png(lfile, width=1800, height=1800, pointsize = 48)
        
        m1 <- raster(matrix(rep(1:20,20),ncol=20,nrow=20))
        m2 <- raster(matrix(rep(1:20,20),byrow=T, ncol=20,nrow=20))
        
        # remove forbidden combinations
        t1 <- (flip(m1,2)/20-0.5)+m2/20
        rc <- matrix(c(-.5,0,NA,1,1.5,NA),ncol=3,byrow=T)
        t2 <- reclassify(t1,rc)
        
        
        par(mar=c(4,4,.5,.1),oma=c(0,0,0,0))
        plot(mask(m1,t2),col=colorRampPalette(c("blue", "white", "red"))(20),legend=F,
             xlab='Suitability',ylab='Delta',axes=F)
        plot(mask(m2,t2),col=grey.colors(20,start=1,end=0),alpha=0.5,add=T,legend=F)
        axis(1,at=seq(0,1,0.25))
        axis(2,at=seq(0,1,0.25),labels=c(-0.5,-.25,0,0.25,0.5))
        abline(0,1,lty='dashed')
        abline(1,-1,lty='dashed')
        text(x=0,y=0.5,'unsuitable',pos=4)
        text(x=1,y=0.5,'suitable stable',pos=2)
        text(x=0.5,y=1,'leading',pos=1)
        text(x=0.5,y=0,'trailing',pos=3)
        
        dev.off()
      }
    }
  }
}


