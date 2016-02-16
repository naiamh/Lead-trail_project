
# Scripts for plotting suitability maps, vegetation, future projections
# and changes (leading/trailing edged) as well as summary plots for landscape units

# Clear workspace
rm(list=ls())

#source("Scripts/00_Functions_trailing-edge.r")

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
  fdir <- 'E:/Lead-trail/Projections/Figures_V5/Four_squares/'
  spdir <- 'E:/Phylo_modelling/Data/Species/Processed2/'
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
myrange=c(2:9,11:19)
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
# # Based on Bay Area climate - note: temperature cut-off is at ~3.2 degrees C, ppt at 0C
# wawet = c("PCM_B1","GISS_AOM_A1B","MPI_rcp45","MRI_rcp26","GISS_rcp26")
# howet = c("CCSM4_rcp85","CNRM_rcp85","PCM_A2","IPSL_rcp85")
# wadry = c("GFDL_B1", "MIROC5_rcp26")
# hodry = c("GFDL_A2","MIROC_rcp85","MIROC_rcp60","MIROC3_2_A2","Fgoals_rcp85","MIROC_rcp45")

# Based on Bay Area climate - note: temperature cut-off is at ~2.5 degrees C, ppt at 0C
wawet = c("PCM_B1","GISS_rcp26","MPI_rcp45","MRI_rcp26")
howet = c("GISS_AOM_A1B","CCSM4_rcp85","CNRM_rcp85","PCM_A2","IPSL_rcp85")
wadry = c("GFDL_B1","MIROC5_rcp26")
hodry = c("GFDL_A2","MIROC_rcp85","MIROC_rcp60","MIROC3_2_A2","Fgoals_rcp85","MIROC_rcp45")


Gr = list(wawet,howet,wadry,hodry)
rm(wawet,howet,wadry,hodry)

#----------#
# Function #
#----------#

# Function to choose color code based on the mean suitability across a region
# in the future as a fraction of present suitability 
# (0-25%: red, 25-75%: orange, 75-125%: grey, >125%: green)
pickCol = function(psuit,fsuit) {
  if(fsuit < 0.25*psuit) {
    return("red") # if mean future suitability less than 25% of present suitability
  } else if (fsuit < 0.75*psuit) {
    return("orange") # if mean future suitability less than 75% than present suitability
  } else if (fsuit <= 1.25*psuit) {
    return("grey") # if mean future suit. between 75-125% of present suit
  } else {
    return("green") # if mean future suit. larger than present
  }   
}

#---------------#
# Set variables #
#---------------#
a=1
b=33

allRES <- list()
for(a in 1:length(allSpecies)) {
#for(a in 1:3) {
  mySpecies <- allSpecies[a]
  writeLines(mySpecies)


  #load historic and future suitabilities
  hfile <- paste0(idir,mxModelType,'/',mySpecies,'/','1951-1980_suitability_Bay_HST.tif')
  ffiles <- lapply(allScenarios[myrange],function(x) paste0(idir,mxModelType,'/',mySpecies,'/','2070-2099_suitability_Bay_',x,'.tif'))
  
  hst <- raster(hfile)
  fut <- stack(ffiles)
  rm(hfile,ffiles)
  
  #load vegetation mask
  spname <- sub(' ','_',mySpecies)
  
#   mask1file <- paste0(vdir,'CLN_mask1/CLN_mask1_',spname,'.rdata')
#   if(file.exists(mask1file)) {
#     mask1 <- readRDS(mask1file)
#     projection(mask1) <- projection(hst)
#   }
#   
#   mask2 <- readRDS(paste0(vdir,'CLN_mask2/CLN_mask2_',spname,'.rdata'))
#   projection(mask2) <- projection(hst)
  
    mymask <- readRDS(paste0(vdir,'CLN_mask3/CLN_mask3_',spname,'.rdata'))
    projection(mymask) <- projection(hst)
  
  
  # Presence/absence threshold from maxent models
  mx.dir <- paste(mdir, mySpecies, mxModelType, 'fullmodel', sep='/')
  mx <- readRDS(paste(mx.dir, 'ModelObject.rdata', sep='/'))
  Threshold = 'Equal.training.sensitivity.and.specificity.logistic.threshold'
  mx.th = as.numeric(mx@results[Threshold,])
  rm(mx.dir, mx, Threshold)
  
  luRES <- list()
#  for(b in 1:4) {
  for(b in 1:length(lunames)) {
    
    luName <- lunames[b]
    writeLines(luName)
    
    fdir2 <- paste0(fdir, luName,'/')
    if(!dir.exists(fdir2)) {dir.create(fdir2, recursive=F)}
#     ffile <- paste0(fdir2, mySpecies,'_4square.png')
#     png(ffile, width=600, height=200)
#     
#     par(mfrow=c(1,3),mar=c(rep(0.1,4)),oma=rep(0,4))
    
    
    slu <- subset(lu,Name==luName)
    # masking out urban-ag
    smask1 <- mask(crop(noveg270,slu),slu) #note that value 1 is what we want to EXCLUDE (ag-urban)
    # masking out with vegmask 3
    smask2 <- mask(crop(mymask,slu),slu) #note that value 1 is what we want to INCLUDE (vegtype)
    
    p0 <- mask(crop(hst,slu),slu)
    f0 <- mask(crop(fut,slu),slu)
    
    subRES <- list()
    for(z in 1:3) {
      # all pixels
      if(z==1) {
        p <- p0
        f <- f0
        type <- 'All'
      #exclude ag-urban
      } else if(z==2) {
        p <- mask(p0, smask1, inverse=T)
        f <- mask(f0, smask1, inverse=T)
        type <- 'Natural'
      # exclude off vegtype
      } else if(z==3) {
        p <- mask(p0, smask2)
        f <- mask(f0, smask2)
        type <- 'InVegtype'
      }
      
      psuit = mean(getValues(p),na.rm=T)
      fsuits = c()
      for(i in 1:nlayers(f)) {
        res = mean(getValues(f[[i]]),na.rm=T)
        fsuits=c(fsuits,res)
      }


      Means = sapply(Gr,function(x) {mean(fsuits[which(allScenarios[myrange]%in%x)])})
      
      # Plot colored summaries of change
      if(!is.na(psuit)) {
        myCols = sapply(Means,function(x) {pickCol(psuit=psuit,fsuit=x)})
      } else {
        myCols = NA
      }
      subRES[[z]] <- myCols
      names(subRES)[[z]] <- type
      
      ffile <- paste0(fdir2, mySpecies,'_',type,'_4square.png')
      png(ffile, width=200, height=200)
      
      par(mar=c(rep(0,4)),oma=rep(0,4))
      
      if(!is.na(psuit)) {
        plot(-1,xlim=c(0.5,3.5),ylim=c(0.5,3.5),axes=F,xlab="",ylab="")
        rect(xleft=c(1,2,1,2),ybottom=c(2,2,1,1),xright=c(2,3,2,3),ytop=c(3,3,2,2), col=myCols)
      } else {
        plot(0,0)
      }
      dev.off()
    }
    luRES[[b]] <- subRES
    names(luRES)[[b]] <- luName
  }
  allRES[[a]] <- luRES
  names(allRES)[[a]] <- mySpecies
}

saveRDS(allRES, paste0(fdir,'All_four_square_results.rdata'))


#----------------#
# Overview plots #
#----------------#
# Plot overview matrix of 4-squares for all species in all landscape units

allSpecies <- sort(names(allRES),decreasing=T)
lunames <- sort(names(allRES[[1]]))


# For 4-squares within vegtype only
pdf(file=paste0(fdir, "Overview_allSp_allLU_InVegtype.pdf"),width=12,height=9, pointsize=12)

par(mfrow=c(1,1),mar=c(0.1,5.5,4,0.5),cex=1)
plot(0,main='', ylim = c(1,length(allSpecies)+1), xlim = c(1,length(lunames)+1),axes=F, xlab="", ylab="",col='white')
text(x=0.5,y=1:length(allSpecies), labels=allSpecies, pos=2, cex=0.6, xpd=T)
text(x=1:length(lunames), y=length(allSpecies)+1, labels=lunames, srt=45, xpd=T, pos=4, cex=0.6)


w <- 0.4
s <- 1
l <- 1
#myCols <- c('red','green','orange','grey')

for(s in 1:length(allSpecies)) {
#for(s in 1:4) {
  sp <- allSpecies[s]
#  for(l in 1:3) {
  for(l in 1:length(lunames)) {
    lu <- lunames[l]
    myCols <- allRES[[sp]][[lu]][[3]]
    
    rect(xleft=c(l-w,l,l-w,l),ybottom=c(s,s,s-w,s-w),xright=c(l,l+w,l,l+w),ytop=c(s+w,s+w,s,s), col=myCols)
  }
}
dev.off()


# For 4-squares within natural areas and within vegtype
pdf(file=paste0(fdir, "Overview_allSp_allLU_Natural_or_InVegtype.pdf"),width=24,height=9, pointsize=12)

par(mfrow=c(1,1),mar=c(0.1,5.5,4,0.5),cex=1)
plot(0,main='', ylim = c(1,length(allSpecies)+1), xlim = c(1,2*length(lunames)+1),axes=F, xlab="", ylab="",col='white')
text(x=0.5,y=1:length(allSpecies), labels=allSpecies, pos=2, cex=0.6, xpd=T)
text(x=seq(1,2*length(lunames),by=2), y=length(allSpecies)+1, labels=lunames, srt=45, xpd=T, pos=4, cex=0.6)


w <- 0.4
s <- 1
l <- 1
#myCols <- c('red','green','orange','grey')

# add background stripes
i <- seq(1.5,length(lunames)*2,by=4)
rect(xleft=i-w*2.5, ybottom=0, xright=i+w*2.5, ytop=length(allSpecies)+w*2,border=F, col='lightblue')


for(s in 1:length(allSpecies)) {
  #for(s in 1:4) {
  sp <- allSpecies[s]
  #  for(l in 1:3) {
  
  for(ll in 1:length(lunames)) {
    lu <- lunames[ll]
    myCols1 <- allRES[[sp]][[lu]][[2]]
    myCols2 <- allRES[[sp]][[lu]][[3]]
    

    
    l=seq(1,2*length(lunames),by=2)[ll]
    rect(xleft=c(l-w,l,l-w,l),ybottom=c(s,s,s-w,s-w),xright=c(l,l+w,l,l+w),ytop=c(s+w,s+w,s,s), col=myCols1)
    
    l=seq(2,2*length(lunames),by=2)[[ll]]
    rect(xleft=c(l-w,l,l-w,l),ybottom=c(s,s,s-w,s-w),xright=c(l,l+w,l,l+w),ytop=c(s+w,s+w,s,s), col=myCols2)

  }
}
dev.off()
