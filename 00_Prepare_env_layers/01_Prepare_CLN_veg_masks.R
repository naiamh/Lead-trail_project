# Script for processing CLN original input into Bay Area wide vegetation masks for 
# each species of interest
# Input required: original CLN map, csv file matching species names and CLN code,
# directory names.
# In addition: creates layer of non-veg, i.e. agriculture, urban and water.

# Naia Morueta-Holme, created 2015/10/22

# Clear workspace
rm(list=ls())
#---------------------#
# Working directories #
#---------------------#
Computer = 'HP'
if(Computer == 'HP') {
  idir <- 'D:/Vegetation/ORIGINAL/'
  odir <- 'D:/Vegetation/PROCESSED/'
  wdir <- 'C:/Users/Naia Morueta Holme/Documents/Documents_share/Projects/'
  bgdir <- paste0(wdir,'100_Postdoc/Data/Background_layers/PROCESSED/')
  cdir <- 'D:/BCM/CA_2014/Summary/HST/Normals_30years/'
  sdir <- paste0(wdir,'101_TBC3_modelling/Lead-trail_R-project/Scripts2/')
  spdir <- paste0(wdir, '101_TBC3_modelling/Lead-trail_R-project/Data/Species/')
}


#-----------#
# Libraries #
#-----------#
require(raster)
require(rgdal)


#------------#
# Parameters #
#------------#
ta.project <- '+proj=aea +datum=NAD83 +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000'
bg.file <- paste0(cdir, 'BCM2014_ppt1951-1980_wy_ave_HST.Rdata')
clip.file <- paste0(bgdir,"GADM_BayArea_proj.rdata")

#----------------------------------------#
# Project original CLN map to Flint proj #
#----------------------------------------#
# original cln file
r <- raster(paste0(idir, 'cln_veg10'))

# get background raster for Bay Area as projection template
bg <- readRDS(bg.file)
bay <- readRDS(clip.file)
bgb <- crop(bg, bay)
bg1 <- disaggregate(bgb, 9)
rm(bay,bgb,bg)

clnp <- projectRaster(r, bg1, method='ngb')

clnp <- readAll(clnp)
saveRDS(clnp, paste0(odir, 'cln_veg10p.rdata'))

rm(bg1,r)

#------------------#
# Create CLN masks #
#------------------#
cln <- readRDS(paste0(odir, 'cln_veg10p.rdata'))
lk <- read.csv(paste0(spdir, 'Species_CLN_matching_v2.csv'),as.is=T)
bg <- readRDS(bg.file)
bay <- readRDS(clip.file)
bgm <- mask(crop(bg, bay),bay)



Species <- sort(unique(lk$Scientific_name))

# function to produce vegetation mask. bg: background raster to get right cell resolution
# vegmap: cln vegetation map in same projection as bg, ids: CLN code ids
getm = function(bg, vegmap, ids) {
  vpoints = xyFromCell(vegmap, which(getValues(vegmap)%in%ids),spatial=T)
  vc = unique(cellFromXY(bg,vpoints))
  gmask = bg
  gmask[] = 1
  gmask[vc] = NA
  return(gmask)  
}

# Matching with Weiss&Ackerly classification and classification based on Sawyer et al.
i=1
for (i in 8:length(Species)) {
  start=Sys.time()
  sp <- Species[i]
  print(sp)
  spname <- gsub(' ','_',sp)
  
  # Masking following Stu/Ackerly classes - takes into account 'Out' category
  sub1 <- subset(lk,Scientific_name==sp & Include1==1)
  row.names <- NULL
  
  if(nrow(sub1)>0) {
    if(all(sub1$In_out=='In')) {
      ids1 <- unique(sub1$CLN_Code)
    } else {
      subout <- subset(sub1, In_out=='Out')
      allids <- unique(lk$CLN_Code)[!is.na(unique(lk$CLN_Code))]
      ids1 <- allids[!allids%in%subout$CLN_Code]
    }
    mask1 <- getm(bg=bgm,vegmap=cln,ids=ids1)
   saveRDS(mask1, paste0(odir,'CLN_mask1/CLN_mask1_',spname,'.rdata'))
  } else {print('No vegtypes for Weiss-Ackerly')}
  
    # Masking following Sawyer et al. classes
  sub2 <- subset(lk,Scientific_name==sp & In_out=='In' & Include2==1)
  if(nrow(sub2)>0) {
    ids2 <- unique(sub2$CLN_Code)
    mask2 <- getm(bg=bgm,vegmap=cln,ids=ids2)
    saveRDS(mask2, paste0(odir,'CLN_mask2/CLN_mask2_',spname,'.rdata'))
  } else {print('No vegtypes for Sawyer')}
  print(Sys.time()-start)
}

#-------------------------------#
# Create mask for non-vegetated #
#-------------------------------#

# Mask of water (59), rural residential (61), urban (56) and cultivated (16)
off <- getm(bg=bgm,vegmap=cln,ids=c(59,61,56,16))
m <- matrix(c(NA,1,1,NA),nrow=2,ncol=2)
res <- reclassify(off,m)
saveRDS(res, paste0(odir,'CLN_Non-veg_270m.rdata'))

off2 <- getm(bg=cln,vegmap=cln,ids=c(59,61,56,16))
res2 <- reclassify(off2,m)
res2 <- readAll(res2)
saveRDS(res2, paste0(odir,'CLN_Non-veg_30m.rdata'))

