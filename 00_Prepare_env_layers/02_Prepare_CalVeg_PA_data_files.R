# Script for processing vegetation layers to be used in modelling 
# Derived from CALVEG data, the script rasterizes the dominance types to 30 m resolution
# and a file with lat long for each vegetation type(s) of interest is produced 
# corresponding to their presence at 270 m resolution for subsequent modelling

# Clear workspace
rm(list=ls())
#---------------------#
# Working directories #
#---------------------#
Computer = 'HP'
if(Computer == 'HP') {
  idir <- 'D:/Vegetation/ORIGINAL/CalVeg/'
  odir <- 'D:/Vegetation/PROCESSED/'
  wdir <- 'C:/Users/Naia Morueta Holme/Documents/Documents_share/Projects/'
  bgdir <- paste0(wdir,'100_Postdoc/Data/Background_layers/PROCESSED/')
  cdir <- 'D:/BCM/CA_2014/Summary/HST/Normals_30years/'
  vdir <- paste0(wdir, '101_TBC3_modelling/Lead-trail_R-project/Data/Species/')
} 

#-----------#
# Libraries #
#-----------#
require(raster)
require(rgdal)

#------------#
# Parameters #
#------------#
#orig.project = '+proj=longlat +ellps=WGS84'
ta.project = '+proj=aea +datum=NAD83 +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000'

ifiles <- c("ExistingVegCenCoast1997_2013_v1","ExistingVegCentralValley1998_2007_v1",
            "ExistingVegGreatBasin1999_2009_v1", "ExistingVegNorCoastEast_1998_2007_v1", 
            "ExistingVegNorCoastMid_1998_2007_v1", "ExistingVegNorCoastWest_2000_2007_v1",
            "ExistingVegNorInterior1999_2009_v1", "ExistingVegNorSierra2000_2009_v1", 
            "ExistingVegSouthCoast2002_2010_v2", "ExistingVegSouthInterior2000_2008_v1",
            "ExistingVegSouthSierra2000_2008_v1")
CA <- readRDS(paste0(bgdir, "GADM_California_proj.rdata"))
reclassfile <- read.csv(paste(vdir, "CalVeg_CLN_matching_v2.csv",sep=''),as.is=T)

#--------------#
# Process data #
#--------------#
# Read/create 100,000 random sample points for California
#CAp <- spsample(CA, 100000, type="random")

#saveRDS(CAp, paste(bgdir, "100000_CA_random_points.rdata", sep="/"))
CAp <- readRDS(paste(bgdir, "100000_CA_random_points.rdata", sep="/"))


# Vegetation data for all of California
j=10

res=list()
for(j in 1:length(ifiles)) {

#for(j in c(6, 10)) {
  print(Sys.time())
  vfile <- ifiles[j]
  ogrListLayers(paste(idir,vfile,".gdb",sep=''))
  
  veg <- readOGR(paste(idir,vfile,".gdb",sep=''),layer=vfile)
  # extract sample points in the zone
  CAproj <- spTransform(CAp, CRS(projection(veg)))
  pin <- as.character(over(CAproj, veg)[,"REGIONAL_DOMINANCE_TYPE_1"])
  
  mydata <- data.frame(CALVEG_ORIG=pin[which(!is.na(pin))])
  coords <- CAp[which(!is.na(pin))]
  
  res[[j]] <- SpatialPointsDataFrame(coords, mydata)

}
print(Sys.time())
rm(mydata, coords, pin, vfile, veg)

allres <- do.call("rbind",res)
saveRDS(allres,paste(odir,"sampledCalVeg_all.rdata"))

#-------------------------#
# Create vegetation files #
#-------------------------#
# For each CLN vegetation type, identify corresponding CalVeg types
# and create file with presences and absences for modelling

allres <- readRDS(paste(odir,"sampledCalVeg_all.rdata"))
allveg <- as.character(unique(reclassfile$Vegtype_naming))

i=1
for(i in 1:length(allveg)) {
  myveg <- allveg[i]
  calveg <- as.character(reclassfile[which(reclassfile$Vegtype_naming==myveg),"CalVeg_Code"])
  res <- allres
  res$occur <- 0
  res$occur[which(allres$CALVEG_ORIG%in%calveg)] <- 1
  res <- spTransform(res, CRS(ta.project))
  saveRDS(res, paste0(odir, 'CalVeg_PA/', myveg, "_CalVeg_PA.rdata"))
  print(i)
}

