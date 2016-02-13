# Get mean MAT for future scenarios for sorting plots by increasing temperature
# and mean PPT to order by warm-dry, warm-wet, hot-dry, hot-wet
# Updated October 29 2015

# Clear workspace
rm(list=ls())

Computer <- "HP"

#-----------------#
# Set directories #
#-----------------#
if(Computer == "EOS") {
  #wdir <- 'bien/Naia/'
  #cdir <- paste(wdir, 'BCM/CA_2014/Summary/', sep='')
} else if (Computer == "HP") {
  wdir <- 'C:/Users/Naia Morueta Holme/Documents/Documents_share/Projects/'
  fdir <- 'D:/BCM/CA_2014/Summary/Futures/Normals_30years/'
  hdir <- 'D:/BCM/CA_2014/Summary/HST/Normals_30years/'
  odir <- 'D:/BCM/BayArea_2014/Summary/Normals_30years/'
  bgdir <- paste0(wdir,'100_Postdoc/Data/Background_layers/PROCESSED/')
}

# Libraries
require(raster)

# Parameters
allScenarios <- c("HST", "GFDL_B1","GFDL_A2","PCM_A2","CNRM_rcp85","CCSM4_rcp85","MIROC_rcp85", 
                  "PCM_B1","MIROC3_2_A2","csiro_A1B","GISS_AOM_A1B","MIROC5_rcp26","MIROC_rcp45",
                  "MIROC_rcp60","GISS_rcp26","MRI_rcp26","MPI_rcp45","IPSL_rcp85","Fgoals_rcp85")
myrange=c(1:8,11:17)
climnames <- c('cwd','djf','jja','ppt')

hstyrs <- '1951-1980'
futyrs <- '2070-2099'

clip.file <- paste0(bgdir,"GADM_BayArea_proj.rdata")
bay <- readRDS(clip.file)

# Create Bay Area climate layer for each scenario and each variable

for(j in myrange) {
  mod <- allScenarios[j]
  print(j)
  if(mod == 'HST') {
    period <- hstyrs
    cdir <- hdir
  } else {
    period <- futyrs
    cdir <- fdir
  }
  
  
  for(a in 1:length(climnames)) {
    clim <- climnames[a]
    writeLines(clim)
    files <- paste("BCM2014_",clim,period,"_wy_ave_",mod,".Rdata", sep='')
    preds <- readRDS(paste(cdir,files,sep=""))
    names(preds) <- clim
    pclip <- mask(crop(preds, bay),bay)
    
    ofile <- paste("Bay_BCM2014_",clim,period,"_wy_ave_",mod,".Rdata", sep='')
    saveRDS(pclip, paste0(odir, ofile))
  }
}
