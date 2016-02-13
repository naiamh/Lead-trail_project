# Plot Bay Area climate with CA climate as background to show change
# in climate over time

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
  wdir <- 'C:/Users/morueta/Documents/Documents_share/Projects/101_TBC3_modelling/Lead-trail_R-project/'
  sdir <- paste(wdir, 'Scripts_2/', sep='')
  hdir <- 'E:/BCM/CA_2014/Summary/HST/Normals_30years/'
  fdir <- 'E:/BCM/CA_2014/Summary/Futures/Normals_30years/'
  cdir  <- 'E:/BCM/CA_2014/Summary/Futures/'
  bgdir <- 'C:/Users/morueta/Documents/Documents_share/Projects/100_Postdoc/Data/Background_layers/PROCESSED/'
  figdir <- 'E:/Lead-trail/Projections/Figs_Climate-Change-Space/'
}

#----------------#
# Load libraries #
#----------------#
require(raster)
source(paste(sdir,"00_Functions_trailing-edge.r",sep=""))

#------------#
# Parameters #
#------------#
allScenarios <- c("HST", "GFDL_B1","GFDL_A2","PCM_A2","CNRM_rcp85","CCSM4_rcp85","MIROC_rcp85", 
                  "PCM_B1","MIROC3_2_A2","csiro_A1B","GISS_AOM_A1B","MIROC5_rcp26","MIROC_rcp45",
                  "MIROC_rcp60","GISS_rcp26","MRI_rcp26","MPI_rcp45","IPSL_rcp85","Fgoals_rcp85")
myrange=c(2:8,11:17)
myFutures=allScenarios[myrange]
#sort by increasing MAT
# Load averages of future climates to order plots
fc <- readRDS(paste(cdir, "Futures_mean_climates.rdata",sep=""))
# Order by MAT
myScenarios <- c("HST", myFutures[match(fc$mod[order(fc$MAT)],myFutures)])



climnames <- sort(c("cwd","djf","jja","ppt")) #use sort to ensure correct names used for predictors

#-------------------#
# Load climate data #
#-------------------#
env.files <- list.files(path=hdir, pattern='.Rdata', full.names=FALSE)
files <- env.files[which(substr(env.files,9,11)%in%climnames)]
predictors <- stack(lapply(files,function(x) readRDS(paste(hdir,x,sep=""))))
names(predictors) = climnames

clim <- getValues(predictors)

# Bay Area climate
sr <- readRDS(paste(bgdir, "GADM_BayArea_proj.rdata",sep=""))

yvars <- c("djf", "jja", "ppt")
xvar <- "cwd"

#----------#
# Plotting #
#----------#

for(j in 1:length(myScenarios)) {
  mod <- myScenarios[j]
  if(mod=="HST") {
    preds=predictors
  } else {
    period <- "2070-2099"
    files <- paste("BCM2014_",climnames,period,"_wy_ave_",mod,".Rdata", sep='')
    preds <- stack(lapply(files,function(x) readRDS(paste(fdir,x,sep=""))))
    names(preds) <- climnames
  }
  srclim <- getValues(mask(crop(preds,sr),sr))
  
  jpeg(paste(figdir, "Clim_space_",j, "_", mod, ".jpg", sep=""),width=700,height=2100,quality=100,res=500,pointsize=8) 
  
  par(mfrow=c(length(yvars),1),mar=c(2,2,0,0), oma=c(2,2,3,1))
  for(y in 1:length(yvars)) {
    plotClim(C=clim,Csub=srclim,fac=c(xvar,yvars[y]),rs=1e4)
    axis(2,ylab=yvars[y])
    mtext(yvars[y], side = 2, line= 2.5,las = 3)
    if(y==length(yvars)) {
      mtext(xvar, side = 1, line= 2.5, cex=0.8)
    }
    if(y==1) {
      mtext(paste(mod), side = 3, line=1.5, cex=0.8)
    }
  }
  
  dev.off()
}

setwd(figdir)
system('"C:\\Program Files\\ImageMagick-6.9.1-Q16\\convert.exe" -delay 80 *.jpg example_1.gif"')
shell("convert -delay 80 *.jpg example_1.gif")


