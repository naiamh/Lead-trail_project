# Get mean MAT for future scenarios for sorting plots by increasing temperature
# and mean PPT to order by warm-dry, warm-wet, hot-dry, hot-wet
# Updated February 12 2016

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
  cdir <- 'E:/BCM/CA_2014/Summary/Futures/Normals_30years/'
  hdir <- 'E:/BCM/CA_2014/Summary/HST/Normals_30years/'
  odir <- 'E:/BCM/CA_2014/Summary/Futures/'
  bgdir <- paste0(wdir,'100_Postdoc/Data/Background_layers/PROCESSED/')
}

# Libraries
require(raster)

# Parameters
allScenarios <- c("HST", "GFDL_B1","GFDL_A2","PCM_A2","CNRM_rcp85","CCSM4_rcp85","MIROC_rcp85", 
                  "PCM_B1","MIROC3_2_A2","csiro_A1B","GISS_AOM_A1B","MIROC5_rcp26","MIROC_rcp45",
                  "MIROC_rcp60","GISS_rcp26","MRI_rcp26","MPI_rcp45","IPSL_rcp85","Fgoals_rcp85")
myrange=c(2:19)
climnames <- c('cwd','djf','jja','ppt','tmn','tmx')
#climnames <- c('ppt','tmn','tmx')
period <- "2070-2099"

# Background Bay Area map
bg <- readRDS(paste(bgdir, "GADM_BayArea_proj.rdata",sep=""))

# Mean values of historic climate
hst <- stack(lapply(climnames, function(x) readRDS(paste0(hdir,'BCM2014_',x,'1951-1980_wy_ave_HST.Rdata'))))
names(hst) <- climnames
  # clip to Bay Area
  hst <- mask(crop(hst,bg),bg)
res1 <- cellStats(hst,'mean')
res1['MAT'] <- mean(c(res1['tmn'],res1['tmx']))


# Mean values of all future scenarios
res = list()
for(j in myrange) {
  mod <- allScenarios[j]
  print(mod)
  files <- paste("BCM2014_",climnames,period,"_wy_ave_",mod,".Rdata", sep='')
  preds <- stack(lapply(files,function(x) readRDS(paste(cdir,x,sep=""))))
  names(preds) <- climnames
  preds <- mask(crop(preds,bg),bg)

  res[[j]]=c(cellStats(preds,"mean"))
}
fc = do.call(rbind.data.frame, res)
colnames(fc) = names(res[[2]])

fc = cbind(mod=allScenarios[myrange],fc)
fc$mod = as.character(fc$mod)
fc$MAT <- rowMeans(fc[,c("tmn","tmx")])

# saveRDS(fc, paste(odir, "Futures_all_mean_climates_Bay_Area.rdata",sep=""))


# png(paste0(odir,"Scenarios_ordered_ppt-mat.png"), pointsize=24, height=800, width=800)
# plot(fc$ppt, fc$MAT, xlab='Annual precipitation', ylab='Mean annual temperature', 
#      xlim=c(400,820), pch=4, col='red')
# text(fc$ppt, fc$MAT, labels=fc$mod, cex=.5)
# abline(v=res1['ppt'])
# abline(h=mean(fc$MAT))
# dev.off()
# 

# Plot difference of each scenario from historic conditions
png(paste0(odir,"Scenarios_Bay_Area_delta_hst.png"), pointsize=24, height=800, width=800)
plot(fc$MAT-res1["MAT"], (fc$ppt-res1["ppt"])/res1["ppt"]*100, xlab='Temperature change (C)', ylab='Precipitation change (%)', 
      pch=4, col='red', xlim=c(0,7))
text(fc$MAT-res1["MAT"], (fc$ppt-res1["ppt"])/res1["ppt"]*100, labels=fc$mod, cex=.5)
abline(v=2.5)
abline(h=0)
dev.off()

