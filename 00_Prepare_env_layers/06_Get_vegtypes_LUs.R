# Script to extract vegetation type frequencies for each landscape unit

rm(list=ls())

# Directories
wdir <- 'C:/Users/Naia Morueta Holme/Documents/Documents_share/Projects/'
bgdir <- paste0(wdir,'100_Postdoc/Data/Background_layers/PROCESSED/')
vdir <- 'D:/Vegetation/PROCESSED/'
vnamedir <- paste0(wdir,'100_Postdoc/Data/Vegetation/ORIGINAL/')
fdir <- 'D:/Lead-trail/Projections/Figures_V4/'

# Load data
# Veg map
veg <- readRDS(paste0(vdir, 'cln_veg10p.rdata'))
# Landscape units
lu <- readRDS(paste(bgdir, "tbc3_landscape_units_p.rdata",sep=""))
# Lookup table for vegtypes
lookup <- read.csv(paste0(vnamedir,'veg10_type.csv'),as.is=T)[,c('ID','VEG_TYPE')]


lunames <- as.character(lu$Name)
lunames <- lunames[which(!is.na(lunames))]

res=list()
for(i in 1:length(lunames)) {
  luName <- lunames[i]
  writeLines(luName)
  slu <- subset(lu,Name==luName)
  subveg <- mask(crop(veg,slu),slu)
  vals <- getValues(subveg)
  r1 <- as.data.frame(sort(table(vals),decreasing=T))
  colnames(r1) <- 'freq'
  r1$vegcode <- row.names(r1)
  row.names(r1) <- NULL
  r1$vegtype <- lookup$VEG_TYPE[match(r1$vegcode,lookup$ID)]
  res[[luName]] <- r1
}

saveRDS(res, paste0(fdir,'Vegtypes_in_each_LU.rdata'))
