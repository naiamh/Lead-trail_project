# Script to get CCH species location data. Current version simply adjusts csv files
# acquired from Andrew Thornhill October 27 2015. Records have been flagged and cleaned.

rm(list=ls())

#-----------------#
# Set directories #
#-----------------#
idir <- 'D:/Phylo_modelling/Data/Species/Clean_CCH/'
odir <- 'D:/Phylo_modelling/Data/Species/Processed2/'


#---------------------------#
# Prepare occurrence tables #
#---------------------------#
files <- list.files(path=idir, pattern='.csv', full.names=T)
cols <- c('id','current_name_binomial', 'latitude', 'longitude', 'exclude')

df <- do.call("rbind", lapply(files, FUN=function(x){
  f <- read.csv(x, as.is=T)[,cols]
  return(f)
}))

# clean occurrences
df_clean <- subset(df, !is.na(latitude) & !is.na(longitude) & (exclude != 'yes'| is.na(exclude)))
row.names(df_clean) <- NULL
splist_clean <- unique(df_clean$current_name_binomial)
#saveRDS(splist_clean, file=paste(outspdir,'0_Species_list.rdata',sep=''))

#For each species, save dataframe with occurrences
for(i in 1:length(splist_clean)) {
  species <- splist_clean[i]
  occur_raw <- subset(df_clean, current_name_binomial==species)
  occur <- subset(occur_raw, !is.na(latitude) & !is.na(longitude))
  row.names(occur) <- NULL
  
  saveRDS(occur, file=paste(odir,species,'.rdata',sep=''))
  print(i)
}

