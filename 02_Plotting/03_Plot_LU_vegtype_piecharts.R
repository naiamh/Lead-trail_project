# Script to create piecharts of the most dominant vegetation types in each landscape unit

rm(list=ls())

wdir <- 'C:/Users/Naia Morueta Holme/Documents/Documents_share/Projects/'
idir <- 'D:/Lead-trail/Projections/Figures_V4/'
vnamedir <- paste0(wdir,'100_Postdoc/Data/Vegetation/ORIGINAL/')
odir <- 'D:/Lead-trail/Projections/Figures_V4/Vegpies_each_LU/'

allv <- readRDS(paste0(idir,'Vegtypes_in_each_LU.rdata'))

# Which vegtypes should be lumped together?
# Lookup table for vegtypes
lookup <- read.csv(paste0(vnamedir,'veg10_type.csv'),as.is=T)[,c('ID','VEG_TYPE')]

lumplookup <- do.call(rbind,list(c('Hot Grasslands','Grasslands'),
                                 c('Moderate Grasslands','Grasslands'),
                                 c('Cool Grasslands','Grasslands'),
                                 c('Native Grassland','Grasslands'),
                                 c('Non-native/Ornamental Grass','Grasslands'),
                                 c('Serpentine Grassland','Grasslands'),
                                 c('Warm Grasslands','Grasslands'),
                                 c('Urban','Urban or Residential'),
                                 c('Rural Residential', 'Urban or Residential')))

allvtypes <- sort(unique(as.character(sapply(lookup$VEG_TYPE, function(x) {ifelse(x%in%lumplookup[,1], lumplookup[which(lumplookup%in%x),2], x)}))))
# add break to long names
allvtypes[which(allvtypes=='Coastal Salt Marsh / Coastal Brackish Marsh')] <- 'Coastal Salt Marsh / Coastal\nBrackish Marsh'
allvtypes[which(allvtypes=='Non-Native Ornamental Conifer-Hardwood Mixture')] <- 'Non-Native Ornamental\nConifer-Hardwood Mixture'


colordf <- c('#4F4D50', '#FF7D7D', '#38A607', '#74DFFF', '#02AAE8', '#01744D', '#02A789',
             '#FF75E4','#A86E02', '#A4FF74', '#FFBDBE', '#9BD5D9', '#E0DEEB', '#495098',
             '#E79901', '#01859C', '#CBA96C', '#5AA48B', '#789636', '#A8FF00', '#ADA20A',
             '#C7F6FE', '#AC007A' ,'#9664DF', '#695000', '#4A7003', '#AB3801', '#69ADA0',
             '#C49CDA', '#F9EABF', '#A0FF7D', '#2FB601', '#795233', '#A47500', '#F6E65F',
             '#A9D4F7', '#00AB87' ,'#ED6D22' ,'#1145AA' ,'#C4B666' ,'#E2D89F', '#E26CE6',
             '#FFB699', '#EBFFB7','#D1FD6C', '#73A802' ,'#ABA801', '#C8D6A1' ,'#939881',
             '#FD74DE' ,'#FDBEE9', '#737400' ,'#686868', '#C0FFEC', '#7675FF', '#BFE8FE') 

collookup <- data.frame(Vegtype=c(allvtypes,'Other'),hcol=c(colordf, '#F6E6E9'))


allv2 <- lapply(allv, function(x) {
  res <- x[,c('freq','vegtype')]
  res$vegtype2 <- sapply(res$vegtype, function(a) {ifelse(a%in%lumplookup[,1], lumplookup[which(lumplookup%in%a),2], a)})
  res2 <- aggregate(res$freq, by=list(Vegtype=res$vegtype2), FUN=sum)
  colnames(res2) <- c('Vegtype','freq')
  res2 <- res2[rev(order(res2$freq)),]
  res2$perc <- res2$freq/sum(res$freq)
  res2$cumperc <- cumsum(res2$perc)
  rownames(res2) <- NULL
  return(res2)
})

# Set cummulative threshold for plotting pie chart
cumth <- 0.95

i=32

par(mar=c(0,3,0,3), oma=c(0,0,0,0))
for(i in 1:length(allv2)) {
  print(i)
  if(allv2[[i]]$cumperc[1] < cumth) {
    d <- allv2[[i]][which(round(allv2[[i]]$cumperc,2) <= cumth),]
  } else {
    d <- allv2[[i]][1,]
  }
  
  tot <- sum(allv2[[i]]$freq)
  d <- rbind(d, c('Other',tot-sum(d$freq),1-sum(d$perc),1))
  d$freq <- as.numeric(d$freq)
  d$perc <- as.numeric(d$perc)
  d$cumperc <- as.numeric(d$cumperc)
  d$color <- as.character(collookup$hcol[match(d$Vegtype,collookup$Vegtype)])
  
  figfile <- paste0(odir,names(allv2)[i], '_current_veg_types.png')
  png(figfile, width=600, height=500)
  pie(d$freq,labels=d$Vegtype, clockwise=T, cex=0.75,col=d$color)
  dev.off()
}
