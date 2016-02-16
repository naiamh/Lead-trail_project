
# How to create SDM lead/trail map using 2d color legend

# packages
library(raster)
library(dismo)

# colormap package has functions to generate 2d palettes
#devtools::install_github("matthewkling/colormap")
library(colormap)

# generate some fake maxent predictions for 2 time periods
clim1 <- stack(raster(matrix(1:10000, nrow=100)),
               raster(matrix(1:10000, nrow=100, byrow=T)))
clim2 <- clim1 + 50
pts <- data.frame(x=rnorm(100, .5, .1), y=rnorm(100, .5, .1))
coordinates(pts) <- c("x", "y")
mxargs <- c("hinge=false", "threshold=false", "product=false", "quadratic=true", "linear=true")
mx <- maxent(clim1, pts, args=mxargs)
r1 <- predict(mx, clim1)
r2 <- predict(mx, clim2)
delta <- r2 - r1

# generate colors using 2d palette, baseline suitability vs suitability delta
mycolors <- c("gray50", "green", "black", "red", "white")
cols <- colorwheel2d(cbind(values(r1), values(delta)), colors=mycolors)
rgb_rasters <- stack(r1, r1, r1)
values(rgb_rasters) <- t(col2rgb(cols))

# raster map and scatterplot legend using base graphics
par(mfrow=c(1,2))
plotRGB(rgb_rasters, axes=T,
        main="bioclimate migration map", xlab="lon", ylab="lat")
plot(values(r1), values(delta), col=cols, 
     main="legend", xlab="suitability at t1", ylab="suitability change, t1 to t2")
abline(0,0)

# or using ggplot
library(ggplot2)
library(gridExtra)
library(grid)
d <- as.data.frame(rasterToPoints(stack(r1, r2, delta)))
names(d)[3:5] <- c("t1", "t2", "delta")
map <- ggplot() + 
      geom_raster(data=d, aes(x, y), fill=cols) +
      labs(title="bioclimate migration map", x="lon", y="lat") +
      theme_minimal()
legend <- ggplot() + 
      geom_point(data=d, aes(t1, delta), color=cols) +
      geom_hline(yintercept=0) +
      labs(title="legend", x="suitability at t1", y="suitability change, t1 to t2") +
      theme_minimal()
dev.off()
grid.draw(arrangeGrob(map, legend, ncol=2))
