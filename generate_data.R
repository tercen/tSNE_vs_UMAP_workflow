# Generate worldmap dataset

library(sp)
library(rgdal)  
library(raster) 
library(rworldmap)

getRnSp <- function(n = 1000) {  
  countriesSP <- rworldmap::getMap(resolution='low')
  rn.sp <- spsample(countriesSP, n = n, type = "random")
  
  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(coords = rn.sp, proj4string=CRS(proj4string(countriesSP)))  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(rn.sp, countriesSP)
  
  #continent <- indices$continent   # returns the continent (6 continent model)
  continent <- as.character(indices$REGION)   # returns the continent (7 continent model)
  rn.coord <- rn.sp@coords
  
  return(cbind.data.frame(rn.coord, continent))
}

rn.coord <- getRnSp(1e4)

plot(
  rn.coord$x,
  rn.coord$y,
  col = as.factor(rn.coord$continent),
  pch = 16, cex = 0.4,
  xlab = "Longitude",
  ylab = "Latitude"
)

write.csv(rn.coord, file = "rn.coordinates.csv", quote = FALSE, row.names = FALSE)