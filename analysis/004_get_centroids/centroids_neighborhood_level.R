#############################################
# Make grid to interpolate values
#############################################
# Load packages
install.packages("rgdal", "rgeos", "sp", "sf", "geosphere", "raster")
library(sf)
library(rgeos)
library(raster)
library(geosphere)
library(sp)
library(rgdal)
library(leaflet)
library(mapview)
library(ggplot2)
#############################################
# Upload polygons at neighborghood level
#############################################
## read polygon
pol <- readOGR(".", 
                      "Colonias"
)
pol <- subset(pol, ST_NAME=="DISTRITO FEDERAL")

#############################################
# Get centroids at neighborghood level
#############################################
#With this function, some centroids are outside 
#the polygonos
#centroids_neigh <- gCentroid(
                   # poly,byid = TRUE)

#To obtain centroids (all) inside polygons
#call the function "gCentroidWithin"
#############################################
gCentroidWithin <- function(pol) {
  require(rgeos)
  
  pol$.tmpID <- 1:length(pol)
  # initially create centroid points with gCentroid
  initialCents <- gCentroid(pol, byid = T)
  
  # add data of the polygons to the centroids
  centsDF <- SpatialPointsDataFrame(initialCents, pol@data)
  centsDF$isCentroid <- TRUE
  
  # check whether the centroids are actually INSIDE their polygon
  centsInOwnPoly <- sapply(1:length(pol), function(x) {
    gIntersects(pol[x,], centsDF[x, ])
  })
  # substitue outside centroids with points INSIDE the polygon
  newPoints <- SpatialPointsDataFrame(gPointOnSurface(pol[!centsInOwnPoly, ], 
                                                      byid = T), 
                                      pol@data[!centsInOwnPoly,])
  newPoints$isCentroid <- FALSE
  centsDF <- rbind(centsDF[centsInOwnPoly,], newPoints)
  
  # order the points like their polygon counterpart based on `.tmpID`
  centsDF <- centsDF[order(centsDF$.tmpID),]
  
  # remove `.tmpID` column
  centsDF@data <- centsDF@data[, - which(names(centsDF@data) == ".tmpID")]
  
  cat(paste(length(pol), "polygons;", sum(centsInOwnPoly), "actual centroids;", 
            sum(!centsInOwnPoly), "Points corrected \n"))
  
  return(centsDF)
}

#############################################
# Add centroid coordinates to polygon dataset
#############################################
pol@data <- cbind(pol@data,
                   gCentroidWithin(
                     pol
                   ) %>% 
                     coordinates()
)

# Plot map at neighborghood level
ggplot(pol) +
  geom_polygon(data=pol,
               aes(x = long,
                   y = lat,
                   group = group
               ), 
               fill = "#F17521",
               color = "#1F77B4",
               alpha = .6) +
  geom_point(data = pol@data,
             aes(x = x, 
                 y = y)
             ) 

#############################################
# Create dataframe with Centrois and codes
#############################################
df <- data.frame("lon" = pol@data$x,
                 "lat" = pol@data$y,
                 "MUN_NAME" = pol@data$MUN_NAME,
                 "SETT_NAME" = pol@data$SETT_NAME
                 )

write.csv(df, file = "neighborghood_centroid_coord.csv", row.names = FALSE)