#############################################
# Make grid to interpolate values
#############################################
# Load packages
#install.packages("rgdal", "rgeos", "sp", "sf", "geosphere", "raster")
library("sf")
library("rgeos")
library("raster")
library("geosphere")
library("sp")
library("rgdal")
library("leaflet")
library("mapview")
library("ggplot2")
# http://mazamascience.com/WorkingWithData/?p=1494
#############################################
# Upload polygons at neighborghood level
#############################################
## read polygon
#
setwd("~/Dropbox/inmegen/DAtos/Karol/Atlas-CDMX/Data/Shapefiles/Colonias")
pol <- readOGR(".")
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
  require("rgeos")
  
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
  if(nrow(pol[!centsInOwnPoly, ])>0){
    newPoints <- SpatialPointsDataFrame(gPointOnSurface(pol[!centsInOwnPoly, ], 
                                                      byid = T), 
                                      pol@data[!centsInOwnPoly,])
    newPoints$isCentroid <- FALSE
    centsDF <- rbind(centsDF[centsInOwnPoly,], newPoints)
  }
  
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

#############################################
pol@data$fill <- 1:nrow(pol@data)

tlalpan <- subset(pol, MUN_NAME == "TLALPAN")
pol <- tlalpan
pol <- gUnaryUnion(pol)

coordinates(gCentroid(pol))

plot(pol)
plot(tlalpan)

SpatialPointsDataFrame

aux <- SpatialPointsDataFrame(coordinates(pol), tlalpan@data[1,,drop=FALSE])
plot(aux)

class(tlalpan)
class(pol)
SpatialPolygons
SpatialPolygonsDataFrame
?SpatialPolygonsDataFrame
aa <- SpatialPolygonsDataFrame(pol, data.frame(test=1, id=1))
plot(aa)
gCentroidWithin(aa)
points(coordinates(aa))


#create a data.frame from our spatial object
pol2 <- fortify(pol, region = "OBJECTID")
# merge the "fortified" data with the data from our spatial object
pol2 <- merge(pol2, pol@data, by.y = "OBJECTID", by.x = "id")

#gUnion

# Plot map at neighborghood level
p <- ggplot(
  data = pol2,
  aes(
    x = long,
    y = lat,
    group = id#,
    #fill = MUN_NAME
  )
) +
  geom_polygon(
   alpha = .6
  ) +
  geom_path(color = "white")+
  #geom_point() + #los del polígono
  coord_equal() +
  xlab("Longitude")+
  ylab("Latitud")+
  theme_bw()+
  theme(
    legend.position = "bottom" 
    #title = element_blank()
    #axis.text = element_blank()
  )
p

#############################################
# Create dataframe with Centrois and codes
#############################################
df <- data.frame(
  "lon" = pol@data$x,
  "lat" = pol@data$y,
  "MUN_NAME" = pol@data$MUN_NAME,
  "SETT_NAME" = pol@data$SETT_NAME
)

write.csv(df, file = "neighborghood_centroid_coord.csv", row.names = FALSE)

