#############################################
# Get centroids at borough level
#############################################
library(rgeos)
library(sf)
library(rgdal)
library(raster)
library(geosphere)
library(mapview)
#############################################
# Upload polygons at borough level
#############################################
polygons_boroughs <- readOGR(
                             dsn = ".", 
                             layer = "09mun"
                             )

#############################################
# Funtion to obtain polygon centroids
#############################################
centroids_boroughs <- gCentroid(
                      polygons_boroughs,
                      byid = TRUE
                      )

#############################################
# Save centroids as coordinates
#############################################
class(centroids_boroughs)
#[1] "SpatialPoints"
# attr(,"package")
# [1] "sp"

# head(coordinates(centroids_boroughs))
# x        y
# 0 2794929 834774.6
# 1 2798597 817331.1
# 2 2781800 816781.0
# 3 2801821 836978.9

#Transform coordinates to the CRS
projection(centroids_boroughs)
#[1] "+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +ellps=GRS80 +units=m +no_defs"

centroids_lonlat <- spTransform(
                    centroids_boroughs, 
                    CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
                    ) 

# head(centroids_lonlat)
# class       : SpatialPoints 
# features    : 1 
# extent      : -99.18211, -99.18211, 19.48533, 19.48533  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0 
head(coordinates(centroids_lonlat))
#           x        y
# 0 -99.18211 19.48533
# 1 -99.15038 19.32667
# 2 -99.31074 19.32462

write.csv(coordinates(centroids_lonlat), file = "borough_centroid_coord.csv")

mapview(centroids_lonlat)
#############################################
# Interactive map of polygons + centroids
#############################################
mapview(polygons_boroughs) + 
  mapview(centroids_boroughs)
