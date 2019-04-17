#!/usr/bin/env Rscript
#############################################
# Required libraries
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
library("ggspatial")
library("optparse")
library("parallel")
# http://mazamascience.com/WorkingWithData/?p=1494
# Make grid to interpolate values
#############################################
option_list <- list(
  make_option(
    c("-d", "--directory"), 
    type = "character", 
    default = NULL, 
    help = "shapefile directory name", 
    metavar = "character"
  ),
  make_option(
    c("-r", "--coordn"), 
    type = "character", 
    default = NULL, 
    help = "neighborghood coordinates output file", 
    metavar = "character"
  ),
  make_option(
    c("-p", "--pdf"), 
    type = "character", 
    default = NULL, 
    help = "CDMX centroids graphical output file", 
    metavar = "character"
  ),
  make_option(
    c("-o", "--out"), 
    type = "character", 
    default = "out.RData", 
    help = "output RData file name [default= %default]", 
    metavar = "character"
  ),
  make_option(
    c("-c", "--cores"), 
    type = "integer", 
    default = "7", 
    help = "number of cores to use [default= %default]", 
    metavar = "integer"
  )
) 

#Build the parse object
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

##Check for parsing options
if(is.null(opt$directory)){
  print_help(opt_parser)
  stop("At least one argument must be supplied (input directory).n", call.=FALSE)
}
############################################################
# Debuging
# opt$directory <- "../data/Colonias"
# opt$coordn <- "../results/neighborghood_centroid_coord.csv"
# opt$pdf <- "../results/CDMX_centroids.pdf"
# opt$out <- "../results/Colonia.RData"
options(mc.cores = opt$cores)
############################################################
## Loading Shapefiles at neighborghood level
###########################################################
neighborghood <- readOGR(opt$directory)
#Select on DF polygons
neighborghood <- subset(
  neighborghood, 
  ST_NAME == "DISTRITO FEDERAL"
)
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
neighborghood@data <- cbind(neighborghood@data,
                   gCentroidWithin(
                     neighborghood
                   ) %>% 
                     coordinates()
)

#############################################
# Create dataframe with Centrois and codes
#############################################
df <- data.frame(
  "lon" = neighborghood@data$x,
  "lat" = neighborghood@data$y,
  "MUN_NAME" = neighborghood@data$MUN_NAME,
  "SETT_NAME" = neighborghood@data$SETT_NAME
)

write.csv(df, file = opt$coordn, row.names = FALSE)

#############################################
# Join neighborghoods to create area matching
# boroughs
#############################################
#Get the MUN_NAME
boroughs <- as.character(unique(neighborghood@data$MUN_NAME))

#Get the neighborghood polygons of each borough
join_neigh <-lapply(
  boroughs,
  function(borough){
    datum <- subset(neighborghood,
                    MUN_NAME == borough
                    )
    datum <- gUnaryUnion(datum) 
    return(datum)
    }
)

#Join the boroughs polygons 
boroughs <- do.call(
  rbind,
  lapply(
    1:length(join_neigh), 
    function(id){
      SpatialPolygonsDataFrame(
        join_neigh[[id]], 
        data.frame(
          ID = id,
          MUN_NAME = boroughs[id]
        )
      )
    }
  )
)

#############################################
# Add centroid coordinates to polygon dataset
#############################################
boroughs@data <- cbind(
  boroughs@data,
  gCentroidWithin(boroughs) %>% coordinates()
)

#############################################
#create a data.frame from our spatial object
#############################################
# neighborghood level
df_neigh <- fortify(neighborghood, region = "OBJECTID")
# merge the "fortified" data with the data from our spatial object
df_neigh <- merge(df_neigh, neighborghood@data, by.y = "OBJECTID", by.x = "id")

# borough level
df_borough <- fortify(boroughs, region = "ID")
df_borough <- merge(df_borough, boroughs@data, by.x = "id", by.y = "ID")

#############################################
# Plot map at boroigh level
#############################################
p_boroughs <- ggplot(
  #data = df_neigh,
  data = df_borough,
  aes(
    x = long,
    y = lat,
    group = id
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
#p_boroughs

#############################################
##Plot jointly borough + neighbourhood the graphs
#############################################
p_complete <- ggplot()+
  geom_polygon(
    data = df_neigh,
    aes(
      x = long,
      y = lat,
      group = id
    ),
    alpha = .6
  ) +
  geom_path(
    data = df_neigh,
    aes(
      x = long,
      y = lat,
      group = group
    ),
    color = "white"
  ) + 
  geom_path(
    data = df_borough,
    aes(
      x = long,
      y = lat,
      group = group
    ),
    color = "blue"
  ) + 
  geom_point(
    data = boroughs@data,
    aes(
      x = x,
      y = y
    ),
    color = "red"
  ) + 
  coord_equal() +
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+
  annotation_north_arrow(
    location = "bl", 
    which_north = "TRUE", 
    pad_x = unit(0.3, "in"), 
    pad_y = unit(0.4, "in"),
    style = north_arrow_fancy_orienteering
  )+
  annotation_scale()+
  coord_sf(crs = 4326)+
  theme(
    panel.grid = element_line(colour = "transparent")
  )
p_complete 

ggsave(
  p_complete,
  file = opt$pdf,
  width = 5,
  height = 5,
  device = cairo_pdf
)
