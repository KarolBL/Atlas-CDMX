#!/usr/bin/env Rscript
############################################################
## Goal: Obtain the kriging plots
############################################################
library("sf")
library("rgeos")
library("raster")
library("geosphere")
library("sp")
library("rgdal")
library("leaflet")
library("mapview")
library("ggplot2")
library("reshape2")
library("cowplot")
library("ggspatial")
library("optparse")
library("parallel")
#Quien es el representante para darle los permisos siguientes
############################################################
option_list <- list(
  make_option(
    c("-o", "--colonia"), 
    type = "character", 
    default = NULL, 
    help = "colonia data points for kriged data [default= %default]", 
    metavar = "character"
  ),
  make_option(
    c("-m", "--mortality"), 
    type = "character", 
    default = NULL, 
    help = "mortality directory data [default= %default]", 
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
if(is.null(opt$vgm)){
  print_help(opt_parser)
  stop("At least one argument must be supplied (input vgm file).n", call.=FALSE)
}
############################################################
# Debuging
# opt$colonia  <- "../data/Colonias.RData"
# opt$mortality <- "../data/Mortality_cdmx"
# opt$out <- "../results/mortality_general.RData"
# opt$cores <- 3
options(mc.cores = opt$cores)
options(width = 150)
############################################################
## Read the data
#Mortality unification
candidate_files <- list.files(
  opt$mortality, 
  ".csv",
  full.names = TRUE
)
mortality <- lapply(
  candidate_files, 
  function(candidate){
    datum <- read.csv(opt$colonia)
  }
)
#Remove total
mortality <- mortality[-1,]

#Coordinates
neighborghood <- read.csv(opt$coordinates)
borough <- read.csv(opt$centroid)

#bestmodel2use
load(opt$vgm)
attr(bestmodel2use, "spatial unit") <- ""

############################################################
#Format Data for Kriging 

##Original data
# Create the SP object of the original data
centroids <- borough[, c("CVE_MUN", "lon", "lat")]
row.names(centroids) <- borough$NOMGEO
#Transform into coordinates
coordinates(centroids) <- ~ lon + lat
centroids <- SpatialPoints(
  centroids, 
  proj4string = CRS("+proj=longlat +datum=WGS84")
)

# Create the STFDF object of the original data
mortality_stfdf <- STFDF(
  sp = centroids,
  time = as.Date(paste(2000:2016, "-01-01", sep = "")),
  data = melt(
    mortality[, names(mortality) %in% paste("X", 2000:2016, sep = "")]
  )[, -1, drop = FALSE]
)

#New data############################################################
#Generate neighbourhood coordinates
coordinates(neighborghood) <- ~ lon + lat
neighborghood <- SpatialPoints(
  neighborghood, 
  proj4string = CRS("+proj=longlat +datum=WGS84")
)

#Time period by trimesters 
times <- as.Date(
  do.call(
    c,
    lapply(
      2000:2016, 
      function(year){
        paste(
          year, 
          c("-01-01", "-04-01", "-07-01", "-10-01"), 
          sep = ""
        )    
      }
    )
  )
)

#Finally the new data object
new_data <- STF(
  sp = neighborghood,
  time = times
)

#Finally the kriging
kriged_data <- krigeST(
  formula = value ~ 1,
  data = mortality_stfdf,
  #nmax = 100,
  newdata = new_data,
  modelList = bestmodel2use,
  computeVar = TRUE,
  progress = FALSE
)

############################################################
save(
  kriged_data,
  file = opt$out,
  compress = "xz"
)
############################################################################
## The end
############################################################################
