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
    c("-m", "--kriging"), 
    type = "character", 
    default = NULL, 
    help = "kriging directory data [default= %default]", 
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
if(is.null(opt$colonia)){
  print_help(opt_parser)
  stop("At least one argument must be supplied (input vgm file).n", call.=FALSE)
}
############################################################
# Debuging
# opt$colonia  <- "../data/Colonias.RData"
# opt$mortality <- "../data/Mortality_cdmx/"
# opt$kriging <- "../data/Kriging/"
# opt$out <- "../results/"
# opt$cores <- 3
options(mc.cores = opt$cores)
options(width = 100)
############################################################
## Read the data
############################################################
## Mortality unification
candidate_files <- list.files(
  opt$mortality, 
  ".csv",
  full.names = TRUE
)
mortality <- lapply(
  candidate_files, 
  function(candidate){
    datum <- read.csv(candidate)
    datum <- datum[-1,]
  }
)
#Name the age specific mortality
age_mortality <- do.call(
    rbind, 
    strsplit(
      split = "mortality_",
      candidate_files
    )
  )[, 2]
age_mortality <- gsub(
  pattern = ".csv",
  replacement = "",
  age_mortality
)
names(mortality) <- age_mortality

## Map coordinates
load(opt$colonia)

## Kriging data
candidate_files <- list.files(
  opt$kriging, 
  ".RData",
  full.names = TRUE
)
kriging <- lapply(
  candidate_files, 
  function(candidate){
    load(candidate)
    return(kriged_data)
  }
)
names(kriging) <- age_mortality
##Total neighbourhood 
# t(t(table(as.character(neighborghood@data$MUN_NAME))))
############################################################
##Format mortality data to plot
############################################################
##mortality to ggplot format
mortality[[2]]$CVE_MUN <- mortality[[2]]$municipio_name
mortality[[2]]$municipio_name <- NULL
mortality <- lapply(
  age_mortality, 
  function(type){
    aux <- mortality[[type]]
    aux$Type <- type
    return(aux)
  }
)
mortality <- do.call(rbind, mortality)
m_mortality <- melt(
  mortality,
  id.vars = c("CVE_MUN", "Type"),
  variable.name = "Year"
)

#Modify the year
m_mortality$Year <- substr(
  as.character(m_mortality$Year),
  start = 2,
  stop = 5
)
m_mortality$Year <- as.integer(m_mortality$Year)

############################################################
##Format kriged data to plot
############################################################
#Time points
times <- as.Date(row.names(as.data.frame(kriging$childhood@time)))
#Coordinates
kriging_coords <- as.data.frame(coordinates(kriging$childhood@sp))
kriging_coords$kriged_index <- 1:nrow(kriging_coords)
neighbourhood_coordinates$neigh_ID <- 1:nrow(neighbourhood_coordinates)
kriging_coords <- merge(kriging_coords, neighbourhood_coordinates, by = c("lon", "lat"))
kriging_coords <- kriging_coords[order(kriging_coords$kriged_index), ]
stopifnot(all(kriging_coords$neigh_ID == kriging_coords$kriged_index)) #same oder
kriging_coords$neigh_ID <- NULL
kriging_coords$kriged_index <- NULL

############################################################################
## The end
############################################################################