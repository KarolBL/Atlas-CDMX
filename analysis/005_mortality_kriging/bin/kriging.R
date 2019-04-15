#!/usr/bin/env Rscript
############################################################
## Goal: Obtain the kriging estimations
############################################################
library("optparse")
library("parallel")
library("gstat")
library("sp")
library("spacetime")
library("gtools")
library("reshape2")
############################################################
option_list <- list(
  make_option(
    c("-v", "--vgm"), 
    type = "character", 
    default = NULL, 
    help = "variogram file name", 
    metavar = "character"
  ),
  make_option(
    c("-d", "--data"), 
    type = "character", 
    default = NULL, 
    help = "original data points for kriging data [default= %default]", 
    metavar = "character"
  ),
  make_option(
    c("-e", "--coordinates"), 
    type = "character", 
    default = "coordinates.csv", 
    help = "coordinates for kriging data [default= %default]", 
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
if(is.null(opt$file)){
  print_help(opt_parser)
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}
############################################################
# Debuging
# opt$vgm  <- "../data/best_variogram/mortality_general.RData"
# opt$data <- "../data/Mortality_cdmx/mortality_general.csv"
# opt$coordinates <- "../data/Centroids/neighborghood_centroid_coord.csv"
# opt$out <- "../results/mortality_general.RData"
# opt$cores <- 3
options(mc.cores = opt$cores)
options(width = 150)
############################################################
## Read the data
#Mortality
mortality <- read.csv(opt$data)
#Remove total
mortality <- mortality[-1,]

#Coordinates
neighborghood <- read.csv(opt$coordinates)

#bestmodel2use
load(opt$vgm)
# attr(bestmodel2use, "spatial unit") <- ""

############################################################
#Format Data for Kriging 



############################################################



############################################################################
## The end
############################################################################
