#!/usr/bin/env Rscript
############################################################
## Goal: Plot the contaminant station location
############################################################
library("aire.zmvm")
library("optparse")
library("parallel")
library("reshape")
library("ggplot2")
############################################################
option_list <- list(
  make_option(
    c("-c", "--contaminants"), 
    type = "character", 
    default = NULL, 
    help = "dataset file name", 
    metavar = "character"
  ),
  make_option(
    c("-m", "--maps"), 
    type = "character", 
    default = NULL, 
    help = "dataset file name", 
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
if(is.null(opt$contaminants)){
  print_help(opt_parser)
  stop("At least one argument must be supplied (input contaminants).n", call.=FALSE)
}
############################################################
# Debuging
# opt$contaminants <- "../data/contaminants"
# opt$maps <- "../data/09mun"
# opt$out <- "../results/sations.pdf"
options(mc.cores = opt$cores)
############################################################
#Load the contaminants to build stations contaminants matrix
contaminants <- mclapply(
  list.files(opt$contaminants, full.names = TRUE),
  function(contaminant){
    load(contaminant)
    return(unique(contaminant$station_code))
  }
)
names(contaminants) <- gsub(
  pattern = ".RData",
  replacement = "",
  fixed = TRUE,
  list.files(opt$contaminants)
)

#Long format data
contaminants <- do.call(
  rbind, 
  lapply(
    names(contaminants),
    function(contaminant){
      return(
        data.frame(
          Contaminant = contaminant,
          station = contaminants[[contaminant]]
        )
      )  
    }
  )
)

#Add 2019 stations
contaminants <- rbind(
  contaminants,
  data.frame(
    Contaminant = rep(c("O3", "CO", "SO2", "PM25", "PM10"), 2),
    station = c(rep("FAR", 5), rep("SAC", 5))
  )
)

#Merge coordinates:
contaminants <- merge(
  x = contaminants, 
  y = stations[, c("station_code", "lon", "lat")],
  sort = FALSE,
  by.x = "station",
  by.y = "station_code",
  all.x = TRUE
)



