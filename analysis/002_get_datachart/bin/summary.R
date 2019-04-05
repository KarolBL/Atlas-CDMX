#!/usr/bin/env Rscript
############################################################
## Goal: Summary the data using a 75% criteria
############################################################
library("aire.zmvm")
library("optparse")
library("parallel")
options(mc.cores = 7)
############################################################
option_list <- list(
  make_option(
    c("-f", "--file"), 
    type = "character", 
    default = NULL, 
    help = "dataset file name", 
    metavar = "character"),
  make_option(
    c("-o", "--out"), 
    type = "character", 
    default = "out.txt", 
    help = "output file name [default= %default]", 
    metavar = "character"
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
# opt$file <- "../data/CO.RData"
# opt$out <- "../results/CO.RData"
############################################################
#Filtering criteria
# 1.- Average day if more than 18 hours (75%).
min_hours <- 18
# 2.- Average week if more than 5.25 days (75%).
# 3.- Average month if more than 22 days (75%).
# 4.- Average trimester if more than 67.5 days (75%).
# 5.- Average semester if more than 135 days (75%).
# 6.- Average year if more than 273.75 days (75%).
############################################################
#Load the contaminant data.frame object
load(opt$file)

#Remove NA data points
contaminant <- na.omit(contaminant)
row.names(contaminant) <- NULL

#Idea: summary acording to the station_code the data i. e., hours -> day -> week
#For each station
contaminant_day <- lapply(
  unique(contaminant$station_code), 
  function(station){
    datum <- subset(
      contaminant,
      station_code == station
    )
    
    #For each date 
    datum_day <- mclapply(
      unique(datum$date), 
      function(day){
        datapoints <- subset(
          datum,
          date == day
        )
        
        ##Check day constraint
        out <- ifelse(
          nrow(datapoints) < min_hours, 
          NA,
          mean(datapoints$value)
        )
        
        datapoints <- datapoints[1, , drop = FALSE]
        datapoints$hour <- NULL
        datapoints$value <- out
          
        return(datapoints)
      }
    )
    #Join the days 
    datum_day <- do.call(rbind, datum_day)
    datum_day <- na.omit(datum_day)
    
    return(datum_day)
  }
)

#Summary by week  
contaminant_day

