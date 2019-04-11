#!/usr/bin/env Rscript
############################################################
## Goal: Measure the data missingness 
############################################################
library("aire.zmvm")
library("optparse")
library("parallel")
############################################################
option_list <- list(
  make_option(
    c("-f", "--file"), 
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
    default = "1", 
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
# opt$file <- "../data/O3.RData"
# opt$out <- "../results/O3.RData"
options(mc.cores = opt$cores)
############################################################
# For each contaminant, summary de data points available to 
# establish data missingness, on a week basis. Moreover, the
# data content will be scaled according to the theorical 
# total, e. g.,
# 
# ```
# Day 1 , 3 hours only  
# Day 2 , 2 hours only
# ------------
# This week = 5 data points / (24 hours times seven days) * 100%
# ```
# 
# Week data will be considered by dividing days by 7. It will
# start from year-01-01 and labelling it according to the
# consecutive week number.
############################################################
#Load the contaminant data.frame object
load(opt$file)

if( "metereological" %in% ls()){
  contaminant <- metereological
  rm(metereological)
}

if( "radiation_all" %in% ls()){
  contaminant <- radiation_all
  rm(radiation_all)
}


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
          nrow(datapoints) == 0, 
          NA,
          length(datapoints$value)
        )
        
        #Format output
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
#For each station
contaminant_week <- lapply(
  contaminant_day,
  function(station){
    #Get the julian day / 7 + 1, to get the week number
    station$week <- as.integer(
      strftime(
        station$date, 
        format = "%j"
      )
    )%/%7+1
    station$week[station$week == 53] <- 52
    
    #Get the year
    station$year <- as.integer(
      strftime(
        station$date, 
        format = "%Y"
      )
    )
    
    #For each year
    station_year <- lapply(
      unique(station$year),
      function(year_number){
        #For each week under a year
        station_week <- lapply(
          unique(station$week),
          function(week_number){
            datapoints <- subset(
              station,
              week == week_number &
              year == year_number
            )
            
            ##Check day constraint
            out <- ifelse(
              nrow(datapoints) == 0 , 
              NA,
              sum(datapoints$value)
            )
            
            #Format the output
            datapoints <- datapoints[1, , drop = FALSE]
            datapoints$value <- out
            
            return(datapoints)
          }
        )
        
        #Join the weeks 
        station_week <- do.call(rbind, station_week)
        station_week <- na.omit(station_week)
        
        return(station_week)
      }
    )
    
    #Join the years 
    station_year <- do.call(rbind, station_year)
    station_year <- na.omit(station_year)

    return(station_year)    
  }
)

##Join contaminat missingness by week
contaminant_week <- do.call(rbind, contaminant_week)
contaminant_week <- na.omit(contaminant_week)
contaminant <- contaminant_week

############################################################
#Save the data
save(contaminant, file = opt$out, compress = "xz")

