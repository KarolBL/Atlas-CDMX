#!/usr/bin/env Rscript
############################################################
## Goal: Summary the data using a 75% criteria
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
# opt$file <- "../data/TMP.RData"
# opt$out <- "../results/TMP.RData"
options(mc.cores = opt$cores)
############################################################
#Filtering criteria
# 1.- Average day if more than 18 hours (75%).
min_hours <- 17 # To consider 18 data points
# 2.- Average week if more than 5.25 days (75%).
min_week <- 4   # To consider 5 data points
# 3.- Average month if more than 22 days (75%).
# 4.- Average trimester if more than 67.5 days (75%).
# 5.- Average semester if more than 135 days (75%).
# 6.- Average year if more than 273.75 days (75%).
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
          nrow(datapoints) < min_hours, 
          NA,
          mean(datapoints$value)
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
              nrow(datapoints) < min_week, 
              NA,
              mean(datapoints$value)
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

##Join contaminat summarized by week
contaminant_week <- do.call(rbind, contaminant_week)
contaminant_week <- na.omit(contaminant_week)
contaminant <- contaminant_week

############################################################
#Save the data
save(contaminant, file = opt$out, compress = "xz")

