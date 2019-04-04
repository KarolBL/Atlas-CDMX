############################################################
## Goal: Get station location
############################################################
library("aire.zmvm")
library("optparse")
library("parallel")
options(mc.cores = 7)
############################################################
## Stations data
############################################################
#dim(stations)
#[1] 68  7
#dim(stations[stations$comment == "",]) #Some ended gathering data
# [1] 48  7
write.table(
  stations, 
  file = "../results/stations.tab",
  sep="\t",
  row.names = FALSE
)
############################################################
## Zones
############################################################
dim(zones)
#[1] 36  6
write.table(
  zones, 
  file = "../results/zones.tab",
  sep="\t",
  row.names = FALSE
)
############################################################
## Available data
############################################################
# - Download Pollution Archives
#?download_pollution(year, progress = interactive) from 2009 to 2018
#contaminants <- c("CO", "NO", "NO2", "NOX", "O3", "PM10", 
#                  "SO2", "PM25", "PMCO")
years <- 2009:2018
test <- download_pollution(2009, progress = TRUE)
# head(test)
#         date hour station_code pollutant unit value
# 1 2009-01-01    1          LAG        CO  ppm   0.6
# 2 2009-01-01    1          TAC        CO  ppm   1.2
# 3 2009-01-01    1          FAC        CO  ppm   0.5
table(test$pollutant)
#     CO     NO    NO2    NOX     O3   PM10   PM25    SO2 
# 157680 157680 157680 157680 192720 131400  78840 227760

#Get all the contaminants per year
pollution <- mclapply(
  years,
  download_pollution,
  progress = TRUE
)
names(pollution) <- as.character(years)
do.call(rbind, lapply(pollution, dim))
#         [,1] [,2]
# 2009 1261440    6
# 2010 1217640    6
# 2011 1445400    6
# 2012 1722000    6
# 2013 1808280    6
# 2014 1838856    6
# 2015 1994736    6
# 2016 2114088    6
# 2017 2111160    6
# 2018 2111160    6

#Join all the years 
pollution_all <- do.call(rbind, pollution)
table(pollution_all$pollutant)
#     CO      NO     NO2     NOX      O3    PM10    PM25    PMCO     SO2 
# 2207688 2272416 2272416 2272416 2476488 1687680 1296576  705240 2433840

#Split them by contaminant
contaminats <- lapply(
  unique(pollution_all$pollutant),
  function(contaminat){
    subset(
      pollution_all,
      pollutant == contaminat
    )    
  }
)
names(contaminats) <- unique(pollution_all$pollutant)
save(contaminats, file = "../results/contaminants.RData", compress = "xz")

#Split the contaminants in separate archives
mclapply(
  names(contaminats),
  function(x){
    contaminant <- contaminats[[x]]
    save(contaminant, file = paste("../results/", x, ".RData", sep = ""), compress = "xz")    
  }
)

##################################################################################################
# - Download Ultraviolet Radiation Archives
#download_radiation(type, year, progress = interactive())
#?download_radiation

#Get all the radiation per year
years <- 2000:2018
"UVA" #long wave ultraviolet A
"UVB" # short wave ultraviolet B
get_radiation <- function(type, years){
  radiation <- mclapply(
    years,
    download_radiation,
    type = type,
    progress = TRUE
  )
  names(radiation) <- as.character(years)
  radiation_all <- do.call(rbind, radiation)
  #table(radiation$pollutant)
  save(radiation_all, file = paste("../results/", type, ".RData", sep = ""), compress = "xz")
  return(radiation_all)
}

#Finally, get the data
radiation <- mclapply(
  c("UVA", "UVB"),
  get_radiation,
  years
)
names(radiation) <- c("UVA", "UVB")
do.call(rbind, lapply(radiation, dim))
#        [,1] [,2]
# UVA 1174704    6
# UVB 1122096    6

######################################################################
# - Download Meteorological Data Archives
#?download_meteorological	
years <- 2000:2018
test <- download_meteorological(2000, progress = TRUE)
metereological <- mclapply(
  years,
  download_meteorological,
  progress = TRUE
)
names(metereological) <- as.character(years)
do.call(rbind, lapply(metereological, dim))
#        [,1] [,2]
# 2000 386496    6
# 2001 490560    6
# 2002 490560    6
# 2003 490560    6
# 2004 500688    6
# 2005 508080    6
# 2006 490560    6
# 2007 490560    6
# 2008 535824    6
# 2009 551880    6
# 2010 551880    6
# 2011 551880    6
# 2012 626880    6
# 2013 639480    6
# 2014 639480    6
# 2015 754728    6
# 2016 920496    6
# 2017 919800    6
# 2018 919800    6

#Join all the years 
metereological_all <- do.call(rbind, metereological)
table(metereological_all$pollutant)
#   PBa      RH     TMP     WDR     WSP 
# 87648 2931936 2826912 2806848 2806848

#Split them by metereological
metereologicals <- lapply(
  unique(metereological_all$pollutant),
  function(metereological){
    subset(
      metereological_all,
      pollutant == metereological
    )    
  }
)
names(metereologicals) <- unique(metereological_all$pollutant)
save(metereologicals, file = "../results/metereologicals.RData", compress = "xz")

#Split the metereologicals in separate archives
mclapply(
  names(metereologicals),
  function(x){
    metereological <- metereologicals[[x]]
    save(metereological, file = paste("../results/", x, ".RData", sep = ""), compress = "xz")    
  }
)

# - Download Atmospheric Pressure Archives
#?download_pressure	

# - Download Acid Rain Measurements Archives
#?download_deposition	

# - Download Lead Pollution Archives
#? download_lead	

##get_latest_imeca	Get the latest pollution values for each station


