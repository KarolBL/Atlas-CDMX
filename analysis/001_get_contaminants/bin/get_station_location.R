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
contaminants <- c("CO", "NO", "NO2", "NOX", "O3", "PM10", 
                  "SO2", "PM25", "PMCO")
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




# - Download Ultraviolet Radiation Archives
download_radiation(type, year, progress = interactive())
download_radiation

# - Download Acid Rain Measurements Archives
download_deposition	

# - Download Lead Pollution Archives
download_lead	

# - Download Meteorological Data Archives
download_meteorological	

# - Download Atmospheric Pressure Archives
download_pressure	

##get_latest_imeca	Get the latest pollution values for each station


