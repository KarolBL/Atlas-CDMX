#!/usr/bin/env Rscript
###########################################################
## Required libraries
###########################################################
library("ggplot2")
library("tidyverse")
library("dplyr")
library("tidyr")
library("reshape2")
library("sp") 
library("sf")
library("rgdal") 
library("viridis")
library("rvest")
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
# opt$file <- "../data/Mortality_cdmx/mortality_general.csv"
# opt$out <- "../results/mortality_general.RData"
options(mc.cores = opt$cores)
############################################################
## Loading Mortality data
###########################################################
mortality_rate <- read_csv(opt$file)
#head(mortality_rate)
# A tibble: 2 x 5
#CVE_MUN municipio_name `2000` `2001` `2002`
#<chr>   <chr>           <dbl>  <dbl>  <dbl>
# 1 NA      Total             5.3    5.3    5.4
#2 002     Azcapotzalco      5.9    6.2    6.1

##Remove total mortality row, AKA, first row
mortality_rate <- mortality_rate[-1,]
#head(mortality_rate)

mortality_rate %>%
  mutate(id = CVE_MUN) -> #Rename CVE_ENT to "id"  
  mortality_rate

##wide to long format  
mortality_tibble <- gather(
    data = mortality_rate, 
    key = Year, 
    value = rate, 
    `2000`:`2016`
)
#head(mortality_tibble)
## A tibble: 6 x 5
#  CVE_MUN municipio_name    id    Year   rate
#  <chr>   <chr>             <chr> <chr> <dbl>
#1 002     Azcapotzalco      002   2000    5.9
#2 003     Coyoacán          003   2000    4.9
#dim(mortality_tibble)
#[1] 272   5

###########################################################
## Loading Polygons from shapefile
###########################################################
polygons <- readOGR(
  dsn = '../data/Shapefiles', 
  layer = 'Colonias'
)
#option 1 doesnt work
polygons <- readOGR(
  dsn = '.', 
  layer = 'Colonias'
)
#option 2
library(raster)
s <- shapefile("/home/cfresno/Dropbox/inmegen/DAtos/Karol/Atlas-CDMX/Data/Shapefiles/Colonias.shp")
#option 3 reads file but without geometry
library(sf) 
polygons <- sf::st_read('.')
#dim(polygons)
#[1] 58227    10


polygons_boroughs <- subset(polygons, ST_NAME == "DISTRITO FEDERAL")
#dim(polygons_boroughs)
#[1] 2097   10
st_coordinates(polygons_boroughs)
#class(polygons_boroughs)
#[1] "SpatialPolygonsDataFrame"
#attr(,"package")
#[1] "sp"

#glimpse(polygons_boroughs)
#Formal class 'SpatialPolygonsDataFrame' [package "sp"] with 5 slots
#..@ data       :'data.frame':	16 obs. of  4 variables:

#Convert to tibble
polygons_boroughs_tibble <- as_tibble(polygons_boroughs)

#head(polygons_boroughs_tibble)
# A tibble: 6 x 10
#OBJECTID POSTALCODE ST_NAME MUN_NAME SETT_NAME SETT_TYPE   AREA Shape_Leng Shape_Area
#<int> <fct>      <fct>   <fct>    <fct>     <fct>      <dbl>      <dbl>      <dbl>
#1    11065 01000      DISTRI… ÁLVARO … SAN ANGEL COLONIA   7.12e8     0.0539  0.0000947
#2    11066 01010      DISTRI… ÁLVARO … LOS ALPES COLONIA   7.12e8     0.0299  0.0000443
#3    11067 01020      DISTRI… ÁLVARO … GUADALUP… COLONIA   7.12e8     0.0342  0.0000576



###########################################################
# Mapping polygons
###########################################################
# Preliminar plots - polygons
ggplot() +  
  geom_polygon(data = polygons, 
               aes_string(x = "lon", y = "lat"), 
               fill="white", color="black")

###########################################################
# Matching mortality rate values to boroughs' polygons
###########################################################
polygons_boroughs_df <- fortify(polygons_boroughs, region="CVE_MUN")

#head(polygons_boroughs_df)
#long      lat order  hole piece  id group
#1 2794860 837218.4     1 FALSE     1 002 002.1
#2 2794862 837217.4     2 FALSE     1 002 002.1
#3 2794901 837220.4     3 FALSE     1 002 002.1
#4 2795366 837099.8     4 FALSE     1 002 002.1

#> dim(polygons_boroughs_df)
#[1] 16615     7

###Joining dataframes 

mortality_by_borough <-inner_join(polygons_boroughs_df,
                                             mortality_rate , by = "id")
#dim(mortality_by_borough)
#[1] 16615    26
#mortality_by_borough[1:2,1:12]
#long      lat order  hole piece  id group CVE_MUN municipio_name 2000 2001 2002
#1 2794860 837218.4     1 FALSE     1 002 002.1     002   Azcapotzalco  5.9  6.2  6.1
#2 2794862 837217.4     2 FALSE     1 002 002.1     002   Azcapotzalco  5.9  6.2  6.1

###Joining tibbles 
mortality_by_borough_tibble <- inner_join(as.tibble(polygons_boroughs_df), mortality_tibble, by = "id")

head(mortality_by_borough_tibble)
# A tibble: 6 x 11
#  long     lat     order hole  piece id    group CVE_MUN municipio_name Year   rate
#  <dbl>   <dbl>    <int> <lgl> <fct> <chr> <fct> <chr>   <chr>          <chr> <dbl>
#1 2794860. 837218.     1 FALSE 1     002   002.1 002     Azcapotzalco   2000    5.9
#2 2794860. 837218.     1 FALSE 1     002   002.1 002     Azcapotzalco   2001    6.2
#3 2794860. 837218.     1 FALSE 1     002   002.1 002     Azcapotzalco   2002    6.1
#4 2794860. 837218.     1 FALSE 1     002   002.1 002     Azcapotzalco   2003    6.5
#5 2794860. 837218.     1 FALSE 1     002   002.1 002     Azcapotzalco   2004    6.6
#6 2794860. 837218.     1 FALSE 1     002   002.1 002     Azcapotzalco   2005    6.9

#dim(mortality_by_borough_tibble)
#[1] 282455     11

###########################################################
# Mapping mortality
###########################################################

ggplot(data = mortality_by_borough_tibble, # the input data
        aes(x = long, y = lat, fill = rate,  group = group)) + # define variables
  geom_polygon() + # plot the boroughs
  geom_path(colour="black", lwd=0.05) + # borough borders
  coord_equal() + # fixed x and y scales
  facet_wrap(~ Year) + # one plot per time slice
  scale_fill_gradient2(low = "blue", mid = "grey", high = "red", # colors
                       midpoint = 7, name = "Rate per 1,000") + # legend options
  ggtitle("Mortality rate per borough") +
  theme_bw()+
  theme(axis.text = element_blank(), # change the theme options
        axis.title = element_blank(), # remove axis titles
        axis.ticks = element_blank()) # remove axis ticks

 ggsave("facet_cdmx.png", width = 9, height = 9) # save figure

