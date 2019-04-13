###########################################################
# Required libraries
###########################################################
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(reshape2)
library(sp) 
library(sf)
library(rgdal) 
library(viridis)
library(rvest)
library(ggspatial)
###########################################################
# DATA
###########################################################
###########################################################
# Loading Mortality data
###########################################################
mortality_rate <- read_csv("~/Data/Mortality/mortality_general.csv")
#head(mortality_rate)
# A tibble: 2 x 5
#CVE_MUN municipio_name `2000` `2001` `2002`
#<chr>   <chr>           <dbl>  <dbl>  <dbl>
# 1 NA      Total             5.3    5.3    5.4
#2 002     Azcapotzalco      5.9    6.2    6.1

#Remove total mortality row
mortality_rate <- mortality_rate[-1,]
#head(mortality_rate)

mortality_rate %>%
  mutate(id = municipio_name) -> #Rename CVE_ENT to "id"  
  mortality_rate

mortality_tibble <- gather(data = mortality_rate, 
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
# Loading Polygons from shapefile
###########################################################
polygons <- readOGR(
  dsn= '.', 
  layer = '09mun')

polygons_boroughs <- subset(polygons, CVE_ENT == "09" )
#dim(polygons_boroughs)
# [1] 16  4

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
# A tibble: 6 x 4
# CVEGEO CVE_ENT CVE_MUN NOMGEO               
# <fct>  <fct>   <fct>   <fct>                
#   1 09002  09      002     Azcapotzalco         
# 2 09003  09      003     Coyoacán             
# 3 09004  09      004     Cuajimalpa de Morelos

###########################################################
# Mapping polygons
###########################################################
# Preliminar plots - polygons
ggplot() +  
  geom_polygon(data = polygons_boroughs, 
               aes(x=long, y=lat, group=group), 
               fill="white", color="black")

###########################################################
# Matching mortality rate values to boroughs' polygons
###########################################################
polygons_boroughs_df <- fortify(polygons_boroughs, region="NOMGEO")

#head(polygons_boroughs_df)
# long      lat order  hole piece             id            group
# 1 2794396 824857.6     1 FALSE     1 Álvaro Obregón Álvaro Obregón.1
# 2 2794434 824765.3     2 FALSE     1 Álvaro Obregón Álvaro Obregón.1
# 3 2794437 824757.3     3 FALSE     1 Álvaro Obregón Álvaro Obregón.1
# 4 2794449 824728.9     4 FALSE     1 Álvaro Obregón Álvaro Obregón.1
# 5 2794462 824697.4     5 FALSE     1 Álvaro Obregón Álvaro Obregón.1

# dim(polygons_boroughs_df)
#[1] 16615     7

###Joining dataframes 

mortality_by_borough <-inner_join(polygons_boroughs_df,
                                  mortality_rate , by = "id")
#dim(mortality_by_borough)
#[1] 11831    26
#mortality_by_borough[1:2,1:12]
# long      lat order  hole piece           id          group CVE_MUN municipio_name 2000 2001 2002
# 1 2794860 837218.4  2028 FALSE     1 Azcapotzalco Azcapotzalco.1     002   Azcapotzalco  5.9  6.2  6.1
# 2 2794862 837217.4  2029 FALSE     1 Azcapotzalco Azcapotzalco.1     002   Azcapotzalco  5.9  6.2  6.1

###Joining tibbles 
mortality_by_borough_tibble <- inner_join(as.tibble(polygons_boroughs_df), mortality_tibble, by = "id")

# head(mortality_by_borough_tibble)
# # A tibble: 6 x 11
# long     lat order hole  piece id           group          CVE_MUN municipio_name Year   rate
# <dbl>   <dbl> <int> <lgl> <fct> <chr>        <fct>          <chr>   <chr>          <chr> <dbl>
# 1 2794860. 837218.  2028 FALSE 1     Azcapotzalco Azcapotzalco.1 002     Azcapotzalco   2000    5.9
# 2 2794860. 837218.  2028 FALSE 1     Azcapotzalco Azcapotzalco.1 002     Azcapotzalco   2001    6.2
# 3 2794860. 837218.  2028 FALSE 1     Azcapotzalco Azcapotzalco.1 002     Azcapotzalco   2002    6.1

#dim(mortality_by_borough_tibble)
# [1] 201127     11