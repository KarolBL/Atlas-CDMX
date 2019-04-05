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
#Google's Terms of Service: https://cloud.google.com/maps-platform/terms/.
#Please cite ggmap if you use it! See citation("ggmap") for details.
###########################################################
# DATA
###########################################################
###########################################################
# Loading Mortality data
###########################################################
mortality_rate <- read_csv("~/Dropbox/Atlas_Environmental_Health/Datos/Data/Mortality/mortality_general.csv")
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
  mutate(id = CVE_MUN) -> #Rename CVE_ENT to "id"  
  mortality_rate

mortality_tibble <- gather(data = mortality_rate, key = Year, value = rate, `2000`:`2016`)

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
  dsn= '/home/kbaca/Dropbox/Atlas_Environmental_Health/Datos/Data/Shapefiles', 
  layer = '09mun')

polygons_boroughs <- subset(polygons, CVE_ENT=="09")
#1] 16615     7

#class(polygons_boroughs)
#[1] "SpatialPolygonsDataFrame"
#attr(,"package")
#[1] "sp"

#glimpse(polygons_boroughs)
#Formal class 'SpatialPolygonsDataFrame' [package "sp"] with 5 slots
#..@ data       :'data.frame':	16 obs. of  4 variables:

#Convert to tibble
polygons_boroughs_tibble <- as.tibble(polygons_boroughs)

#head(polygons_boroughs)
# A tibble: 6 x 4
#  CVEGEO CVE_ENT CVE_MUN NOMGEO               
#  <fct>  <fct>   <fct>   <fct>                
#1 09002  09      002     Azcapotzalco         
#2 09003  09      003     Coyoacán 
#dim(polygons_boroughs)
#[1] 16  4


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














#https://rstudio-pubs-static.s3.amazonaws.com/301056_e188ebc4c8644410b4abbc4ae98b6c98.html
