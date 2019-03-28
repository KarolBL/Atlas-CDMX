# Atlas-CDMX
Contains pollution, mortality datasets and utilities for creating the Mexico City's Atlas of Health and the Environment.

## Data sets

### Mortality rates

This data set is divided into 6 categories, each containing mortality rates for the 16 boroughs in Mexico City from 2000 to 2016.
Source: Secretaría de Salud de la Ciudad de México (SEDESA) 
http://data.salud.cdmx.gob.mx/portal/index.php/informacion-en-salud/103-informacion-salud/354-mortalidad

+ mortality_general.csv
+ mortality_childhood.csv
+ mortality_productive.csv
+ mortality_post_productive.csv
+ mortality_pre_scholar.csv
+ mortality_scholar.csv

### Air pollution

+ NO2
+ O3
+ CO
+ PM10
+ PM2.5

Available at http://www.aire.cdmx.gob.mx/default.php. 
Direct download from the using R the package `aire.zmvm`.

### Spatial data for Polygons

Shapefiles to map polygons at state and borough levels can be downloaded from the INEGI website - *Marco Geoestadístico 2018*.
Source:
https://www.inegi.org.mx/app/biblioteca/ficha.html?upc=889463674658

+ 09_ciudaddemexico.zip (80.6 Mb)
  + catalogos
  + conjunto de datos
    + 09_ent.shp (43 Kb)
    + 09_mun.shp (261 Kb)
  + metadatos
  
Shapefiles to map polygons at distict level can be downloaded from the website *Datos Abiertos Ciudad de México*.
Source:
https://datos.cdmx.gob.mx/explore/dataset/coloniascdmx/export/

+ coloniascdmx.shp

Additionally, district, boroughs and state level spatial data are available from the GADM webpage in the following formats: 

+ R (sp): level-0, level1, level2
+ R (sf): level-0, level1, level2
+ KMZ: level-0, level1, level2

