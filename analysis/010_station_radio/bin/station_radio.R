#!/usr/bin/env Rscript
############################################################
## Goal: Get the contaminant station radio
############################################################
library("aire.zmvm")
library("optparse")
library("parallel")
library("reshape")
library("ggplot2")
library("ggspatial")
library("rgdal")
library("sf")
library("grDevices")
library("dplyr")
############################################################
option_list <- list(
  make_option(
    c("-n", "--contaminants"), 
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
    default = "1", 
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
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}
############################################################
# Debuging
# opt$contaminants <- "../data/missing"
# opt$maps <- "../data/09mun"
# opt$out <- "../results/stations_radios.pdf"
options(mc.cores = opt$cores)
############################################################
#Load the contaminants to build stations contaminants matrix
years <- c(2009, 2012, 2015, 2018)
contaminants <- mclapply(
  list.files(opt$contaminants, full.names = TRUE),
  function(contaminant){
    load(contaminant)
    out <- subset(
      contaminant,
      year %in% years
    )
    
    return(unique(out[, c("station_code", "pollutant", "year")]))
  }
)
names(contaminants) <- gsub(
  pattern = ".RData",
  replacement = "",
  fixed = TRUE,
  list.files(opt$contaminants)
)
contaminants <- contaminants[
  !names(contaminants) %in% c("TMP",  "UVA", "UVB", "WDR", "WSP", "PBa", "RH")
]
contaminants <- do.call(
  rbind,
  contaminants
)

#Merge coordinates:
contaminants <- merge(
  x = contaminants, 
  y = stations[, c("station_code", "lon", "lat")],
  sort = FALSE,
  by.x = "station_code",
  by.y = "station_code",
  all.x = TRUE
)

contaminants$Contaminant <- factor(
  as.character(contaminants$pollutant),
  levels = c(
    "CO", "NO2", "O3", "SO2", "PM25", "PM10", "NO", "NOX", "PMCO"
  )
)
levels(contaminants$Contaminant) <- c(
  "CO", "NO[2]", "O[3]", "SO[2]", "PM[25]", "PM[10]", "NO", "NO[X]", "PM[CO]"
  )
############################################################
#Loading shape files
boroughs <- readOGR(
  dsn = opt$maps, 
  layer = '09mun'
)
boroughs <- spTransform(
  boroughs, 
  CRS=CRS("+proj=longlat +datum=WGS84")
)
polygons_boroughs_df <- fortify(boroughs, region="CVE_MUN")

############################################################
#Generate the contaminant radios according to the corresponding year
ranges <- c(
  "CO"  = 20.72, 
  "NO2" = 43.37,
  "O3" = 57.82, 
  "SO2" = 5.92,
  "PM25" = 13, 
  "PM10" = 12.98, 
  "NO" = 77.15, 
  "NOX" = 84.44, 
  "PMCO" = 28.04
)
ranges <- ranges * 1000 #in meters

getRadio <- function(
  contaminants, 
  contaminant = "CO", 
  Year = 2009,
  ranges
  ){
    #Get the appropiate stations
    ids <- subset(
      contaminants,
      year == Year &
      pollutant == contaminant
    )
    ids <- unique(ids$station_code)
    
    if(length(ids) != 0 ){
      #Get the coordinates
      xy <- stations[stations$station_code %in% ids, c("lon", "lat")]
      stations_sp <- SpatialPointsDataFrame(
        coords = xy, 
        data = stations[stations$station_code %in% ids, ],
        proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      )
      stations_sf <- st_as_sf(
        stations_sp, 
        coords = c("lon","lat"), 
        crs = 4326
      ) %>%
      st_transform(3050)
  
      #Add the circles
      stations_circles <- st_buffer(stations_sf, dist = ranges[contaminant])
      stations_circles <- st_union(stations_circles)
      stations_circles <- as_Spatial(stations_circles)
      stations_circles <- spTransform(
        stations_circles, 
        CRS=CRS("+proj=longlat +datum=WGS84")
      )
      stations_circles_df <- fortify(stations_circles) #, region="CVE_MUN")
      stations_circles_df$pollutant <- contaminant
      stations_circles_df$year <- Year 
      
      return(stations_circles_df)
    }else{
      return(NA)
    }
}

#For each contaminant
radios <- lapply(
  unique(contaminants$pollutant),
  function(contaminant){
    #For each year
    out <- lapply(
      years, 
      function(Year){
        getRadio(
          contaminants, 
          contaminant = contaminant, 
          Year = Year,
          ranges
        )
      }
    )
    do.call(rbind, na.omit(out))
  }
)

radios <- do.call(rbind, radios)
radios$ID <- paste(radios$pollutant, radios$year)
radios <- na.omit(radios)
radios$Contaminant <- factor(
  as.character(radios$pollutant),
  levels = c(
    "CO", "NO2", "O3", "SO2", "PM25", "PM10", "NO", "NOX", "PMCO"
  )
)
levels(radios$Contaminant) <- c(
  "CO", "NO[2]", "O[3]", "SO[2]", "PM[25]", "PM[10]", "NO", "NO[X]", "PM[CO]"
)

############################################################
#Plot the stations

p <- ggplot(
  data = contaminants,
  aes(
    x = lon,
    y = lat,
    colour = Contaminant
  )
)+
  geom_polygon(
    data = polygons_boroughs_df,
    aes(
      x = long,
      y = lat,
      group = group
    ),
    fill="transparent",
    color="black"
  )+
  geom_polygon(
    data = radios,
    aes(
      x = long,
      y = lat,
      group = piece, 
      color = Contaminant
    ),
    fill="transparent"
  )+
  geom_point( size = 1)+
  annotation_scale(
    data = data.frame(
      Contaminant = "CO",
      year = 2018
    )
  )+
  xlab("Longitude")+
  ylab("Latitude")+
  facet_grid(year ~ Contaminant, labeller = label_parsed)+
  theme_bw()+
  coord_sf(crs = 4326)+
  theme(
    panel.grid = element_line(colour = "transparent"),
    legend.position = "NULL",
    axis.text.x = element_text(angle = 35, hjust = 1)
  )
p

ggsave(
  p,
  file = opt$out, 
  width = 15,
  height = 8,
  device = cairo_pdf 
)


#Format the data
contaminant$station_code <- factor(
  as.character(contaminant$station_code),
  levels = names(sort(table(contaminant$station_code)))
)

if(regexpr(opt$file, pattern = "missing")>0){
  contaminant$value <- contaminant$value / 168 * 100
}

#Make the ggplot of missing data
p <- ggplot(
  data = contaminant,
  aes(
    x = date,
    y = station_code,
    fill = value
  )
) + 
  geom_tile(width=7)+
  scale_fill_gradient2(low = "green", mid = "yellow", high = "red", name = unique(contaminant$pollutant))+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Station code") + 
  xlab("Date")
p

#Saving the plot!!
ggsave(
  p, 
  file = opt$out, 
  width = 11,
  height = 8.50
)
