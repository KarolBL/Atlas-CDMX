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
  geom_point()+
  xlab("Longitude")+
  ylab("Latitude")+
  facet_grid(year ~ Contaminant, labeller = label_parsed)+
  theme_bw()+
  # annotation_scale()+
  # annotation_north_arrow(
  #   #location = "bl",
  #   which_north = "TRUE",
  #   pad_x = unit(0, "in"),
  #   pad_y = unit(0.2, "in"),
  #   height = unit(.8, "cm"), width = unit(.8, "cm"),
  #   style = north_arrow_fancy_orienteering
  # )+
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
  width = 5.5,
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
