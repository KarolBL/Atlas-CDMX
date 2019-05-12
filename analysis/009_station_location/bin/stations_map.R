#!/usr/bin/env Rscript
############################################################
## Goal: Plot the contaminant station location
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
    default = "7", 
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
  stop("At least one argument must be supplied (input contaminants).n", call.=FALSE)
}
############################################################
# Debuging
# opt$contaminants <- "../data/contaminants"
# opt$maps <- "../data/09mun"
# opt$out <- "../results/stations.pdf"
options(mc.cores = opt$cores)
############################################################
#Load the contaminants to build stations contaminants matrix
contaminants <- mclapply(
  list.files(opt$contaminants, full.names = TRUE),
  function(contaminant){
    load(contaminant)
    return(unique(contaminant$station_code))
  }
)
names(contaminants) <- gsub(
  pattern = ".RData",
  replacement = "",
  fixed = TRUE,
  list.files(opt$contaminants)
)

#Long format data
contaminants <- do.call(
  rbind, 
  lapply(
    names(contaminants),
    function(contaminant){
      return(
        data.frame(
          Contaminant = contaminant,
          station = contaminants[[contaminant]]
        )
      )  
    }
  )
)

#Add 2019 stations
contaminants <- rbind(
  contaminants,
  data.frame(
    Contaminant = rep(c("O3", "CO", "SO2", "PM25", "PM10"), 2),
    station = c(rep("FAR", 5), rep("SAC", 5))
  )
)

#Merge coordinates:
contaminants <- merge(
  x = contaminants, 
  y = stations[, c("station_code", "lon", "lat")],
  sort = FALSE,
  by.x = "station",
  by.y = "station_code",
  all.x = TRUE
)

contaminants$Contaminant <- factor(
  as.character(contaminants$Contaminant),
  levels = c(
    "CO", "NO2", "O3", "SO2", "PM25", "PM10", "NO", "NOX", "PMCO"
  )
)
levels(contaminants$Contaminant) <- c(
  "CO", "NO[2]", "O[3]", "SO[2]", "PM[25]", "PM[10]", "NO", "NO[X]", "PM[CO]")

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
  geom_point()+
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
  xlab("Longitude")+
  ylab("Latitude")+
  facet_wrap(Contaminant ~ ., nrow = 3, labeller = label_parsed)+
  theme_bw()+
  annotation_scale(
    data = data.frame(
      Contaminant = "NO"
    )
  )+
  annotation_north_arrow(
    data = data.frame(
      Contaminant = "NO"
    ),
    #location = "bl",
    which_north = "TRUE",
    pad_x = unit(0, "in"),
    pad_y = unit(0.2, "in"),
    height = unit(.8, "cm"), width = unit(.8, "cm"),
    style = north_arrow_fancy_orienteering
  )+
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


