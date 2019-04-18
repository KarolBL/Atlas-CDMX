#!/usr/bin/env Rscript
############################################################
## Goal: Obtain the kriging plots
############################################################
library("sf")
library("rgeos")
library("raster")
library("geosphere")
library("sp")
library("rgdal")
library("leaflet")
library("mapview")
library("ggplot2")
library("reshape2")
library("cowplot")
library("ggspatial")
library("optparse")
library("parallel")
#Quien es el representante para darle los permisos siguientes
############################################################
option_list <- list(
  make_option(
    c("-o", "--colonia"), 
    type = "character", 
    default = NULL, 
    help = "colonia data points for kriged data [default= %default]", 
    metavar = "character"
  ),
  make_option(
    c("-m", "--mortality"), 
    type = "character", 
    default = NULL, 
    help = "mortality directory data [default= %default]", 
    metavar = "character"
  ),
  make_option(
    c("-m", "--kriging"), 
    type = "character", 
    default = NULL, 
    help = "kriging directory data [default= %default]", 
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
if(is.null(opt$colonia)){
  print_help(opt_parser)
  stop("At least one argument must be supplied (input vgm file).n", call.=FALSE)
}
############################################################
# Debuging
# opt$colonia  <- "../data/Colonias.RData"
# opt$mortality <- "../data/Mortality_cdmx/"
# opt$kriging <- "../data/Kriging/"
# opt$out <- "../results/"
# opt$cores <- 3
options(mc.cores = opt$cores)
options(width = 100)
options(max.print = 100)
############################################################
## Read the data
############################################################
## Mortality unification
candidate_files <- list.files(
  opt$mortality, 
  ".csv",
  full.names = TRUE
)
mortality <- lapply(
  candidate_files, 
  function(candidate){
    datum <- read.csv(candidate)
    datum <- datum[-1,]
  }
)
#Name the age specific mortality
age_mortality <- do.call(
    rbind, 
    strsplit(
      split = "mortality_",
      candidate_files
    )
  )[, 2]
age_mortality <- gsub(
  pattern = ".csv",
  replacement = "",
  age_mortality
)
names(mortality) <- age_mortality

## Map coordinates
load(opt$colonia)

## Kriging data
candidate_files <- list.files(
  opt$kriging, 
  ".RData",
  full.names = TRUE
)
kriging <- lapply(
  candidate_files, 
  function(candidate){
    load(candidate)
    return(kriged_data)
  }
)
names(kriging) <- age_mortality
##Total neighbourhood 
# t(t(table(as.character(neighborghood@data$MUN_NAME))))
############################################################
##Format mortality data to plot
############################################################
##mortality to ggplot format
mortality[[2]]$CVE_MUN <- mortality[[2]]$municipio_name
mortality[[2]]$municipio_name <- NULL
mortality <- lapply(
  age_mortality, 
  function(type){
    aux <- mortality[[type]]
    aux$Type <- type
    return(aux)
  }
)
mortality <- do.call(rbind, mortality)
#Same levels
mortality$CVE_MUN <- droplevels(mortality$CVE_MUN)
levels(mortality$CVE_MUN) <- c(
  levels(borough_coordinates$MUN_NAME),
  "ÁLVARO OBREGÓN",
  "BENITO JUÁREZ",
  "COYOACÁN",
  "CUAUHTÉMOC",
  "TLÁHUAC",
  "GUSTAVO A MADERO"
)

#Adjust order with borough_coordinates
mortality <- mortality[order(mortality$Type, as.numeric(mortality$CVE_MUN)),]

#Adding aditional columns
#lon      lat       MUN_NAME     SETT_NAME ID kriging_times      Type    value         level
mortality$lon <- rep(borough_coordinates$x, length(unique(mortality$Type)))
mortality$lat <- rep(borough_coordinates$y, length(unique(mortality$Type)))
mortality$MUN_NAME <- mortality$CVE_MUN
mortality$SETT_NAME <- mortality$CVE_MUN
mortality$CVE_MUN <- NULL
mortality$ID <- NA

#Melting data
m_mortality <- melt(
  mortality,
  id.vars = c("lon", "lat", "MUN_NAME", "SETT_NAME", "ID", "Type"),
  variable.name = "Year"
)
#Modify the year
m_mortality$Year <- substr(
  as.character(m_mortality$Year),
  start = 2,
  stop = 5
)
m_mortality$Year <- as.integer(m_mortality$Year)
m_mortality$level <- "Borough"
m_mortality <- m_mortality[, c(1:5,7,6,8:9)]

#Change year into date
m_mortality$Year <- as.factor(m_mortality$Year)
levels(m_mortality$Year) <- paste(2000:2016, "-01-01", sep = "")
names(m_mortality)[6] <- "kriging_times"
m_mortality$kriging_times <- as.Date(as.character(m_mortality$kriging_times))

############################################################
##Format kriged data to plot
############################################################
#Time points
kriging_times <- as.Date(row.names(as.data.frame(kriging$childhood@time)))
#Coordinates
kriging_coords <- as.data.frame(coordinates(kriging$childhood@sp))
kriging_coords$kriged_index <- 1:nrow(kriging_coords)
neighbourhood_coordinates$neigh_ID <- 1:nrow(neighbourhood_coordinates)
kriging_coords <- merge(kriging_coords, neighbourhood_coordinates, by = c("lon", "lat"))
kriging_coords <- kriging_coords[order(kriging_coords$kriged_index), ]
stopifnot(all(kriging_coords$neigh_ID == kriging_coords$kriged_index)) #same oder
kriging_coords$neigh_ID <- NULL
kriging_coords$kriged_index <- NULL
#Values
kriging_values <- lapply(
  age_mortality, 
  function(type){
      kriging[[type]]@data$var1.pred
  }
)
kriging_values <- as.data.frame(do.call(cbind, kriging_values))
names(kriging_values) <- age_mortality
head(kriging_values)
##Join the data
kriged_data <- cbind(
  kriging_coords,
  kriging_times,
  kriging_values
)
m_kriged_data <- melt(
  kriged_data,
  id.vars = c("lon", "lat", "MUN_NAME", "SETT_NAME", "ID", "kriging_times"),
  variable.name = "Type"
)
m_kriged_data$level <- "Neighbourhood" 

#Joining borough and neighbourhood data into a single object
m_complete <- rbind(
  m_kriged_data,
  m_mortality
)
levels(m_complete$Type) <- c(
  "Infant",
  "Global",
  "Post-productive",
  "Pre-school",
  "Productive",
  "School"
)
m_complete$Type <- factor(
  as.character(m_complete$Type),
  levels = c(
    "Global",
    "Post-productive",
    "Productive",
    "School",
    "Pre-school",    
    "Infant"
  )
)
m_complete$Borough <- m_complete$MUN_NAME
m_complete$MUN_NAME <- NULL

############################################################################
##Ploting data at last
############################################################################
##Time evolution
#head(m_complete)
p_time_raw <- ggplot(
  data = subset(
    m_complete,
    level == "Borough" & Type == "Global"
  ),
  aes(
    x = kriging_times,
    y = value,
    group = Borough,
    colour = Borough
  )
)+
  xlab("Year")+
  ylab("Mortality rate [x1000]")+
  geom_point()+
  #geom_smooth(se = FALSE)+
  geom_line(linetype = "dashed")+
  facet_grid(Type ~ ., scales = "free_y")+
  theme_bw()+
  theme(
    legend.position = "bottom"
  )
p_time_raw
ggsave(
  p_time_raw,
  file = paste(opt$out, "Mortality_borough_crude.pdf"),
  width = 10,
  height = 8
)
write.csv(
  m_complete,
  row.names = FALSE,
  quote = FALSE,
  file = paste(opt$out, "Mortality_borough_crude.csv")
)
############################################################################
## Using kriging data
############################################################################
p_neigh_time <- ggplot(
  data = subset(
    m_complete,
    level == "Neighbourhood" & Type == "Global" & Borough == "TLALPAN" &
      kriging_times %in% as.Date(
       paste(2000:2016, "-01-01", sep = "")
      ) &
      SETT_NAME %in% unique(m_complete$SETT_NAME[m_complete$Borough == "TLALPAN"])[8:15]
  ),
  aes(
    x = kriging_times,
    y = value,
    group = SETT_NAME,
    colour = SETT_NAME
  )
)+
  xlab("Year")+
  ylab("Mortality rate [x1000]")+
  geom_point()+
  #geom_smooth(se = FALSE)+
  geom_line(linetype = "dashed")+
  #facet_grid(Type ~ ., scales = "free_y")+
  theme_bw()+
  theme(
    legend.position = "none"
  )
p_neigh_time

############################################################################
## The end
############################################################################