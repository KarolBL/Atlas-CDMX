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
library("RColorBrewer")
library("gridExtra")
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
kriging_coords <- cbind(
  kriging_coords, 
  neighborghood@data[, c("OBJECTID", "MUN_NAME", "SETT_NAME")]
)
#head(kriging_coords)
#head(neighborghood@data)

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

##Join the data, i.e., add coords + time columnas replicates to each predicted kriging value
kriged_data <- cbind(
  do.call(
    rbind, 
    lapply(
      1:length(kriging_times),
      function(iteration){
        return(kriging_coords)
      }
    )
  ),
  times = do.call(
    c,
    lapply(
      kriging_times, 
      rep,
      nrow(kriging_coords)
    )
  ),
  kriging_values
)
head(kriged_data)  
  
#Melt the data  
m_kriged_data <- melt(
  kriged_data,
  id.vars = c("lon", "lat", "MUN_NAME", "SETT_NAME", "OBJECTID", "times"),
  variable.name = "Type"
)
m_kriged_data$level <- "Neighbourhood" 
head(m_kriged_data)

#Joining borough and neighbourhood data into a single object
names(m_mortality)[5:6] <- c("OBJECTID", "times")
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
#m_complete$MUN_NAME <- NULL

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
    x = times,
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
  file = paste(opt$out, "Time_mortality_borough_crude.pdf"),
  width = 10,
  height = 8
)
ggsave(
  p_time_raw,
  file = paste(opt$out, "Time_mortality_borough_global_crude.pdf"),
  width = 10,
  height = 8
)
m_complete$days <- julian(m_complete$times) - min(julian(m_complete$times))
m_time_neigh <- subset(
  m_complete,
  level != "Borough" & times < "2016-01-02"
) 
m_neigh <- subset(
  m_complete,
  level != "Borough" & times %in% as.Date(paste(2000:2016, "-01-01", sep = ""))
) 
m_borough <- subset(
  m_complete,
  level == "Borough"
)
write.csv(
  m_complete,
  row.names = FALSE,
  quote = FALSE,
  file = paste(opt$out, "Mortality_borough_crude.csv")
)
write.csv(
  m_time_neigh,
  row.names = FALSE,
  quote = FALSE,
  file = paste(opt$out, "Time_neighbourhood.csv")
)
write.csv(
  m_neigh,
  row.names = FALSE,
  quote = FALSE,
  file = paste(opt$out, "Neighbourhood.csv")
)
write.csv(
  m_borough,
  row.names = FALSE,
  quote = FALSE,
  file = paste(opt$out, "Borough.csv")
)

############################################################################
## Using kriging data
############################################################################
p_neigh_time <- ggplot(
  data = subset(
    m_complete,
    level == "Neighbourhood" & Borough == "MILPA ALTA" & #Type == "Global" &
      # times < as.Date(
      #  paste(2016, "-01-02", sep = "")
      # )
      times %in% as.Date(
       paste(2000:2016, "-01-01", sep = "")
      ) # &
      # SETT_NAME %in% unique(m_complete$SETT_NAME[m_complete$Borough == "TLALPAN"])
  ),
  aes(
    x = times,
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
  facet_grid(Type ~ ., scales = "free_y")+
  theme_bw()+
  theme(
    legend.position = "none"
  )
p_neigh_time

#############################################
# Plot map at borough level
#############################################
head(df_borough)
add_mortality <- function(
  df_borough, 
  m_complete, 
  time = as.Date(
    paste(
      seq(from = 2000, to = 2016, by = 5),
      "-01-01", 
      sep = ""
    )
  ),
  type = levels(m_complete$Type),
  Level = "Borough"
){
  #Copy original data
  map <- df_borough
  
  #Select time + level + type level
  kriged <- subset(
    m_complete,
    Type %in% type &
      level %in% Level &
      times %in% time
  )
  
  mm <- merge(
    map, 
    kriged,
    by.x = "MUN_NAME",
    by.y = "Borough",
    all.y = TRUE
  )
  return(mm)
}

mm <- add_mortality(df_borough, m_complete)
mm$times <- format(mm$times, "%Y")

p_kriged_time_borough <- ggplot(
  data = mm,
  aes(
    x = long,
    y = lat.x,
    group = id,
    fill = value
  )
) +
  geom_polygon(
    alpha = .6
  ) +
  geom_path(color = "white")+
  #geom_point() + #los del polígono
  facet_grid(Type ~ times)+
  coord_equal() +
  xlab("Longitude")+
  ylab("Latitud")+
  theme_bw()+
  theme(
    #legend.position = "left",
    panel.grid = element_line(colour = "transparent")
    #title = element_blank()
    #axis.text = element_blank()
  )+
  scale_fill_gradientn(
    #name = "Global mortality rate", 
    name = "Rate",
    colours = brewer.pal(9, name = "YlOrRd")
  )
p_kriged_time_borough

##Global 
p_global <- ggplot(
  data = subset(
    mm,
    Type == "Global"
  ),
  aes(
    x = long,
    y = lat.x,
    group = id,
    fill = value
  )
) +
  geom_polygon(
    alpha = .6
  ) +
  geom_path(color = "white")+
  #geom_point() + #los del polígono
  facet_grid(Type ~ times)+
  coord_equal() +
  #xlab("Longitude")+
  #ylab("Latitude")+
  #ylab(" ")+
  theme_bw()+
  theme(
    #legend.position = "left",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_line(colour = "transparent")
    #title = element_blank()
    #axis.text = element_blank()
  )+
  scale_fill_gradientn(
    #name = "Global mortality rate", 
    name = "",
    colours = brewer.pal(9, name = "YlOrRd")
  )
#p_global

##Intermediate plots
intermediate_plot <-function(mm, type){
 ggplot(
  data = subset(
    mm,
    Type == type
  ),
  aes(
    x = long,
    y = lat.x,
    group = id,
    fill = value
  )
) +
  geom_polygon(
    alpha = .6
  ) +
  geom_path(color = "white")+
  #geom_point() + #los del polígono
  facet_grid(Type ~ times)+
  coord_equal() +
  #xlab("Longitude")+
  #ylab("Latitude")+
  #ylab(" ")+
  theme_bw()+
  theme(
    #legend.position = "left",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_line(colour = "transparent")
    #title = element_blank()
    #axis.text = element_blank()
  )+
  scale_fill_gradientn(
    #name = "Global mortality rate", 
    name = "",
    colours = brewer.pal(9, name = "YlOrRd")
  )
}
p <- lapply(
  levels(mm$Type)[-c(1,6)],
  intermediate_plot, 
  mm = mm
)

p_infant <- ggplot(
  data = subset(
    mm,
    Type == "Infant"
  ),
  aes(
    x = long,
    y = lat.x,
    group = id,
    fill = value
  )
) +
  geom_polygon(
    alpha = .6
  ) +
  geom_path(color = "white")+
  #geom_point() + #los del polígono
  facet_grid(Type ~ times)+
  coord_equal() +
  #xlab("Longitude")+
  #ylab("Latitude")+
  #ylab(" ")+
  theme_bw()+
  theme(
    #legend.position = "left",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_line(colour = "transparent")
    #title = element_blank()
    #axis.text = element_blank()
  )+
  scale_fill_gradientn(
    #name = "Global mortality rate", 
    name = "",
    colours = brewer.pal(9, name = "YlOrRd")
  )
#p_infant
#grid.arrange

raw <- plot_grid(
  plotlist = list(
    p_global,
    p[[1]],
    p[[2]],
    p[[3]],
    p[[4]],
    p_infant
  ),
  ncol = 1
)
raw
ggsave(
  raw,
  file = paste(opt$out,"Space_mortality_borough_crude.pdf",sep=""),
  width = 7,
  height= 11
)
################################################################################
## Neigbourhood level for 2000, 2005, 2010, 2015
################################################################################
#p_global
mm <- add_mortality(df_borough, m_complete)
mm$times <- format(mm$times, "%Y")
mm <- subset(
  mm,
  Type == "Global"
)
mm$Type <- "Borough level"

p_global <- ggplot(
  data = mm,
  aes(
    x = long,
    y = lat.x,
    group = id,
    fill = value
  )
) +
  geom_polygon(
    alpha = .6
  ) +
  geom_path(color = "white")+
  #geom_point() + #los del polígono
  facet_grid(Type ~ times)+
  coord_equal() +
  xlab("Longitude")+
  ylab("Latitude")+
  #ylab(" ")+
  theme_bw()+
  theme(
    #legend.position = "left",
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank(),
    #axis.title.x = element_blank(),
    panel.grid = element_line(colour = "transparent"),
    axis.text.x = element_text(angle = 35, h = 1)
    #title = element_blank()
    #axis.text = element_blank()
  )+
  scale_fill_gradientn(
    name = "Rate", 
    colours = brewer.pal(9, name = "YlOrRd"),
    limits=c(3.6,10)
  )
p_global

add_kriging_data <- function(
  df_neigh, 
  m_complete, 
  time = as.Date(
    paste(
      seq(from = 2000, to = 2016, by = 5),
      "-01-01", 
      sep = ""
    )
  ),
  type = "Global",
  Level = "Neighbourhood"
){
  #Copy original data
  map <- df_neigh
  
  #Select time + level + type level
  kriged <- subset(
    m_complete,
    Type %in% type &
      level %in% Level &
      times %in% time
  )
  head(kriged,2)
  
  mm <- merge(
    map, 
    kriged,
    by.x = "id",
    by.y = "OBJECTID",
    all.x = TRUE,
    all.y = FALSE
  )
  return(mm)
}

mm <- add_kriging_data(df_neigh, m_complete)
mm$times <- format(mm$times, "%Y")
mm$Type <- "Neighbourhood level"
  
p_neigh_global <- ggplot(
  data = mm,
  aes(
    x = long,
    y = lat.x,
    group = group,
    fill = value
  )
)+
  geom_polygon(
    alpha = .6
  ) +
  facet_grid(Type ~ times)+
  coord_equal() +
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+
  theme(
    panel.grid = element_line(colour = "transparent"),
    axis.text.x = element_text(angle = 35, h = 1)
  )+
  scale_fill_gradientn(
    #name = "Global mortality rate", 
    name = "Rate",
    colours = brewer.pal(9, name = "YlOrRd"),
    limits=c(3.6,10)
  )
p_neigh_global

mm$Type <- "Cuauhtémoc level"
p_neigh_global_zoom <- ggplot()+
  geom_polygon(
    data = subset(
      mm,
      Borough == "CUAUHTÉMOC"
    ),
    aes(
      x = long,
      y = lat.x,
      group = group,
      fill = value
    ),
    alpha = .6
  ) +
  facet_grid(Type ~ times)+
  coord_equal() +
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+
  theme(
    panel.grid = element_line(colour = "transparent"),
    axis.text.x = element_text(angle = 35, h = 1)
  )+
  scale_fill_gradientn(
    #name = "Global mortality rate", 
    name = "Rate",
    colours = brewer.pal(9, name = "YlOrRd"),
    limits=c(3.6,10)
  )
  # geom_point(
  #   data = kriging_coords,
  #   aes(x = lon, y = lat),
  #   color = "black"
  # )+
  #xlim(range(df_borough[df_borough$MUN_NAME == "CUAUHTÉMOC",]$long))+
  #ylim(range(df_borough[df_borough$MUN_NAME == "CUAUHTÉMOC",]$lat))
p_neigh_global_zoom

p_global_borough_neigh <- plot_grid(
  plotlist = list(
    p_global,
    p_neigh_global,
    p_neigh_global_zoom
  ),
  ncol = 1,
  labels = c("A", "B","C")
)
p_global_borough_neigh
ggsave(
  p_global_borough_neigh,
  file = paste(opt$out, "Spatial_time_global_3level_zoom.pdf", sep = ""),
  width = 8,
  height = 8,
  device = cairo_pdf  
)

################################################################################
##borough with numbers
################################################################################
p_boroughs <- ggplot() +
  geom_polygon(
    data = df_borough,
    aes(
      x = long,
      y = lat,
      group = id
    ),
    alpha = .6,
    fill = "transparent"
  ) +
  geom_path(
    data = df_borough,
    aes(
      x = long,
      y = lat,
      group = group
    ),
    color = "black"
  )+
  geom_text(
    data = boroughs@data,
    aes(
      x = x,
      y = y,
      label = ID
    ),
    color = "red"
  ) + 
  coord_equal() +
  xlab("Longitude")+
  ylab("Latitud")+
  theme_bw()+
  annotation_north_arrow(
    location = "bl", 
    which_north = "TRUE", 
    pad_x = unit(0.3, "in"), 
    pad_y = unit(0.4, "in"),
    style = north_arrow_fancy_orienteering
  )+
  annotation_scale()+
  coord_sf(crs = 4326)+
  theme(
    panel.grid = element_line(colour = "transparent")
  )
p_boroughs
ggsave(
  p_boroughs,
  file = paste(opt$out, "CDMX_map_borough.pdf", sep = ""),
  width = 5,
  height = 5,
  device = cairo_pdf
)

#############################################
##Plot jointly borough + neighbourhood the graphs
#############################################
p_complete <- ggplot()+
  geom_polygon(
    data = df_neigh,
    aes(
      x = long,
      y = lat,
      group = id
    ),
    alpha = .6
  ) +
  geom_path(
    data = df_neigh,
    aes(
      x = long,
      y = lat,
      group = group
    ),
    color = "white"
  ) + 
  geom_path(
    data = df_borough,
    aes(
      x = long,
      y = lat,
      group = group
    ),
    color = "blue"
  ) + 
  geom_point(
    data = boroughs@data,
    aes(
      x = x,
      y = y
    ),
    color = "red"
  ) + 
  coord_equal() +
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+
  annotation_north_arrow(
    location = "bl", 
    which_north = "TRUE", 
    pad_x = unit(0.3, "in"), 
    pad_y = unit(0.4, "in"),
    style = north_arrow_fancy_orienteering
  )+
  annotation_scale()+
  coord_sf(crs = 4326)+
  theme(
    panel.grid = element_line(colour = "transparent")
  )
p_complete 
ggsave(
  p_complete,
  file = paste(opt$out, "CDMX_map.pdf", sep = ""),
  width = 5,
  height = 5,
  device = cairo_pdf
)

p_map <- plot_grid(
  plotlist = list(
    p_boroughs,
    p_complete
  ),
  ncol = 2,
  labels = c("A", "B")
)
p_map
ggsave(
  p_map,
  file = paste(opt$out, "CDMX_map_double.pdf", sep = ""),
  width = 10,
  height = 5,
  device = cairo_pdf
)

############################################################################
## The end
############################################################################

