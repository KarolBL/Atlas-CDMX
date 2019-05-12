#!/usr/bin/env Rscript
############################################################
## Goal: Get the range panel
############################################################
library("optparse")
library("parallel")
library("reshape")
library("ggplot2")
library("cowplot")
library("RColorBrewer")
############################################################
option_list <- list(
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
if(is.null(opt$out)){
  print_help(opt_parser)
  stop("At least one argument must be supplied (output file).n", call.=FALSE)
}
############################################################
# Debuging
# opt$out <- "../results/range_panel.pdf"
options(mc.cores = opt$cores)
############################################################
#Generate the contaminant radios according to the corresponding year
ranges <- data.frame(
  Contaminant = c("CO", "NO2",  "O3", "SO2", "PM25", "PM10",  "NO", "NOX", "PMCO"),
  Spatial =    c(20.72, 43.37, 57.82,  5.92,     13,  12.98, 77.15, 84.44,  28.04), #km
  Temporal=    c(   12, 27.49, 73.36, 37.47,  36.65, 178.95,133.50,175.61,  21.24)#days
)
ranges$Contaminant <- factor(
  as.character(ranges$Contaminant),
  levels = c("CO", "NO2",  "O3", "SO2", "PM25", "PM10",  "NO", "NOX", "PMCO")[9:1]
)
ranges<- ranges[9:1, ]
ranges <- melt(
  ranges,
  id.vars = "Contaminant"
)

p1 <- ggplot(
  data = subset(
    ranges,
    variable == "Spatial"
  ), 
  aes(
    x = Contaminant,
    y = value,
    fill = Contaminant
  )
)+
  geom_bar(stat = "identity")+
  ylab("Distance [km]")+
  facet_grid(. ~ variable)+
  coord_flip()+
  theme_bw()+
  theme(
    legend.position = "NULL",
    panel.grid = element_blank()#,
    #axis.text.y = label_parsed
  ) +
  scale_x_discrete(
    "Contaminant", 
    labels = c("CO", expression(NO[2]),  expression(O[3]), expression(SO[2]),
               expression(PM[25]), expression(PM[10]),  "NO", expression(NO[X]),
               expression(PM[CO]))[9:1]
  )+
  scale_fill_discrete(direction = -1)
p1

ranges$Contaminant <- factor(
  as.character(ranges$Contaminant),
  levels = c("CO", "NO2",  "O3", "SO2", "PM25", "PM10",  "NO", "NOX", "PMCO")
)

p2 <- ggplot(
  data = subset(
    ranges,
    variable == "Temporal"
  ), 
  aes(
    x = Contaminant,
    y = value,
    fill = Contaminant
  )
)+
  geom_bar(stat = "identity")+
  ylab("Time [days]")+
  facet_grid(. ~ variable)+
  #coord_flip()+
  theme_bw()+
  theme(
    legend.position = "NULL",
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1)
  )+
  scale_x_discrete(
    "Contaminant", 
    labels = c("CO", expression(NO[2]),  expression(O[3]), expression(SO[2]),
               expression(PM[25]), expression(PM[10]),  "NO", expression(NO[X]),
               expression(PM[CO]))
  )

p <- plot_grid(
  plotlist = list(p1, p2),
  labels = c("A", "B"),
  ncol = 2
) 
#p

ggsave(
  p,
  file = opt$out, 
  width = 7,
  height = 5,
  device = cairo_pdf 
)

