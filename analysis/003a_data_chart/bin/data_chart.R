#!/usr/bin/env Rscript
############################################################
## Goal: Get the data chart for each contaminant
############################################################
library("aire.zmvm")
library("optparse")
library("parallel")
library("reshape")
library("ggplot2")
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
    default = "1", 
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
# opt$file <- "../data/missing/O3.RData"
# opt$out <- "../results/O3.pdf"
options(mc.cores = opt$cores)
############################################################
#Load the contaminant data.frame object
load(opt$file)

###############################################################################
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
