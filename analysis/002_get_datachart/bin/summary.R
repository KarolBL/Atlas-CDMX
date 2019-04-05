#!/usr/bin/env Rscript
############################################################
## Goal: Summary the data using a 75% criteria
############################################################
library("aire.zmvm")
library("optparse")
library("parallel")
options(mc.cores = 7)
############################################################
option_list <- list(
  make_option(
    c("-f", "--file"), 
    type = "character", 
    default = NULL, 
    help = "dataset file name", 
    metavar = "character"),
  make_option(
    c("-o", "--out"), 
    type = "character", 
    default = "out.txt", 
    help = "output file name [default= %default]", 
    metavar = "character"
  )
) 

#Build the parse object
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

##Check for parsing options
if (is.null(opt$file)){
  print_help(opt_parser)
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}
############################################################
# Debuging
# opt$file <- "../data/CO.RData"
# opt$out <- "../results/CO.RData"
############################################################

  
  
  
  
