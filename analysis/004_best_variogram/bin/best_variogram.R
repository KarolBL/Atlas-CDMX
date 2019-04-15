#!/usr/bin/env Rscript
############################################################
## Goal: Obtain the best variogram model 
############################################################
library("optparse")
library("parallel")
library("gstat")
library("sp")
library("spacetime")
library("gtools")
library("reshape2")
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
    default = "7", 
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
# opt$file <- "../data/mortality_general.RData"
# opt$out <- "../results/mortality_general.RData"
# opt$cores <- 3
options(mc.cores = opt$cores)
options(width = 150)
############################################################
## Read the data
load(opt$file)

############################################################
#Variogram models 
############################################################
##metric:          vgmST("metric",                  joint, stAni)
##separable:       vgmST("separable",  space, time, sill)
##productSum:      vgmST("productSum", space, time, k)
##sumMetric:       vgmST("sumMetric",  space, time, joint, stAni)
##simpleSumMetric: vgmST("simpleSumMetric", space, time, joint, nugget, stAni)

#RMSE statistics
create_RMSE_table <- function(fittedSTVariograms){
  lapply(fittedSTVariograms, function(model){
    do.call(c, lapply(model, function(variograma){
      attr(variograma, "optim")$value
    }))
  })
}

#Get the best vgmST fitted model
best_vgmST_model <- function(fittedSTVariograms){
  #Get the rmse of every model
  rmse <- create_RMSE_table(fittedSTVariograms)
  
  #Best covariance model
  covariance <- which.min(do.call(c,lapply(rmse, min)))
  
  #Best vgm model for the best covariance
  model <- which.min(rmse[[covariance]])
  
  #Get the name plus de best one
  bvgm <- fittedSTVariograms[[covariance]][[model]]
  
  out <- data.frame(
    rmse = rmse[[covariance]][model],
    covariance = bvgm$stModel, 
    join = ifelse(
      is.null(bvgm$join$model), 
      NA, 
      as.character(bvgm$join$model)
    ),
    space = ifelse(
      is.null(bvgm$space$model[2]),
      NA, 
      as.character(bvgm$space$model[2])
    ),
    time = ifelse(
      is.null(bvgm$time$model[2]),
      NA,
      as.character(bvgm$time$model[2])
    )
  )
  
  return(out)
}

best_vgmSTs <- function(fittedSTVariograms){
  #Get the rmse of every model
  rmse <- create_RMSE_table(fittedSTVariograms)
  
  #get the index of the best of each
  return(do.call(c, lapply(rmse, which.min)))
}


############################################################################
#Running the code
############################################################################
vgm_table <- create_RMSE_table(fittedSTVariograms)
do.call(rbind, vgm_table)

#Winner covariance structure
winner <- best_vgmSTs(fittedSTVariograms)
# metric       separable      productSum       sumMetric simpleSumMetric 
# 2               3               2              15               3 

#Best fitted variograms
best <- best_vgmST_model(fittedSTVariograms)
#        rmse covariance join space time
# 1 0.3172613  sumMetric  Nug   Sph  Gau

best_model <- fittedSTVariograms[[as.character(best_vgmST$covariance)]][[best_vgmST_models[as.character(best_vgmST$covariance)]]]

#Get the best covariance model to use
bestmodel2use <- fittedSTVariograms[[
  as.character(best$covariance)
  ]][[
  winner[as.character(best$covariance)]
]]
#attr(bestmodel2use, "spatial unit") <- ""

save(
  bestmodel2use, 
  file = opt$out,
  compress = "xz"
)
############################################################################
## The end
############################################################################
