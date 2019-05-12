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
library("xtable")
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
    c("-i", "--initial"), 
    type = "character", 
    default = NULL, 
    help = "output initial csv file name [default= %default]", 
    metavar = "character"
  ),
  make_option(
    c("-r", "--rmse"), 
    type = "character", 
    default = NULL, 
    help = "output rsme file name [default= %default]", 
    metavar = "character"
  ),
  make_option(
    c("-w", "--winner"), 
    type = "character", 
    default = NULL, 
    help = "output winner file name [default= %default]", 
    metavar = "character"
  ),
  make_option(
    c("-b", "--best"), 
    type = "character", 
    default = NULL, 
    help = "output best vgmST file name [default= %default]", 
    metavar = "character"
  ),
  make_option(
    c("-p", "--pdf"), 
    type = "character", 
    default = NULL, 
    help = "output variogram pdf file name [default= %default]", 
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
# opt$file <- "../data/pollution/na/NO2.RData"
# opt$out <- "../results/pollution/na/NO2.RData"
# opt$initial <- "../results/pollution/na/NO2.initial.txt"
# opt$rmse <- "../results/pollution/na/NO2.rsme.txt"
# opt$winner <- "../results/pollution/na/NO2.winner.txt"
# opt$best <- "../results/pollution/na/NO2.best.txt"
# opt$pdf <- "../results/pollution/na/NO2.pdf"
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
#vgm_initial
initial_vgm <- print(xtable(do.call(rbind, vgm_initial)))
write(initial_vgm, file = opt$initial)

#vgm RMSE table
vgm_table <- create_RMSE_table(fittedSTVariograms)
vgm_table <- rbind(
  metric =c(vgm_table$metric, rep(NA_real_, 9-3)),
  do.call(rbind, vgm_table[c("separable", "productSum")]),
  matrix(
    vgm_table$sumMetric, 
    nrow = 3, 
    ncol = 9, 
    byrow = TRUE,
    dimnames = list(
      c("Exp", "Gau", "Sph"),
      NULL
    )
  ),
  matrix(
    vgm_table$simpleSumMetric, 
    nrow = 3, 
    ncol = 9, 
    byrow = TRUE,
    dimnames = list(
      c("Exp", "Gau", "Sph"),
      NULL
    )
  )
)

vgm_table <- print(xtable(vgm_table, display = rep("E", ncol(vgm_table)+1), digits = 7))
write(vgm_table, file = opt$rmse)

#Winner covariance structure
winner <- best_vgmSTs(fittedSTVariograms)
write.csv(winner, file = opt$winner)
# metric       separable      productSum       sumMetric simpleSumMetric 
# 2               3               2              15               3 

#Best fitted variograms
best <- best_vgmST_model(fittedSTVariograms)
#        rmse covariance join space time
# 1 0.3172613  sumMetric  Nug   Sph  Gau
write.csv(best, file = opt$best)

#Get the best covariance model to use
bestmodel2use <- fittedSTVariograms[[
  as.character(best$covariance)
  ]][[
  winner[as.character(best$covariance)]
]]
#attr(bestmodel2use, "spatial unit") <- ""

#Plotting the variograms
pdf(
    file = opt$pdf,
    width = 7, height = 7
)  
plot(
  sample_vgmST, 
  list(
    fittedSTVariograms$metric[[winner["metric"]]],
    fittedSTVariograms$separable[[winner["separable"]]],
    fittedSTVariograms$productSum[[winner["productSum"]]],
    fittedSTVariograms$sumMetric[[winner["sumMetric"]]],
    fittedSTVariograms$simpleSumMetric[[winner["simpleSumMetric"]]] 
  ),
  all = TRUE, 
  layout = c(3,2)
)
dev.off()

save(
  bestmodel2use, 
  file = opt$out,
  compress = "xz"
)
############################################################################
## The end
############################################################################
