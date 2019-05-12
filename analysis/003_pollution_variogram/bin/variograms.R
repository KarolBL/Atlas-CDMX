#!/usr/bin/env Rscript
############################################################
## Goal: Obtain the empirical and best teorical variogram 
############################################################
library("optparse")
library("parallel")
library("gstat")
library("sp")
library("spacetime")
library("aire.zmvm")
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
# opt$file <- "../data/filtered/O3.RData"
# opt$out <- "../results/filtered/O3.RData"
# opt$cores <- 3
options(mc.cores = opt$cores)
options(width = 150)
############################################################
## Read the data
load(opt$file)
contaminant <- contaminant[order(contaminant$station_code, contaminant$date), ]
# head(contaminant, 2)
#            date station_code pollutant unit    value week year
# 15   2009-01-01          ACO        O3  ppb 30.25669    1 2009
# 2466 2009-01-07          ACO        O3  ppb 27.96528    2 2009
contaminant <- contaminant[, c("date", "station_code", "value")]
 
##Stations
#head(stations, 2)
#   station_code station_name       lon      lat altitude comment   station_id
# 1          ACO      Acolman -98.91200 19.63550     2198         484150020109
# 2          AJU       Ajusco -99.16261 19.15429     2942         484090120400

############################################################
# Create the SP object
stations <- stations[, c("station_code", "lon", "lat")]
row.names(stations) <- stations$station_code
#Filter the one available in the contaminant object
stations <- stations[
    stations$station_code %in% unique(contaminant$station_code), 
]
stations <- stations[order(stations$station_code), ]
#Transform into coordinates
coordinates(stations) <- ~ lon + lat
stations <- SpatialPoints(
  stations, 
  proj4string = CRS("+proj=longlat +datum=WGS84")
)

#Times 
times <- unique(contaminant$date)
times <- sort(times)

#Data
#Contaminant
datos <- mclapply(
    times,
    function(week){
        #For each week
        do.call(c,
            lapply(
                row.names(stations@coords),
                function(station){
                    #For each station
                    #Get the values
                    out <- subset(
                        contaminant,
                        station_code == station &
                        date == week
                    )$value
                    out <- ifelse(
                        length(out) == 0,
                        NA_real_,
                        out
                    ) 
                }
            )
        )
    }
)
datos <- do.call(
    c,
    datos
)
############################################################
# Create the STFDF object (spatial index moving fastest)
contaminant_stfdf <- STFDF(
  sp = stations,
  time = times,
  data = data.frame(value = datos)
)

##Adjust the empirical variogram
sample_vgmST <- variogramST(
  value ~ 1, 
  contaminant_stfdf,
  assumeRegular = FALSE,
  progress = FALSE,
  na.omit = TRUE #FALSE
)

############################################################
##Get initial parameter values 
get_initial_values <- function(sample_vgmST, stations){
  #Check for class
  stopifnot(all(class(sample_vgmST) %in% c("StVariogram", "data.frame" )))
  
  #Get the matrix vgst 
  vgst <- data.frame(
    x = sample_vgmST$spacelag, 
    t = sample_vgmST$timelag,
    gamma = sample_vgmST$gamma
  )
  vgst_matrix <- acast(vgst, x ~ t, value.var = "gamma")
  
  ##Let's get the mean of each row(time)/column(space) for the initial guess
  #Parameter: nugget mean(gamma[1:3])
  #Parameter: sill mean(gamma[-(0:4)+length(gamma)])
  #Parameter: range 1/3 estación mas lejana
  nugget <- c(
    t = median(
      rowMeans(vgst_matrix[, 1:3], na.rm = TRUE), 
      na.rm = TRUE
    ),
    s = median(
      colMeans(vgst_matrix[1:3, ], na.rm = TRUE),
      na.rm = TRUE
    )
  )
  nugget <- c(
    nugget,
    join = mean(nugget)
  )
  sill <- c(
    t = median(
      rowMeans(vgst_matrix[,-(0:4)+ncol(vgst_matrix)], na.rm = TRUE),
      na.rm = TRUE
    ),
    s = median(
      colMeans(vgst_matrix[-(0:4)+nrow(vgst_matrix),], na.rm = TRUE),
      na.rm = TRUE
    )
  )
  sill <- c(
    sill,
    join = mean(sill)
  )
  
  #range
  coordinates <- as.matrix(coordinates(stations)[, c("lon", "lat")])
  colnames(coordinates) <- c("x", "y")
  prange <- c(
    t = 12,
    s = 1/3 * max(spDists(coordinates, longlat = TRUE)) 
  )
  prange <- c(
    prange,
    join = mean(prange)
  )
  #stAni
  stAni <- estiStAni(
    sample_vgmST, 
    interval = c(0, 1000), 
    method = "linear"
  )
  
  return(
    list(
      nugget = nugget,
      sill = sill,
      range = prange,
      stAni = stAni
    )
  )
}

############################################################
#Obtain the vgm model using the initial guess 
generate_vgm <- function(
  type = c("s", "t", "join")[1],
  model = c("Exp", "Sph", "Gau", "Mat")[1],
  vgm_initial){
  #Check parameters
  stopifnot(type %in% c("s", "t", "join"))
  stopifnot(model %in% c("Exp", "Sph", "Gau", "Mat"))
  stopifnot(!missing(vgm_initial))
  stopifnot(all(names(vgm_initial) %in% c("nugget", "sill", "range", "stAni")))
  
  return(
    vgm(
      psill = vgm_initial$sill[type], 
      model = model,
      range = vgm_initial$range[type],
      nugget = vgm_initial$nugget[type]
    )
  )
}

############################################################
#Generate the vgmST model using the intial empirical values
generate_vgmST <- function(
  vgmST_model = c(
    "metric", "separable", "productSum", "sumMetric", "simpleSumMetric"
  )[1],
  vgm_model_join = c("Exp", "Sph", "Gau", "Mat")[1],
  vgm_model_t = c("Exp", "Sph", "Gau", "Mat")[1],
  vgm_model_s = c("Exp", "Sph", "Gau", "Mat")[1],
  vgm_initial
){
  
  #Check parameters
  stopifnot(
    all(
      vgmST_model %in% c(
        "metric", "separable", "productSum", "sumMetric", "simpleSumMetric"
      )
    )
  )
  stopifnot(vgm_model_join %in% c("Exp", "Sph", "Gau", "Mat"))
  stopifnot(vgm_model_s %in% c("Exp", "Sph", "Gau", "Mat"))
  stopifnot(vgm_model_t %in% c("Exp", "Sph", "Gau", "Mat"))
  stopifnot(!missing(vgm_initial))
  stopifnot(all(names(vgm_initial) %in% c("nugget", "sill", "range", "stAni")))
  
  out <- switch(vgmST_model,
                "metric" = vgmST(
                  "metric",
                  joint = generate_vgm(
                    type = "join",
                    model = vgm_model_join,
                    vgm_initial
                  ),
                  stAni = vgm_initial$stAni 
                ),
                "separable" = vgmST(
                  "separable",
                  space = generate_vgm(
                    type = "s",
                    model = vgm_model_s,
                    vgm_initial
                  ),
                  time = generate_vgm(
                    type = "t",
                    model = vgm_model_t,
                    vgm_initial
                  ),
                  sill = vgm_initial$sill["join"]
                ),
                "productSum" = vgmST(
                  "productSum", 
                  space = generate_vgm(
                    type = "s",
                    model = vgm_model_s,
                    vgm_initial
                  ), 
                  time = generate_vgm(
                    type = "t",
                    model = vgm_model_t,
                    vgm_initial
                  ),
                  k = vgm_initial$stAni
                ),
                "sumMetric" = vgmST(
                  "sumMetric",  
                  space = generate_vgm(
                    type = "s",
                    model = vgm_model_s,
                    vgm_initial
                  ),
                  time = generate_vgm(
                    type = "t",
                    model = vgm_model_t,
                    vgm_initial
                  ),
                  joint = generate_vgm(
                    type = "join",
                    model = vgm_model_join,
                    vgm_initial
                  ),
                  stAni = vgm_initial$stAni
                ),
                "simpleSumMetric" = vgmST(
                  "simpleSumMetric", 
                  space = generate_vgm(
                    type = "s",
                    model = vgm_model_s,
                    vgm_initial
                  ),
                  time = generate_vgm(
                    type = "t",
                    model = vgm_model_t,
                    vgm_initial
                  ),
                  joint = generate_vgm(
                    type = "join",
                    model = vgm_model_join,
                    vgm_initial
                  ), 
                  nugget = vgm_initial$nugget["join"], 
                  stAni = vgm_initial$stAni
                )
  )
  
  return(out)
}

############################################################
##metric:          vgmST("metric",                  joint, stAni)
##separable:       vgmST("separable",  space, time, sill)
##productSum:      vgmST("productSum", space, time, k)
##sumMetric:       vgmST("sumMetric",  space, time, joint, stAni)
##simpleSumMetric: vgmST("simpleSumMetric", space, time, joint, nugget, stAni)

generate_parameters <- function(){
  vgms <- c("Exp", "Sph", "Gau")
  vgms2 <- t(permutations(n = length(vgms), r = 2, v = vgms, repeats.allowed = TRUE))
  vgms3 <- t(permutations(n = length(vgms), r = 3, v = vgms, repeats.allowed = TRUE))
  parameters <- list(
    ##metric:     vgmST("metric",     joint,        stAni)
    metric = list(
      vgm_model_join = vgms
    ),
    ##separable:           vgmST("separable",  space, time, sill)
    separable = list(
      vgm_model_t = vgms2[1,],
      vgm_model_s = vgms2[2,]
    ),
    ##productSum:          vgmST("productSum", space, time, k)
    productSum = list(
      vgm_model_t = vgms2[1,],
      vgm_model_s = vgms2[2,]
    ),
    ##sumMetric:           vgmST("sumMetric",  space, time, joint, stAni)
    sumMetric = list(
      vgm_model_join = vgms3[1, ],
      vgm_model_t = vgms3[2, ],
      vgm_model_s = vgms3[3, ]
    ),
    ##simpleSumMetric: vgmST("simpleSumMetric", space, time, joint, nugget,   stAni)
    simpleSumMetric = list(
      vgm_model_join = vgms3[1, ],
      vgm_model_t = vgms3[2, ],
      vgm_model_s = vgms3[3, ]
    )
  )
  return(parameters)
}

#generate_vgmST_complete
generate_vgmST_complete <- function(parameters, vgm_initial){
  out <- lapply(names(parameters), function(modelo){
    lapply(1:length(parameters[[modelo]][[1]]), function(vario){
      generate_vgmST(
        vgmST_model = modelo,
        vgm_model_join = parameters[[modelo]]$vgm_model_join[vario],
        vgm_model_t = parameters[[modelo]]$vgm_model_t[vario],
        vgm_model_s = parameters[[modelo]]$vgm_model_s[vario],
        vgm_initial
      )
    })  
  })
  names(out) <- names(parameters)
  return(out)
}

#Finally!!! fit the ST variogram
fit_Variogram_ST <- function(
  sample_vgmST, 
  vgmST2Try,
  verbose = TRUE
){
  output <- lapply(names(vgmST2Try), function(model){
    if (verbose) {
      message(paste("Fitting ST variogram models for", model, "..."))  
    }
    fittedvariograma <- mclapply(vgmST2Try[[model]], function(variograma){
      out <- fit.StVariogram(
        sample_vgmST, 
        variograma,
        method = "L-BFGS-B",
        lower = c(0.001,0.001,0.001,0.001,0.001)
      )
#       save(
#         out,
#         file = paste("model_", model, ".RData", sep = ""),
#         compress = "xz"
#       )
      return(out)
    }
    )
    if (verbose) {
      message(paste("Fitting ST variogram models for", model, 
                    "... done!!!"))  
    }
    return(fittedvariograma)
  })
  names(output) <- names(vgmST2Try)
  return(output)
}

############################################################################
#Running the code
############################################################################
#sample_vgmST
vgm_initial <- get_initial_values(
  sample_vgmST, 
  stations
)
parameters  <- generate_parameters()
vgmST2Try <- generate_vgmST_complete(
  parameters, 
  vgm_initial
)
fittedSTVariograms <- fit_Variogram_ST(
  sample_vgmST, 
  vgmST2Try, 
  verbose = TRUE
)
                                       
save(
  sample_vgmST,
  vgm_initial,
  fittedSTVariograms,
  file = opt$out,
  compress = "xz"
)

############################################################################
## The end
############################################################################
