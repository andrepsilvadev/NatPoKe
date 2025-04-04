## Name: SpeciesDistributionModellingTest ##
## Author: Jorinde-M. Rieger ##
## Description: test SDM main function with true species occurence in R ##
## Date: April 4th 2025 ##

# Settings & libraries -----------------------------------------------------------------
library(easypackages)
packages("readr","ggplot2","RColorBrewer",
         "rworldmap","sp","raster", "gam","mda", "earth", "maxnet", "ggtext","xgboost",
         "rgbif","biomod2", "dplyr", 
         "sf", "rnaturalearth", "rnaturalearthdata","terra", "tidyterra", "ggpubr", "randomForest", prompt = FALSE)

source("./src/libraries.R") # libraries
source("./src/customFunctions.R") # functions
source("./scripts/inputSpeciesData.R") # format and reads input data
source("./scripts/inputClimate.R") # format and reads input raster landscapes

# customFunctions.R -----------------------------------------------------------------

formatInputDataFrame <- function(speciesData, targetSpecies, landscape){
  # speciesData = species record coordinates with the following format c("species", "latitude", "longitude")
  # targetSpecies = species to be modelled
  # landscape = raster with all the environmental variables we wish to use

  speciesStack <- list()
  
  for(i in 1:length(targetSpecies)){
    #i = 1
    subset <- speciesData %>% dplyr::filter(species == targetSpecies[i])
    xy <- data.frame(x=subset$decimalLongitude,
                     y=subset$decimalLatitude)
    # Convert to SpatVector ensure CRS consistency
    xy_vect <- terra::vect(xy, geom = c("x", "y"), crs = terra::crs(landscape))
    spRaster <- terra::rasterize(xy_vect, landscape[[1]], fun="count")
    
    # reclassify species raster 
    m <- c(NA, NA, NA,
           0, +Inf, 1)
    rclmat <- matrix(m, ncol=3, byrow=TRUE) # criteria for reclassification
    rc <- terra::classify(spRaster, rclmat)
    names(rc) <- paste0(targetSpecies[i])
    speciesStack[[i]] <- rc
  }
  # Combine all species rasters into a single SpatRaster
  speciesStack <- terra::rast(speciesStack)
  
  # join species and environmental data
  fullData <- c(speciesStack, landscape)
  plot(fullData)
  xylandscape <- terra::crds(landscape, df = TRUE)
  inputDataFrame <- terra::extract(fullData, # raster or rasterstack
                                   xylandscape,
                                    method='simple', # or "bilinear" - value of the four nearest raster cells
                                    cells=TRUE)
  #xylandscape <- terra::crds(landscape, df = TRUE)
  inputDataFrame <- cbind(xylandscape, inputDataFrame)
  
  write.csv(inputDataFrame,"~/data/data/inputDataFrame.csv", row.names = FALSE)
  return(inputDataFrame)
}


# load dataset and variables -----------------------------------------------------------------
# Crop the landscapes to the extent of the biome
biome_name <- "Tropical & Subtropical Moist Broadleaf Forests"
biome_name <- "Boreal Forests/Taiga"

# Function to load and select the biome shapefile
load_select_biome <- function(biome_name) {
  biome_sf <- st_read("~/data/data/Ecoregions2017/Ecoregions2017/Ecoregions2017.shp")
  biome_sf[biome_sf$BIOME_NAME == biome_name, ]}

biome_sf <- load_select_biome(biome_name)
biome_sp <- vect(biome_sf)

# Function to crop and mask rasters to biome
crop_mask_raster <- function(raster, biome_sp) {
  mask(crop(raster, biome_sp), biome_sp)}

# Crop and mask trainingLandscapes to biome extent
trainingLandscapes <- crop_mask_raster(trainingLandscapes, biome_sp)
plot(trainingLandscapes)

# Select the name of the studied species
targetSpecies <- c("Alces alces", "Canis lupus")

# Format species occurence to true presence and NAs with corresonding coordinates
# test with trainingLandscape? - use as species input data
speciesData <- formatInputDataFrame(
  speciesData = speciesDataOcc,
  targetSpecies = targetSpecies, 
  landscape = trainingLandscapes)
head(speciesData)


# Get corresponding presence/absence data
myResp <- as.numeric(speciesData[, targetSpecies[[1]]])

# Get corresponding presence/absence data
myResp <- as.matrix(speciesData[, targetSpecies[[1]]])  # Convert to a matrix
myResp <- as.numeric(myResp)  # Flatten the matrix into a numeric vector
View(myResp)

# Get corresponding XY coordinates
myRespXY <- speciesData[, c('x', 'y')]


## Crop the data to biome extent
biome_name <- "Tropical & Subtropical Moist Broadleaf Forests"
biome_name <- "Boreal Forests/Taiga"

# Function to load and select the biome shapefile
load_select_biome <- function(biome_name) {
biome_sf <- st_read("~/data/data/Ecoregions2017/Ecoregions2017/Ecoregions2017.shp")
biome_sf[biome_sf$BIOME_NAME == biome_name, ]}

biome_sf <- load_select_biome(biome_name)
biome_sp <- vect(biome_sf)

# Function to crop and mask rasters to biome
crop_mask_raster <- function(raster, biome_sp) {
mask(crop(raster, biome_sp), biome_sp)}

# Crop and mask trainingLandscapes to biome extent
trainingLandscapes <- crop_mask_raster(trainingLandscapes, biome_sp)

plot(trainingLandscapes)

# Prepare data & Parameters -----------------------------------------------------------------

# Format Data with true presences
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp, # myResp
                                     expl.var = trainingLandscapes, # myExpl
                                     resp.xy = myRespXY, 
                                     resp.name = targetSpecies[[1]]) # myRespNames
myBiomodData
plot(myBiomodData)

# Prseudo-absence extraction -----------------------------------------------------------------
# # Transform true absences into potential pseudo-absences
# myResp.PA <- ifelse(myResp == 1, 1, NA)
# 
# # Format Data with pseudo-absences : random method
# myBiomodData.r <- BIOMOD_FormatingData(resp.var = myResp.PA,
#                                        expl.var = myExpl,
#                                        resp.xy = myRespXY,
#                                        resp.name = myRespName,
#                                        PA.nb.rep = 4,
#                                        PA.nb.absences = 1000,
#                                        PA.strategy = 'random')
# 
# myBiomodData.r
# plot(myBiomodData.r)
#
# # Select multiple sets of pseudo-absences
#
# # Transform true absences into potential pseudo-absences
# myResp.PA <- ifelse(myResp == 1, 1, NA)
# 
# # Format Data with pseudo-absences : random method
# myBiomodData.multi <- BIOMOD_FormatingData(resp.var = myResp.PA,
#                                            expl.var = myExpl,
#                                            resp.xy = myRespXY,
#                                            resp.name = myRespName,
#                                            PA.nb.rep = 4,
#                                            PA.nb.absences = c(1000, 500, 500, 200),
#                                            PA.strategy = 'random')
# myBiomodData.multi
# summary(myBiomodData.multi)
# plot(myBiomodData.multi)

# Cross-validation dataset -----------------------------------------------------------------
# # k-fold selection
# cv.k <- bm_CrossValidation(bm.format = myBiomodData,
#                            strategy = "kfold",
#                            nb.rep = 2,
#                            k = 3)
# 
# # stratified selection (geographic)
# cv.s <- bm_CrossValidation(bm.format = myBiomodData,
#                            strategy = "strat",
#                            k = 2,
#                            balance = "presences",
#                            strat = "x")
# head(cv.k)
# head(cv.s)

# Retrieve modeling options -----------------------------------------------------------------
# bigboss parameters
# opt.b <- bm_ModelingOptions(data.type = 'binary',
#                             models = c('SRE', 'XGBOOST'),
#                             strategy = 'bigboss')
# 
# # tuned parameters with formated data
# opt.t <- bm_ModelingOptions(data.type = 'binary',
#                             models = c('SRE', 'XGBOOST'),
#                             strategy = 'tuned',
#                             bm.format = myBiomodData)
# 
# opt.b
# opt.t

# Run modeling -----------------------------------------------------------------
# Model single models
myBiomodModelOut <- BIOMOD_Modeling(bm.format = myBiomodData,
                                    modeling.id = 'AllModels',
                                    CV.strategy = 'random',
                                    CV.nb.rep = 2,
                                    CV.perc = 0.8,
                                    OPT.strategy = 'bigboss',
                                    var.import = 3,
                                    metric.eval = c('TSS','ROC'))
# seed.val = 123)
# nb.cpu = 8)
myBiomodModelOut

# Get evaluation scores & variables importance
get_evaluations(myBiomodModelOut)
get_variables_importance(myBiomodModelOut)

# Represent evaluation scores & variables importance
bm_PlotEvalMean(bm.out = myBiomodModelOut)
bm_PlotEvalBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'algo'))
bm_PlotEvalBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'run'))
bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('expl.var', 'algo', 'algo'))
bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('expl.var', 'algo', 'run'))
bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'expl.var', 'run'))

# Represent response curves
bm_PlotResponseCurves(bm.out = myBiomodModelOut, 
                      models.chosen = get_built_models(myBiomodModelOut)[c(1:3, 12:14)],
                      fixed.var = 'median')
bm_PlotResponseCurves(bm.out = myBiomodModelOut, 
                      models.chosen = get_built_models(myBiomodModelOut)[c(1:3, 12:14)],
                      fixed.var = 'min')
bm_PlotResponseCurves(bm.out = myBiomodModelOut, 
                      models.chosen = get_built_models(myBiomodModelOut)[3],
                      fixed.var = 'median',
                      do.bivariate = TRUE)


# Model ensemble models
myBiomodEM <- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut,
                                      models.chosen = 'all',
                                      em.by = 'all',
                                      em.algo = c('EMmean', 'EMcv', 'EMci', 'EMmedian', 'EMca', 'EMwmean'),
                                      metric.select = c('TSS'),
                                      metric.select.thresh = c(0.7),
                                      metric.eval = c('TSS', 'ROC'),
                                      var.import = 3,
                                      EMci.alpha = 0.05,
                                      EMwmean.decay = 'proportional')
myBiomodEM

# Get evaluation scores & variables importance
get_evaluations(myBiomodEM)
get_variables_importance(myBiomodEM)

# Represent evaluation scores & variables importance
bm_PlotEvalMean(bm.out = myBiomodEM, group.by = 'full.name')
bm_PlotEvalBoxplot(bm.out = myBiomodEM, group.by = c('full.name', 'full.name'))
bm_PlotVarImpBoxplot(bm.out = myBiomodEM, group.by = c('expl.var', 'full.name', 'full.name'))
bm_PlotVarImpBoxplot(bm.out = myBiomodEM, group.by = c('expl.var', 'algo', 'merged.by.run'))
bm_PlotVarImpBoxplot(bm.out = myBiomodEM, group.by = c('algo', 'expl.var', 'merged.by.run'))

# Represent response curves
bm_PlotResponseCurves(bm.out = myBiomodEM, 
                      models.chosen = get_built_models(myBiomodEM)[c(1, 6, 7)],
                      fixed.var = 'median')
bm_PlotResponseCurves(bm.out = myBiomodEM, 
                      models.chosen = get_built_models(myBiomodEM)[c(1, 6, 7)],
                      fixed.var = 'min')
bm_PlotResponseCurves(bm.out = myBiomodEM, 
                      models.chosen = get_built_models(myBiomodEM)[7],
                      fixed.var = 'median',
                      do.bivariate = TRUE)

# Project models -----------------------------------------------------------------
# Project single models
myBiomodProj <- BIOMOD_Projection(bm.mod = myBiomodModelOut,
                                  proj.name = 'Current',
                                  new.env = myExpl,
                                  models.chosen = 'all',
                                  metric.binary = 'all',
                                  metric.filter = 'all',
                                  build.clamping.mask = TRUE)
myBiomodProj
plot(myBiomodProj)

# Project ensemble models (from single projections)
myBiomodEMProj <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM, 
                                             bm.proj = myBiomodProj,
                                             models.chosen = 'all',
                                             metric.binary = 'all',
                                             metric.filter = 'all')

# Project ensemble models (building single projections)
myBiomodEMProj <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM,
                                             proj.name = 'CurrentEM',
                                             new.env = myExpl,
                                             models.chosen = 'all',
                                             metric.binary = 'all',
                                             metric.filter = 'all')
myBiomodEMProj
plot(myBiomodEMProj)

# Compare range sizes -----------------------------------------------------------------
# Load environmental variables extracted from BIOCLIM (bio_3, bio_4, bio_7, bio_11 & bio_12)
#data("bioclim_future")
#myExplFuture = rast(bioclim_future)

predictionLandscapes
# rast(predictionLandscape)

# crop the predictionLandscapes to the extent of the biome 
predictionLandscapes <- crop_mask_raster(predictionLandscapes, biome_sp)

# Project onto future conditions
myBiomodProjectionFuture <- BIOMOD_Projection(bm.mod = myBiomodModelOut,
                                              proj.name = 'Future',
                                              new.env = predictionLandscapes, # myExplFuture
                                              models.chosen = 'all',
                                              metric.binary = 'TSS',
                                              build.clamping.mask = TRUE)

# Load current and future binary projections
CurrentProj <- get_predictions(myBiomodProj, metric.binary = "TSS")
FutureProj <- get_predictions(myBiomodProjectionFuture, metric.binary = "TSS")

# Compute differences
myBiomodRangeSize <- BIOMOD_RangeSize(proj.current = CurrentProj, 
                                      proj.future = FutureProj)

myBiomodRangeSize$Compt.By.Models
plot(myBiomodRangeSize$Diff.By.Pixel)

# Represent main results 
gg = bm_PlotRangeSize(bm.range = myBiomodRangeSize, 
                      do.count = TRUE,
                      do.perc = TRUE,
                      do.maps = TRUE,
                      do.mean = TRUE,
                      do.plot = TRUE,
                      row.names = c("Species", "Dataset", "Run", "Algo"))

