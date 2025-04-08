## Name: SpeciesDistributionModellingTest ##
## Author: Jorinde-M. Rieger ##
## Description: test SDM main function with true species occurence in R ##
## Date: April 4th 2025 ##

# Settings & libraries -----------------------------------------------------------------
library(easypackages)
packages("readr","ggplot2","RColorBrewer",
         "rworldmap","sp","raster", "gam","mda", "earth", "maxnet", "ggtext","xgboost",
         "rgbif","biomod2", "dplyr", "doParallel", "MAXENT",
         "sf", "rnaturalearth", "rnaturalearthdata","terra", "tidyterra", "ggpubr", "randomForest", prompt = FALSE)

source("./src/libraries.R") # libraries
source("./src/customFunctions.R") # functions
source("./scripts/inputClimate.R") # format and reads input raster landscapes
source("./scripts/inputSpeciesData.R") # format and reads input data

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
biome_name # run test with SE

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

#### Test with Sweden ####
# Load Sweden's shapefile using rnaturalearth
library(rnaturalearth)
library(rnaturalearthdata)
sweden_sf <- ne_countries(scale = "medium", country = "Sweden", returnclass = "sf")
sweden_sp <- vect(sweden_sf)  # Convert to SpatVector for terra compatibility
# Function to crop and mask rasters to Sweden
crop_mask_raster <- function(raster, sweden_sp) {
  mask(crop(raster, sweden_sp), sweden_sp)
}
# Crop and mask trainingLandscapes to Sweden's extent
trainingLandscapes <- crop_mask_raster(trainingLandscapes, sweden_sp)
plot(trainingLandscapes)

####### 

# Select the name of the studied species
targetSpecies <- c("Alces alces", "Canis lupus")

# Format species occurence to true presence and NAs with corresonding coordinates
# test with trainingLandscape? - use as species input data
speciesData <- formatInputDataFrame(
  speciesData = speciesDataOcc,
  targetSpecies = targetSpecies, 
  landscape = trainingLandscapes)
head(speciesData)


# Get corresponding presence/absence data (for one specie)
myResp <- as.numeric(speciesData[, targetSpecies[[1]]])

# Get corresponding presence/absence data
myResp <- as.matrix(speciesData[, targetSpecies[[1]]])  # Convert to a matrix
myResp <- as.numeric(myResp)  # Flatten the matrix into a numeric vector

# Get corresponding XY coordinates
myRespXY <- speciesData[, c('x', 'y')]



# Prepare data & Parameters -----------------------------------------------------------------

# Format Data with true presences
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp, # myResp
                                     expl.var = trainingLandscapes, # myExpl
                                     resp.xy = myRespXY, 
                                     resp.name = targetSpecies[[1]]) # myRespNames
myBiomodData
plot(myBiomodData)

# Save the plot as a PNG file with higher resolution
png(
  filename = paste0("PresencePoints_", targetSpecies[[1]], ".png"),
  width = 2000,  # Width in pixels
  height = 1500, # Height in pixels
  res = 300      # Resolution in DPI
)
plot(myBiomodData)
dev.off()

# Prseudo-absence extraction -----------------------------------------------------------------
# # Transform true absences into potential pseudo-absences
# myResp.PA <- ifelse(myResp == 1, 1, NA)
# 
# Format Data with pseudo-absences : random method
myBiomodData.PA <- BIOMOD_FormatingData(resp.var = myResp,
                                        expl.var = trainingLandscapes, # myExpl
                                        resp.xy = myRespXY,
                                        resp.name = targetSpecies[[1]], # myRespNames
                                        PA.nb.rep = 2, # Number of pseudo-absences 4
                                        PA.nb.absences = 1000, # Number of pseudo-absences per set
                                        PA.strategy = 'random') # Random pseudo-absence generation
 
myBiomodData.PA
print(myBiomodData.PA)
summary(myBiomodData.PA)
plot(myBiomodData.PA)

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
# k-fold selection
#cv.k <- bm_CrossValidation(bm.format = myBiomodData.PA, # failed I got only NAs
                          strategy = "kfold",
                          nb.rep = 2,
                          k = 3)

# stratified selection (geographic)
# cv.s <- bm_CrossValidation(bm.format = myBiomodData,
#                            strategy = "strat",
#                            k = 2,
#                            balance = "presences",
#                            strat = "x")
head(cv.k) # NAs as result
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
# Register a parallel backend using the doParallel package
#cl <- makeCluster(detectCores() - 1)  # Use all but one core
#registerDoParallel(cl)

# Check the structure of the formatted data
print(myBiomodData.PA)
summary(myBiomodData.PA)

# Check the training landscapes
print(trainingLandscapes)
summary(trainingLandscapes)

# Model single models
myBiomodModelOut <- BIOMOD_Modeling(bm.format = myBiomodData.PA,
                                    modeling.id = 'AllModelsExMAXENT',
                                    models = c('GLM', 'RF', 'GAM', 'GBM', 'ANN', 'CTA', 'FDA', 'MARS', 'XGBOOST'), # Exclude 'SRE', 'MAXENT','GAM', 'ANN', 'CTA', 'FDA', 'MARS', 'XGBOOST'
                                    CV.strategy = 'random',
                                    CV.nb.rep = 2,
                                    CV.perc = 0.8,
                                    OPT.strategy = 'bigboss',
                                    var.import = 3,
                                    metric.eval = c('TSS','ROC'))
# seed.val = 123)
# nb.cpu = 8)

# When done, stop the cluster
#stopCluster(cl)

myBiomodModelOut


# Get evaluation scores & variables importance
get_evaluations(myBiomodModelOut)
get_variables_importance(myBiomodModelOut)

# Represent evaluation scores & variables importance
bm_PlotEvalBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'algo'))
bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('expl.var', 'algo', 'run'))

# Create a plot for variable importance for all runs
# Extract the data from bm_PlotVarImpBoxplot
varImpData <- bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('expl.var', 'algo', 'run'))$tab
# Filter the data to include only 'allRun'
filteredData <- varImpData[varImpData$run == "allRun", ]
# Create a custom boxplot for 'allRun'
ggplot(filteredData, aes(x = expl.var, y = var.imp, fill = algo)) +
  geom_boxplot() +
  labs(
    title = "Variable Importance for All Runs",
    x = "Explanatory Variable",
    y = "Variable Importance",
    fill = "Model"
  ) +
  theme_minimal()
ggsave("VarImpBoxplot_AllRun.png", width = 10, height = 6, dpi = 300)


# Represent response curves

# Check differnece between median, min output
bm_PlotResponseCurves(bm.out = myBiomodModelOut, 
                      models.chosen = get_built_models(myBiomodModelOut)[c(1:3, 12:14)],
                      fixed.var = 'median') # non-focal var are fixed at median values which represents a "typical" condition for the non-focal var
bm_PlotResponseCurves(bm.out = myBiomodModelOut, 
                      models.chosen = get_built_models(myBiomodModelOut)[c(1:3, 12:14)],
                      fixed.var = 'min') # non-focal var are fixed at minimum values, which represents an extreme condition of the non-focal variables (lowest observed values)
# bm_PlotResponseCurves(bm.out = myBiomodModelOut, 
#                      models.chosen = get_built_models(myBiomodModelOut)[3],
#                      fixed.var = 'median',
#                      do.bivariate = TRUE)


# Check which/if evaluations scores are below threshold
eval_scores <- get_evaluations(myBiomodModelOut)
tss_scores <- eval_scores[eval_scores$metric.eval == "TSS", ]
threshold <- 0.4
low_tss_models <- tss_scores[tss_scores$calibration < threshold, ]
print(low_tss_models)

# Check which models are included in the ensemble
included_models <- get_built_models(myBiomodModelOut)
print(included_models)

# Inspect evaluation scores for all models
# Get evaluation scores for all models
grouped_scores <- tss_scores %>%
  group_by(PA, run) %>%
  summarize(mean_tss = mean(calibration, na.rm = TRUE))
print(grouped_scores)

# Register a parallel backend using the doParallel package
#cl <- makeCluster(detectCores() - 1)  # Use all but one core
#registerDoParallel(cl)

# Model ensemble models
myBiomodEM <- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut,
                                      models.chosen = 'all',
                                      em.by = 'PA+run', #'PA+run' Allow merging of datasets; #'all'
                                      em.algo = c('EMmean', 'EMcv', 'EMci', 'EMmedian', 'EMca', 'EMwmean'),
                                      metric.select = c('TSS'),
                                      metric.select.thresh = c(0.4), # no model passed the threshold of 0.7 (suggested by main function)
                                      metric.eval = c('TSS', 'ROC'),
                                      var.import = 3,
                                      EMci.alpha = 0.05,
                                      EMwmean.decay = 'proportional')
myBiomodEM
# Retrieve models included in the ensemble
ensemble_models <- get_built_models(myBiomodEM)
print(ensemble_models)

# When done, stop the cluster
#stopCluster(cl)

# Get evaluation scores & variables importance
get_evaluations(myBiomodEM)
get_variables_importance(myBiomodEM)

# Represent evaluation scores & variables importance
#bm_PlotEvalMean(bm.out = myBiomodEM, group.by = 'full.name')
bm_PlotEvalBoxplot(bm.out = myBiomodEM, group.by = c('full.name', 'full.name'))
#bm_PlotVarImpBoxplot(bm.out = myBiomodEM, group.by = c('expl.var', 'full.name', 'full.name'))
bm_PlotVarImpBoxplot(bm.out = myBiomodEM, group.by = c('expl.var', 'algo', 'merged.by.run'))
#bm_PlotVarImpBoxplot(bm.out = myBiomodEM, group.by = c('algo', 'expl.var', 'merged.by.run'))

# Represent response curves
bm_PlotResponseCurves(bm.out = myBiomodEM, 
                      models.chosen = get_built_models(myBiomodEM)[c(1, 6, 7)],
                      fixed.var = 'median')
bm_PlotResponseCurves(bm.out = myBiomodEM, 
                      models.chosen = get_built_models(myBiomodEM)[c(1, 6, 7)],
                      fixed.var = 'min')
#bm_PlotResponseCurves(bm.out = myBiomodEM, 
#                      models.chosen = get_built_models(myBiomodEM)[7],
#                      fixed.var = 'median',
#                      do.bivariate = TRUE)

# Project models -----------------------------------------------------------------

# Check the names of the environmental variables in trainingLandscapes
print(names(trainingLandscapes))


# Register a parallel backend using the doParallel package
cl <- makeCluster(detectCores() - 1)  # Use all but one core
registerDoParallel(cl)

# Project single models
myBiomodProj <- BIOMOD_Projection(bm.mod = myBiomodModelOut,
                                  proj.name = 'Current',
                                  new.env = trainingLandscapes, #myExpl
                                  models.chosen = 'all',
                                  metric.binary = 'all',
                                  metric.filter = 'all',
                                  build.clamping.mask = TRUE)
myBiomodProj
plot(myBiomodProj)

# Inspect the structure of the projection object
str(myBiomodProj)
# Retrieve predictions from the projection object
predictions <- get_predictions(myBiomodProj)
print(predictions)

# Find missing models
ensemble_models <- get_built_models(myBiomodEM)
single_models <- names(get_predictions(myBiomodProj))
missing_models <- setdiff(ensemble_models, single_models)
print(missing_models)


# what is the difference between the projection options?
# Project ensemble models (from single projections)
myBiomodEMProj <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM, 
                                             bm.proj = myBiomodProj,
                                             models.chosen = 'all',
                                             metric.binary = 'all',
                                             metric.filter = 'all')

# Project ensemble models (building single projections)
myBiomodEMProj <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM,
                                             proj.name = 'CurrentEM',
                                             new.env = trainingLandscapes,
                                             models.chosen = 'all',
                                             metric.binary = 'all',
                                             metric.filter = 'all')
# When done, stop the cluster
stopCluster(cl)

myBiomodEMProj
plot(myBiomodEMProj)

# Compare range sizes -----------------------------------------------------------------
# Load environmental variables extracted from BIOCLIM (bio_3, bio_4, bio_7, bio_11 & bio_12)
#data("bioclim_future")
#myExplFuture = rast(bioclim_future)

plot(predictionLandscapes)
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
