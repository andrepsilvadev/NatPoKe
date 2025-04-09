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

myResp# Get corresponding XY coordinates
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
  filename = paste0("PresencePoints_", targetSpecies[[1]], "_", biome_name, ".png"),
  width = 2000,  # Width in pixels
  height = 1500, # Height in pixels
  res = 300      # Resolution in DPI
)
plot(myBiomodData)
dev.off()

# Prseudo-absence extraction -----------------------------------------------------------------
# # Transform true absences into potential pseudo-absences
#myResp.PA <- ifelse(myResp == 1, 1, NA)
 
# Format Data with pseudo-absences : random method
myBiomodData.PA <- BIOMOD_FormatingData(resp.var = myResp,
                                        expl.var = trainingLandscapes, # myExpl
                                        resp.xy = myRespXY,
                                        resp.name = targetSpecies[[1]], # myRespNames
                                        PA.nb.rep = 2, # Number of pseudo-absences 4
                                        PA.nb.absences = 1000, # Number of pseudo-absences per set
                                        PA.strategy = 'random') # Random pseudo-absence
 
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
#                          strategy = "kfold",
#                         nb.rep = 2,
#                          k = 3)

# stratified selection (geographic)
# cv.s <- bm_CrossValidation(bm.format = myBiomodData,
#                            strategy = "strat",
#                            k = 2,
#                            balance = "presences",
#                            strat = "x")
#head(cv.k) # NAs as result
# head(cv.s)

# random selection + random pseudo-absences
#cv.r.r <- bm_CrossValidation(bm.form = myBiomodData.PA,
                                    strategy = 'random',
                                    nb.rep = 3,
                                    perc = 0.7)
print(cv.r.r)
summary(myBiomodData.PA, calib.lines = cv.r.r)
pp <- plot(myBiomodData.PA, calib.lines = cv.r.r, plot.type = 'raster') # distribution of different combinations

# Retrieve modeling options -----------------------------------------------------------------
# default paratmeters
#opt.d <- bm_ModelingOptions(data.type = 'binary',
                            models = c('GLM', 'RF', 'XGBOOST'),
                            strategy = 'default')
opt.d

# bigboss parameters + formated data +randeom cross validation
#myOpt <- bm_ModelingOptions(data.type = 'binary',
                              models = c('GLM', 'RF', 'XGBOOST'),
                              strategy = 'bigboss',
                              bm.format = myBiomodData.PA,
                              calib.lines = cv.r.r)
#print(myOpt)
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
myBiomodModelOut <- BIOMOD_Modeling(bm.format = myBiomodData.PA,
                                    modeling.id = 'ModelExampels',
                                    models = c('GLM', 'RF', 'XGBOOST'), # Exclude 'SRE', 'MAXENT', 'ANN', 'GAM'; 'GLM', 'RF', 'GBM', 'CTA', 'FDA', 'MARS', 'XGBOOST'
                                    CV.strategy = 'random',
                                    #CV.user.table = cv.r.r,
                                    CV.nb.rep = 2, # 10
                                    #DataSplit = 70,
                                    CV.perc = 0.8,
                                    OPT.strategy = 'bigboss',
                                    #OPT.user = myOpt,
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


# Model ensemble models
myBiomodEM <- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut,
                                      models.chosen = 'all',
                                      em.by = 'all', #'PA+run' Allow merging of datasets;
                                      em.algo = c('EMmean', 'EMcv', 'EMci', 'EMmedian', 'EMca', 'EMwmean'),
                                      metric.select = c('TSS'),
                                      metric.select.thresh = c(0.4), # no model passed the threshold of 0.7 (suggested by main function)
                                      metric.eval = c('TSS', 'ROC'),
                                      var.import = 3,
                                      EMci.alpha = 0.05,
                                      EMwmean.decay = 'proportional')
myBiomodEM

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

# Project ensemble models (from single projections)
myBiomodEMProj <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM, 
                                             bm.proj = myBiomodProj, # uses precomputed single model projections
                                             models.chosen = 'all',
                                             metric.binary = 'all',
                                             metric.filter = 'all')

myBiomodEMProj
plot(myBiomodEMProj)

# Compare range sizes -----------------------------------------------------------------
# Load environmental variables extracted from BIOCLIM (bio_3, bio_4, bio_7, bio_11 & bio_12)
#data("bioclim_future")
#myExplFuture = rast(bioclim_future)

# Loop through each raster in the list and mask and crop to biome (test extent)
for (i in seq_along(predictionLandscapes)) {
  # Crop and mask the raster
  predictionLandscapes[[i]] <- mask(crop(predictionLandscapes[[i]], sweden_sp), sweden_sp)
}

print(predictionLandscapes)
plot(predictionLandscapes[["ssp126_2071-2100"]])

# Project onto future conditions
futureProjections <- list()
for (i in seq_along(predictionLandscapes)) {
myBiomodProjectionFuture <- BIOMOD_Projection(bm.mod = myBiomodModelOut,
                                              proj.name = names(predictionLandscapes)[i], 
                                              new.env = predictionLandscapes[[i]], # myExplFuture #it needs the same variable names as the calibration variables!
                                              models.chosen = 'all',
                                              metric.binary = 'TSS',
                                              build.clamping.mask = TRUE)}

# Create an empty list to store future projections
futureProjections <- list()

# Loop through each prediction landscape and project onto future conditions
for (i in seq_along(predictionLandscapes)) {
  # Ensure variable names match the calibration variables
  names(predictionLandscapes[[i]]) <- names(trainingLandscapes)
  
  # Perform the projection for the current prediction landscape
  futureProjections[[names(predictionLandscapes)[i]]] <- BIOMOD_Projection(
    bm.mod = myBiomodModelOut,
    proj.name = names(predictionLandscapes)[i],  # Use the name of the current prediction landscape
    new.env = predictionLandscapes[[i]],        # Use the current prediction landscape
    models.chosen = 'all',
    metric.binary = 'TSS',
    build.clamping.mask = TRUE
  )
}

print(futureProjections)

# Make ensemble-models projections on current variable
#myBiomodEMProj <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM, 
                                             bm.proj = myBiomodProjectionFuture,
                                             models.chosen = 'all',
                                             metric.binary = 'all',
                                             metric.filter = 'all')



# Load current and future binary projections
CurrentProj <- get_predictions(myBiomodProj, metric.binary = "TSS")
print(CurrentProj)

# Create an empty list to store range size differences
rangeSizeDifferences <- list()
# Loop through each future projection and compute differences
for (scenario in names(futureProjections)) {
  # Load future binary projections for the current scenario
  FutureProj <- get_predictions(futureProjections[[scenario]], metric.binary = "TSS")

# Compute differences
rangeSizeDifferences[[scenario]] <- BIOMOD_RangeSize(proj.current = CurrentProj, 
                                      proj.future = FutureProj)
}

rangeSizeDifferences[["ssp126_2011-2040"]]$Compt.By.Models
plot(rangeSizeDifferences[["ssp126_2011-2040"]]$Diff.By.Pixel)


# Loop through each scenario and plot the differences
for (scenario in names(rangeSizeDifferences)) {
  print(paste("Scenario:", scenario))
  plot(rangeSizeDifferences[[scenario]]$Diff.By.Pixel, main = scenario)
}

# Represent main results 
gg = bm_PlotRangeSize(bm.range = rangeSizeDifferences[["ssp126_2011-2040"]], 
                      do.count = TRUE,
                      do.perc = TRUE,
                      do.maps = TRUE,
                      do.mean = TRUE,
                      do.plot = TRUE,
                      row.names = c("Species", "Dataset", "Run", "Algo"))
