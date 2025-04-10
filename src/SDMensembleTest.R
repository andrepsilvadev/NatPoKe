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


# SDMensemble Function -----------------------------------------------------------------

SDMensemble <- function(targetSpecies, speciesData, trainingLandscapes, predictionLandscapes, biome_name){
  # Create output folder
  output_folder <- "~/data/output/SDMensemble"
  if(!dir.exists(output_folder)){
    dir.create(output_folder, recursive = TRUE)
  }
  
  #Test the function
  targetSpecies = c("Alces alces", "Canis lupus")
  species = "Alces alces"
  speciesData = speciesData
  trainingLandscapes = trainingLandscapes
  predictionLandscapes = predictionLandscapes
  biome_name = "SwedenTest"
  
  # Initialize lists to store results for each species
  biomodDataList <- list()
  biomodDataPAList <- list()
  biomodModelOutList <- list()
  biomodEMList <- list()
  biomodProjList <- list()
  futureProjectionsList <- list()
  biomodEFList <- list()
  rangeSizeDifferencesList <- list()
  
  # Loop through each species
  for (species in targetSpecies){
    cat("\n", species, "modeling started...")
    
    # Format species occurence data
    myResp <- as.matrix(speciesData[, species])  # Convert to a matrix
    myResp <- as.numeric(myResp)  # Flatten the matrix into a numeric vector
    #myResp <- as.numeric(speciesData[, species])  # Presence/absence data for the species
    myRespXY <- speciesData[, c('x', 'y')]        # Coordinates for the species
    
    # Format Data with only true presences
    myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                            expl.var = trainingLandscapes,
                                            resp.xy = myRespXY,
                                            resp.name = species) 
    # Store the formatted data
    biomodDataList[[species]] <- myBiomodData
    
    # Save the presence points plot
    png(
      filename = file.path(output_folder, paste0("PresencePoints_", species, "_", biome_name, ".png")),
      width = 2000,
      height = 1500,
      res = 300
    )
    plot(myBiomodData)
    dev.off()
    
    # Format Data with true presences and pseudo-absences
    myBiomodData.PA <- BIOMOD_FormatingData(resp.var = myResp,
                                            expl.var = trainingLandscapes,
                                            resp.xy = myRespXY,
                                            resp.name = species, 
                                            PA.nb.rep = 2, # Number of pseudo-absences 4
                                            PA.nb.absences = 1000, # Number of pseudo-absences per set
                                            PA.strategy = 'random') # Random pseudo-absence
    # Store the formatted data
    biomodDataPAList[[species]] <- myBiomodData.PA
    
    # Save the presence and pseudo absence points plot
    png(
      filename = file.path(output_folder, paste0("PresencePAPoints_", species, "_", biome_name, ".png")),
      width = 2000,
      height = 1500,
      res = 300
    )
    plot(myBiomodData.PA)
    dev.off()
    
    # Run single models
    myBiomodModelOut <- BIOMOD_Modeling(bm.format = myBiomodData.PA,
                                        modeling.id = paste0("Model_", species),
                                        models = c('RF', 'GLM', 'XGBOOST'), # Exclude 'SRE', 'MAXENT', 'ANN', 'GAM'; 'GLM', 'RF', 'GBM', 'CTA', 'FDA', 'MARS', 'XGBOOST'
                                        #OPT.user = biomodOptions,
                                        CV.strategy = 'random',
                                        CV.nb.rep = 2, # 10
                                        CV.perc = 0.8, # data split, percentage that will be kept for calibaration
                                        OPT.strategy = 'bigboss',
                                        var.import = 3,
                                        metric.eval = c('TSS','ROC'))
    # Store the model output
    biomodModelOutList[[species]] <- myBiomodModelOut
    
    # Get evaluation scores & variable importance
    eval_scores <- get_evaluations(myBiomodModelOut)
    var_importance <- get_variables_importance(myBiomodModelOut)
    
    # Save evaluation scores and variable importance to files
    write.csv(eval_scores, file = file.path(output_folder, paste0("EvalScores_", species, ".csv")), row.names = FALSE)
    write.csv(var_importance, file = file.path(output_folder, paste0("VarImportance_", species, ".csv")), row.names = FALSE)
    
    # Save evaluation score boxplots and variables importance
    png(
      filename = file.path(output_folder, paste0("EvalBoxplot_", species, ".png")),
      width = 2000,
      height = 1500,
      res = 300
    )
    bm_PlotEvalBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'algo'))
    dev.off()
    
    # Create a plot for variable importance for all runs
    varImpData <- bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('expl.var', 'algo', 'run'))$tab
    filteredData <- varImpData[varImpData$run == "allRun", ]
    ggplot(filteredData, aes(x = expl.var, y = var.imp, fill = algo)) +
      geom_boxplot() +
      labs(
        title = "Variable Importance for All Runs",
        x = "Explanatory Variable",
        y = "Variable Importance",
        fill = "Model"
      ) +
      theme_minimal()
    ggsave(file.path(output_folder, paste0("VarImpBoxplot_AllRun_", species, ".png")), width = 10, height = 6, dpi = 300)
    
    # Building ensemble-models
    myBiomodEM <- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut,
                                          models.chosen = 'all',
                                          em.by = 'all',
                                          em.algo = c('EMmean', 'EMcv', 'EMci', 'EMmedian', 'EMca', 'EMwmean'),
                                          metric.select = c('TSS'),
                                          metric.select.thresh = c(0.4), # no model passed the threshold of 0.7 (suggested by main function)
                                          metric.eval = c('TSS', 'ROC'),
                                          var.import = 3,
                                          EMci.alpha = 0.05,
                                          EMwmean.decay = 'proportional')
    biomodEMList <- myBiomodEM
    
    # Save ensemble model evaluation plots
    png(
      filename = file.path(output_folder, paste0("EnsembleEvalBoxplot_", species, ".png")),
      width = 2000,
      height = 1500,
      res = 300
    )
    bm_PlotEvalBoxplot(bm.out = myBiomodEM, group.by = c('full.name', 'full.name'))
    dev.off()
    
    # Save ensemble model evaluation plots
    png(
      filename = file.path(output_folder, paste0("EnsembleVarImpBoxplot_", species, ".png")),
      width = 2000,
      height = 1500,
      res = 300
    )
    bm_PlotVarImpBoxplot(bm.out = myBiomodEM, group.by = c('expl.var', 'algo', 'merged.by.run'))
    dev.off()
    
   
    # Project onto current conditions
    myBiomodProj <- BIOMOD_Projection(bm.mod = myBiomodModelOut,
                                      proj.name = paste0("Current_", species),
                                      new.env = trainingLandscapes,
                                      models.chosen = 'all',
                                      metric.binary = 'all',
                                      metric.filter = 'all',
                                      build.clamping.mask = TRUE)
    
    # Store the projection
    biomodProjList[[species]] <- myBiomodProj
    
    # Project onto future conditions
    futureProjections <- list()
    for (i in seq_along(predictionLandscapes)) {
      futureProjections[[names(predictionLandscapes)[i]]] <- BIOMOD_Projection(bm.mod = myBiomodModelOut,
                                                                                proj.name = paste0(names(predictionLandscapes)[i], "_", species),
                                                                                new.env = predictionLandscapes[[i]],
                                                                                models.chosen = 'all',
                                                                                metric.binary = 'TSS',
                                                                                build.clamping.mask = TRUE)
    }
    
    # Store future projections
    futureProjectionsList[[species]] <- futureProjections
    
    # Make ensemble-models projections on future variables
    biomodEFList[[species]] <- list()
    for (scenario in names(futureProjections)) {
      myBiomodEF <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM,
                                                bm.proj = futureProjections[[scenario]],
                                                models.chosen = 'all',
                                                metric.binary = 'all',
                                                metric.filter = 'all')
      
      biomodEFList[[species]][[scenario]] <- myBiomodEF
      
      # Save ensemble forecast plots
      png(
        filename = file.path(output_folder, paste0("EnsembleForecast_", species, "_", scenario, ".png")),
        width = 2000,
        height = 1500,
        res = 300
      )
      plot(myBiomodEF)
      dev.off()
    }
    
    # Compute range size differences for each future scenario
    rangeSizeDifferences <- list()
    CurrentProj <- get_predictions(myBiomodProj, metric.binary = "TSS")
    for (scenario in names(futureProjections)) {
      FutureProj <- get_predictions(futureProjections[[scenario]], metric.binary = "TSS")
      rangeSizeDifferences[[scenario]] <- BIOMOD_RangeSize(
        proj.current = CurrentProj,
        proj.future = FutureProj
      )
    }
    
    # Store the range size differences
    rangeSizeDifferencesList[[species]] <- rangeSizeDifferences
    
    # Save range size difference plots
    for (scenario in names(rangeSizeDifferences)) {
      png(
        filename = file.path(output_folder, paste0("RangeSizeDiff_", species, "_", scenario, ".png")),
        width = 2000,
        height = 1500,
        res = 300
      )
      plot(rangeSizeDifferences[[scenario]]$Diff.By.Pixel, main = paste(species, scenario))
      dev.off()
    }
    
    cat("\n", species, "modeling finished.")
  }
  # Return all results as a list
  return(list(
    biomodData = biomodDataList,
    biomodDataPA = biomodDataPAList,
    biomodModelOut = biomodModelOutList,
    biomodEM = biomodEMList,
    biomodProj = biomodProjList,
    futureProjections = futureProjectionsList,
    biomodEF = biomodEFList,
    rangeSizeDifferences = rangeSizeDifferencesList
  ))
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

#### Test with Sweden ####

biome_name <- "Sweden"
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

# Loop through each predictionLandscapes raster in the list and mask and crop to biome (test extent)
for (i in seq_along(predictionLandscapes)) {
  # Crop and mask the raster
  predictionLandscapes[[i]] <- mask(crop(predictionLandscapes[[i]], sweden_sp), sweden_sp)
}

print(predictionLandscapes)
plot(predictionLandscapes[["ssp126_2071-2100"]])

####### 

# Select the name of the studied species
targetSpecies <- c("Alces alces", "Canis lupus")

# Format species occurence to true presence and NAs with corresonding coordinates
# test with trainingLandscape
speciesData <- formatInputDataFrame(
  speciesData = speciesDataOcc,
  targetSpecies = targetSpecies, 
  landscape = trainingLandscapes)
head(speciesData)

# Run the SEMensemble function
results <- SDMensembleMultiSpecies(
  targetSpecies = targetSpecies,
  speciesData = speciesData,
  trainingLandscapes = trainingLandscapes,
  predictionLandscapes = predictionLandscapes,
  biome_name = biome_name)

# Access results
results$biomodData[["Alces alces"]]
results$rangeSizeDifferences[["Canis lupus"]][["ssp126_2011-2040"]]
