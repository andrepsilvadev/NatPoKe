## Name: SDMensembleTest ##
## Author: Jorinde-M. Rieger ##
## Description: test SDM main function with true species occurence in R ##
## Date: April 15th 2025 ##

# Settings & libraries -----------------------------------------------------------------
library(easypackages)
packages("readr","RColorBrewer", "patchwork",
         "raster", "sp", "sf", "terra", "tidyterra", #geospatial data packages
         "rworldmap", 
         "biomod2", "gam","mda", "earth", "maxnet", "ggtext","xgboost","MAXENT", "randomForest", # models
         "rgbif",  "doParallel", 
         "rnaturalearth", "rnaturalearthdata", #background global maps
         "ggpubr",
         "ggplot2", "gridExtra", "dplyr", # plotting
         "writexl",
         prompt = FALSE)

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

SDMensembleMultiSpecies <- function(targetSpecies, speciesData, trainingLandscapes, predictionLandscapes, biome_name){
  # Create output folder
  output_folder <- "~/data/output/SDMensemble"
  if(!dir.exists(output_folder)){
    dir.create(output_folder, recursive = TRUE)
  }
  
  #Test the function
#  targetSpecies = c("Alces alces", "Canis lupus")
#  species = "Alces alces"
#  speciesData = speciesData
#  trainingLandscapes = trainingLandscapes
#  predictionLandscapes = predictionLandscapes
#  biome_name = "SwedenTest"
  
  # Initialize lists to store results for each species
  biomodDataList <- list()
  biomodDataPAList <- list()
  biomodModelOutList <- list()
  biomodEMList <- list()
  currentProjectionsList <- list()
  biomodECList <- list()
  futureProjectionsList <- list()
  biomodEFList <- list()
  evaluationScores <- data.frame()
  variableImportance <- data.frame()
  evaluationScoresEM <- data.frame()
  variableImportanceEM <- data.frame()
  responseCurvesData <- list()
  combinedPlots <- list()
  responseCurvesDataEM <- list()
  combinedPlotsEM <- list()
  projectionMetadata <- data.frame()
  
  
  # Loop through each species
  for (species in targetSpecies){
    cat("\n", species, "modeling started...")
    
    # Format species occurence data
    myResp <- as.matrix(speciesData[, species])  # Convert to a matrix
    myResp <- as.numeric(myResp)  # Flatten the matrix into a numeric vector
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
                                        models = c('ANN', 'GAM', 'GLM', 'RF', 'GBM', 'CTA', 'FDA', 'MARS', 'XGBOOST'), # Exclude 'SRE', 'MAXENT', 'ANN', 'GAM', 'GLM', 'RF', 'GBM', 'CTA', 'FDA', 'MARS', 'XGBOOST'
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
    eval_scores$species <- species  # Add species column
    evaluationScores <- rbind(evaluationScores, eval_scores)  # Combine scores across species
    
    var_importance <- get_variables_importance(myBiomodModelOut)
    var_importance$species <- species  # Add species column
    variableImportance <- rbind(variableImportance, var_importance)  # Combine importance across species
    
    # Save evaluation scores and variable importance to files
    write.csv(eval_scores, file = file.path(output_folder, paste0("EvalScores_", species, ".csv")), row.names = FALSE)
    write.csv(var_importance, file = file.path(output_folder, paste0("VarImportance_", species, ".csv")), row.names = FALSE)
    
    # Save evaluation score boxplots and variables importance
    png(
      filename = file.path(output_folder, paste0("EvalBoxplot_", species, ".png")),
      width = 2000,
      height = 1500,
      res = 300)
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
    
    # Generate response curves and save data for individual models
    responseCurves <- bm_PlotResponseCurves(bm.out = myBiomodModelOut, 
                                            models.chosen = get_built_models(myBiomodModelOut) [c(1:3, 12:14)],
                                            fixed.var = 'median') # 'min'
    responseCurvesData[[species]] <- responseCurves  # Store response curve data
    
    # Save response curve plots
    png(
      filename = file.path(output_folder, paste0("ResponseCurves_", species, ".png")),
      width = 2000,
      height = 1500,
      res = 300)
    bm_PlotResponseCurves(bm.out = myBiomodModelOut, 
                               models.chosen = get_built_models(myBiomodModelOut)[c(1:3, 12:14)],
                               fixed.var = 'median')
    dev.off()
    
    # Store response curve plot objects for later combination
    combinedPlots[[species]] <- responseCurves
    
    # Building ensemble-models
    myBiomodEM <- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut,
                                          models.chosen = 'all',
                                          em.by = 'all', #'PA+run'
                                          em.algo = c('EMmean', 'EMcv', 'EMci', 'EMmedian', 'EMca', 'EMwmean'),
                                          metric.select = c('TSS'),
                                          metric.select.thresh = c(0.4), # no model passed the threshold of 0.7 (suggested by main function)
                                          metric.eval = c('TSS', 'ROC'),
                                          var.import = 3,
                                          EMci.alpha = 0.05,
                                          EMwmean.decay = 'proportional')
    biomodEMList <- myBiomodEM
    
    # Get evaluation scores & variable importance for ensemble models
    eval_scoresEM <- get_evaluations(myBiomodEM)
    eval_scoresEM$species <- species  # Add species column
    evaluationScoresEM <- rbind(evaluationScoresEM, eval_scoresEM)  # Combine scores across species
    
    var_importanceEM <- get_variables_importance(myBiomodEM)
    var_importanceEM$species <- species  # Add species column
    variableImportanceEM <- rbind(variableImportanceEM, var_importanceEM)  # Combine importance across species
    
    # Save evaluation scores and variable importance to files
    write.csv(eval_scoresEM, file = file.path(output_folder, paste0("EvalScoresEM_", species, ".csv")), row.names = FALSE)
    write.csv(var_importanceEM, file = file.path(output_folder, paste0("VarImportanceEM_", species, ".csv")), row.names = FALSE)
    
    # Save evaluation score boxplots and variables importance
    png(
      filename = file.path(output_folder, paste0("EvalBoxplotEM_", species, ".png")),
      width = 2000,
      height = 1500,
      res = 300)
    bm_PlotEvalBoxplot(bm.out = myBiomodEM, group.by = c('full.name', 'full.name'))
    dev.off()
    
    png(
      filename = file.path(output_folder, paste0("VarImpBoxplotEM_", species, ".png")),
      width = 2000,
      height = 1500,
      res = 300)
    bm_PlotVarImpBoxplot(bm.out = myBiomodEM, group.by = c('expl.var', 'algo', 'merged.by.run'))
    dev.off()
    
    # Generate response curves and save data for individual models
    responseCurvesEM <- bm_PlotResponseCurves(bm.out = myBiomodEM, 
                                            models.chosen = get_built_models(myBiomodEM)[c(1, 6, 7)],
                                            fixed.var = 'median') # 'min'
    responseCurvesDataEM[[species]] <- responseCurvesEM  # Store response curve data
    
    # Save response curve plots
    png(
      filename = file.path(output_folder, paste0("ResponseCurvesEM_", species, ".png")),
      width = 2000,
      height = 1500,
      res = 300)
    bm_PlotResponseCurves(bm.out = myBiomodEM, 
                          models.chosen = get_built_models(myBiomodEM)[c(1, 6, 7)],
                          fixed.var = 'median')
    dev.off()
    
    # Store response curve plot objects for later combination
    combinedPlotsEM[[species]] <- responseCurvesEM
    
    # Project current conditions
    currentProjections <- list()
    currentProjections <- BIOMOD_Projection(bm.mod = myBiomodModelOut,
                                      proj.name = 'Current',
                                      new.env = trainingLandscapes,
                                      models.chosen = 'all',
                                      metric.binary = 'TSS',
                                      metric.filter = 'all',
                                      build.clamping.mask = TRUE)
    plot(currentProjections)
    currentProjectionsList[[species]] <- currentProjections
    
    # Project ensemble-models (from single projections) on current variables
    biomodECList[[species]] <- list()
    myBiomodEC <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM, 
                                                 bm.proj = currentProjections,
                                                 models.chosen = 'all',
                                                 metric.binary = 'all',
                                                 metric.filter = 'all')
    biomodECList[[species]] <- myBiomodEC
    
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
    
    # Project ensemble-models projections on future variables
    biomodEFList[[species]] <- list()
    for (scenario in names(futureProjections)) {
      myBiomodEF <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM,
                                                bm.proj = futureProjections[[scenario]],
                                                models.chosen = 'all',
                                                metric.binary = 'all',
                                                metric.filter = 'all')
      
      biomodEFList[[species]][[scenario]] <- myBiomodEF
      
      # Save ensemble forecast as raster files
      ensembleRaster <- get_predictions(myBiomodEF)
      rasterFilename <- file.path(output_folder, paste0("EnsembleForecast_", species, "_", scenario, ".tif"))
      terra::writeRaster(ensembleRaster, rasterFilename, overwrite = TRUE)
      
      # Save ensemble forecast plots
      png(
        filename = file.path(output_folder, paste0("EnsembleForecast_", species, "_", scenario, ".png")),
        width = 2000,
        height = 1500,
        res = 300
      )
      plot(myBiomodEF)
      dev.off()
      
      # Collect metadata for the projection, why does it not work with myBiomodEF?
      projectionMetadata <- rbind(
        projectionMetadata,
        data.frame(
          species = species,
          scenario = scenario,
          rasterFile = rasterFilename,
          evaluationMetrics = paste(get_evaluations(myBiomodEM), collapse = ";") # Use myBiomodEM here
        )
      )
    }
    
    cat("\n", species, "modeling finished.")
  }
  
  # Save combined evaluation scores and variable importance
  # single Models
  write.csv(evaluationScores, file = file.path(output_folder, "Combined_EvalScores.csv"), row.names = FALSE)
  write.csv(variableImportance, file = file.path(output_folder, "Combined_VarImportance.csv"), row.names = FALSE)
  
  # ensemble Models
  write.csv(evaluationScoresEM, file = file.path(output_folder, "Combined_EvalScoresEM.csv"), row.names = FALSE)
  write.csv(variableImportanceEM, file = file.path(output_folder, "Combined_VarImportanceEM.csv"), row.names = FALSE)
  
  # Save metadata as a CSV file
  write.csv(projectionMetadata, file = file.path(output_folder, paste0("ProjectionMetadata_", species, ".csv")), row.names = FALSE)
  
  # Return all results as a list
  return(list(
    biomodData = biomodDataList,
    biomodDataPA = biomodDataPAList,
    biomodModelOut = biomodModelOutList,
    biomodEM = biomodEMList,
    currentProjections = currentProjectionsList,
    biomodEC = biomodECList,
    futureProjections = futureProjectionsList,
    biomodEF = biomodEFList,
    evaluationScores = evaluationScores,
    variableImportance = variableImportance,
    evaluationScoresEM = evaluationScoresEM,
    variableImportanceEM = variableImportanceEM,
    responseCurvesData = responseCurvesData,
    combinedPlots = combinedPlots,
    responseCurvesDataEM = responseCurvesDataEM,
    combinedPlotsEM = combinedPlotsEM,
    projectionMetadata = projectionMetadata
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
results <- SDMensembleMultiSpecies(targetSpecies = targetSpecies,
                                    speciesData = speciesData,
                                    trainingLandscapes = trainingLandscapes,
                                    predictionLandscapes = predictionLandscapes,
                                    biome_name = biome_name)

# Access results
results$biomodData[["Alces alces"]]

results$combinedPlots
results$biomodEM


# Plot Presence Points for multiple species ------------------------------------------------

# Initialize a list to store ggplot objects for each species
presencePlots <- list()

# Loop through each species to create presence point plots
for (species in names(results$biomodData)) {
  # Extract the myBiomodData object for the current species
  biomod_data <- results$biomodData[[species]]
  
  # Extract the presence points (coordinates where response variable is 1)
  presence_points <- biomod_data@coord[biomod_data@data.species == 1, ]
  
  # Convert the presence points to a data frame
  presence_df <- as.data.frame(presence_points)
  colnames(presence_df) <- c("Longitude", "Latitude")
  
  # Create a ggplot object for the species
  p <- ggplot() +
    geom_point(data = presence_df, aes(x = Longitude, y = Latitude), color = "blue", size = 1) +
    labs(
      title = paste("Presence Points -", species),
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 10)
    )
  
  # Add the plot to the list
  presencePlots[[species]] <- p
}

# Arrange the plots in a grid
combined_presence_plot <- grid.arrange(
  grobs = presencePlots,
  ncol = length(presencePlots),  # Number of columns corresponds to the number of species
  top = textGrob("Presence Points for Multiple Species", gp = gpar(fontsize = 16))
)

# Save the combined plot
ggsave(filename = file.path(output_folder, "PresencePoints_MultipleSpecies.png"),
       plot = combined_presence_plot,
       width = 12, height = 6, dpi = 300)


# Initialize a list to store ggplot objects for each species
presencePlots <- list()

# Loop through each species to create presence point plots
for (species in names(results$biomodData)) {
  # Extract the myBiomodData object for the current species
  biomod_data <- results$biomodData[[species]]
  
  # Extract the presence points (coordinates where response variable is 1)
  presence_points <- biomod_data@coord[biomod_data@data.species == 1, ]
  presence_df <- as.data.frame(presence_points)
  colnames(presence_df) <- c("Longitude", "Latitude")
  
  # Remove rows with missing values
  presence_df <- na.omit(presence_df)
  
  # Convert the training landscape to a data frame for ggplot
  training_landscape_df <- as.data.frame(trainingLandscapes, xy = TRUE, na.rm = TRUE)
  colnames(training_landscape_df) <- c("Longitude", "Latitude", "Value")
  
  # Create a ggplot object for the species
  p <- ggplot() +
    geom_raster(data = training_landscape_df, aes(x = Longitude, y = Latitude, fill = Value), alpha = 0.5) +
    geom_point(data = presence_df, aes(x = Longitude, y = Latitude), color = "blue", size = 1) +
    scale_fill_terrain_c(name = "Background") +
    labs(
      title = paste("Presence Points -", species),
      x = "Longitude",
      y = "Latitude"
    ) +
    coord_fixed() +  # Preserve aspect ratio
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 10),
      legend.position = "right"
    )
  
  # Add the plot to the list
  presencePlots[[species]] <- p
}

# Check if there are valid plots
if (length(presencePlots) == 0) {
  stop("No valid plots were created.")
}

# Arrange the plots in a grid
combined_presence_plot <- grid.arrange(
  grobs = presencePlots,
  ncol = length(presencePlots),  # Number of columns corresponds to the number of species
  top = textGrob("Presence Points with Training Landscape for Multiple Species", gp = gpar(fontsize = 16))
)

# Save the combined plot
ggsave(filename = file.path(output_folder, "PresencePoints_WithTrainingLandscape.png"),
       plot = combined_presence_plot,
       width = 12, height = 6, dpi = 300)

# SDM evaluation metrics ------------------------------------------------
# Plot Evaluation Metrics for single models
# Filter for TSS metric
eval_scores_tss <- results$evaluationScores[eval_scores_combined$metric.eval == "TSS", ]

# Plot evaluation metrics
eval_plot <- ggplot(eval_scores_tss, aes(x = species, y = validation, fill = algo)) +
  geom_boxplot() +
  labs(
    title = "Evaluation Metrics (TSS) for Target Species",
    x = "Species",
    y = "TSS",
    fill = "Algorithm"
  ) +
  scale_fill_viridis_d(name = "Metric") +  
  theme_minimal()
print(eval_plot)
ggsave(file.path(output_folder, "EvaluationMetrics_TSS.png"), plot = eval_plot, width = 10, height = 6, dpi = 300)


# Plot evaluation metrics for ensemble models
eval_plot_em <- ggplot(results$evaluationScoresEM, aes(x = species, y = validation, fill = metric.eval)) +
  geom_boxplot() +
  labs(
    title = "Evaluation Metrics for Ensemble Models (EM)",
    x = "Species",
    y = "Value",
    fill = "Metric"
  ) +
  scale_fill_viridis_d(name = "Metric") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
print(eval_plot_em)
ggsave(file.path(output_folder, "EvaluationMetrics_EnsembleModels.png"), plot = eval_plot_em, width = 10, height = 6, dpi = 300)



# Plot evaluation metrics for Run1 and Run2
# Filter the evaluation scores for the metrics ROC and TSS
eval_scores_filtered <- results$evaluationScores %>%
  filter(metric.eval %in% c("ROC", "TSS")) %>%  # Keep only ROC and TSS metrics
  mutate(run = ifelse(grepl("RUN1", full.name), "RUN1", 
                      ifelse(grepl("RUN2", full.name), "RUN2", NA)))  # Extract run information

# Ensure the run column is not NA
eval_scores_filtered <- eval_scores_filtered %>% filter(!is.na(run))

# Create the boxplot
eval_plot <- ggplot(eval_scores_filtered, aes(x = species, y = validation, fill = metric.eval)) +
  geom_boxplot(position = position_dodge(1)) +
  facet_wrap(~ run, ncol = 2) +  # Create separate facets for RUN1 and RUN2
  labs(
    title = "Evaluation Metrics (ROC and TSS) for Single Models",
    x = "Species",
    y = "Validation Score",
    fill = "Metric"
  ) +
  scale_fill_viridis_d(name = "Metric") +  
  theme_minimal()
print(eval_plot)

# Save the plot
ggsave(file.path(output_folder, "EvaluationMetrics_SingleModels_RUN1_RUN2.png"), plot = eval_plot, width = 12, height = 8, dpi = 300)

# Plot Response Curves for target species
#species_plots <- lapply(names(results$responseCurvesData), function(species) {
#  results$responseCurvesData[[species]]$plot
#})
#combined_plot <- wrap_plots(species_plots) + 
#  plot_annotation(title = "Response Curves for Target Species")
#print(combined_plot)
#ggsave(file.path(output_folder, "ResponseCurves.png"), plot = combined_plot, width = 12, height = 6, dpi = 300)


# variable importance ------------------------------------------------  
# Extract variable importance data
#var_importance_em <- results$variableImportanceEM
# Summarize the data to calculate mean and SD for each species and variable
#importance_summary <- var_importance_em %>%
#  group_by(species, algo, expl.var) %>% # filter for EMmean #
#  summarize(
#    mean_importance = mean(var.imp, na.rm = TRUE),
#    sd_importance = sd(var.imp, na.rm = TRUE),
#    .groups = "drop")
# Reshape the data to create a table with species as rows and variables as columns
# Add a row for mean and a row for SD for each species
#importance_table <- importance_summary %>%
#  pivot_longer(cols = c(mean_importance, sd_importance), names_to = "metrics", values_to = "importance") %>%
#  mutate(metrics = ifelse(metrics == "mean_importance", "Mean", "SD")) %>%
#  pivot_wider(names_from = expl.var, values_from = importance) %>%
#  arrange(species, metrics)
#print(importance_table)
# Save the table as a CSV file
#write.csv(importance_table, file = file.path(output_folder, "VariableImportanceSummary.csv"), row.names = FALSE)

# Filter the data for algo == "EMmean"
var_importance_em_filtered <- results$variableImportanceEM %>%
  filter(algo == "EMmean")  # Keep only rows where algo is EMmean

# Summarize the data to calculate mean and SD for each species and variable
importance_summary <- var_importance_em_filtered %>%
  group_by(species, expl.var) %>%  # Group by species and variable
  summarize(
    mean_importance = mean(var.imp, na.rm = TRUE),  # Calculate mean
    sd_importance = sd(var.imp, na.rm = TRUE),      # Calculate standard deviation
    .groups = "drop"
  )

# Reshape the data to create a table with species as rows and variables as columns
# Add a row for mean and a row for SD for each species
importance_table <- importance_summary %>%
  pivot_longer(cols = c(mean_importance, sd_importance), names_to = "metrics", values_to = "importance") %>%
  mutate(metrics = ifelse(metrics == "mean_importance", "Mean", "SD")) %>%
  pivot_wider(names_from = expl.var, values_from = importance) %>%
  arrange(species, metrics)

print(importance_table)

# Save the table as a CSV file and Excel file
write.csv(importance_table, file = file.path(output_folder, "VariableImportanceSummary.csv"), row.names = FALSE)
write_xlsx(importance_table, path = file.path(output_folder, "VariableImportanceSummary.xlsx"))


# Current Landscapes ------------------------------------------------



# Predicted Landscapes ------------------------------------------------
# Define the scenario to plot
scenario <- "ssp126_2071-2100"

# Loop through each species and plot the ensemble forecast (EMmean)
for (species in names(results$biomodEF)) {
  # Extract the BIOMOD_EnsembleForecasting object for the given scenario
  ensemble_forecast <- results$biomodEF[[species]][[scenario]]
  
  # Extract the SpatRaster for the `EMmean` ensemble model
  raster <- get_predictions(ensemble_forecast)
  
  # Ensure only the `EMmean` layer is selected
  emmean_layer <- raster[[grep("EMmean", names(raster))]]
  
  # Plot the raster with a title
  plot(emmean_layer,
       main = paste(species, "- EMmean -", scenario),
       col = terrain.colors(100))
  }




# Define the scenarios and species to plot
scenarios <- c("ssp126_2071-2100", "ssp585_2071-2100")
species_list <- names(results$biomodEF)

# Create a function to extract years and map scenario names
extract_years <- function(scenario) {
  sub(".*_(\\d{4}-\\d{4})$", "\\1", scenario)  # Extract the year range (e.g., "2071-2100")
}

# Create a mapping function for scenario names
scenario_names <- function(scenario) {
  if (grepl("ssp126", scenario)) {
    return("SSP1-RCP2.6")
  } else if (grepl("ssp585", scenario)) {
    return("SSP5-RCP8.5")
  } else {
    return(scenario)  # Default to the original name if no match
  }
}
# Initialize a list to store plots for each species and scenario
plots_spatial <- list()

# Loop through each scenario and species to create plots
for (scenario in scenarios) {
  for (species in species_list) {
      # Extract the BIOMOD_EnsembleForecasting object
      ensemble_forecast <- results$biomodEF[[species]][[scenario]]
      
      # Extract the SpatRaster for the `EMmean` ensemble model
      raster <- get_predictions(ensemble_forecast)
      
      # Ensure only the `EMmean` layer is selected
      emmean_layer <- raster[[grep("EMmean", names(raster))]]
      
      # Convert the raster to a data frame for ggplot
      raster_df <- as.data.frame(emmean_layer, xy = TRUE, na.rm = TRUE)
      colnames(raster_df)[3] <- "value"  # Rename the value column
      
      # Create a ggplot object
      plot <- ggplot(raster_df, aes(x = x, y = y, fill = value)) +
        geom_raster() +
        scale_fill_terrain_c(name = "Prediction") +
        labs(
          title = NULL,  # Remove individual titles
          x = "Longitude",
          y = "Latitude"
        ) +
        coord_sf(expand = FALSE) +  # Ensure correct aspect ratio
        theme_bw() +  # Use a theme with grid lines
        theme(
          axis.text = element_text(),
          axis.ticks = element_line(),
          panel.grid.major = element_line(color = "gray"),
          panel.grid.minor = element_blank(),
          legend.position = "none"  # Remove individual legends
        )
      
      # Add the plot to the list
      plots_spatial[[paste0(scenario, "_", species)]] <- plot
  }
}

# Extract the legend from one of the plots
example_plot <- ggplot(raster_df, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  scale_fill_terrain_c(name = "Prediction") +
  theme_bw() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )
shared_legend <- cowplot::get_legend(example_plot)

# Combine the plots into a grid layout
combined_plot_spatial <- grid.arrange(
  arrangeGrob(
    grobs = lapply(species_list, function(species) {
      textGrob(species, gp = gpar(fontsize = 14))
    }),
    ncol = length(species_list),
    heights = unit(c(0.5), "null")
  ),
  arrangeGrob(
    grobs = c(
      list(textGrob(scenario_names(scenarios[1]), rot = 90, gp = gpar(fontsize = 14))),
      lapply(species_list, function(species) {
        plots_spatial[[paste0(scenarios[1], "_", species)]]
      })
    ),
    ncol = length(species_list) + 1,
    widths = unit(c(0.5, rep(5, length(species_list))), "null")
  ),
  arrangeGrob(
    grobs = c(
      list(textGrob(scenario_names(scenarios[2]), rot = 90, gp = gpar(fontsize = 14))),
      lapply(species_list, function(species) {
        plots_spatial[[paste0(scenarios[2], "_", species)]]
      })
    ),
    ncol = length(species_list) + 1,
    widths = unit(c(0.5, rep(5, length(species_list))), "null")
  ),
  heights = unit(c(0.5, 5, 5), "null")
)

# Combine the plot and legend side by side
final_plot <- grid.arrange(
  combined_plot_spatial,
  arrangeGrob(
    grobs = list(shared_legend),
    ncol = 1
  ),
  ncol = 2,  # Two columns: one for the plot and one for the legend
  widths = unit(c(15, 3), "null"),
  top = textGrob(paste0("Future Projections for ", extract_years(scenario[1])), gp = gpar(fontsize = 16))
)

# Save the combined plot
ggsave(filename = file.path(output_folder, paste0("FutureProjections_", extract_years(scenario[1]), ".png")), 
       plot = final_plot, 
       width = 24, height = 10, dpi = 300)
