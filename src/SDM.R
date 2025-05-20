## Name: SDM.R ##
## Author: Jorinde-M. Rieger ##
## Description: SDM main function with true species occurence in R ##
## Date: May 15th 2025 ##

# Functions to format Data and SDM -----------------------------------------------------------------

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
  
  # Calculate the average number of presence points across species

  
  # Loop through each species
  for (species in targetSpecies){
    cat("\n", species, "modeling started...")
    
    # Format species occurence data
    myResp <- as.matrix(speciesData[, species])  # Convert to a matrix
    myResp <- as.numeric(myResp)  # Flatten the matrix into a numeric vector
    myRespXY <- speciesData[, c('x', 'y')]        # Coordinates for the species
    
    # Calculate the number of presence point for the current species
    num_presence <- sum(myResp ==1, na.rm = TRUE) # counts presence points
    
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
    #myBiomodData.PA <- BIOMOD_FormatingData(resp.var = myResp,
#                                            expl.var = trainingLandscapes,
#                                            resp.xy = myRespXY,
#                                            resp.name = species, 
#                                            PA.nb.rep = 5, # Number of pseudo-absences repetitions
#                                            PA.nb.absences = 10000, # Number of pseudo-absences per set
#                                            PA.strategy = 'random') # Random pseudo-absence
    # Store the formatted data
    #biomodDataPAList[[species]] <- myBiomodData.PA
    
    # Save the presence and pseudo absence points plot
    #png(
#      filename = file.path(output_folder, paste0("PresencePAPoints_", species, "_", biome_name, ".png")),
#      width = 2000,
#      height = 1500,
#      res = 300
#    )
    #plot(myBiomodData.PA)
    #dev.off()
    
    # Format data with true presence and generate pseudo-absence sets
    #myBiomodData.PA <- BIOMOD_FormatingData(resp.var = myResp,
#                                            expl.var = trainingLandscapes,
#                                            resp.xy = myRespXY,
#                                            resp.name = species,
#                                            PA.nb.rep = 2,  # Two sets of pseudo-absences
#                                            PA.nb.absences = c(10000, round(avg_presence)),  # 10,000 for MAXENT, balanced for others
#                                            PA.strategy = 'random')  # Random pseudo-absence strategy
    
    # Format data with true presence and generate pseudo-absence sets
    myBiomodData.PA <- BIOMOD_FormatingData(resp.var = myResp,
                                            expl.var = trainingLandscapes,
                                            resp.xy = myRespXY,
                                            resp.name = species,
                                            PA.nb.rep = 2,  # Two sets of pseudo-absences
                                            PA.nb.absences = c(10000, num_presence),  # 10,000 for MAXENT, number of presence points per species
                                            PA.strategy = 'random')  # Random pseudo-absence strategy
    
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
                                        models = c('ANN', 'RF', 'XGBOOST'), # 'MAXENT' needs to be added, but did not work on the server
                                        models.pa = list(#MAXENT = "PA1", # needs to be added, uses the first pseudo-absence set
                                                        ANN = "PA2", # use the second PA set
                                                        RF = "PA2",
                                                        XGBOOST = "PA2"),
                                        CV.strategy = 'random',
                                        CV.nb.rep = 5, # cross-validation repetitions
                                        CV.perc = 0.7, # data split, percentage that will be kept for calibration
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
    write.csv(eval_scores, file = file.path(output_folder, paste0("EvalScores_", species, biome_name, ".csv")), row.names = FALSE)
    write.csv(var_importance, file = file.path(output_folder, paste0("VarImportance_", species, biome_name, ".csv")), row.names = FALSE)
    
    # Save evaluation score boxplots and variables importance
    png(
      filename = file.path(output_folder, paste0("EvalBoxplot_", species, biome_name, ".png")),
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
    ggsave(file.path(output_folder, paste0("VarImpBoxplot_AllRun_", species, biome_name, ".png")), width = 10, height = 6, dpi = 300)
    
    # Generate response curves and save data for individual models
    responseCurves <- bm_PlotResponseCurves(bm.out = myBiomodModelOut, 
                                            models.chosen = get_built_models(myBiomodModelOut) [c(1:3, 12:14)],
                                            fixed.var = 'median') # 'min'
    responseCurvesData[[species]] <- responseCurves  # Store response curve data
    
    # Save response curve plots
    png(
      filename = file.path(output_folder, paste0("ResponseCurves_", species, biome_name, ".png")),
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
                                          metric.select.thresh = c(0.25), # no model passed the threshold of 0.6
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
    write.csv(eval_scoresEM, file = file.path(output_folder, paste0("EvalScoresEM_", species, biome_name, ".csv")), row.names = FALSE)
    write.csv(var_importanceEM, file = file.path(output_folder, paste0("VarImportanceEM_", species, biome_name, ".csv")), row.names = FALSE)
    
    # Save evaluation score boxplots and variables importance
    png(
      filename = file.path(output_folder, paste0("EvalBoxplotEM_", species, biome_name, ".png")),
      width = 2000,
      height = 1500,
      res = 300)
    bm_PlotEvalBoxplot(bm.out = myBiomodEM, group.by = c('full.name', 'full.name'))
    dev.off()
    
    png(
      filename = file.path(output_folder, paste0("VarImpBoxplotEM_", species, biome_name, ".png")),
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
      filename = file.path(output_folder, paste0("ResponseCurvesEM_", species, biome_name, ".png")),
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
      rasterFilename <- file.path(output_folder, paste0("EnsembleForecast_", species, "_", scenario, "_", biome_name, ".tif"))
      terra::writeRaster(ensembleRaster, rasterFilename, overwrite = TRUE)
      
      # Save ensemble forecast plots
      png(
        filename = file.path(output_folder, paste0("EnsembleForecast_", species, "_", scenario, biome_name, ".png")),
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
