## Name: SDMnew.R ##
## Author: Jorinde-M. Rieger ##
## Description: SDM main function with true species occurence in R ##
## Date: July 29th 2025 ##

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
  
  inputDataFrame <- cbind(xylandscape, inputDataFrame)
  
  write.csv(inputDataFrame,
            file.path(outputPathLandscapes, paste0("inputDataFrame.csv")), row.names = FALSE)
  return(inputDataFrame)
}

#Test the function with Gulo gulo
#targetSpecies <- c("GuloGulo")
#for {i in 1:length(targetSpecies){
#i = 1
#  DataSingleSpecies <- DataSpecies %>%
#    dplyr::select(all_of(c(myRespName,'X_WGS84','Y_WGS84')))
#  DataSingleSpecies.pres <- DataSingleSpecies %>%
#    dplyr::filter(.data[[myRespName]] == 1) # selects only presence data
#}
#results <- SDMensembleMultiSpecies(targetSpecies = targetSpecies,
#                                   speciesData = DataSingleSpecies.pres[i], #SpeciesPresences, #speciesDataOcc
#                                   myExpl = trainingLandscapes,
#                                   myExplFuture = predictionLandscapes,
#                                   extent = extent)


SDMensembleMultiSpecies <- function(targetSpecies, speciesData, myExpl, myExplFuture, extent){
  # Create output folder
  output_folder <- "output/SDMensemble/BoS_Mammals" # adapt output folder path as needed
  if(!dir.exists(output_folder)){
    dir.create(output_folder, recursive = TRUE)
  }
  
  #Test the function
  #targetSpecies <- targetSpecies
  # species = 'Vulpes vulpes'
  # speciesData = speciesPresence
  # myExpl = trainingLandscapes
  # myExplFuture = predictionLandscapes
  # extent = "Russia"
  
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
  #  responseCurvesData <- list()
  #  combinedPlots <- list()
  #  responseCurvesDataEM <- list()
  #  combinedPlotsEM <- list()
  projectionMetadata <- data.frame()
  
  # Loop through each species
  #results <- foreach(species = targetSpecies, .combine = 'list', .packages = c("terra", "BIOMOD2", "ggplot2")) %dopar% {
  for (species in targetSpecies){
    cat("\n", species, "modeling started...")
    
    # Format species occurence data (presence only data)
    myResp <- as.matrix(speciesData[, species])  # Convert to a matrix
    myResp <- as.numeric(myResp)  # Flatten the matrix into a numeric vector
    myRespXY <- speciesData[, c('x', 'y')]        # Coordinates for the species
    
    n.pres <-sum(myResp == 1) # counts presence points before formatting the filtered presence data
    nb.PA <- c(n.pres, n.pres, n.pres, 10000, 10000, 10000) # 10000, 10000, 10000 number of pseudo-absences per set
    
    # 1. Format input data (presence only)
    #    system.time(
    #      myBiomodData <- BIOMOD_FormatingData(
    #        resp.var = myResp,
    #        expl.var = myExpl,
    #        resp.xy = myRespXY,
    #        resp.name = species,
    #        na.rm = TRUE, # missing values for explanatory variables
    #        filter.raster = TRUE) # Removes cell duplicates.
    #    )
    #    biomodDataList[[species]] <- myBiomodData
    #summary(myBiomodData)
    
    # Save the presence only points plot
    #    png(
    #      filename = file.path(output_folder, paste0("PresencePoints_", species, "_", extent, ".png")),
    #      width = 2000,
    #      height = 1500,
    #      res = 300
    #    )
    #    plot(myBiomodData)
    #    dev.off()
    
    # 1. Format input data (with initial pseudo-absences set) 
    system.time(
      myBiomodData.PA <- BIOMOD_FormatingData(
        resp.var = myResp,
        expl.var = myExpl,
        resp.xy = myRespXY,
        resp.name = species,
        PA.nb.rep = 6, # Number of pseudo-absences sets
        PA.nb.absences = nb.PA,  # Adjust as needed. Different for each model
        PA.strategy = 'random', # random PA selection within the given raster
        na.rm = TRUE, # missing values for explanatory variables
        filter.raster = TRUE) # Removes cell duplicates.
    )
    # Extract the actual number of presences
    n.pres <- sum(myBiomodData.PA@data.species == 1, na.rm = TRUE)
    nb.PA <- c(n.pres, n.pres, n.pres, 10000, 10000, 10000) #  10000, 10000, 10000 number of pseudo-absences per set
    
    # 1. Format input data (with final pseudo-absences set)
    system.time(
      myBiomodData.PA <- BIOMOD_FormatingData(
        resp.var = myResp,
        expl.var = myExpl,
        resp.xy = myRespXY,
        resp.name = species,
        PA.nb.rep = 6, # Number of pseudo-absences sets
        PA.nb.absences = nb.PA,  # Adjust as needed. Different for each model
        PA.strategy = 'random', # random PA selection within the given raster
        na.rm = TRUE, # missing values for explanatory variables
        filter.raster = TRUE) # Removes cell duplicates.
    )
#    biomodDataPAList[[species]] <- myBiomodData.PA
    
    # Save the presence and pseudo absence points plot
    png(
      filename = file.path(output_folder, paste0("PresencePAPoints_", species, "_", extent, ".png")),
      width = 2000,
      height = 1500,
      res = 300
    )
    plot(myBiomodData.PA)
    dev.off()
    
    # Selection of models and pseudo-absences set
    models.pa.list <- list(
      RF = c("PA1", "PA2", "PA3"),
      XGBOOST = c("PA1", "PA2", "PA3"),
      ANN = c("PA1", "PA2", "PA3") #,
      #MAXENT = c("PA4", "PA5", "PA6")
    )
    
    # Set up the parallel back-end
     ncoresToUse <- parallel::detectCores() - 35 # Use all cores except one (40 % cores)
    
    # 2. Run single models
    system.time(
      myBiomodModelOut <- BIOMOD_Modeling(
        bm.format = myBiomodData.PA,
        modeling.id = paste0("Model_", species),
        models = c("RF", "XGBOOST", "ANN"), # "MAXENT" maxent.jar needs to be inside the working directory (output/SDMensemble)
        models.pa = models.pa.list,
        CV.strategy = "random",
        CV.nb.rep = 5, # Number of cross-validation runs
        CV.perc = 0.7, # data split, percentage that will be kept for calibration
        OPT.strategy = 'bigboss',
        prevalence = 0.5, # same weight for presences and abs since we have a very inbalanced dataset
        metric.eval = c("TSS", "ROC"),
        var.import = 3,
        nb.cpu = ncoresToUse, #Parallelization
        do.progress = TRUE)
    )
    # Store the model output
#    biomodModelOutList[[species]] <- myBiomodModelOut
    
    # Get evaluation scores & variable importance
    eval_scores <- get_evaluations(myBiomodModelOut)
    eval_scores$species <- species  # Add species column
    evaluationScores <- rbind(evaluationScores, eval_scores)  # Combine scores across species
    var_importance <- get_variables_importance(myBiomodModelOut)
    var_importance$species <- species  # Add species column
    variableImportance <- rbind(variableImportance, var_importance)  # Combine importance across species
    
    # Save evaluation scores and variable importance to files
    write.csv(eval_scores, file = file.path(output_folder, paste0("EvalScores_", species, extent, ".csv")), row.names = FALSE)
    write.csv(var_importance, file = file.path(output_folder, paste0("VarImportance_", species, extent, ".csv")), row.names = FALSE)
    
    # Save evaluation score boxplots and variables importance
    png(
      filename = file.path(output_folder, paste0("EvalBoxplot_", species, extent, ".png")),
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
    ggsave(file.path(output_folder, paste0("VarImpBoxplot_AllRun_", species, extent, ".png")), width = 10, height = 6, dpi = 300)
    
    rm(myBiomodData.PA)
    rm(eval_scores, evaluationScores, var_importance, variableImportance)  # Clean up to save memory
    
    # Generate response curves and save data for individual models
    #    responseCurves <- bm_PlotResponseCurves(bm.out = myBiomodModelOut, 
    #                                            models.chosen = get_built_models(myBiomodModelOut) [c(1:3, 12:14)],
    #                                            fixed.var = 'median') # 'min'
    #    responseCurvesData[[species]] <- responseCurves  # Store response curve data
    
    # Save response curve plots
    #    png(
    #      filename = file.path(output_folder, paste0("ResponseCurves_", species, extent, ".png")),
    #      width = 2000,
    #      height = 1500,
    #      res = 300)
    #    bm_PlotResponseCurves(bm.out = myBiomodModelOut, 
    #                          models.chosen = get_built_models(myBiomodModelOut)[c(1:3, 12:14)],
    #                          fixed.var = 'median')
    #    dev.off()
    
    # Store response curve plot objects for later combination
    #    combinedPlots[[species]] <- responseCurves
    
    # Crop myExpl to extent size
    myExpl <- crop_mask_raster(myExpl, extent_sp)
    
    # Project single models
    system.time(
      myBiomodProj <- BIOMOD_Projection(
        bm.mod = myBiomodModelOut,
        proj.name = 'Current',
        new.env = myExpl,
        models.chosen ='all',
        build.clamping.mask = TRUE,
        nb.cpu = ncoresToUse
      )
    )
   # plot(myBiomodProj)
    # currentProjectionsList[[species]] <- myBiomodProj
    
    # Model ensemble models
    system.time(
      myBiomodEM <- BIOMOD_EnsembleModeling(
        bm.mod = myBiomodModelOut,
        models.chosen ='all',
        em.by ='all',
        em.algo = c('EMmean','EMci','EMca'),
        metric.select = c('TSS'),
        metric.select.thresh = c(0.1), # threshold will be updated to 0.6
        metric.eval = c('TSS','ROC'),
        nb.cpu = ncoresToUse, #Parallelization
        do.progress = TRUE,
        var.import = 3,
        EMci.alpha = 0.05)
    )
   # biomodEMList <- myBiomodEM
    
    # Get evaluation scores & variable importance for ensemble models
    eval_scoresEM <- get_evaluations(myBiomodEM)
    eval_scoresEM$species <- species  # Add species column
    evaluationScoresEM <- rbind(evaluationScoresEM, eval_scoresEM)  # Combine scores across species
    
    var_importanceEM <- get_variables_importance(myBiomodEM)
    var_importanceEM$species <- species  # Add species column
    variableImportanceEM <- rbind(variableImportanceEM, var_importanceEM)  # Combine importance across species
    
    # Save evaluation scores and variable importance to files
    write.csv(eval_scoresEM, file = file.path(output_folder, paste0("EvalScoresEM_", species, extent, ".csv")), row.names = FALSE)
    write.csv(var_importanceEM, file = file.path(output_folder, paste0("VarImportanceEM_", species, extent, ".csv")), row.names = FALSE)
    
    # Save evaluation score boxplots and variables importance
    png(
      filename = file.path(output_folder, paste0("EvalBoxplotEM_", species, extent, ".png")),
      width = 2000,
      height = 1500,
      res = 300)
    bm_PlotEvalBoxplot(bm.out = myBiomodEM, group.by = c('full.name', 'full.name'))
    dev.off()
    
    png(
      filename = file.path(output_folder, paste0("VarImpBoxplotEM_", species, extent, ".png")),
      width = 2000,
      height = 1500,
      res = 300)
    bm_PlotVarImpBoxplot(bm.out = myBiomodEM, group.by = c('expl.var', 'algo', 'merged.by.run'))
    dev.off()
    
    rm(eval_scoresEM, evaluationScoresEM, var_importanceEM, variableImportanceEM)  # Clean up to save memory
    
    
    # Generate response curves and save data for individual models
    #    responseCurvesEM <- bm_PlotResponseCurves(bm.out = myBiomodEM, 
    #                                              models.chosen = get_built_models(myBiomodEM)[c(1, 6, 7)],
    #                                              fixed.var = 'median') # 'min'
    #    responseCurvesDataEM[[species]] <- responseCurvesEM  # Store response curve data
    
    # Save response curve plots
    #    png(
    #      filename = file.path(output_folder, paste0("ResponseCurvesEM_", species, extent, ".png")),
    #      width = 2000,
    #      height = 1500,
    #      res = 300)
    #    bm_PlotResponseCurves(bm.out = myBiomodEM, 
    #                          models.chosen = get_built_models(myBiomodEM)[c(1, 6, 7)],
    #                          fixed.var = 'median')
    #    dev.off()
    
    # Store response curve plot objects for later combination
    #    combinedPlotsEM[[species]] <- responseCurvesEM
    
    # Project ensemble models (from single projections) on current variables
    biomodECList[[species]] <- list()
    system.time(
      myBiomodEMProj <- BIOMOD_EnsembleForecasting(
        bm.em = myBiomodEM,
        bm.proj = myBiomodProj,
        models.chosen ='all',
        metric.binary ='all',
        nb.cpu = ncoresToUse
      )
    )
    #biomodECList[[species]] <- myBiomodEMProj
    
    # Project single models onto future conditions
    futureProjections <- list()
    for (i in seq_along(myExplFuture)) {
      futureProjections[[names(myExplFuture)[i]]]
      system.time(
        myBiomodProjectionFuture <- BIOMOD_Projection(
          bm.mod = myBiomodModelOut,
          proj.name = paste0(names(myExplFuture)[i], "_", species),
          new.env = myExplFuture[[i]],
          models.chosen = 'all',
          metric.binary = 'TSS',
          build.clamping.mask = TRUE,
          nb.cpu = ncoresToUse
        )
      )}
#    futureProjectionsList[[species]] <- futureProjections
    
    # Project ensemble-models projections on future variables
    biomodEFList[[species]] <- list()
    for (scenario in names(futureProjections)) {
      system.time(
        myBiomodEF <- BIOMOD_EnsembleForecasting(
          bm.em = myBiomodEM,
          bm.proj = futureProjections[[scenario]],
          models.chosen = 'all',
          metric.binary = 'all',
          nb.cpu = ncoresToUse
        )
      )
#      biomodEFList[[species]][[scenario]] <- myBiomodEF
      
      # Save ensemble forecast as raster files
      ensembleRaster <- get_predictions(myBiomodEF)
      rasterFilename <- file.path(output_folder, paste0("EnsembleForecast_", species, "_", scenario, "_", extent, ".tif"))
      terra::writeRaster(ensembleRaster, rasterFilename, overwrite = TRUE)
      
      # Save ensemble forecast plots
      png(
        filename = file.path(output_folder, paste0("EnsembleForecast_", species, "_", scenario, extent, ".png")),
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
  #list(
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
    #    responseCurvesData = responseCurvesData,
    #    combinedPlots = combinedPlots,
    #    responseCurvesDataEM = responseCurvesDataEM,
    #    combinedPlotsEM = combinedPlotsEM,
    projectionMetadata = projectionMetadata
  ))
}
