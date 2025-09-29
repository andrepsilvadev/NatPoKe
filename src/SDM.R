## Name: SDM.R ##
## Author: Jorinde-M. Rieger & André P. Silva & Inês Silva ##
## Description: run sdms for multiple species and multiple environmental scenarios ##
## Date: September 05th 2025 ##

# !! NAVIGATION WARNINGS !! ----------------------------------------------------

# This is a highly sensible function! Here are some good practices to make sure
# we don't get errors running it:
# (1) do not work inside a One Drive folder;
# (2) avoid saving the outputs using long path (MAXNET does not deal well with them)
# (3) some minor chnages were done to André's function, namely adding
# the path to the maxent folder as a function argument (might not be necessary in
# the future if we stick with MAXNET but I am still leaving it here)
# (4) function stops if one species has an error


# Settings & libraries ---------------------------------------------------------
source("src/libraries.R") # libraries
source("src/customFunctions2.R") # functions


myExpl_full <- rast("data/Landscapes/traininglandscapes_2015_5km.tif")

#Tropical Biome
extent_tropical <- "Tropical Biome"
extent_tropical_name <- "Tropical & Subtropical Moist Broadleaf Forests" # full name of the biome
extent_tropical_sf <- load_biome(extent_tropical_name)
extent_tropical_crs <- sf::st_transform(extent_tropical_sf, crs = crs(myExpl_full)) # Ensure CRS consistency
extent_tropical_sp <- terra::vect(extent_tropical_crs) # Convert the sf to a spatial object

# # Boreal Biome
# extent_boreal <- "Boreal Biome"
# extent_boreal_name <- "Boreal Forests/Taiga" # full name of the biome
# extent_boreal_sf <- load_biome(extent_boreal_name)
# extent_boreal_crs <- sf::st_transform(extent_boreal_sf, crs = crs(myExpl_full)) # Ensure CRS consistency
# extent_boreal_sp <- terra::vect(extent_boreal_crs) # Convert the sf to a spatial object
# gc()

myExpl_tropical <- crop(rast("data/Landscapes/traininglandscapes_2015_5km.tif"), extent_tropical_sp)
#myExpl_boreal <- crop(rast("data/Landscapes/traininglandscapes_2015_5km.tif"), extent_boreal_sp)
gc()


ssp126_2030_tropical <- crop(rast("data/Landscapes/predictionLandscapes_ssp126_2030_5km.tif"), extent_tropical_sp)
ssp126_2050_tropical <- crop(rast("data/Landscapes/predictionLandscapes_ssp126_2050_5km.tif"), extent_tropical_sp)
ssp126_2100_tropical <- crop(rast("data/Landscapes/predictionLandscapes_ssp126_2100_5km.tif"), extent_tropical_sp)
ssp585_2030_tropical <- crop(rast("data/Landscapes/predictionLandscapes_ssp585_2030_5km.tif"), extent_tropical_sp)
ssp585_2050_tropical <- crop(rast("data/Landscapes/predictionLandscapes_ssp585_2050_5km.tif"), extent_tropical_sp)
ssp585_2100_tropical <- crop(rast("data/landscapes/predictionLandscapes_ssp585_2100_5km.tif"), extent_tropical_sp)
gc()

# ssp126_2030_boreal <- crop(rast("data/Landscapes/predictionLandscapes_ssp126_2030_5km.tif"), extent_boreal_sp)
# ssp126_2050_boreal <- crop(rast("data/Landscapes/predictionLandscapes_ssp126_2050_5km.tif"), extent_boreal_sp)
# ssp126_2100_boreal <- crop(rast("data/Landscapes/predictionLandscapes_ssp126_2100_5km.tif"), extent_boreal_sp)
# ssp585_2030_boreal <- crop(rast("data/Landscapes/predictionLandscapes_ssp585_2030_5km.tif"), extent_boreal_sp)
# ssp585_2050_boreal <- crop(rast("data/Landscapes/predictionLandscapes_ssp585_2050_5km.tif"), extent_boreal_sp)
# ssp585_2100_boreal <- crop(rast("data/Landscapes/predictionLandscapes_ssp585_2100_5km.tif"), extent_boreal_sp)

# Now create the named list
myExplCurrent <- list(
  myExpl_tropical#,
 #myExpl_boreal
)

# Now create the named list
myExplFuture <- list(
  ssp126_2030_tropical = ssp126_2030_tropical,
  ssp126_2050_tropical = ssp126_2050_tropical,
  ssp126_2100_tropical = ssp126_2100_tropical,
  ssp585_2030_tropical = ssp585_2030_tropical,
  ssp585_2050_tropical = ssp585_2050_tropical,
  ssp585_2100_tropical = ssp585_2100_tropical#,
  # ssp126_2030_boreal = ssp126_2030_boreal,
  # ssp126_2050_boreal = ssp126_2050_boreal,
  # ssp126_2100_boreal = ssp126_2100_boreal,
  # ssp585_2030_boreal = ssp585_2030_boreal,
  # ssp585_2050_boreal = ssp585_2050_boreal,
  # ssp585_2100_boreal = ssp585_2100_boreal
)


## BUILDING A FUCNTION FOR SDMs ##

SDMensembleMultiSpecies <- function(targetSpecies, speciesData,
                                    myExpl_full, myExplCurrent, myExplFuture,
                                    extent, output_folder, maxent_source, ncoresToUse) {
  
  # # If changes are required use these args for testing inside the function
  # targetSpecies <- targetSpecies[1]
  # speciesData <- speciesData
  # myExpl_full <- myExpl_full
  # myExplCurrent <- myExplCurrent
  # myExplFuture <- myExplFuture
  # extent <- "Global Terrestrial"
  # output_folder <- "./output/28Aug2025"
  # maxent_source <- "C:/Users/maria/Desktop/maxent/maxent/maxent.jar"
  # #"C:/Users/User/OneDrive - Universidade de Lisboa/Ambiente de Trabalho/maxent/maxent/maxent.jar"
  # ncoresToUse <- 6
  
  ##########
  # STEP 1 # Setup & Folder Prep
  ##########
  
  # Create output folder
  if(!dir.exists(output_folder)){
    dir.create(output_folder, recursive = TRUE)
  }
  
  if (file.exists(maxent_source)) {
    file.copy(from = maxent_source,
              to = file.path(output_folder, "maxent.jar"),
              overwrite = TRUE)
  } else {
    warning("maxent.jar not found at: ", maxent_source,
            "\nDownload it or place it in this folder before running.")
  }
  
  # Set working directory to output folder
  setwd(output_folder) 
  
  ##########
  # STEP 2 # Filter Occurrence Data & Prepare Presence/Pseudo-absence data
  ##########
  
  # print starting message
  message(paste0("Starting for ", targetSpecies))
  
  # Select single species data
  DataSingleSpecies <- speciesData %>%
    dplyr::filter(species == !!targetSpecies)
  
  # Remove NAs and filter out records older than 2015
  # this might reduce the number of presence data to <30 occurences
  DataSingleSpecies <- DataSingleSpecies %>%
    drop_na(decimalLongitude,decimalLatitude, year)
  
  DataSingleSpecies <- DataSingleSpecies %>%
    filter(year >= 2015)
  
  # skip species if there are not at least 100 occ
  if (nrow(DataSingleSpecies) < 100) {
    msg <- paste("Skipping", targetSpecies, 
                 "- only", nrow(DataSingleSpecies), "occurrences >= 2015.")
    message(msg)
    next
  }
  
  # Assign cell IDs to each occurrence based on myExpl raster
  cellValues <- terra::extract(
    myExpl_full,
    cbind(DataSingleSpecies$decimalLongitude, DataSingleSpecies$decimalLatitude)
  )
  
  cellValues$cell <- terra::cellFromXY(myExpl_full,
                                       cbind(DataSingleSpecies$decimalLongitude, DataSingleSpecies$decimalLatitude))
  
  DataSingleSpecies <- cbind(DataSingleSpecies, cellValues)
  
  DataSingleSpecies_unique <- DataSingleSpecies %>%
    group_by(cell) %>%
    slice_max(year, with_ties = FALSE) %>%  # or slice_head(n = 1) for the first
    ungroup() %>%
    dplyr::filter(complete.cases(.))  # biomod excludes all cells that do not have any data
  
  set.seed(123)
  # get number of rows
  #nrows <- nrow(DataSingleSpecies_unique)
  # safe sample: if <30 rows, take all
  DataSingleSpecies_unique <- DataSingleSpecies_unique %>%
    slice_sample(n = 100) %>% 
    as.data.frame()
  
  # Format species occurence data (presence only data)
  myResp <- as.numeric(DataSingleSpecies_unique$species == targetSpecies)
  myRespXY <- DataSingleSpecies_unique[, c("decimalLongitude", "decimalLatitude")]
  
  n.pres <- sum(myResp == 1)
  nb.PA <- c(n.pres, n.pres, n.pres, 1000, 1000, 1000) # number of pseudo-absences per set
  
  # Format input data (with initial pseudo-absences set) 
  myBiomodData.PA <- BIOMOD_FormatingData(
    resp.var = myResp,
    expl.var = myExpl_full,
    resp.xy = myRespXY,
    resp.name = targetSpecies,
    PA.nb.rep = 6, # Number of pseudo-absences sets
    PA.nb.absences = nb.PA,  # Adjust as needed. Different for each model
    PA.strategy = 'random', # random PA selection within the given raster
    na.rm = TRUE, # missing values for explanatory variables
    filter.raster = TRUE) # Removes cell duplicates.
  
  # print message
  message(paste0("Data formatting done for ", targetSpecies))
  
  # Save present points
  presence_points <- myBiomodData.PA@coord[myBiomodData.PA@data.species == 1, ]
  presence_df <- as.data.frame(presence_points)
  colnames(presence_df) <- c("Longitude", "Latitude")
  presence_df$type <- "Presence Points"
  presence_df$species <- as.character(targetSpecies)
  presence_df <- na.omit(presence_df)
  
  # save presence points as .csv 
  write.csv(
    presence_df,
    file = file.path(paste0("PresencePoints_", targetSpecies, "_", extent, ".csv")),
    row.names = FALSE
  )
  
  # Save the presence and pseudo absence points plot
  #png(
  #  filename = file.path(output_folder, paste0("PresencePAPoints_", targetSpecies, "_", extent, ".png")),
  #  width = 2000,
  #  height = 1500,
  #  res = 300
  #)
  #plot(myBiomodData.PA)
  #dev.off()
  
  ##########
  # STEP 3 # Run Models 
  ##########
  
  # Selection of models and pseudo-absences set
  models.pa.list <- list(
    RF = c("PA1", "PA2", "PA3"),
    XGBOOST = c("PA1", "PA2", "PA3"),
    ANN = c("PA1", "PA2", "PA3"),
    #MAXENT = c("PA4", "PA5", "PA6")
    MAXNET = c("PA4", "PA5", "PA6") # REPLACED MAXENT WITH MAXENT
  )
  
  # Run single models
  myBiomodModelOut <- BIOMOD_Modeling(
    bm.format = myBiomodData.PA,
    modeling.id = paste0("Model_", targetSpecies),
    models = c("RF", "XGBOOST", "ANN", 
               "MAXNET"
               #"MAXENT"
    ), # maxent.jar needs to be inside the working directory
    models.pa = models.pa.list,
    CV.strategy = "random",
    CV.nb.rep = 5, # Number of cross-validation runs
    CV.perc = 0.7, # data split, percentage that will be kept for calibration
    OPT.strategy = 'bigboss',
    prevalence = 0.5, # same weight for presences and abs since we have a very inbalanced dataset
    metric.eval = c("TSS", "ROC"), # ADD BOYCE?
    var.import = 3, # PROBABLY CHANGE TO 1 TO SAVE TIME
    nb.cpu = ncoresToUse, #Parallelization
    do.progress = TRUE)
  
  # print starting message
  message(paste0("Models completed for ", targetSpecies))
  
  # Get evaluation scores & variable importance
  eval_scores <- get_evaluations(myBiomodModelOut)
  eval_scores$species <- targetSpecies  # Add species column
  #evaluationScores <- rbind(evaluationScores, eval_scores)  # Combine scores across species
  var_importance <- get_variables_importance(myBiomodModelOut)
  var_importance$species <- targetSpecies  # Add species column
  #variableImportance <- rbind(variableImportance, var_importance)  # Combine importance across species
  
  # Save evaluation scores and variable importance to files
  write.csv(eval_scores, file = file.path(paste0("EvalScores_", targetSpecies, "_", extent, ".csv")), row.names = FALSE)
  write.csv(var_importance, file = file.path(paste0("VarImportance_", targetSpecies, "_", extent, "_", ".csv")), row.names = FALSE)
  
  # Save evaluation score boxplots and variables importance
  #png(
  #  filename = file.path(paste0("EvalBoxplot_", targetSpecies, extent, ".png")),
  #  width = 2000,
  #  height = 1500,
  #  res = 300)
  #bm_PlotEvalBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'algo'))
  #dev.off()
  
  # Create a plot for variable importance for all runs
  #varImpData <- bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('expl.var', 'algo', 'run'))$tab
  #filteredData <- varImpData[varImpData$run == "allRun", ]
  #ggplot2::ggplot(filteredData, aes(x = expl.var, y = var.imp, fill = algo)) +
  #  ggplot2::geom_boxplot() +
  #  ggplot2::labs(
  #    title = "Variable Importance for All Runs",
  #    x = "Explanatory Variable",
  #    y = "Variable Importance",
  #    fill = "Model"
  #  ) +
  #  ggplot2::theme_minimal()
  #ggplot2::ggsave(file.path(paste0("VarImpBoxplot_AllRun_", species, extent, ".png")), width = 10, height = 6, dpi = 300)
  
  rm(myBiomodData.PA)# Clean up to save memory
  rm(eval_scores, var_importance)  # Clean up to save memory
  
  ##########
  # STEP 4 # Project single models
  ##########
  
  # Project single models
  myBiomodProj <- lapply(myExplCurrent, function(env_raster) { # as list to apply to multiple current landscapes 
    BIOMOD_Projection(
      bm.mod = myBiomodModelOut,
      proj.name = 'Current',
      new.env = env_raster,
      models.chosen ='all',
      build.clamping.mask = TRUE,
      nb.cpu = ncoresToUse
    )
  })    
  
  message(paste0("Single models projections done for ", targetSpecies))
  
  ##########
  # STEP 5 # Do ensemble models
  ##########
  
  # Model ensemble models
  myBiomodEM <- BIOMOD_EnsembleModeling(
    bm.mod = myBiomodModelOut,
    models.chosen ='all',
    em.by ='all',
    em.algo = c('EMmean'),
    metric.select = c('TSS'),
    metric.select.thresh = c(0.6), # threshold will be updated to 0.6
    metric.eval = c('TSS','ROC'),
    nb.cpu = ncoresToUse, #Parallelization
    do.progress = TRUE,
    var.import = 3,
    EMci.alpha = 0.05)
  message(paste0("Ensemble model done for ", targetSpecies))
  
  # Get evaluation scores & variable importance for ensemble models
  eval_scoresEM <- get_evaluations(myBiomodEM)
  eval_scoresEM$species <- targetSpecies  # Add species column
  #evaluationScoresEM <- rbind(evaluationScoresEM, eval_scoresEM)  # Combine scores across species
  
  var_importanceEM <- get_variables_importance(myBiomodEM)
  var_importanceEM$species <- targetSpecies  # Add species column
  
  # Save evaluation scores and variable importance to files
  write.csv(eval_scoresEM, file = file.path(paste0("EvalScoresEM_", targetSpecies, "_", extent, ".csv")), row.names = FALSE)
  write.csv(var_importanceEM, file = file.path(paste0("VarImportanceEM_", targetSpecies, "_", extent, ".csv")), row.names = FALSE)
  
  #variableImportanceEM <- rbind(variableImportanceEM, var_importanceEM)  # Combine importance across species
  
  
  # Save evaluation score boxplots and variables importance
  #png(
  #  filename = file.path(paste0("EvalBoxplotEM_", species, extent, ".png")),
  #  width = 2000,
  #  height = 1500,
  #  res = 300)
  #bm_PlotEvalBoxplot(bm.out = myBiomodEM, group.by = c('metric', 'metric'))
  #dev.off()
  
  #png(
  #  filename = file.path(paste0("VarImpBoxplotEM_", species, extent, ".png")),
  #  width = 2000, 
  # height = 1500,
  #  res = 300)
  #bm_PlotVarImpBoxplot(bm.out = myBiomodEM, group.by = c('expl.var', 'algo', 'merged.by.run'))
  #dev.off()
  
  rm(eval_scoresEM, var_importanceEM)  # Clean up to save memory
  
  ##########
  # STEP 6 # Project ensemble models for current conditions
  ##########
  
  # Project ensemble models (from single projections) on current conditions
  myBiomodEMProj <- lapply(myBiomodProj, function(Proj) { # as list to apply to multiple proj 
    BIOMOD_EnsembleForecasting(
      bm.em = myBiomodEM,
      bm.proj = Proj,
      models.chosen ='all',
      metric.binary ='all',
      nb.cpu = ncoresToUse,
      binary.meth = c("TSS"),
      compress = TRUE
    )
  }) 
  message(paste0("Ensemble models' projections for current conditions done for", targetSpecies))
  
  ##########
  # STEP 7 # Project single and ensemble models to future conditions 
  ##########
  
  # Project single models onto future conditions
  myBiomodProjectionFuture <- lapply(myExplFuture, function(future_raster) { # as list to apply to multiple current landscapes 
    BIOMOD_Projection(
      bm.mod = myBiomodModelOut,
      proj.name = "Future",
      new.env = future_raster,
      models.chosen = 'all',
      metric.binary = 'TSS',
      build.clamping.mask = TRUE,
      nb.cpu = ncoresToUse
    )
  }) 
  message(paste0("Single models' projection for future scenarios done for ", targetSpecies))
  
  # Project ensemble-models projections on future variables
  myBiomodEF <- lapply(myBiomodProjectionFuture, function(future_proj) { # as list to apply to multiple future landscapes 
    BIOMOD_EnsembleForecasting(
      bm.em = myBiomodEM,
      bm.proj = future_proj, # not sure if this making the correct correspondence to the layers in  myBiomodProjectionFuture
      models.chosen = 'all',
      #metric.binary = 'all',
      nb.cpu = ncoresToUse,
      binary.meth = c("TSS"),
      compress = "xz"
    )
  })
  message(paste0("Ensemble models' projections for future scenarios done for ", targetSpecies))
  
  ##########
  # STEP 8 # Save ensemble for current and future conditions rasters for each scenario
  ##########
  
  # Get evaluation results to extract threshold
  evals <- get_evaluations(myBiomodEM)
  th_TSS <- evals$cutoff[evals$metric.eval == "TSS"]
  
  ## CURRENT CONDITIONS RASTER ##
  
  EMcurrent <- get_predictions(myBiomodEMProj[[1]], as.data.frame = FALSE)
  
  # save normal suitability (continuous) raster
  EMcurrent_filename <- file.path(
    #output_folder,
    paste0("proj_Current_EM_", gsub(" ", ".", targetSpecies), "_continuous.tif"))
  terra::writeRaster(EMcurrent, EMcurrent_filename, overwrite = TRUE)
  
  # Save binary raster
  bin_rasters <- bm_BinaryTransformation(data = EMcurrent, threshold = th_TSS, do.filtering = FALSE)
  names(bin_rasters) <- paste0("ssp126_2030", names(bin_rasters), "_TSSbin")
  
  # save a converted (binary) raster
  bin_filename <- file.path(
    #output_folder,
    paste0("proj_Current_EM_",gsub(" ", ".", targetSpecies), "_binary.tif"))
  terra::writeRaster(bin_rasters, bin_filename, overwrite = TRUE)
  
  rm(EMcurrent, EMcurrent_filename, bin_rasters, bin_filename)  
  
  invisible(gc())
  
  ## FUTURE CONDITIONS ##
  
  # go through each scenario to save it
  lapply(names(myBiomodEF), function(sc){
    
    EFproj <- myBiomodEF[[sc]]
    
    # --- Continuous raster ---
    cont_rasters <- get_predictions(EFproj, as.data.frame = FALSE)
    names(cont_rasters) <- paste0(sc, "_", names(cont_rasters))
    
    # save normal suitability (continuous) raster
    cont_filename <- file.path(
      #output_folder,
      paste0("proj_", sc, "_", gsub(" ", ".", targetSpecies), "_continuous.tif"))
    terra::writeRaster(cont_rasters, cont_filename, overwrite = TRUE)
    
    # --- Binary raster (using TSS threshold & biomod2 function) ---
    bin_rasters <- bm_BinaryTransformation(data = cont_rasters, threshold = th_TSS, do.filtering = FALSE)
    names(bin_rasters) <- paste0("ssp126_2030_", names(bin_rasters), "_TSSbin")
    
    # save a converted (binary) raster
    bin_filename <- file.path(
      #output_folder,
      paste0("proj_", sc, "_", gsub(" ", ".", targetSpecies), "_binary.tif"))
    terra::writeRaster(bin_rasters, bin_filename, overwrite = TRUE)
    #plot(bin_rasters)
    rm(EFproj, cont_rasters, bin_rasters, cont_filename, bin_filename)
    invisible(gc())
  })
  
  ## Moves up two directories
  setwd("../../")
  
  # # # Save ensemble forecast as raster files
  # # # !! WARNING THIS ONLY WORKS IF WE ONLY DO ONE TYPE OF ENSEMBLE !!
  # ensemble_list <- lapply(names(myBiomodEF), function(sc){
  #  ef <- myBiomodEF[[sc]]
  #  r <- get_predictions(ef, as.data.frame = FALSE)  # returns SpatRaster
  #  #names(r) <- sc
  #  
  # # r
  # })
  # names(ensemble_list) <- names(myBiomodEF)
  # 
  # # combine all scenarios into one
  # ensembleRaster <- terra::rast(ensemble_list)
  # rasterFilename <- file.path(paste0("EnsembleForecast_", gsub(" ", "_", targetSpecies), ".tif"))
  # terra::writeRaster(ensembleRaster, rasterFilename, overwrite = TRUE)
  # 
  # Save ensemble forecast plots
  #png(
  #  filename = file.path(output_folder, paste0("EnsembleForecast_", species, "_", scenario, extent, ".png")),
  #  width = 2000,
  #  height = 1500,
  #  res = 300)
  #plot(myBiomodEF)
  #dev.off()
  
  # Collect metadata for the projection
  #projectionMetadata <- rbind(
  #  projectionMetadata,
  #  data.frame(
  #    species = species,
  #    scenario = scenario,
  #    rasterFile = rasterFilename,
  #    evaluationMetrics = paste(get_evaluations(myBiomodEM), collapse = ";") # Use myBiomodEM here
  #  )
  #)
  #}
  # Return all results as a list
  #return(list(
  #  evaluationScores = evaluationScores,
  #  variableImportance = variableImportance,
  #  evaluationScoresEM = evaluationScoresEM,
  #  variableImportanceEM = variableImportanceEM)
  #)
  
}

################################# BIRD SPECIES #################################


# occurence data for BIRDS
speciesData <- read.csv("data/GBIF_mammalsWTrait_30+occurrences_Global Terrestrial.csv")
speciesData$species <- gsub(" ", ".", speciesData$species)
invisible(gc())

# Select target species (from TaxaOccurence.R)
targetSpecies <- c(# BOREAL SPS
                   #"Alces alces", 
                  #"Bison bonasus", "Cervus elaphus", 
                  #"Sus scrofa"#,
                  # "Vulpes vulpes", #"Canis latrans", 
                   #"Lynx rufus",
                   #"Martes americana", "Taxidea taxus", 
                   #"Ursus americanus", "Panthera tigris", "Lynx lynx", "Ursus arctos",
                   #"Canis lupus", "Rangifer tarandus" 
                   #, "Puma concolor", 
                   #"Bison bison",
                   # TROPICAL SPS
                   #"Leontopithecus caissara", # hase only 4 occurences
                    #"Leopardus pardalis",
                    #"Nasua nasua",
                    #"Aepyceros melampus",
                   # "Colobus angolensis", "Daubentonia madagascariensis",
                   # "Diceros bicornis", "Erythrocebus patas", "Gorilla beringei",
                   # "Gorilla gorilla", "Orycteropus afer", "Pan paniscus",
                   # "Pan troglodytes", "Papio anubis", "Papio ursinus", "Cervus nippon",
                   # "Cuon alpinus", "Felis chaus", "Macaca fuscata", "Pongo abelii",
                   # "Pongo pygmaeus",  "Panthera onca", "Crocuta crocuta", "Mandrillus sphinx",
                   # "Panthera pardus", "Syncerus caffer", "Acinonyx jubatus",
                   # "Panthera leo", "Connochaetes taurinus", "Loxodonta africana"
                    "Puma concolor"
                   )
targetSpecies <- gsub(" ", ".", targetSpecies)
# check target species are included in occ file
targetSpecies %in% speciesData$species
invisible(gc())


species_times <- list()   # store timing info

SDM_NatPoke <- lapply(targetSpecies, function(sp) {
  tryCatch({
    # measure start time
    start_time <- Sys.time()
    
    result <- SDMensembleMultiSpecies(
      targetSpecies = sp,
      speciesData = speciesData,
      myExpl_full = myExpl_full,
      myExplCurrent = myExplCurrent,
      myExplFuture = myExplFuture,
      extent = "GlobalTerrestrial",
      output_folder = "C:/Users/maria/OneDrive - Universidade de Lisboa (1)/NatPokeTropicalPuma",
      maxent_source = "C:/Users/maria/Desktop/maxent/maxent/maxent.jar", 
      ncoresToUse = 6
    )
    
    #measure time end
    end_time <- Sys.time()
    elapsed <- difftime(end_time, start_time, units = "mins")
    
    message("✅ Finished ", sp, " in ", round(elapsed, 2), " minutes")
    
    # store timing
    species_times[[sp]] <<- elapsed
    
    return(result)
    
  }, error = function(e) {
    message(paste("⚠️ Skipping", sp, "due to error:", e$message))
    species_times[[sp]] <<- NA   # store NA if failed
    return(NULL)
  })
})

# 
# 
# SDM_NatPoke <- lapply(targetSpecies, function(sp) {
#   tryCatch({
#     SDMensembleMultiSpecies(
#       targetSpecies = sp,
#       speciesData = speciesData,
#       myExpl_full = myExpl_full,
#       myExplCurrent = myExplCurrent,
#       myExplFuture = myExplFuture,
#       extent = "GlobalTerrestrial",
#       output_folder = "C:/Users/maria/Desktop/NatPokeBoreal",
#       maxent_source = "C:/Users/maria/Desktop/maxent/maxent/maxent.jar", 
#       ncoresToUse = 6
#     )
#   }, error = function(e) {
#     message(paste("⚠️ Skipping", sp, "due to error:", e$message))
#     return(NULL)   # or NA if you prefer
#   })
# })
# #11:48
