## Name: SDM.R ##
## Author: Jorinde-M. Rieger & André P. Silva & Inês Silva ##
## Description: builds and projects ensemble Species Distribution Models (SDMs)
## for multiple target species under current and future climate scenarios, using BIOMOD2
## Date: October 10th 2025 ##

## Script overview:
##   STEP 1 – Load and crop environmental rasters to target biome.
##   STEP 2 – Define and prepare current and future climate scenarios.
##   STEP 3 – For each species, prepare presence/pseudo-absence data.
##   STEP 4 – Train individual models (RF, XGBOOST, ANN, MAXNET/MAXENT).
##   STEP 5 – Build ensemble models and evaluate them.
##   STEP 6 – Project models to current and future conditions.
##   STEP 7 – Save continuous and binary suitability rasters for each scenario.
##   STEP 8 – Record processing time per species and export results.


# !! NAVIGATION WARNINGS !! ----------------------------------------------------

# This is a highly sensible function! Here are some good practices to make sure
# we don't get errors running it:
# (1) do not work inside a One Drive folder;
# (2) avoid saving the outputs using long paths (MAXNET does not deal well with them)
# (3) some minor chnages were done to André's function, namely adding
# the path to the maxent folder as a function argument (might not be necessary in
# the future if we stick with MAXNET but I am still leaving it here)
# (4) function stops if one species has an error SOLUTION? use TryCatch()

#setwd("./data") # only necessary on gunvor
# settings & libraries
source("./libraries.R") # libraries
source("./customFunctions2.R") # functions

#myExpl_full <- rast(list.files(pattern ='trainingLandscapes_2015_5km.tif$'))
myExpl_full <- rast("./Landscapes/trainingLandscapes_2015_5km.tif")
invisible(gc())

# define target biome (or use the one in run.R)
target_biome <- "Boreal Forests/Taiga" # Options: "Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga"

# load and project biome extent
extent_sf  <- load_biome(target_biome)
extent_crs <- sf::st_transform(extent_sf, crs = crs(myExpl_full))
extent_sp  <- terra::vect(extent_crs)
invisible(gc())

#determine biome short name
biome_short <- if (grepl("Tropical", target_biome)) "tropical" else "boreal"

# crop current landscape to the target biome
myExpl_current <- crop(
  rast("./Landscapes/trainingLandscapes_2015_5km.tif"),
  extent_sp)
invisible(gc())

# crop future landscapes dynamically 
years <- c(2030, 2050, 2100)
scenarios <- c("ssp126", "ssp585")

myExpl_future <- list()

# dynamically crop teh landscapes rasters per year and scenario by the biome
for (scen in scenarios) {
  for (yr in years) {
    name <- paste0(scen, "_", yr, "_", biome_short)
    message(paste0("Cropping Landscapes for ", name))
    path <- sprintf("./Landscapes/predictionLandscapes_%s_%s_5km.tif", scen, yr)
    myExpl_future[[name]] <- crop(rast(path), extent_sp)
    rm(name, path)
    invisible(gc())
  }
}


# get results into lists (IMPORTANT STEP! Do not skip!!)
myExplCurrent <- list(myExpl_current)
myExplFuture  <- myExpl_future

################################
## Multi species SDM function ## -----------------------------------------------
################################

# THIS FUNCTION IS LIKELY TO MOVE TO THE CUSTOM FUNCTIONS SCRIPT IN THE FUTURE

SDMensembleMultiSpecies <- function(targetSpecies, # vector of target species names
                                    speciesData, # target species occurrences file from GBIF
                                    myExpl_full, # training landscape (whole world)
                                    myExplCurrent, # current environment landscape (cropped to biome)
                                    myExplFuture, # future environment landscapes (cropped to biome)
                                    extent, # extent name for files' names (e.g. tropical OR boreal)
                                    output_folder, # folder path to save outputs
                                    maxent_source, # path to maxent.jar file
                                    ncoresToUse # n cores to use in parallelization jobs
                                    ) {
  
  # # If changes are required use these args for testing inside the function
  # targetSpecies <- targetSpecies[1]
  # speciesData <- speciesData
  # myExpl_full <- myExpl_full
  # myExplCurrent <- myExplCurrent
  # myExplFuture <- myExplFuture
  # extent <- "GlobalTerrestrial"
  # output_folder <- "./output/28Aug2025"
  # maxent_source <- "C:/Users/maria/Desktop/maxent/maxent/maxent.jar"
  # #"C:/Users/User/OneDrive - Universidade de Lisboa/Ambiente de Trabalho/maxent/maxent/maxent.jar"
  # ncoresToUse <- 6
  
  ##########
  # STEP 1 # Setup & Folder Prep
  ##########
  
  # create output folder
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
  
  # set working directory to output folder
  setwd(output_folder) 
  invisible(gc())
  
  ##########
  # STEP 2 # Filter Occurrence Data & Prepare Presence/Pseudo-absence data
  ##########
  
  # print starting message
  message(paste0("Starting for ", targetSpecies))
  
  # Select single species data
  DataSingleSpecies <- speciesData %>%
    dplyr::filter(species == !!targetSpecies)
  invisible(gc())
  
  # Remove NAs and filter out records older than 2015
  # this might reduce the number of presence data to <30 occurences
  DataSingleSpecies <- DataSingleSpecies %>%
    drop_na(decimalLongitude,decimalLatitude, year)
  invisible(gc())
  
  # keep occurrence records after 2015
  DataSingleSpecies <- DataSingleSpecies %>%
    filter(year >= 2015)
  
  # skip sps with less than 30 occ records
  if (nrow(DataSingleSpecies) < 30) {
    message("Skipping ", targetSpecies, " - only ", nrow(DataSingleSpecies), " occurrences >= 2015.")
    next
  }
  
  # assign cell IDs to each occurrence based on myExpl raster
  cellValues <- terra::extract(
    myExpl_full,
    cbind(DataSingleSpecies$decimalLongitude, DataSingleSpecies$decimalLatitude))
  
  cellValues$cell <- terra::cellFromXY(myExpl_full,
                                       cbind(DataSingleSpecies$decimalLongitude, DataSingleSpecies$decimalLatitude))
  
  DataSingleSpecies <- cbind(DataSingleSpecies, cellValues)
  
  # keep one record per cell (to avoid biased occ points)
  DataSingleSpecies_unique <- DataSingleSpecies %>%
    group_by(cell) %>%
    slice_max(year, with_ties = FALSE) %>%  # or slice_head(n = 1) for the first
    ungroup() %>%
    dplyr::filter(complete.cases(.))  # biomod excludes all cells that do not have any data
  
  # in case we want to use a subset of the occ (DELETE IN FINAL VERSIONS)
  set.seed(123)
  DataSingleSpecies_unique <- DataSingleSpecies_unique %>%
    slice_sample(n = 100) %>%
    as.data.frame()
  
  # format species occurence data (presence only data)
  myResp <- as.numeric(DataSingleSpecies_unique$species == targetSpecies)
  myRespXY <- DataSingleSpecies_unique[, c("decimalLongitude", "decimalLatitude")]
  
  n.pres <- sum(myResp == 1)
  nb.PA <- c(n.pres, n.pres, n.pres, 1000, 1000, 1000) # number of pseudo-absences per set
  
  # format input data (with initial pseudo-absences set) 
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
  
  # print a message
  message(paste0("Data formatting done for ", targetSpecies))
  
  # save presence points as .csv 
  presence_points <- myBiomodData.PA@coord[myBiomodData.PA@data.species == 1, ]
  presence_df <- as.data.frame(presence_points)
  colnames(presence_df) <- c("Longitude", "Latitude")
  presence_df$type <- "Presence Points"
  presence_df$species <- as.character(targetSpecies)
  presence_df <- na.omit(presence_df)
  
  write.csv(
    presence_df,
    file = file.path(paste0("PresencePoints_", targetSpecies, "_", extent, ".csv")),
    row.names = FALSE)
  
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
  # STEP 3 # Run the models 
  ##########
  
  # selection of models and pseudo-absences set
  models.pa.list <- list(
    RF = c("PA1", "PA2", "PA3"), # Random-forest
    XGBOOST = c("PA1", "PA2", "PA3"), # Extreme Gradient Boosting
    ANN = c("PA1", "PA2", "PA3"), # Artificial Neural Network
    #MAXENT = c("PA4", "PA5", "PA6") # Maximum Entropy Models
    MAXNET = c("PA4", "PA5", "PA6") # replaced MAXENT for MAXNET
  )
  
  # run single models
  myBiomodModelOut <- BIOMOD_Modeling(
    bm.format = myBiomodData.PA,
    modeling.id = paste0("Model_", targetSpecies),
    models = c("RF", "XGBOOST", "ANN", 
               "MAXNET"
               #"MAXENT" # to use MAXENT maxent.jar needs to be inside the working directory
               ), 
    models.pa = models.pa.list,
    CV.strategy = "random",
    CV.nb.rep = 5, # Number of cross-validation runs
    CV.perc = 0.7, # data split, percentage that will be kept for calibration
    OPT.strategy = 'bigboss',
    prevalence = 0.5, # same weight for presences and abs since we have a very inbalanced dataset
    metric.eval = c("TSS", "ROC"), # ADD BOYCE?
    var.import = 3, # could be changed to 1 if we need to save time
    nb.cpu = ncoresToUse, # parallelization
    do.progress = TRUE)
  
  # print progress message
  message(paste0("Single models completed for ", targetSpecies))
  
  # get evaluation scores & variable importance
  eval_scores <- get_evaluations(myBiomodModelOut)
  eval_scores$species <- targetSpecies  # Add species column
  #evaluationScores <- rbind(evaluationScores, eval_scores)  # Combine scores across species
  var_importance <- get_variables_importance(myBiomodModelOut)
  var_importance$species <- targetSpecies  # Add species column
  #variableImportance <- rbind(variableImportance, var_importance)  # Combine importance across species
  
  # save evaluation scores and variable importance to files
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
  
  invisible(gc(rm(myBiomodData.PA)))# clean up to save memory
  invisible(gc(rm(eval_scores, var_importance)))  # clean up to save memory
  
  ##########
  # STEP 4 # Project single models
  ##########
  
  # project single models
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
  
  # print progress message
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
  
  # print progress message
  message(paste0("Ensemble model done for ", targetSpecies))
  
  # get evaluation scores & variable importance for ensemble models
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
  
  invisible(gc(rm(eval_scoresEM, var_importanceEM)))  # Clean up to save memory
  
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
  
  # print progress message
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
  
  # print progress message
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
  
  # print progress message
  message(paste0("Ensemble models' projections for future scenarios done for ", targetSpecies))
  
  ##########
  # STEP 8 # Save ensemble for current and future conditions rasters for each scenario
  ##########
  
  # print progress message
  message(paste0("Saving output rasters for ", targetSpecies))
  
  # Get evaluation results to extract threshold
  evals <- get_evaluations(myBiomodEM)
  th_TSS <- evals$cutoff[evals$metric.eval == "TSS"]
  
  ## Current Conditions Raster ##
  
  EMcurrent <- get_predictions(myBiomodEMProj[[1]], as.data.frame = FALSE)
  
  # save normal suitability (continuous) raster
  EMcurrent_filename <- file.path(
    #output_folder,
    paste0("proj_Current_EM_", gsub(" ", ".", targetSpecies), "_continuous.tif"))
  terra::writeRaster(EMcurrent, EMcurrent_filename, overwrite = TRUE)
  
  # save binary (converted) raster
  bin_rasters <- bm_BinaryTransformation(data = EMcurrent, threshold = th_TSS, do.filtering = FALSE)
  names(bin_rasters) <- paste0("ssp126_2030", names(bin_rasters), "_TSSbin")
  bin_filename <- file.path(
    #output_folder,
    paste0("proj_Current_EM_",gsub(" ", ".", targetSpecies), "_binary.tif"))
  terra::writeRaster(bin_rasters, bin_filename, overwrite = TRUE)
    
  # clean up to save memory
  invivible(gc(rm(EMcurrent, EMcurrent_filename, bin_rasters, bin_filename)))  
  invisible(gc())
  
  ## Future Conditions Rasters ##
  
  # go through each scenario to save it
  lapply(names(myBiomodEF), function(sc){
    
    EFproj <- myBiomodEF[[sc]]
    
    # ---Continuous raster ---
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
  
  ## move up two directories
  setwd("../../")
  
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


# occurence data
speciesData <- read.csv("./data/GBIF_mammalsWTrait_30+occurrences_Global Terrestrial.csv")
# the way biomod2 works has issues if we write sps names without a "."
speciesData$species <- gsub(" ", ".", speciesData$species)
invisible(gc())

# select target species
targetSpecies <- c(## BOREAL SPS ##
                   "Alces alces", "Canis lupus"#, "Bison bonasus", "Cervus elaphus", 
                   #"Sus scrofa", "Vulpes vulpes", "Canis latrans", "Lynx rufus",
                   #"Martes americana", "Taxidea taxus", "Ursus americanus", "Panthera tigris",
                   #"Lynx lynx", "Ursus arctos", "Rangifer tarandus",
                   #"Puma concolor", "Bison bison",
                   
                   ## TROPICAL SPS ##
                   #"Leontopithecus caissara", # has only 4 occurences
                   #"Leopardus pardalis", "Nasua nasua", "Aepyceros melampus",
                   #"Colobus angolensis", "Daubentonia madagascariensis",
                   #"Diceros bicornis", "Erythrocebus patas", "Gorilla beringei",
                   #"Gorilla gorilla", "Orycteropus afer", "Pan paniscus",
                   #"Pan troglodytes", "Papio anubis", "Papio ursinus", "Cervus nippon",
                   #"Cuon alpinus", "Felis chaus", "Macaca fuscata", "Pongo abelii",
                   #"Pongo pygmaeus",  "Panthera onca", "Crocuta crocuta", "Mandrillus sphinx",
                   #"Panthera pardus", "Syncerus caffer", "Acinonyx jubatus",
                   #"Panthera leo", "Connochaetes taurinus", "Loxodonta africana",
                   #"Puma concolor"
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
      extent = "boreal",
      output_folder = "/mnt/data/maria/TestRun08Oct25",
      maxent_source = "/mnt/data/maria/maxent/maxent/maxent.jar", 
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
