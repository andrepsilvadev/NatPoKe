## Name: SDM.R ##
<<<<<<< HEAD
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
source("./src/libraries.R") # libraries
source("./src/customFunctions2.R") # functions

##########
# STEP 1 # Load & Process Environmental rasters
##########

#myExpl_full <- rast(list.files(pattern ='trainingLandscapes_2015_5km.tif$'))
myExpl_full <- rast("./Landscapes/trainingLandscapes_2015_5km.tif")
invisible(gc())

# load and project biome extent
extent_sf  <- load_biome(target_biome)
extent_crs <- sf::st_transform(extent_sf, crs = crs(myExpl_full))
extent_sp  <- terra::vect(extent_crs)
invisible(gc())

# determine biome short name
biome_short <- if (grepl("Tropical", target_biome)) "tropical" else "boreal"

# crop CURRENT landscape to the target biome
myExpl_current <- crop(
  rast("./Landscapes/trainingLandscapes_2015_5km.tif"),
  extent_sp)
invisible(gc())

# crop FUTURE landscapes dynamically 
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

##########
# STEP 2 # Load and Prep Species Occurrences Data
##########

# read .csv file with occurence data
speciesData <- read.csv("./data/GBIF_occurrences.csv")
# the way biomod2 works has issues if we write sps names without a "."
speciesData$species <- gsub(" ", ".", speciesData$species)
invisible(gc())

# select target species
targetSpecies <- c(## BOREAL SPS ##
  #"Alces alces", "Canis lupus"#, "Bison bonasus", "Cervus elaphus", 
  #"Sus scrofa", "Vulpes vulpes", "Canis latrans", "Lynx rufus",
  #"Martes americana", "Taxidea taxus", "Ursus americanus", "Panthera tigris",
  #"Lynx lynx", "Ursus arctos", "Rangifer tarandus",
  #"Puma concolor", "Bison bison",
  
  ## TROPICAL SPS ##
  "Sus scrofa", "Vulpes vulpes", "Panthera tigris", "Lynx lynx")
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
#)

# ajust names for biomod2
target_species <- gsub(" ", ".", target_species)
# check target species are included in occ file
target_species %in% speciesData$species
invisible(gc())

##########
# STEP 3 # Run Multi-Species SDM function
##########

# SDMensembleMultiSpecies() – this function automates the full SDM workflow for
# each species: filtering and formatting GBIF data, generating pseudo-absences, 
# fitting multiple algorithms (RF, XGBoost, ANN, MAXNET), building ensemble models, 
# and projecting them to current and future climate scenarios. 
# It saves all evaluation metrics, variable importance tables, and raster predictions 
# (continuous and binary) into the specified output folder.

# store timing info
species_times <- list()   

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
      output_folder = "/mnt/data/maria/NatPoKe/output/NatPoKe_October25_tropical",
      maxent_source = "/mnt/data/maria/NatPoKe/maxent/maxent/maxent.jar", 
      ncoresToUse = 6)
    
    # measure time end
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


# get running time into excel
library(writexl)

# Convert the list to a data frame
df <- data.frame(
  Species = names(species_times),
  Time_difference_mins = as.numeric(species_times)
)

#BEFORE RUNNING THE NEXT LINE GO TO  "Session" on the top ribbon > Set working directory > To Project Directory

# Write to Excel
write_xlsx(df, "./data/tropicalSpecies_times_05Nov2025.xlsx")


=======
## Author: Jorinde-M. Rieger ##
## Description: SDM main function with true species occurrence in R and defined extent of predictionLandscapes ##
## Date: August 4th 2025 ##

#Test the function with Gulo gulo from biomod2 package
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
  output_folder <- "output/SDMensemble/Mammals" # adapt output folder path as needed
  if(!dir.exists(output_folder)){
    dir.create(output_folder, recursive = TRUE)
  }
  
  # Initialize lists to store results for each species
  evaluationScores <- data.frame()
  variableImportance <- data.frame()
  evaluationScoresEM <- data.frame()
  variableImportanceEM <- data.frame()
  
  # Format species occurence data (presence only data)
  myResp <- as.matrix(speciesData[, species])  # Convert to a matrix
  myResp <- as.numeric(myResp)  # Flatten the matrix into a numeric vector
  myRespXY <- speciesData[, c('x', 'y')]        # Coordinates for the species
  
  n.pres <-sum(myResp == 1) # counts presence points before formatting the filtered presence data
  nb.PA <- c(n.pres, n.pres, n.pres, 10000, 10000, 10000) # 10000, 10000, 10000 number of pseudo-absences per set
  
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
  
  # Save present points 
  presence_points <- myBiomodData.PA@coord[myBiomodData.PA@data.species == 1, ]
  presence_df <- as.data.frame(presence_points)
  colnames(presence_df) <- c("Longitude", "Latitude")
  presence_df$Type <- "Presence Points"
  presence_df <- na.omit(presence_df)
  write.csv(
    presence_df,
    file = file.path(output_folder, paste0("PresencePoints_", species, "_", extent, ".csv")),
    row.names = FALSE
  )
  
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
  ggplot2::ggplot(filteredData, aes(x = expl.var, y = var.imp, fill = algo)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(
      title = "Variable Importance for All Runs",
      x = "Explanatory Variable",
      y = "Variable Importance",
      fill = "Model"
    ) +
    ggplot2::theme_minimal()
  ggplot2::ggsave(file.path(output_folder, paste0("VarImpBoxplot_AllRun_", species, extent, ".png")), width = 10, height = 6, dpi = 300)
  
  rm(myBiomodData.PA)# Clean up to save memory
  rm(eval_scores, var_importance)  # Clean up to save memory
  
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
      nb.cpu = ncoresToUse)
  )
  
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
  
  rm(eval_scoresEM, var_importanceEM)  # Clean up to save memory
  
  # Project ensemble models (from single projections) on current variables
  system.time(
    myBiomodEMProj <- BIOMOD_EnsembleForecasting(
      bm.em = myBiomodEM,
      bm.proj = myBiomodProj,
      models.chosen ='all',
      metric.binary ='all',
      nb.cpu = ncoresToUse)
  )
  
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
        nb.cpu = ncoresToUse)
    )
  }
  
  # Project ensemble-models projections on future variables
  for (scenario in names(futureProjections)) {
    system.time(
      myBiomodEF <- BIOMOD_EnsembleForecasting(
        bm.em = myBiomodEM,
        bm.proj = futureProjections[[scenario]],
        models.chosen = 'all',
        metric.binary = 'all',
        nb.cpu = ncoresToUse)
    )
    
    # Save ensemble forecast as raster files
    ensembleRaster <- get_predictions(myBiomodEF)
    rasterFilename <- file.path(output_folder, paste0("EnsembleForecast_", species, "_", scenario, "_", extent, ".tif"))
    terra::writeRaster(ensembleRaster, rasterFilename, overwrite = TRUE)
    
    # Save ensemble forecast plots
    png(
      filename = file.path(output_folder, paste0("EnsembleForecast_", species, "_", scenario, extent, ".png")),
      width = 2000,
      height = 1500,
      res = 300)
    plot(myBiomodEF)
    dev.off()
    
    # Collect metadata for the projection
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
  # Return all results as a list
  return(list(
    evaluationScores = evaluationScores,
    variableImportance = variableImportance,
    evaluationScoresEM = evaluationScoresEM,
    variableImportanceEM = variableImportanceEM)
  )
}
>>>>>>> c6a946ef84e6cbe6619ddb2264eed0d3d45ce642
