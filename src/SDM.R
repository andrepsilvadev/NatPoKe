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
source("./src/libraries.R") # libraries
source("./src/customFunctions2.R") # functions

##########
# STEP 1 # Load target species from .csv
##########

species_table <- read.csv("./data/species_by_region.csv", stringsAsFactors = FALSE)
target_species <- gsub(" ", ".", species_table$sci_name)

##########
# STEP 2 # Load full environmental training raster & occurrences (done only once!)
##########

# training raster
myExpl_full <- rast("./data/sdm/Landscapes/trainingLandscapes_2015_5km.tif")
invisible(gc())

# species occurrences
speciesData <- read.csv("./data/sdm/GBIF_occurrences.csv")
speciesData$species <- gsub(" ", ".", speciesData$species)
invisible(gc())

# crop FUTURE landscapes dynamically 
years <- c(2030, 2050, 2100)
future_scenario <- c("ssp126", "ssp585")

##########
# STEP 4 # Loop over biomes
##########

for (target_biome in biomes) {
  
  message("====================================")
  message("Starting biome: ", target_biome)
  message("====================================")
  
  # determine biome short name
  biome_short <- if (grepl("Tropical", target_biome)) "tropical" else "boreal"
  
  ## output directory per biome
  sdm_output_dir <- file.path(
    "/mnt/data/maria/NatPoKe/data/sdm",
    paste0(biome_short, "_SDMS"))
  dir.create(sdm_output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # STEP 4.1 # load and crop biome extent
  extent_sf  <- load_biome(target_biome)
  extent_crs <- sf::st_transform(extent_sf, crs = crs(myExpl_full))
  extent_sp  <- terra::vect(extent_crs)
  invisible(gc())
  
  # STEP 4.2 # crop CURRENT landscape to the target biome
  myExpl_current <- crop(
    rast("./Landscapes/trainingLandscapes_2015_5km.tif"),
    extent_sp)
  invisible(gc())
  
  # STEP 4.3 # crop FUTURE landscapes
  
  myExpl_future <- list()
  
  # dynamically crop teh landscapes rasters per year and scenario by the biome
  for (scen in scenarios) {
    for (yr in years) {
      name <- paste0(scen, "_", yr, "_", biome_short)
      message(paste0("Cropping Landscapes for ", name))
      path <- sprintf("./data/sdm/Landscapes/predictionLandscapes_%s_%s_5km.tif", scen, yr)
      myExpl_future[[name]] <- crop(rast(path), extent_sp)
      rm(name, path)
      invisible(gc())
    }
  }
  
  # get results into lists (IMPORTANT STEP! Do not skip!!)
  myExplCurrent <- list(myExpl_current)
  myExplFuture  <- myExpl_future
  
  # STEP 4.4 # run SDMs for this biome
  
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
        extent = biome_short,
        output_folder = sdm_output_dir,
        maxent_source = "/mnt/data/maria/NatPoKe/maxent/maxent/maxent.jar", 
        ncoresToUse = 6)
      
      # measure time gone by
      elapsed <- difftime(Sys.time(), start_time, units = "mins")
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
  
  # STEP 4.5 # save timming results
  
  timing_df <- data.frame(Species = names(species_times),
                          Time_difference_mins = as.numeric(species_times),
                          stringsAsFactors = FALSE)
  
  write_xlsx(timing_df, file.path(output_dir,
                                  paste0(biome_short, "_species_times.xlsx")))
  
  rm(extent_sf, extent_crs, extent_sp, myExpl_current, myExpl_future,
    myExplCurrent, myExplFuture, species_times, SDM_NatPoke)
  gc()
}
