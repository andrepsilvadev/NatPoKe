## Name: SDM.R ##
## Author: Jorinde-M. Rieger & André P. Silva & Inês Silva ##
## Date: October 10th 2025 ##
## Description: builds and projects ensemble Species Distribution Models (SDMs)
## for multiple target species under current and future climate scenarios, using BIOMOD2

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

# This is a highly sensible script! Here are some good practices to make sure
# we don't get errors running it:
# (1) do not work inside a One Drive folder;
# (2) avoid saving the outputs using long paths (MAXNET does not deal well with them)

# settings & libraries
source("./src/libraries.R") # libraries
source("./src/customFunctions2.R") # functions

##########
# STEP 1 # Load target species, full environmental training raster & occurrences (done only once!)
##########

# target species
species_table <- read.csv("./data/traitData/CompleteMammalSpsDataframe_2025-12-20.csv",
                          stringsAsFactors = FALSE)

# environmental variables to keep in landscapes & models
vars_to_keep <- c("Elevation",
                  "Urban",
                  "Cropland",
                  "Pasture_Grassland",
                  "Forest",
                  "Nonforest_vegetation",
                  "Water",
                  "Barren_other",
                  #"bio1", # Annual Mean Temperature
                  #"bio10", # Mean Temp. Warmest Quarter
                  "bio11", # Mean Temp. Coldest Quarter
                  "bio12"#, # Annual Percipitation
                  #"bio16", # Percipitation Wettest Quarter
                  #"bio17" # Percipitation Driest Quarter
)

# training raster
myExpl_full <- rast("./data/sdm/Landscapes/trainingLandscapes_2015_5km.tif")
myExpl_full <- myExpl_full[[vars_to_keep]]
invisible(gc())

# species occurrences
speciesData <- read.csv("./data/sdm/GBIF_occurrences_mammals_2026-02-04.csv")
speciesData$species <- gsub(" ", ".", speciesData$species)
invisible(gc())

# crop FUTURE landscapes dynamically 
years <- c(2030, 2050, 2100)
future_scenario <- c("ssp126", "ssp585")

# set up a temporary folder for writeRatser() from terra package
### Because the terra package function writeRaster needs to write supporting rasters
### during its processes, and because the SDM function we have can write quite 
### heavy raster we should clean up teh temporary files to avoid filling up our
### RAM space and stopping the whole process. For that the solution MIS has found 
### starts by creating a folder path that we know exactely where it is (so we can monitor)
### using terraOptions() and then cleaning the temporary files from that folder.
### WHY CREATE A NEW FOLDER AND NOT USE THE DEFAULT? because terra is used inside
### biomod2 if we define the path oursefs we ensure the folder exists and terra
### does not fall back to the another location or default folder it builds on
### each R session

# # set terra temporary folder
# terra::terraOptions(
#   # set teh temporary files folder to a path we now where it is
#   tempdir = "C:/Users/maria/Desktop/testing",
#   # fraction of RAM the PC is allowed to use
#   memfrac = 0.7,
#   # wether or not to show a progress bar
#   progress = 1)


##########
# STEP 2 # Loop over biomes
##########

biomes <- c("Tropical & Subtropical Moist Broadleaf Forests"#,
  #"Boreal Forests/Taiga"
            )
target_biome <- "Tropical & Subtropical Moist Broadleaf Forests"
#target_biome <- "Boreal Forests/Taiga"
for (target_biome in biomes) {
  
  message("====================================")
  message("Starting biome: ", target_biome)
  message("====================================")
  
  # determine biome short name
  biome_short <- if (grepl("Tropical", target_biome)) "tropical" else "boreal"
  
  ## output directory per biome
  sdm_output_dir <- file.path(
    #"D:/NatPoKe_SDMs"
    #"E:/metaRange_May26/data/sdm"
    "./data/sdm",
    paste0(biome_short, "_SDMS"))
  dir.create(sdm_output_dir, recursive = TRUE, showWarnings = FALSE)
  
  ############
  # STEP 2.1 # load and crop biome extent
  ############
  extent_sf  <- load_biome(target_biome)
  extent_crs <- sf::st_transform(extent_sf, crs = crs(myExpl_full))
  extent_bm  <- terra::vect(extent_crs)
  invisible(gc())
  
  ############
  # STEP 2.2 # crop CURRENT landscape to the target biome
  ############
  # crop to biome
  myExpl_current <- crop(
    rast("./data/sdm/Landscapes/trainingLandscapes_2015_5km.tif"),
    extent_bm)
  # select approapriate environmental variables 
  myExpl_current <- myExpl_current[[vars_to_keep]]
  invisible(gc())
  
  ############
  # STEP 2.3 # crop FUTURE landscapes
  ############
  myExpl_future <- list()
  
  # dynamically crop teh landscapes rasters per year and scenario by the biome
  for (scen in future_scenario) {
    for (yr in years) {
      name <- paste0(scen, "_", yr, "_", biome_short)
      message(paste0("Cropping Landscapes for ", name))
      path <- sprintf("./data/sdm/Landscapes/predictionLandscapes_%s_%s_5km.tif", scen, yr)
      # Load & crop
      r <- crop(rast(path), extent_bm)
      # Select same variables as current
      myExpl_future[[name]] <- r[[vars_to_keep]]
      rm(name, path, r)
      invisible(gc())
    }
  }

  
  # get results into lists (IMPORTANT STEP! Do not skip!!)
  myExplCurrent <- list(myExpl_current)
  myExplFuture  <- myExpl_future
  
  ############
  # STEP 4.4 # run SDMs for this biome
  ############
  # SDMensembleMultiSpecies() – this function automates the full SDM workflow for
  # each species: filtering and formatting GBIF data, generating pseudo-absences, 
  # fitting multiple algorithms (RF, XGBoost, ANN, MAXNET), building ensemble models, 
  # and projecting them to current and future climate scenarios. 
  # It saves all evaluation metrics, variable importance tables, and raster predictions 
  # (continuous and binary) into the specified output folder.
  
  # store timing info
  species_times <- list()   
  
  # select taregt species for specific biome
  target_species <- species_table %>% 
    # replace spaces with . to match biomod2
    mutate(sci_name = gsub(" ", ".", sci_name)) %>% 
    # filter species for target biome
    dplyr::filter(BIOME_NAME == target_biome) %>% 
    # keep only names
    dplyr::pull(sci_name) %>% 
    unique()
  
  #testing
  target_species <- c("Ateles.belzebuth", "Lycalopex.griseus", "Puma.concolor")
  #sp <- target_species[[1]]  
  
  SDM_NatPoke <- lapply(target_species, function(sp) {
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
        maxent_source = "C:/Users/maria/Desktop/maxent/maxent/maxent.jar",
          #"/mnt/data/maria/NatPoKe/maxent/maxent/maxent.jar", 
        ncoresToUse = 6)
      
      # clean temporary files to avoid filling RAM up
      # message("Cleaning temporary raster files")
      # terra::tmpFiles(remove = TRUE)
      # invisible(gc())
      
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
  
  ############
  # STEP 4.5 # save timming results
  ############
  
  timing_df <- data.frame(Species = names(species_times),
                          Time_difference_mins = as.numeric(species_times),
                          stringsAsFactors = FALSE)
  
  write_xlsx(timing_df, file.path(sdm_output_dir,
                                  paste0(biome_short, "_species_times6.xlsx")))
  
  rm(extent_sf, extent_crs, extent_sp, myExpl_current, myExpl_future,
    myExplCurrent, myExplFuture, species_times, SDM_NatPoke)
  gc()
}
