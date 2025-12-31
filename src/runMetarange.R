## Name: runMetaRange.R
## Author: Inês Silva ##
## Date: December 20th, 2025 ##
## Description: Getting everything to work in one go! ##

##########
# STEP 1 # Load libraries and custom functions
##########

source(file.path("src", "libraries.R"))
source(file.path("src", "customFunctions.R"))
source(file.path("src", "customFunctions2.R"))

##########
# STEP 2 # Read runs table
##########

runs <- read.csv("data/run_table.csv", stringsAsFactors = FALSE)
TNIND_paths <- list()

for (i in seq_len(nrow(runs))) {
  
  target_region <- runs$region[i]
  target_biome <- runs$biome[i]
  future_scenario <- runs$scenario[i]
  
  runname <- paste(
    target_region,
    future_scenario,
    "20251228",
    #format(Sys.time(), "%Y%m%d"),
    sep = "_"
  )
  
  message("============================================")
  message("Starting run ", i, " of ", nrow(runs))
  message("Run name : ", runname)
  message("Region   : ", target_region)
  message("Biome    : ", target_biome)
  message("Scenario : ", future_scenario)
  message("============================================")
  
  tryCatch({
    
    # --------------------------------------------------------
    # General settings (folders, paths, logging)
    # --------------------------------------------------------
    source(file.path("src", "generalSettings.R"))
    
    # --------------------------------------------------------
    # Load species list
    # --------------------------------------------------------
    species_table <- read.csv(
      file.path(data_dir, "species_by_region.csv"),
      stringsAsFactors = FALSE
    )
    
    target_species <- species_table |>
      dplyr::filter(
        BIOME_NAME == target_biome,
        CONTINENT  == target_region
      ) |>
      dplyr::pull(sci_name) |>
      unique()
    
    if (length(target_species) == 0) {
      stop("No species found for this region/biome.")
    }
    
    # --------------------------------------------------------
    # Run pipeline steps
    # --------------------------------------------------------
    
    # build input trait dataframe
    source(file.path("src", "mammalMetaRangeSpeciesDataframe.R"))
    
    # build environmental layers from SDM's outputs
    source(file.path("src", "speciesSuitabilityLayers.R"))
    
    # run metaRange custom model
    source(file.path("src", "mammalModel.R"))
    
    cat("Run finished:", Sys.time(), "\n")
    sink()
    
    # --------------------------------------------------------
    # Collect TNIND_yr.csv paths for each run
    # --------------------------------------------------------
    tnind_file <- file.path(
      dirout,
      paste0(
        "TNIND_yr_", runname, ".csv"
      )
    )
    
    if (!file.exists(tnind_file)) {
      stop("TNIND file not found: ", tnind_file)
    }
    # store in list
    TNIND_paths[[runname]] <- tnind_file
    
  }, error = function(e) {
    
    message("❌ ERROR in run: ", runname)
    message(e$message)
    
    # make sure sink is closed even on error
    while (sink.number() > 0) sink()
    
  })
}

message("============================================")
message("All runs finished")
message("============================================")

##########
# STEP 3 #
##########

# produce diagnostic plots and dataframes

source(file.path("src", "readMetaRangeOutput.R"))
