## Name: runMetaRange.R
## Author: Inês Silva ##
## Date: December 20th, 2025 ##
## Description: Getting everything to work in one go! ##

##########
# STEP 1 # Load libraries and custom functions
##########

source(file.path("src", "libraries.R"))
source(file.path("src", "customFunctions2.R"))

# project directory
project_root <- getwd()

# data paths (shared across runs)
data_dir <- file.path(project_root, "data")

## path for biomes' SDM outputs
biome_paths <- list(                      
  tropical = "D:/NatPoKe_SDMs/tropical_SDMS", #"./data/sdm/tropical_SDMS",
  boreal   = "D:/NatPoKe_SDMs/boreal_SDMS" #"./data/sdm/boreal_SDMS"
)

# folder to save intermediate SDMs (processedSDMs)
processedSDM_dir <- "D:/NatPoKe_SDMs/processedSDMs"

# output root (all runs live here)
output_root <- "E:/metaRange_May26"

terra::terraOptions(
  # define new temporary folder
  tempdir = "E:/lixo",
  # fractino of memory to use
  memfrac = 0.6,
  # progress bar enabled
  progress = 1)

##########
# STEP 2 # Read runs table
##########

runs <- read.csv("data/run_table.csv", stringsAsFactors = FALSE)
#TNIND_paths <- list()

for (i in seq_len(nrow(runs))) {
  
  target_region <- runs$region[i]
  target_biome <- runs$biome[i]
  future_scenario <- runs$scenario[i]
  start_time <- Sys.time()
  
  runname <- paste(
    target_region,
    future_scenario,
    "20260517",
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
    
    # Remove specific species
    target_species <- target_species[
      !target_species %in% c("Lycalopex griseus", "Ateles belzebuth")
    ]
    
    if (length(target_species) == 0) {
      stop("No species found for this region/biome.")
    }
    
    # --------------------------------------------------------
    # Run pipeline steps
    # --------------------------------------------------------
    
    # build input trait dataframe
    source(file.path("src", "mammalMetaRangeSpeciesDataframe.R"))
    
    # build environmental layers from SDM's outputs
    #source(file.path("src", "speciesSuitabilityLayers.R"))
    # clean temporary files
    #tmpFiles(current = TRUE, remove = TRUE)
    #invisible(gc())
    
    # run metaRange custom model
    source(file.path("src", "mammalModel.R"))
    # clean temporary files
    tmpFiles(current = TRUE, remove = TRUE)
    invisible(gc())
    
    # run meanTraitValuesAcrossReplicate.R (build an average raster per timestep)
    source(file.path("src", "meanTraitValuesAcrossReplicate.R"))
    
    cat("Run finished:", round(
      as.numeric(difftime(Sys.time(), start_time, units = "mins")), 2
    ), "\n")
    sink()
    
  }, error = function(e) {
    
    message("❌ ERROR in run: ", runname)
    message(e$message)
    
    # while (sink.number(type="message") > 0)
    #   sink(type="message")
    # 
    # while (sink.number() > 0)
    #   sink()
    
  })
}

message("=====================")
message("= All runs finished =")
message("=====================")

##########
# STEP 3 # Diganostics & basic validation outputs
##########

# produce diagnostic plots and dataframes
source(file.path("src", "readMetaRangeOutput.R"))

# run model Validation & produce plots
source(file.path("src", "modelValidation.R"))
