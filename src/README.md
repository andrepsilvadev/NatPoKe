# Src folder - How to run the complete pipeline for multiple regions and scenarios

[![Input Data](https://img.shields.io/badge/input-data-green)](https://zenodo.org/uploads/17951701?token=eyJhbGciOiJIUzUxMiJ9.eyJpZCI6IjA0MWI2N2NkLTA0ZDktNDQxNS04ZTMzLWMwNTg2NWU5NTM3NCIsImRhdGEiOnt9LCJyYW5kb20iOiJlYzRlM2I4Y2EwNjdmMDlkZWJjZjkxYTU0NTY5NjBjZSJ9.TYgjuAkY-U5kIEc7eBjv2wElvYDKh799AoS7Y2DBMYFwVS5P0PoGk0YbLf22IJxdsGF6iTFu3FtugfOtwFYBeA) [![Models: Mammals](https://img.shields.io/badge/_Models-🦣_Mammals-yellow?style=flat&labelColor=grey)](https://github.com/andrepsilvadev/NatPoKe/blob/ines_silva/src/mammalModel.R) [![Figures & Maps](https://img.shields.io/badge/visualizations-figures-orange)](#0) [![Validation](https://img.shields.io/badge/validation-modelValidation-red)](https://github.com/andrepsilvadev/NatPoKe/blob/ines_silva/src/modelValidation.R)
<br>

 ## 🛠 Quick Guide <br>

Use the [`runMetaRange.R`](https://github.com/andrepsilvadev/NatPoKe/blob/5b180bb0e875460bef3b3bd129c40bf1b59fc048/src/runMetarange.R) to run the complete pipeline from creating the inputs for specific species, region and scenario up to running the metaRange model and producing results.

``` r

###########
# STEP 1️⃣ # Load libraries and custom functions
###########

source(file.path("src", "libraries.R"))
source(file.path("src", "customFunctions2.R"))

# project directory
project_root <- getwd()

# data paths (shared across runs)
data_dir <- file.path(project_root, "data")

## path for biomes' SDM outputs
biome_paths <- list(                      
  tropical = "SPECIFY_YOUR_OWN_PATH", 
  boreal   = "SPECIFY_YOUR_OWN_PATH"
)

# folder to save intermediate SDMs (processedSDMs, meaning raster outputs once they are finshed running)
processedSDM_dir <- "SPECIFY_YOUR_OWN_PATH"

# output root (all runs live here)
output_root <- "SPECIFY_YOUR_OWN_PATH"

terra::terraOptions(
  # define new temporary folder just for extra files created by terra (usefull when modelling multiple sps, to avoid filling up ram storage)
  tempdir = "SPECIFY_YOUR_OWN_PATH",
  # fractino of memory to use
  memfrac = 0.6,
  # progress bar enabled
  progress = 1)

###########
# STEP 2️⃣ # Read runs table
###########

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
    "20260517", #format(Sys.time(), "%Y%m%d"),
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
    source(file.path("src", "speciesSuitabilityLayers.R"))
    # clean temporary files
    tmpFiles(current = TRUE, remove = TRUE)
    invisible(gc())
    
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

###########
# STEP 3️⃣ # Diganostics & basic validation outputs
###########

# produce diagnostic plots and dataframes
source(file.path("src", "readMetaRangeOutput.R"))

# run model Validation & produce plots
source(file.path("src", "modelValidation.R"))
```

**Environment setup**

 -  `generalSettings.R` - create file directories to save runs inputs and outputs
  
 -  `libraries.R` - install & load all necessary packages

 -  `customFunctions2.R` - load created functions necessary for inputClimate.R, inputLandUse.R, inputElev.R, SDMRun.R, ClimateChange.R, LandUseChange.R
 
