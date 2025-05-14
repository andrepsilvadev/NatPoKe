#################
# MASTER SCRIPT #
#################
# Inês Silva
# 13 Feb 2024

# Load Settings & Libraries ----------------------------------------------------
source("./src/libraries.R")            # Load necessary packages
source("./src/customFunctions.R")      # Load customized functions

runname <- "13May_Europe_Robinson"   # Unique id for the run e.g. date_region_scenario
source("./src/generalSettings.R")      # Load paths and spatial settings


# Input Selection --------------------------------------------------------------

## Select Target Biome (choose one)
target_biome <- "Boreal Forests/Taiga" # Options: "Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga"

## Select Target Region (choose one)
target_region <- "Europe" # Options: "North America", "South America", "Europe", "Asia", "Africa"

## Select scenario
scenario <- "SSP1"

## Select Target Species (multiple allowed with spaces)
target_species <- c(
  "Alces alces",
  "Lynx lynx")

# Prepare & Load Species Data --------------------------------------------------

source("./src/mammalMetaRangeSpeciesDataframe.R")

#source("./src/inputFiles.R")
source("./src/inputFiles_Robinson.R")

source("./src/mammalModel.R")

#source("./src/mammalModelSpeciesSpecific.R") # run metaRange model for mammals with species specific resolution

# Main outputs ----------------------------------------------------------------------
source("./updatedSpeciesResilienceMetrics.R") # calculate and plot stability metrics for all functional groups

source("./updatedCommunityMetrics.R") # produces Shannon's index change maps for each continent

source("./updatedSpatiallyExplicitMaps.R") # produces Shannon's index change maps for each continent


# other analysis ---------------------------------------------------------------
source("./src/updatedModelValidation.R") # using multiple directories
source("./updatedsensitivityAnalysis.R") # using multiple directories
source("./mammalSpeciesSpecificPlots.R") # produce multiple maps (abund, repRate and prop abund change) and model validation plot per species


# DEPRECATED SCRIPTS #

#source("./speciesResilienceMetrics.R") # calculate and plot stability metrics for all taxa
#source("./communityMetrics.R") # calculate community metrics and build plots over time
#source("./spatiallyExplicitMaps.R") # DEPRECATED
# saving simulation outputs ----------------------------------------------------
#source("./src/savingSimulationOutputs.R") # DEPRECATED
