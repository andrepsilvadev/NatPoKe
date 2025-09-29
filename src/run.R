#################
# MASTER SCRIPT #
#################
# Inês Silva
# 13 Feb 2024

# Load Settings & Libraries ----------------------------------------------------
source("./src/libraries.R")            # Load necessary packages
source("./src/customFunctions.R")      # Load customized functions

runname <- "13Sep_SouthAmerica_ssp585"   # Unique id for the run e.g. date_region_scenario
source("./src/generalSettings.R")      # Load paths and spatial settings


# Input Selection --------------------------------------------------------------

## Select Target Biome (choose one)
target_biome <- "Tropical & Subtropical Moist Broadleaf Forests" # Options: "Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga"

## Select Target Region (choose one)
target_region <- "South America" # Options: "North America", "South America", "Europe", "Asia", "Africa"

## Select scenario
scenario <- "ssp585"

## Select Target Species (multiple allowed with spaces)
target_species <- c(
  #"Alces alces",
  #"Bison bonasus", "Cervus elaphus", 
  #"Sus scrofa", 
  #"Lynx rufus",
  #"Canis lupus",
  #"Rangifer tarandus",
  #"Gorilla gorilla", "Orycteropus afer", "Pan troglodytes", 
  #"Panthera onca", 
  #"Crocuta crocuta", "Syncerus caffer",
  #"Panthera leo"
  #,"Loxodonta africana"
  "Puma concolor")

# Prepare & Load Input Data ----------------------------------------------------

## create species traits dataframe
source("./src/mammalMetaRangeSpeciesDataframe.R")

## build Species Distribution Models & save output rasters
#source("./src/SDM.R")

## transform SDM outputs into input data for MetaRange model
source("./src/environmentalLayers.R")

# Run MetaRange Model ----------------------------------------------------------

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
