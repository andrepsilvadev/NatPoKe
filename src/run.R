#################
# MASTER SCRIPT #
#################
# Inês Silva
# 13 Feb 2024

# Load Settings & Libraries ----------------------------------------------------
source("./src/libraries.R")            # Load necessary packages
source("./src/customFunctions.R")      # Load customized functions

runname <- "23April_Africa"   # Unique identifier for the run
source("./src/generalSettings.R")      # Load paths and spatial settings


# Input Selection --------------------------------------------------------------

## Select Target Biome (choose one)
target_biome <- "Tropical & Subtropical Moist Broadleaf Forests" # Options: "Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga"

## Select Target Region (choose one)
target_region <- "Africa" # Options: "North America", "South America", "Europe", "Asia", "Africa"

## Select scenario
scenario <- "SSP1"

## Select Target Species (multiple allowed with spaces)
target_species <- c(
  "Crocuta crocuta",      
  "Panthera leo"
)
# See here possible species options: https://ulisboa-my.sharepoint.com/:x:/g/personal/misilva_fc_ul_pt/EVOf6YCgWLVBnAWRzyFahPMBWcKv-2TRGKud35fyjf3Kig?e=zraW2t

# Prepare & Load Species Data --------------------------------------------------

source("./src/mammalMetaRangeSpeciesDataframe.R")
#species_traits$initialAbundance <- species_traits$initialAbundance*1.05
# write table to .csv file
#write_csv(species_traits, file = file.path(dirinput,"metaRangeSpeciesDataframe.csv"))

source("./src/inputFiles.R")

source("./src/mammalModel.R")

#source("./src/mammalModelSpeciesSpecific.R") # run metaRange model for mammals with species specific resolution

# outputs ----------------------------------------------------------------------
source("./mammalSpeciesSpecificPlots.R") # produce multiple maps (abund, repRate and prop abund change) and model validation plot per species

# next scripts can take multiple directories to produce figures and maps
source("./speciesResilienceMetrics.R") # calculate and plot stability metrics for all taxa
source("./communityMetrics.R") # calculate community metrics and build plots over time
source("./updatedSpatiallyExplicitMaps.R") # produces Shannon's index change maps for each continent

# other analysis ---------------------------------------------------------------
source("./src/modelValidation.R")
source("./updatedsensitivityAnalysis.R")


#source("./spatiallyExplicitMaps.R") # DEPRECATED

# saving simulation outputs ----------------------------------------------------
#source("./src/savingSimulationOutputs.R") # DEPRECATED
