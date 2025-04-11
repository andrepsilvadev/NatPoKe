#################
# MASTER SCRIPT #
#################
# Inês Silva
# 13 Feb 2024

# Load Settings & Libraries ----------------------------------------------------
source("./src/libraries.R")            # Load necessary packages
source("./src/customFunctions.R")      # Load customized functions

runname <- "10April_Europe_abund1.05"   # Unique identifier for the run
source("./src/generalSettings.R")      # Load paths and spatial settings


# Input Selection --------------------------------------------------------------
## Select Target Biome (choose one)
target_biome <- "Boreal Forests/Taiga" 
# Options: "Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga"

## Select Target Region (choose one)
target_region <- "Europe" 
# Options: "North America", "South America", "Europe", "Asia", "Africa"

## Select Target Species (multiple allowed)
target_species <- c(
                    "Alces alces",      
                    "Canis lupus",
                    "Cervus elaphus",
                    "Dama dama",
                    "Lynx lynx",
                    "Rangifer tarandus",
                    "Sus scrofa"
                    )
# Simply modify or add species names in the list above
# Options: see https://ulisboa-my.sharepoint.com/:x:/g/personal/misilva_fc_ul_pt/EVOf6YCgWLVBnAWRzyFahPMBWcKv-2TRGKud35fyjf3Kig?e=zraW2t

# Load Species Data ------------------------------------------------------------
source("./src/mammalMetaRangeSpeciesDataframe.R")
species_traits$initialAbundance <- species_traits$initialAbundance*1.05
# write table to .csv file
write_csv(species_traits, file = file.path(dirinput,"metaRangeSpeciesDataframe.csv"))

## load global suitability raster files & crop 
source("./src/inputFiles.R") 
# if a species modelling resolution is 1 this will throw a warning. It's ok!

# models -----------------------------------------------------------------------
source("./src/mammalModel.R") # run metaRange model for mammals species


# saving simulation outputs ----------------------------------------------------
#source("./src/savingSimulationOutputs.R") # DEPRECATED

# outputs ----------------------------------------------------------------------
source("./mammalSpeciesSpecificPlots.R") # produce multiple maps (abund, repRate and prop abund change) and model validation plot per species

# next scripts can take multiple directories to produce figures and maps
source("./speciesResilienceMetricsFigures.R") # calculate and plot stability metrics for all taxa
source("./communityMetricsFigures.R") # calculate community metrics and build plots over time
source("./updatedSpatiallyExplicitMaps.R") # produces Shannon's index change maps for each continent

# other analysis ---------------------------------------------------------------
source("./src/modelValidation.R")
source("./sensitivityAnalysis.R")


#source("./spatiallyExplicitMaps.R") # DEPRECATED

