#################
# MASTER SCRIPT #
#################
# Inês Silva
# 13 Feb 2024

# Load Settings & Libraries ----------------------------------------------------
source("./src/libraries.R")            # Load necessary packages
source("./src/customFunctions.R")      # Load customized functions
source("./src/customFunctions2.R")      # Load customized functions

runname <- "Asia_ssp585_31Oct25"   # Unique id for the run e.g. date_region_scenario
source("./src/generalSettings.R")      # Load paths and spatial settings


# Input Selection --------------------------------------------------------------

## Select Target Biome (choose one)
target_biome <- "Tropical & Subtropical Moist Broadleaf Forests" # Options: "Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga"

## Select Target Region (choose one)
target_region <- "Asia" # Options: "North America", "South America", "Europe", "Asia", "Africa"

## Select scenario
scenario <- "ssp585"

## Select Target Species (multiple allowed with spaces)
target_species <- c(
  ##############
  # BOREAL SPS #
  ##############
  
  # Europe
  #"Alces alces", "Bison bonasus", "Cervus elaphus", "Sus scrofa", "Vulpes vulpes",
  #"Panthera tigris", "Lynx lynx", "Ursus arctos", "Canis lupus", "Rangifer tarandus"#,
  
  # North America
  #"Alces alces", "Canis latrans", "Lynx rufus", "Martes americana", "Taxidea taxus",
  #"Ursus americanus", "Vulpes vulpes", "Puma concolor", "Bison bison", "Ursus arctos",
  #"Canis lupus", "Rangifer tarandus"#,
  
  # South America
  #"Leontopithecus caissara", 
  #"Leopardus pardalis", "Nasua nasua", "Panthera onca",
  #"Puma concolor"#,
  
  # Africa
  #"Aepyceros melampus", "Colobus angolensis", #"Daubentonia madagascariensis",
  #"Diceros bicornis", "Erythrocebus patas", "Gorilla beringei", "Gorilla gorilla",
  #"Orycteropus afer", #"Pan paniscus",
  #"Pan troglodytes", "Papio anubis", "Papio ursinus", 
  #"Crocuta crocuta", "Mandrillus sphinx", "Panthera pardus", "Syncerus caffer",
  #"Acinonyx jubatus", "Panthera leo", "Connochaetes taurinus", "Loxodonta africana"#,
  
  # Asia
  "Cervus nippon", "Cuon alpinus", "Felis chaus", "Macaca fuscata", "Pongo abelii",
  "Pongo pygmaeus", "Sus scrofa", "Vulpes vulpes", "Panthera pardus", "Acinonyx jubatus",
  "Lynx lynx", "Panthera leo", "Panthera tigris", "Ursus arctos",
  "Canis lupus"
)

# Prepare & Load Input Data ----------------------------------------------------

## build Species Distribution Models & save output rasters
#source("./src/SDM.R")
### DONT FORGET !!! CHNAGE THE SCRIPT TO PUT THE SDMs FOLDER AS THE OUTPUT FOLDER FOR THE FUNCTION HERE, 
### THEN CHNAGE THE NEXT SCRIPTS TO FISH TEH OUTPUTS FROM THERE !!!!!!!

## produce supplemnatry figures to visualise SDM results
#source(".src/SDMfigures.R")

## create species traits dataframe
source("./src/mammalMetaRangeSpeciesDataframe.R")

## transform SDM outputs into input data for MetaRange model
source("./src/speciesSuitabilityLayers.R")

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
