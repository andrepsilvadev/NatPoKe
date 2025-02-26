#################
# MASTER SCRIPT #
#################
# Inês Silva
# 13 Feb 2024

# settings & libraries ---------------------------------------------------------
source("./src/libraries.R") # load necessary packages
source("./src/customFunctions.R") # load customized functions

# working directories ----------------------------------------------------------
runname <- "26Feb2025_regionalExtent"
source("./src/generalSettings.R") # paths and spatial settings

# Input Files ------------------------------------------------------------------
source("./src/metaRangeSpeciesDataframe.R") # species dataframe

# CHECK this script before running
source("./src/inputFiles.R") # load global suitability raster files & crop 

# models -----------------------------------------------------------------------
source("./src/mammalModel.R") # run metaRange model for mammals species
#source("./birdsModel.R") # run metaRange model for bird species !! DOES NOT EXIST YET !!
#source("./largeTreesModel.R") # run metaRange model for large tree species !! DOES NOT EXIST YET !!

# saving simulation outputs ----------------------------------------------------
source("./src/savingSimulationOutputs.R")
#this script needs to be changed to deal with very big data #20250225 FOR NOW ITS OK

# metrics and plotting figures -------------------------------------------------
source("./speciesResilienceMetricsFigures.R") # calculate and plot stability metrics for all taxa
# builds 2 figures with impact & recovery values plus time to impact & recovery

source("./communityMetricsFigures.R") # calculate community metrics and build plots over time


source("./spatiallyExplicitMaps.R") # build spatially explicit maps of the world to show community metrics

# model validation and sensitivity analysis ------------------------------------
source("./src/modelValidation.R")
source("./sensitivityAnalysis.R")