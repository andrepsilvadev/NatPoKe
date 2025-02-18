#################
# MASTER SCRIPT #
#################
# Inês Silva
# 13 Feb 2024

# settings & libraries ---------------------------------------------------------
source("./src/libraries.R") # load necessary packages
source("./src/customFunctions.R") # load customized functions

# working directories ----------------------------------------------------------
runname <- "18Feb2025"
source("./src/generalSettings.R") # paths and spatial settings

# Input Files ------------------------------------------------------------------
source("./src/metaRangeSpeciesDataframe.R") # species dataframe

# CHECK this script before running
source("./src/inputFiles.R") # load global suitability raster files & crop 

# models -----------------------------------------------------------------------
source("./mammalModel.R") # run metaRange model for mammals species
source("./OLDmammalModel.R") # CURRENT model which runs for mammals but it is not the most accurate version YET!
#source("./birdsModel.R") # run metaRange model for bird species !! DOES NOT EXIST YET !!
#source("./largeTreesModel.R") # run metaRange model for large tree species !! DOES NOT EXIST YET !!

# saving simulation outputs ----------------------------------------------------
source("./savingSimulationOutputs.R")
#this script needs to be changed to deal with very big data

# metrics and plotting figures -------------------------------------------------
source("./speciesResilienceMetricsFigures.R") # calculate and plot stability metrics for all taxa
# builds 2 figures with impact & recovery values plus time to impact & recovery

source("./src/communityMetricsFigures.R") # calculate community metrics and build plots over time


source("./src/spatiallyExplicitMaps.R") # build spatially explicit maps of the world to show community metrics

# model validation and sensitivity analysis ------------------------------------
source("./modelValidation.R")
source("./sensitivityAnalysis.R")