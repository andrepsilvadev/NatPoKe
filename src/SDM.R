## Name: SDM.R ##
## Author: Jorinde-M. Rieger & André P. Silva & Inês Silva ##
## Description: builds and projects ensemble Species Distribution Models (SDMs)
## for multiple target species under current and future climate scenarios, using BIOMOD2
## Date: October 10th 2025 ##

## Script overview:
##   STEP 1 – Load and crop environmental rasters to target biome.
##   STEP 2 – Define and prepare current and future climate scenarios.
##   STEP 3 – For each species, prepare presence/pseudo-absence data.
##   STEP 4 – Train individual models (RF, XGBOOST, ANN, MAXNET/MAXENT).
##   STEP 5 – Build ensemble models and evaluate them.
##   STEP 6 – Project models to current and future conditions.
##   STEP 7 – Save continuous and binary suitability rasters for each scenario.
##   STEP 8 – Record processing time per species and export results.


# !! NAVIGATION WARNINGS !! ----------------------------------------------------

# This is a highly sensible function! Here are some good practices to make sure
# we don't get errors running it:
# (1) do not work inside a One Drive folder;
# (2) avoid saving the outputs using long paths (MAXNET does not deal well with them)
# (3) some minor chnages were done to André's function, namely adding
# the path to the maxent folder as a function argument (might not be necessary in
# the future if we stick with MAXNET but I am still leaving it here)
# (4) function stops if one species has an error SOLUTION? use TryCatch()

#setwd("./data") # only necessary on gunvor
# settings & libraries
source("./src/libraries.R") # libraries
source("./src/customFunctions2.R") # functions

##########
# STEP 1 # Load & Process Environmental rasters
##########

#myExpl_full <- rast(list.files(pattern ='trainingLandscapes_2015_5km.tif$'))
myExpl_full <- rast("./Landscapes/trainingLandscapes_2015_5km.tif")
invisible(gc())

# load and project biome extent
extent_sf  <- load_biome(target_biome)
extent_crs <- sf::st_transform(extent_sf, crs = crs(myExpl_full))
extent_sp  <- terra::vect(extent_crs)
invisible(gc())

# determine biome short name
biome_short <- if (grepl("Tropical", target_biome)) "tropical" else "boreal"

# crop CURRENT landscape to the target biome
myExpl_current <- crop(
  rast("./Landscapes/trainingLandscapes_2015_5km.tif"),
  extent_sp)
invisible(gc())

# crop FUTURE landscapes dynamically 
years <- c(2030, 2050, 2100)
scenarios <- c("ssp126", "ssp585")

myExpl_future <- list()

# dynamically crop teh landscapes rasters per year and scenario by the biome
for (scen in scenarios) {
  for (yr in years) {
    name <- paste0(scen, "_", yr, "_", biome_short)
    message(paste0("Cropping Landscapes for ", name))
    path <- sprintf("./Landscapes/predictionLandscapes_%s_%s_5km.tif", scen, yr)
    myExpl_future[[name]] <- crop(rast(path), extent_sp)
    rm(name, path)
    invisible(gc())
  }
}


# get results into lists (IMPORTANT STEP! Do not skip!!)
myExplCurrent <- list(myExpl_current)
myExplFuture  <- myExpl_future

##########
# STEP 2 # Load and Prep Species Occurrences Data
##########

# read .csv file with occurence data
speciesData <- read.csv("./data/GBIF_occurrences.csv")
# the way biomod2 works has issues if we write sps names without a "."
speciesData$species <- gsub(" ", ".", speciesData$species)
invisible(gc())

# select target species
targetSpecies <- c(## BOREAL SPS ##
  #"Alces alces", "Canis lupus"#, "Bison bonasus", "Cervus elaphus", 
  #"Sus scrofa", "Vulpes vulpes", "Canis latrans", "Lynx rufus",
  #"Martes americana", "Taxidea taxus", "Ursus americanus", "Panthera tigris",
  #"Lynx lynx", "Ursus arctos", "Rangifer tarandus",
  #"Puma concolor", "Bison bison",
  
  ## TROPICAL SPS ##
  "Sus scrofa", "Vulpes vulpes", "Panthera tigris", "Lynx lynx")
#"Leontopithecus caissara", # has only 4 occurences
#"Leopardus pardalis", "Nasua nasua", "Aepyceros melampus",
#"Colobus angolensis", "Daubentonia madagascariensis",
#"Diceros bicornis", "Erythrocebus patas", "Gorilla beringei",
#"Gorilla gorilla", "Orycteropus afer", "Pan paniscus",
#"Pan troglodytes", "Papio anubis", "Papio ursinus", "Cervus nippon",
#"Cuon alpinus", "Felis chaus", "Macaca fuscata", "Pongo abelii",
#"Pongo pygmaeus",  "Panthera onca", "Crocuta crocuta", "Mandrillus sphinx",
#"Panthera pardus", "Syncerus caffer", "Acinonyx jubatus",
#"Panthera leo", "Connochaetes taurinus", "Loxodonta africana",
#"Puma concolor"
#)

# ajust names for biomod2
target_species <- gsub(" ", ".", target_species)
# check target species are included in occ file
target_species %in% speciesData$species
invisible(gc())

##########
# STEP 3 # Run Multi-Species SDM function
##########

# SDMensembleMultiSpecies() – this function automates the full SDM workflow for
# each species: filtering and formatting GBIF data, generating pseudo-absences, 
# fitting multiple algorithms (RF, XGBoost, ANN, MAXNET), building ensemble models, 
# and projecting them to current and future climate scenarios. 
# It saves all evaluation metrics, variable importance tables, and raster predictions 
# (continuous and binary) into the specified output folder.

# store timing info
species_times <- list()   

SDM_NatPoke <- lapply(targetSpecies, function(sp) {
  tryCatch({
    # measure start time
    start_time <- Sys.time()
    
    result <- SDMensembleMultiSpecies(
      targetSpecies = sp,
      speciesData = speciesData,
      myExpl_full = myExpl_full,
      myExplCurrent = myExplCurrent,
      myExplFuture = myExplFuture,
      extent = "boreal",
      output_folder = "/mnt/data/maria/NatPoKe/output/NatPoKe_October25_tropical",
      maxent_source = "/mnt/data/maria/NatPoKe/maxent/maxent/maxent.jar", 
      ncoresToUse = 6)
    
    # measure time end
    end_time <- Sys.time()
    elapsed <- difftime(end_time, start_time, units = "mins")
    
    message("✅ Finished ", sp, " in ", round(elapsed, 2), " minutes")
    
    # store timing
    species_times[[sp]] <<- elapsed
    
    return(result)
    
  }, error = function(e) {
    message(paste("⚠️ Skipping", sp, "due to error:", e$message))
    species_times[[sp]] <<- NA   # store NA if failed
    return(NULL)
  })
})


# get running time into excel
library(writexl)

# Convert the list to a data frame
df <- data.frame(
  Species = names(species_times),
  Time_difference_mins = as.numeric(species_times)
)

#BEFORE RUNNING THE NEXT LINE GO TO  "Session" on the top ribbon > Set working directory > To Project Directory

# Write to Excel
write_xlsx(df, "./data/tropicalSpecies_times_05Nov2025.xlsx")