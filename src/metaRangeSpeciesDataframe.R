###############################################
# FORMATING metaRange SPECIES INPUT DATAFRAME #
###############################################
# Ines Silva
# 04 Feb 2025

# GOAL: Format the pre-existing species traits dataframe to serve as input in the
# metaRange model based on the current landscape

# packages
library(readr)
library(here)
library(terra)
library(stringr) # to remove spaces between words
library(tibble)
library(dplyr)
library(tidyr)

##########################
# Import Trait Dataframe #
##########################

combined_traits_data <- read_csv(here("data", "combined_traits_data_20250110.csv")) %>% 
  mutate(Species = stringr::str_replace_all(Species, " ", ""))

#####################
# Import Landscapes #
#####################

landscapes <- list.files(path = here("example/mammals_try2/clean_data_2species"),
           pattern = "_suitability_cropped_modified.tif",
           full.names = TRUE)

# create empty dataframe
landscape_df <- data.frame(Species = character(),
                           pixel_size_x = numeric(),
                           pixel_size_y = numeric(),
                           stringsAsFactors = FALSE)

# loop through each file and extract pixel size
for (file in landscapes) {
  rast_obj <- rast(file)  # Read raster
  res_x <- res(rast_obj)[1]  # Pixel size in x direction
  res_y <- res(rast_obj)[2]  # Pixel size in y direction
  
  # get species name (first word before "_")
  filename <- basename(file)
  species <- strsplit(filename, "_")[[1]][1]
  
  # put those sizes into the dataframe
  landscape_df <- rbind(landscape_df, data.frame(Species = species,
                                                 # for now 20250296 we are simplifying because we know pixels are 5km
                                                 pixel_size_x = 5, # this should be the pixel size * 110
                                                 pixel_size_y = 5) # this should be the pixel size * 110
                        ) 
}
# check pixels sizes
#landscape_df
crs(rast_obj)
# merge with combined traits dataframe
combined_traits_data <- merge(combined_traits_data, landscape_df, by = "Species", all.x = TRUE)

####################
# FORMAT DATAFRAME #
####################


spData <- tibble(
  Index = 1:nrow(combined_traits_data), # species index
  Species = combined_traits_data$Species, # scientific name WITHOUT spaces
  Family = combined_traits_data$Family, # family
  Order = combined_traits_data$Order, # order
  BodyMass = combined_traits_data$BodyMass, # species body mass (kg)
  CellResolution = as.numeric(combined_traits_data$pixel_size_x*combined_traits_data$pixel_size_y), # cell area  in Km2 (as santini data comes in Ind/km)
  #ModellingRes = ceiling(sqrt(2/as.numeric(combined_traits_data$IndsHaCell))),
  #ProjRes = ModellingRes*1000,
  initialAbundance = (as.numeric(combined_traits_data$IndsHaCell))*CellResolution, # initial number of individuals per cell (from PredMd, in Ind/km2, Santini et al. 2022)
  carryingCapacity = as.numeric(combined_traits_data$TargetHaDensity)*CellResolution, # maximum number of individuals per cell (from up75, in Ind/km2, Santini et al. 2022)
  reproductionRate = combined_traits_data$Stage1Fecundity, # Litter size
  dispersalDistance = combined_traits_data$MeanDisp, # mean dispersal distance based om trophic level (km, Schloss et al. 2012)
  dispersalMaxDistance = combined_traits_data$LongDisp, # maximum long distance dispersal based on trophic level (km, Schloss et al. 2012)
) %>%
  drop_na()

# check NA's
sapply(spData, function(x) sum(is.na(x))) # number NA per column
sapply(spData, function(x) sum(is.na(x)/length(x))) # proportion NA per column

# write table to .csv file
write_csv(spData, file = file.path(here("data"),"metaRangeSpeciesDataframe.csv")) 
