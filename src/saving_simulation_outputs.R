####################################################
# Combining metaRange rasters & building dataframe #
####################################################
# MIS
# 31 Jan 2025

# GOAL: Build a script to same metaRange simulations output as a dataframe with 
# all variables and species with coordinates

##########
# STEP 1 #  Select species and traits for which raster might exist
##########

# Create empty list to store results for each raster type
results_list <- list()

# Define raster types
raster_types <- c("abundance", "reproductionRate", "mortality", "carrying_capacity", "dispersal_distance")

# Read species data
species_traits <- read.csv(here("data","metaRangeSpeciesDataframe.csv"))
species_names <- species_traits$Species

##########
# STEP 2 #  loop through each species and raster type
##########

for (sp in species_names) {
  
  # create an empty list to store values for a sps
  species_data <- list()
  
  for (raster_type in raster_types) {
    
    # find the raster files (for a sps and raster type)
    flist <- list.files(here("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/results06Feb2025v2"), 
                        pattern = paste0(sp, "_", raster_type, ".tif"), full.names = TRUE)
    
    # skip if no files found print WARNING
    if (length(flist) == 0) {
      message(paste("No rasters found for", sp, raster_type, "- Someone should check if this is a mistake!"))
      next
    }
    
    # process each raster
    for (raster in flist) {
      
      # read
      r <- terra::rast(raster)
      invisible(gc())
      
      # retrieve filename and split it
      filename <- basename(raster)
      filename_parts <- strsplit(tools::file_path_sans_ext(filename), "_")[[1]]
      
      # convert raster to data frame (with coordinates and values)
      raster_data <- terra::as.data.frame(r, xy = TRUE, na.rm = TRUE, row.names = FALSE)
      invisible(gc())
      
      # add more info as new columns
      raster_data$scenario <- filename_parts[1] # BAU
      raster_data$biome <- filename_parts[2] # TropicalForests
      raster_data$region <- filename_parts[3] # Asia    
      raster_data$timestep <- filename_parts[4] # 001  
      raster_data$species <- sp 
      invisible(gc())
      
      # rename raster value column to the corresponding variable = raster type
      names(raster_data)[names(raster_data) == "lyr1"] <- raster_type
      
      # store list for that species
      if (!is.null(species_data[[raster_type]])) {
        species_data[[raster_type]] <- rbind(species_data[[raster_type]], raster_data)
      } else {
        species_data[[raster_type]] <- raster_data
        invisible(gc())
      }
    }
  }
  
  # merge all rasters for that species 
  merged_species_data <- Reduce(function(x, y) merge(x, y, by = intersect(names(x), names(y)), all = TRUE), species_data)
  
  # store final merged data for that species in a list (esch specis a new element in this list)
  results_list[[sp]] <- merged_species_data
}

# combine all results for all species into one big data frame
final_results <- do.call(rbind, results_list)

# check results!!!!!!!!!!!
head(final_results)


write.csv(final_results, "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/results06Feb2025v2/metaRangeOutputs06Fev2025v2.csv" )
invisible(gc())

#####################
# JUST TESTING DATA #
#####################

library(data.table)
library(ggplot2)
library(dplyr)
library(terra)

results06feb <- fread("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/results06Feb2025v2/metaRangeOutputs06Fev2025v2.csv")
results06feb$taxa <- "Mammal"


mean_abund <- results06feb %>% 
  group_by(x, y) %>%
  mutate(cell_id = cur_group_id()) %>%
  ungroup() %>% 
  group_by(scenario, biome, region, taxa, species, timestep) %>%
  dplyr::filter(abundance != 0) %>% 
  summarise(mean_abundance = mean(abundance, na.rm = TRUE))

ggplot(mean_abund, aes(x= timestep , y = mean_abundance, group = species, fill = species)) +
  geom_line()

plot(rast("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/results06Feb2025v2/BAU_Tropical_Asia_030_Rangifertarandus_abundance.tif"))

