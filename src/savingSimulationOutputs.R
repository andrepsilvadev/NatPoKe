####################################################
# Combining metaRange rasters & building dataframe #
####################################################
# MIS
# 31 Jan 2025

# GOAL: Build a script to save metaRange simulations output as a dataframe with 
# all variables and species with coordinates

##########
# STEP 1 #  Select species and traits for which raster might exist
##########

# Create empty list to store results for each raster type
results_list <- list()

# Define raster types
raster_types <- c("abundance", "reproductionRate", "dispersal_change")

# Read species data
species_traits <- read.csv(file.path(dirinput,"metaRangeSpeciesDataframe.csv"))
species_names <- species_traits$Species

##########
# STEP 2 #  loop through each species and raster type
##########

for (sp in species_names) {
  
  # create an empty list to store values for a sps
  species_data <- list()
  
  for (raster_type in raster_types) {
    
    # find the raster files (for a sps and raster type)
    flist <- list.files(dirout, 
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
      #raster_data$scenario <- filename_parts[1] # BAU
      raster_data$biome <- filename_parts[1] # TropicalForests
      raster_data$region <- filename_parts[2] # Asia    
      raster_data$timestep <- filename_parts[3] # 001  
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

# remove r obj to save space
rm(r)
invisible(gc()) 

# remove raster_data obj to save space
rm(raster_data)
invisible(gc()) 

# remove species_data obj to save space
rm(species_data)
invisible(gc())

# remove merged_species_data obj to save space
rm(merged_species_data)
invisible(gc())

# combine all results for all species into one big data frame
final_results <- do.call(rbind, results_list)
invisible(gc())


# add taxa and trophic level before writing final .csv
final_results <- final_results %>% 
  left_join(species_traits %>% dplyr::select(Species, TrophicLevel, Taxa), 
            by = c("species" = "Species"))

# check results!!!!!!!!!!!
head(final_results)
unique(final_results$species)

# writing a .csv file
write.csv(final_results, file.path(dirout, paste0("metaRangeOutputs", runname, ".csv")),
          row.names = FALSE)
invisible(gc())

#####################
# JUST TESTING DATA #
#####################
# 
# library(data.table)
# library(ggplot2)
# library(dplyr)
# library(terra)
# 
# #final_results$Taxa <- "Mammal"
# 
# # Total number of individuals (TNIND) per year and cellid
# TNIND <- final_results %>%
#   group_by(species, Taxa, biome, scenario, timestep) %>% # ADD HERE WHEN THEY EXIST SIM AND REP VARIABLES (SIM FOR SIMULATION NAME AND REP FOR REPLICATES)
#   dplyr::summarize(sum_TNIND = sum(abundance, na.rm = TRUE), # n individuals in each cell in each group (per replicate basically)
#                    n = n()) %>% 
#   dplyr::select(!n) %>% 
#   group_by(species, Taxa, biome, scenario, timestep) %>% # KEEP SIM BUT REMOVE REP HERE
#   dplyr::summarize(mean_TNIND = mean(sum_TNIND, na.rm = TRUE))
# 
# 
# # Total number of individuals per year
# TNIND_yr <- TNIND %>% # n cells used for the calculus
#   group_by(species, Taxa, biome, scenario, timestep) %>%
#   dplyr::summarize(mean_yr = mean(mean_TNIND, na.rm = TRUE), # cell mean 
#                    sd_yr = sd(mean_TNIND, na.rm = TRUE),
#                    n = n()) %>% 
#   dplyr::select(!n) 
# 
# TNIND_per_year <- ggplot(data = TNIND_yr, aes(x = timestep, y = mean_yr, group = species)) + 
#   geom_line() + 
#   facet_wrap(scenario~ species, ncol = 2, scales="free_y") +
#   labs(y = "Total number of individuals") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))
#   geom_vline(xintercept = 5, linetype = "dotted", color = "black", size = 0.8)  # add line at time of disturbance
# 
# # saving the plot
# ggsave(plot = TNIND_per_year, file.path(dirout, paste0("totalNumberIndividuals", runname, ".tiff")),
#        bg = 'white', width = 300, height = 230, units = "mm", dpi = 1200, compression = "lzw")
# 
