########################################
# SAVING metaRange OUTPUT RASTER FILES #
########################################
# MIS
# 28 Jan 25



# create empty list
results <- list()

time_steps <- c(1:12)
species_names <- c("Alcesalces", "Ursusarctos", "Lynxlynx")

# loop through species and process ONE raster at a time
for (sp in species_names) {
  
  # find raster files for the current species
  flist <- list.files(here("example/mammals_try2/results_28Jan_StefanLandscape"), 
                      pattern = paste0(sp, "_abundance.tif"), full.names = TRUE)
  
  # check if any files were found; if not, skip to the next
  if (length(flist) == 0) {
    message(paste("No rasters found for:", sp, "- Someone should check if this is a MISTAKE!"))
    next
  }
  
  # loop through the raster files for the current species
  for (raster in flist) {
    
    # read raster 
    r <- terra::rast(raster)
    
    # retrieve file name and split it
    filename <- basename(raster)
    filename_parts <- str_split(file_path_sans_ext(filename), "_")[[1]]
    
    # convert raster to a data frame with coordinates and values
    raster_data <- terra::as.data.frame(r, xy = TRUE, na.rm = TRUE, row.names = FALSE) 
    
    # add information for easier identification of each raster (sp, timestpe, scenario, etc..)
    raster_data$scenario <- filename_parts[1]  # BAU
    raster_data$biome <- filename_parts[2]     # Tropical
    raster_data$region <- filename_parts[3]    # Asia
    raster_data$timestep <- filename_parts[4] 
    raster_data$species <- sp                  # use current species name
    names(raster_data)[names(raster_data) == "lyr1"] <- filename_parts[6]
    
    # store result in a list, then append by species
    if (!is.null(results[[sp]])) {
      results[[sp]] <- rbind(results[[sp]], raster_data)
    } else {
      results[[sp]] <- raster_data
    }
  }
}

# combine all species results into one data frame
final_results <- do.call(rbind, results)
#View(final_results)






#####################################
# CHECKING IF ITS WORKING CORRECTLY #
#####################################

flist <- here("example/mammals_try2/results_28Jan_StefanLandscape/BAU_Tropical_Asia_009_Alcesalces_abundance.tif")
# read in the raster files
r <- terra::rast("~/NatPoKe/example/mammals_try2/results_28Jan_StefanLandscape/BAU_Tropical_Asia_009_Alcesalces_abundance.tif")
r2<-terra::rast("~/NatPoKe/example/mammals_try2/results_sensitivityRuns/SR095_reproductionRate_BAU_Tropical_Asia_001_Alcesalces_abundance.tif")

# retrieve file name and split it
filename <- basename(flist)
filename_parts <- str_split(filename, "_")[[1]]

# convert raster to a data frame with coordinates and values
raster_data <- terra::as.data.frame(r, xy = TRUE, na.rm = TRUE)

# add species and time step information for easier identification
raster_data$species <- sp
raster_data$time <- rep(time_steps, each = nrow(raster_data) / length(time_steps))
raster_data$scenario <- filename_parts[1]  # for e.g BAU
raster_data$biome <- filename_parts[2]    # for e.g Tropical
raster_data$region <- filename_parts[3] # for e.g Asia
raster_data$timestep <- filename_parts[4]

raster_data

melt(dt,
     measure.vars = measure (
       value.name, y, sep="_"))