## Name: inputClimate.R ##
## Authors: Jorinde-M. Rieger ##
## Description: Applies functions to calculate climate input data (temperature and precipitation)
## for the ssp126 and ssp585 scenarios in various years##
## Date: June 9th 2025 ##

# Input variables -------------------------------------------
yearsOrigin <- c("2011-2040", "2041-2070", "2071-2100") # original in time periods"2011-2040", "2041-2070", "2071-2100"
baseline_yearOrigin <- "1981-2010"

# Climate Models (GCMs)
models <- c("gfdl-esm4", "ipsl-cm6a-lr", "mpi-esm1-2-hr", "mri-esm2-0", "ukesm1-0-ll")

# Map time periods to adapted years
yearsMapping <- setNames(years, yearsOrigin)

# Define the file paths
basePathClim <- "~/data/data/CHELSA_gfdl-esm4_V.2.1"

# Create environmental input Data (climate) as training landscapes-------------------------------------------
# Create an empty list to store climate training Landscapes
trainingLandscapesClim <- list()
# Loop through the training landscapes
for (variable in variables) {
  # Load the raster
  raster <- load_baseline_clim(variable, baseline_yearOrigin)
  
  # Ensure CRS consistency
  extent_crs <- sf::st_transform(extent_sf, crs = crs(raster))
  
  # Convert the sf to a spatial object
  extent_sp <- terra::vect(extent_crs)
  
  # Crop and mask the raster
  raster_extent <- crop_mask_raster(raster, extent_sp)
  
  # Aggregate to target resolution
  input_resolution <- res(raster_extent)[1]  # Assuming square cells, take the resolution of the first dimension
  aggregation_factor <- round(target_resolution / input_resolution)
  raster_agg <- aggregate(raster_extent, fact = aggregation_factor, fun = mean)
  
  trainingLandscapesClim[[variable]] <- raster_agg
}

# Convert the list into a SpatRaster stack
trainingLandscapesClim <- terra::rast(trainingLandscapesClim)

# Rename layers to match variable names
names(trainingLandscapesClim) <- variables

# Save the trainingLandscape with the adapted baseline year
output_file <- file.path(outputPathLandscapes, paste0("trainingLandscapesClim_", baseline_year, "_5km.tif"))
terra::writeRaster(trainingLandscapesClim, output_file, overwrite = TRUE)

# Test the rasters
plot(trainingLandscapesClim)

# Create environmental input Data (climate) as prediction landscapes-------------------------------------------
# Register parallel backend
num_cores <- min(parallel::detectCores() - 1, 10)  # Use up to 10 cores
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Create an empty list to store all processed rasters
#all_rasters <- list()

# Create raster of averaged GCMs in a paralleled loop
foreach(scenario = scenarios, .combine = 'c', .packages = c("terra", "sf")) %:%
  foreach(yearOrigin = yearsOrigin, .combine = 'c') %dopar% {
    # Create a list for each scenario-year combination
    raster_list <- list()
    
    for (variable in variables) {
      # Load averaged raster of climate models
      raster <- average_climate_models(outputPathLandscapes, scenario, yearOrigin, variable)
      
      # Store the processed raster in the list
      raster_list[[variable]] <- raster
    }
    
    # Combine the rasters for this scenario and year into a SpatRaster stack
    #combined_raster <- terra::rast(raster_list)
    
    # Return the combined raster as a list element
    #list(paste0(scenario, "_", yearOrigin) = combined_raster)
  }

# Stop the cluster
stopCluster(cl)

# Create an empty list to store prediction landscapes
predictionLandscapesClim <- list()

for (scenario in scenarios) {
  for (yearOrigin in yearsOrigin) {
    
    # Create a list for each scenario-year combination
    raster_list <- list()
    
    for (variable in variables) {
      # Load averaged raster of climate models
      raster <- load_average_scenario_clim(outputPathLandscapes, scenario, yearOrigin, variable)
      
      # Extent
      #extent_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
      
      # Ensure CRS consistency
      #extent_crs <- sf::st_transform(extent_sf, crs = crs(raster))
      
      # Convert the sf to a spatial object
      #extent_sp <- terra::vect(extent_crs)
      
      # Crop and mask the raster
      raster_extent <- crop_mask_raster(raster, extent_sp)
      
      # Aggregate to target resolution
      input_resolution <- res(raster_extent)[1]  # Assuming square cells, take the resolution of the first dimension
      aggregation_factor <- round(target_resolution / input_resolution)
      raster_agg <- aggregate(raster_extent, fact = aggregation_factor, fun = mean)
      
      # Store the processed raster in the list
      raster_list[[variable]] <- raster_agg
    }
    
    # Convert the list of rasters into a SpatRaster stack
    predictionLandscapesClim[[paste0(scenario, "_", yearOrigin)]] <- terra::rast(raster_list)
  }
}

# Rename raster layers within each stack to match variable names
for (i in seq_along(predictionLandscapesClim)) {
  names(predictionLandscapesClim[[i]]) <- variables
}

# Save the climate prediction landscapes  with adapted years
for (scenario in scenarios) {
  for (yearOrigin in yearsOrigin) {
    # Get the adapted year
    year <- yearsMapping[yearOrigin]
    
    # Save the prediction landscape
    output_file <- file.path(outputPathLandscapes, paste0("predictionLandscapesClim_", scenario, "_", year, "_5km.tif"))
    terra::writeRaster(predictionLandscapesClim[[paste0(scenario, "_", yearOrigin)]], output_file, overwrite = TRUE)
  }
}

# Remove all data from memory/global Environment
rm()
gc()
# Print the structure of the final list
#print(predictionLandscapesClim)
#plot(predictionLandscapesClim[["ssp126_2071-2100"]])
