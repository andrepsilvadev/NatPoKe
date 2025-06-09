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
  
  trainingLandscapesClim[[variable]] <- raster_extent
}

# Convert the list into a SpatRaster stack
trainingLandscapesClim <- terra::rast(trainingLandscapesClim)

# Rename layers to match variable names
names(trainingLandscapesClim) <- variables

# Save the trainingLandscape with the adapted baseline year
output_file <- file.path(outputPathLandscapes, paste0("trainingLandscapesClim_", baseline_year, ".tif"))
terra::writeRaster(trainingLandscapesClim, output_file, overwrite = TRUE)

# Test the rasters
plot(trainingLandscapesClim)

# Create environmental input Data (climate) as prediction landscapes-------------------------------------------
# Function to calculate the average for a given scenario, year, and variable
# to save computation time this could be saved as netCDF (terra::writeCDF)
average_climate_models <- function(scenario, yearOrigin, variable) {
  # Initialize a list to store rasters for all models
  model_rasters <- list()
  
  # Loop through the models
  for (model in models) {
    # Construct the file path for the raster
    raster_file <- file.path(basePathClim, scenario, paste0("CHELSA_", variable, "_", yearOrigin, "_", model, "_", scenario, "_V.2.1.tif"))
    
    # Check if the file exists
    if (!file.exists(raster_file)) {
      warning(paste("File not found:", raster_file))
      next
    }
    
    # Load the raster
    model_rasters[[model]] <- terra::rast(raster_file)
  }
  
  # Combine the rasters into a SpatRaster stack
  model_stack <- terra::rast(model_rasters)
  
  # Calculate the average using terra::app
  averaged_raster <- terra::app(model_stack, fun = mean, na.rm = TRUE)
  
  # Rename the layer
  names(averaged_raster) <- variable
  
  # Save the averaged raster to disk
  output_file <- file.path(outputPathLandscapes, paste0("AverageCHELSA",variable, "_", scenario, "_", yearOrigin, ".tif"))
  terra::writeRaster(averaged_raster, output_file, overwrite = TRUE)
  
  return(averaged_raster)
}

# Register parallel backend
num_cores <- min(parallel::detectCores() - 1, 10)  # Use up to 10 cores
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Create an empty list to store all processed rasters
all_rasters <- list()

# Parallelized loop using foreach
foreach(scenario = scenarios, .combine = 'c', .packages = c("terra", "sf")) %:%
  foreach(yearOrigin = yearsOrigin, .combine = 'c') %dopar% {
    # Create a list for each scenario-year combination
    raster_list <- list()
    
    for (variable in variables) {
      # Load averaged raster of climate models
      raster <- average_climate_models(scenario, yearOrigin, variable)
      
      # Store the processed raster in the list
      raster_list[[variable]] <- raster
    }
    
    # Combine the rasters for this scenario and year into a SpatRaster stack
    combined_raster <- terra::rast(raster_list)
    
    # Return the combined raster as a list element
    list(paste0(scenario, "_", yearOrigin) = combined_raster)
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
      raster <- load_average_scenario_clim(scenario, yearOrigin, variable)
      
      # Extent
      #extent_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
      
      # Ensure CRS consistency
      #extent_crs <- sf::st_transform(extent_sf, crs = crs(raster))
      
      # Convert the sf to a spatial object
      #extent_sp <- terra::vect(extent_crs)
      
      # Crop and mask the raster
      raster_extent <- crop_mask_raster(raster, extent_sp)
      
      # Store the processed raster in the list
      raster_list[[variable]] <- raster_extent
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
    output_file <- file.path(outputPathLandscapes, paste0("predictionLandscapesClim_", scenario, "_", year, ".tif"))
    terra::writeRaster(predictionLandscapesClim[[paste0(scenario, "_", yearOrigin)]], output_file, overwrite = TRUE)
  }
}

# Print the structure of the final list
#print(predictionLandscapesClim)
#plot(predictionLandscapesClim[["ssp126_2071-2100"]])
