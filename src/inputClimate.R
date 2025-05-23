## Name: inputClimate.R ##
## Authors: Jorinde-M. Rieger ##
## Description: Applies functions to calculate climate input data (temperature and precipitation)
## for the ssp126 and ssp585 scenarios in various years##
## Date: May 22nd 2025 ##

# Input variables -------------------------------------------
# Define input variables
#scenarios <- c("ssp126", "ssp585")
#scenario_names <- c("SSP1-RCP2.6", "SSP5-RCP8.5")
variables <- c("bio1", "bio12")

#years <- c("2011-2040", "2041-2070", "2071-2100") # original in time periods"2011-2040", "2041-2070", "2071-2100"
#yearsAdapted <- c(2030, 2050, 2100) # adapted from landuse 
#baseline_year <- "1981-2010"
#baseline_yearAdapted <- 2015 # adapted from landuse

yearsOrigin <- c("2011-2040", "2041-2070", "2071-2100") # original in time periods"2011-2040", "2041-2070", "2071-2100"
#years <- c(2030, 2050, 2100) # adapted from landuse 
baseline_yearOrigin <- "1981-2010"
#baseline_year <- 2015 # adapted from landuse

#Map time periods to adapted years
yearsMapping <- setNames(years, yearsOrigin)

# Define the file paths
basePathClim <- "~/data/data/CHELSA_gfdl-esm4_V.2.1"

# Create environmental input Data (climate) as training and prediction landscapes-------------------------------------------
# Create an empty list to store climate training Landscapes
trainingLandscapesClim <- list()
# Loop through the training landscapes
for (variable in variables) {
    # Load the raster
    raster <- load_baseline_clim(variable, baseline_yearOrigin)
    
    # Load global terrestrial extent
    land <- ne_countries(scale = "medium", returnclass = "sf")
    
    # Ensure CRS consistency
    land <- st_transform(land, crs = crs(raster))
    
    # Convert the sf to a spatial object
    land <- vect(land)
    
    # Crop and mask the raster
    raster_land <- crop_mask_raster(raster, land)
    
    trainingLandscapesClim[[variable]] <- raster_land
}
# Convert the list into a SpatRaster stack
trainingLandscapesClim <- rast(trainingLandscapesClim)

# Rename layers to match variable names
names(trainingLandscapesClim) <- variables

# Save the trainingLandscape with the adapted baseline year
output_file <- file.path(outputPathLandscapes, paste0("trainingLandscapesClim_", baseline_year, ".tif"))
writeRaster(trainingLandscapesClim, output_file, overwrite = TRUE)

# Test the rasters
print(trainingLandscapesClim)
plot(trainingLandscapesClim)

# Create an empty list to store prediction landscapes
predictionLandscapesClim <- list()

# Loop through the prediction landscapes
for (scenario in scenarios) {
  for (yearOrigin in yearsOrigin) {
    
    # Create a list for each scenario-year combination
    raster_list <- list()
    
    for (variable in variables) {
      # Load the raster
      raster <- load_scenario_clim(scenario, variable, yearOrigin)
      
      # Load the global terrestrial extent
      land <- ne_countries(scale = "medium", returnclass = "sf")
      
      # Ensure CRS consistency
      land <- st_transform(land, crs = crs(raster))
      
      # Convert the sf object to a spatial vector
      land <- vect(land)
      
      # Crop and mask the raster
      raster_land <- crop_mask_raster(raster, land)
      
      # Store the processed raster in the list
      raster_list[[variable]] <- raster_land
    }
    
    # Convert the list of rasters into a SpatRaster stack
    predictionLandscapesClim[[paste0(scenario, "_", yearOrigin)]] <- rast(raster_list)
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
    writeRaster(predictionLandscapesClim[[paste0(scenario, "_", year)]], output_file, overwrite = TRUE)
  }
}
# Print the structure of the final list
print(predictionLandscapesClim)
plot(predictionLandscapesClim[["ssp126_2100"]])

