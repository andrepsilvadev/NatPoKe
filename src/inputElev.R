## Name: inputElev.R ##
## Authors: Jorinde-M. Rieger ##
## Description: Processes elevation data as input for training and prediction landscapes ##
## Date: May 23rd 2025 ##

# Input variables -------------------------------------------
# Define the file paths
basePathElev <- "~/data/data/wc2.1_30s_elev/wc2.1_30s_elev.tif" # Path to the elevation raster

# Load the elevation raster -------------------------------------------
elevation_raster <- terra::rast(basePathElev)

# Crop and mask the elevation raster to the global terrestrial extent
# Ensure CRS consistency
extent_crs <- sf::st_transform(extent_sf, crs = crs(elevation_raster))
extent_sp <- terra::vect(extent_crs)
elevation_raster <- crop_mask_raster(elevation_raster, extent_sp) # extent_sp defined in inputClimate.R
names(elevation_raster) <- "Elevation"

# Create training landscape -------------------------------------------
trainingLandscapesElev <- elevation_raster
output_file <- file.path(outputPathLandscapes, paste0("trainingLandscapesElev_", baseline_year, ".tif"))
terra::writeRaster(trainingLandscapesElev, output_file, overwrite = TRUE)

#Example plot
#plot(trainingLandscapesElev)

# Create prediction landscapes -------------------------------------------
# Create an empty list to store prediction landscapes
predictionLandscapesElev <- list()

# Loop through scenarios and years
for (scenario in scenarios) {
  for (year in years) {
    # Use the same elevation raster for all scenarios and years
    predictionLandscapesElev[[paste0(scenario, "_", year)]] <- elevation_raster
    
    # Save the elevation raster for this scenario and year
    output_file <- file.path(outputPathLandscapes, paste0("predictionLandscapesElev_", scenario, "_", year, ".tif"))
    terra::writeRaster(predictionLandscapesElev[[paste0(scenario, "_", year)]], output_file, overwrite = TRUE)
  }
}

# Remove all data from memory/global Environment
rm()
gc()
# Print the structure of the final list
#print(predictionLandscapesElev)