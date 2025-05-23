## Name: inputElev.R ##
## Authors: Jorinde-M. Rieger ##
## Description: Processes elevation data as input for training and prediction landscapes ##
## Date: May 23rd 2025 ##

# Input variables -------------------------------------------
# Define input variables
#scenarios <- c("ssp126", "ssp585")
#years <- c(2030, 2050, 2100) # adapted from land use
#baseline_year <- 2015 # adapted from land use

# Define the file paths
basePathElev <- "~/data/data/wc2.1_30s_elev/wc2.1_30s_elev.tif" # Path to the elevation raster

# Load the elevation raster -------------------------------------------
elevation_raster <- rast(basePathElev)

# Load global terrestrial extent
extent_sf <- ne_countries(scale = "medium", returnclass = "sf")
extent_sf <- st_transform(extent_sf, crs = crs(elevation_raster))
extent_sp <- vect(extent_sf)

# Crop and mask the elevation raster to the global terrestrial extent
elevation_raster <- crop_mask_raster(elevation_raster, extent_sp)

# Create training landscape -------------------------------------------
trainingLandscapesElev <- elevation_raster
output_file <- file.path(outputPathLandscapes, paste0("trainingLandscapesElev_", baseline_year, ".tif"))
writeRaster(trainingLandscapesElev, output_file, overwrite = TRUE)

plot(trainingLandscapesElev)

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
    writeRaster(predictionLandscapesElev[[paste0(scenario, "_", year)]], output_file, overwrite = TRUE)
  }
}

# Print the structure of the final list
#print(predictionLandscapesElev)
