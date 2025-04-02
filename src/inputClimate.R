## Name: inputClimate.R ##
## Authors: Jorinde-M. Rieger ##
## Description: Applies functions to calculate environmental input data (temperature and precipitation)
## for the ssp126 and ssp585 scenarios in various time periods ##
## Date: April 1st 2025 ##

# Input variables -------------------------------------------
# Define input variables
scenarios <- c("ssp126", "ssp585")
scenario_names <- c("SSP1-RCP2.6", "SSP5-RCP8.5")
variables <- c("bio1", "bio12")

years <- c("2011-2040", "2041-2070", "2071-2100") 
baseline_year <- "1981-2010"

# Define the file paths
base_path <- "~/data/data/CHELSA_gfdl-esm4_V.2.1"
output_path <- "~/data/data/CHELSA_gfdl-esm4_V.2.1/outputData"
output_folder <- "~/data/output"

# Define the target resolution (based on the landUsePercentage rasters)
target_resolution <- 0.277


# Functions - later add them to CustomFunctions.R -------------------------------------------
# Function to load rasters
load_baseline_raster <- function(variable, baseline_year){
  file_path <- file.path(base_path, paste0("CHELSA_", variable, "_", baseline_year, "_V.2.1.tif"))
  rast(file_path)}

load_raster <- function(scenario, variable, year) {
  file_path <- file.path(base_path, scenario, paste0("CHELSA_", variable, "_", year, "_gfdl-esm4_", scenario, "_V.2.1.tif"))
  rast(file_path)}

# Function to aggregate rasters
aggregate_raster <- function(raster, aggregation_factor) {
  aggregate(raster, aggregation_factor, fun = mean)
}

# Function to crop and mask rasters
crop_mask_raster <- function(raster, land) {
  mask(crop(raster, land), land)
}

# Create environmental input Data (climate) as training and prediction landscapes-------------------------------------------

# Create an empty list to store training Landscapes
trainingLandscapes <- list()
# Loop through the training landscapes
for (variable in variables) {
    # Load the raster
    raster <- load_baseline_raster(variable, baseline_year)
    
    # Get the original resolution from the raster
    original_resolution <- res(raster)[1]
    
    # Calculate the aggregation factor
    aggregation_factor <- target_resolution / original_resolution
    
    # Aggregate the raster
    raster_agg <- aggregate_raster(raster, aggregation_factor)
    
    # Load terrestrial extent
    land <- ne_countries(scale = "medium", returnclass = "sf")
    
    # Ensure CRS consistency
    land <- st_transform(land, crs = crs(raster_agg))
    
    # Convert the sf to a spatial object
    land <- vect(land)
    
    # Crop and mask the raster
    raster_land <- crop_mask_raster(raster_agg, land)
    
    trainingLandscapes[[variable]] <- raster_land
}
# Convert the list into a SpatRaster stack
trainingLandscapes <- rast(trainingLandscapes)

# Rename layers to match variable names
names(trainingLandscapes) <- variables

# Test the rasters
print(trainingLandscapes)
plot(trainingLandscapes)

# Create an empty list to store prediction landscapes
predictionLandscapes <- list()

# Loop through the prediction landscapes
for (scenario in scenarios) {
  for (year in years) {
    
    # Create a list for each scenario-year combination
    raster_list <- list()
    
    for (variable in variables) {
      # Load the raster
      raster <- load_raster(scenario, variable, year)
      
      # Get the original resolution from the raster
      original_resolution <- res(raster)[1]
      
      # Calculate the aggregation factor
      aggregation_factor <- target_resolution / original_resolution
      
      # Aggregate the raster
      raster_agg <- aggregate_raster(raster, aggregation_factor)
      
      # Load the terrestrial extent
      land <- ne_countries(scale = "medium", returnclass = "sf")
      
      # Ensure CRS consistency
      land <- st_transform(land, crs = crs(raster_agg))
      
      # Convert the sf object to a spatial vector
      land <- vect(land)
      
      # Crop and mask the raster
      raster_land <- crop_mask_raster(raster_agg, land)
      
      # Store the processed raster in the list
      raster_list[[variable]] <- raster_land
    }
    
    # Convert the list of rasters into a SpatRaster stack
    predictionLandscapes[[paste0(scenario, "_", year)]] <- rast(raster_list)
  }
}

# Rename raster layers within each stack to match variable names
for (i in seq_along(predictionLandscapes)) {
  names(predictionLandscapes[[i]]) <- variables
}

# Print the structure of the final list
print(predictionLandscapes)
plot(predictionLandscapes[["ssp126_2071-2100"]])

# Combined raster with all envrionmental variables
#landscapes <- list()
#for (name in names(predictionLandscapes)) {
#  landscapes[[name]] <- c(trainingLandscapes, predictionLandscapes[[name]])
#}
#landscapes <- rast(landscapes)

#print(landscapes)  # List of combined raster stacks
