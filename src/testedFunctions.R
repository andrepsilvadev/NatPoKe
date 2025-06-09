## Name: testedFunctions.R ##
## Authors: Jorinde-M. Rieger ##
## Description: tests functions and classifications ##
## Date: June 4th 2025 ##

# Testing classification of land-use classes -------------------------------------------
library(terra)

# Create a small raster with a 4x4 grid and specific values
test_raster <- rast(nrows = 4, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 4)
values(test_raster) <- c(
  190, 10, 11, 12,  # Urban, Cropland
  130, 50, 60, 61,  # Pasture/Grassland, Forest
  200, 210, 220, 40, # Barren, Water, Other, Cropland
  151, 160, 170, 180 # Forest, Forest, Forest, Non-forest vegetation
)
# To reclassify LandUse classes create a matrix that ranges from and to to define intervals
value_to_landUse <- c(
  190, 1,  # Urban
  10, 2, # Cropland
  11, 2,
  12, 2,
  20, 2,
  30, 2,
  40, 2,
  130, 3,  # Pasture/Grassland
  50, 4, # Forest
  60, 4,
  61, 4,
  62, 4, 
  70, 4, 
  71, 4,
  72, 4, 
  80, 4,
  81, 4, 
  82, 4,
  90, 4, 
  100, 4,
  151, 4,
  160, 4,
  170, 4,
  110, 5, # Non-forest vegetation
  120, 5,
  121, 5,
  122, 5, 
  140, 5,
  150, 5,
  152, 5,
  153, 5,
  180, 5,
  210, 6, # Water
  200, 7, # Barren or Other
  201, 7,
  202, 7,
  220, 7)

# Convert the reclassification rules into a matrix
landUsematrix <- matrix(value_to_landUse, ncol = 2, byrow = TRUE)

# Apply the reclassification
mapped_raster_test <- terra::classify(test_raster, landUsematrix, include.lowest = TRUE)

# Plot the original and reclassified raster
plot(test_raster, main = "Original Raster")
plot(mapped_raster_test, main = "Reclassified Raster")


# Testing calculateRasterClass function -------------------------------------------
# Create a simple raster with 4x4 grid cells and land-use classes (1 to 3)
test_raster <- rast(nrows = 4, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 4)
values(test_raster) <- c(
  1, 1, 2, 2,
  1, 3, 3, 2,
  2, 3, 1, 1,
  3, 2, 2, 1
)

# Define a simple extent (e.g., a subset of the raster)
test_extent <- ext(0, 4, 0, 4)  # xmin, xmax, ymin, ymax

# Plot the test raster
plot(test_raster, main = "Test Raster")

target_resolution <- 2  # Aggregation to 2x2 grid cells

# Function to create binary maps and classes of land-use types
calculateRasterClass <- function(OriginalRaster, extent, target_resolution) {
  # Crop and mask the raster to the biome's boundary
  raster <- mask(crop(OriginalRaster, extent), extent)
  
  # Define the unique land-use classes and remove NAs
  land_use_classes <- terra::freq(raster)[,2] #unique(values(raster))
  land_use_classes <- na.omit(land_use_classes)
  
  # Function to create binary raster for each land-use class
  create_binary_raster <- function(raster, land_use_class) {
    binary_raster <- app(raster, fun = function(x) {
      ifelse(x == land_use_class, 1, 0)
    })
    return(binary_raster)
  }
  
  # Create a list to store binary rasters
  binary_rasters <- list()
  
  # Loop through each land-use class and create binary rasters
  for (class in land_use_classes) {
    binary_rasters[[as.character(class)]] <- create_binary_raster(raster, class)
  }
  
  # Calculate the aggregation factor based on the target resolution
  input_resolution <- res(raster)[1]  # Assuming square cells, take the resolution of the first dimension
  aggregation_factor <- round(target_resolution / input_resolution)
  
  # Aggregate each binary raster by a factor of 10
  aggregated_rasters <- list()
  for (class in names(binary_rasters)) {
    aggregated_raster <- terra::aggregate(binary_rasters[[class]], fact = aggregation_factor, fun = function(x) sum(x > 0, na.rm = TRUE)) # change aggregation faktor to 1km
    masked_raster <- mask(crop(aggregated_raster, extent), extent)
    aggregated_rasters[[class]] <- masked_raster
  }
  
  # Convert the list of rasters to a SpatRaster stack
  aggregated_rasters_stack <- rast(aggregated_rasters)
  return(aggregated_rasters_stack)
}

# Apply the function to the test raster
result_stack <- calculateRasterClass(
  OriginalRaster = test_raster,
  extent = test_extent,
  target_resolution = target_resolution
)

# Plot the resulting raster stack
plot(result_stack, main = "Binary and Aggregated Rasters")

plot(result_stack[[1]], main = "Binary Raster for Class 1")
plot(result_stack[[2]], main = "Aggregated Raster for Class 2")
plot(result_stack[[3]], main = "Aggregated Raster for Class 3")
print(res(result_stack))
