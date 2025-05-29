## Name: inputLandUse.R ##
## Authors: Jorinde-M. Rieger ##
## Description: Applies functions to calculate land-use input data for the ssp126 and ssp585 scenarios in various years##
## Date: May 29th 2025 ##

# Input variables -------------------------------------------
# Define the file paths
basePathLandUse <- "~/data/data/stitched_lulc_esa_scenarios"

# Simplify and define ESA LULC types (39) to the 7 (SEALS) LULC types
# ESA LULC simplification scheme based on  Johnson, J. A., & Thakrar, S., (2024)
# (Code availability: https://github.com/jandrewjohnson/seals)
LULC_Types <- 1:7
LULC_Types_names <- c(
  "Urban",
  "Cropland",
  "Pasture/Grassland",
  "Forest",
  "Non-forest vegetation",
  "Water",
  "Barren or other"
)

# Create land-use input raster for the trainingLandscape -------------------------------------------
# Process baseline year
baseline_raster <- load_baseline_landUse(baseline_year)

# Crop and mask baseline raster to biome
baseline_raster_extent <- crop_mask_raster(baseline_raster, extent_sp)

# Apply land-use type mapping
mapped_baseline<- terra::app(x = baseline_raster_extent, fun = map_values_to_landUse) # takes approx. ?
plot(mapped_baseline)

# Save the mapped baseline raster
output_file <- file.path(outputPathLandscapes, paste0("MappedLandUse_base_", baseline_year, "_", gsub(" ", "_", extent), ".tif"))
terra::writeRaster(mapped_baseline, output_file, overwrite = TRUE)
assign(paste0("MappedLandUse_base_", baseline_year, "_", gsub(" ", "_", extent)), mapped_baseline, envir = .GlobalEnv)

# Load the mapped raster stack for the baseline year
mapped_baseline <- load_mapped_baseline_landUse(baseline_year)

# Apply calculateRasterClass to the baseline raster to create raster classes for the land use types
trainingLandscapesLandUse <- calculateRasterClass(
  OriginalRaster = mapped_baseline,
  extent = extent_sp, # defined in inputClimate.R
  target_resolution = target_resolution
)

# Replace land use numbers with names
trainingLandscapesLandUse <- replace_numbers_with_names(trainingLandscapesLandUse, LULC_Types, LULC_Types_names)

# Save the processed baseline raster
output_file <- file.path(outputPathLandscapes, paste0("trainingLandscapesLandUse_", baseline_year, ".tif")) # add extent name if needed "gsub(" ", "_", extent)"
terra::writeRaster(trainingLandscapesLandUse, output_file, overwrite = TRUE)

#plot(trainingLandscapesLandUse)

# Create land-use input raster for the predictionLandscape -------------------------------------------
# Loop through the scenarios and years crop to the biome
for (scenario_des in scenarios_des) {
  for (year in years) {
    # Load the raster
    raster <- load_scenario_landUse(scenario_des, year)
    
    # Crop and mask the raster
    raster_extent <- crop_mask_raster(raster, extent_sp)
    
    # Map the scenario name
    scenario <- scenario_name_mapping[scenario_des]
    
    # Save the aggregated raster
    output_file <- file.path(outputPathLandscapes, paste0("LandUse_", scenario, "_", year, "_", gsub(" ", "_", extent), ".tif"))
    terra::writeRaster(raster_extent, output_file, overwrite = TRUE)
    
    # Assign the raster to name
    assign(paste0("LandUse_", scenario, "_", year, "_", gsub(" ", "_", extent)), raster_extent)
  }
}

library(foreach)
library(doParallel)
# Register parallel backend
num_cores <- min(parallel::detectCores() - 1, 10)  # use up to 10 cores
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Loop through the years to create raster stacks and map land-use types
mapped_rasters <- foreach(year = years, .combine = 'c', .packages = c("terra", "sf")) %dopar% {
  raster_stack <- stack_rasters(year, scenarios, extent, outputPathLandscapes)
  
  # Apply land-use type mapping
  mapped_scenarios <- terra::app(x = raster_stack, fun = map_values_to_landUse)
  
  # Save the mapped raster stack to disk
  output_file <- file.path(outputPathLandscapes, paste0("MappedLandUse_scenarios_", year, "_", gsub(" ", "_", extent), ".tif"))
  writeRaster(mapped_scenarios, output_file, overwrite = TRUE)
  
  # Return the mapped raster stack
  mapped_scenarios
}
stopCluster(cl)

# Load the mapped raster stacks for the target years
LULC_scenarios_list <- list()
for (year in years) {
  LULC_scenarios_list[[as.character(year)]] <- load_mapped_landUse(year)
}

#plot(LULC_scenarios_list$`2100`$ssp126_2100)

# Apply calculateRasterClass to the target year rasters
# Loop through the years to process each layer (scenario)
for (year in names(LULC_scenarios_list)) {
  # Get the raster for the year
  target_raster <- LULC_scenarios_list[[year]]
  
  # Ensure the raster has the same number of layers as the scenarios
  if (nlyr(target_raster) != length(scenarios)) {
    stop(paste("The raster for year", year, "does not have the same number of layers as the scenarios."))
  }
  
  # Dynamically assign scenario names to the raster layers
  scenario_rasters <- setNames(
    lapply(seq_along(scenarios), function(i) target_raster[[i]]),
    scenarios
  )
  
  # Create a list to store the processed rasters for this year
  processed_scenario_rasters <- list()
  
  # Process each scenario separately
  for (scenario in names(scenario_rasters)) {
    # Get the raster for the scenario
    scenario_raster <- scenario_rasters[[scenario]]
    
    # Apply calculateRasterClass to classify the raster
    scenario_raster_classified <- calculateRasterClass(
      OriginalRaster = scenario_raster,  # Single-layer raster
      extent = extent_sp,
      target_resolution = target_resolution
    )
    
    # Save the processed raster
    output_file <- file.path(outputPathLandscapes, paste0("LandUseClass_", scenario, "_", year,"_", gsub(" ", "_", extent), ".tif"))
    terra::writeRaster(scenario_raster_classified, output_file, overwrite = TRUE)
    
    # Store the processed raster in the list
    processed_scenario_rasters[[scenario]] <- scenario_raster_classified
  }
  
  # Update the LULC_scenarios_list with the processed rasters
  LULC_scenarios_list[[year]] <- processed_scenario_rasters
}

# Load land-use classes
predictionLandscapesLandUse <- list()
# Loop through scenarios and years to load rasters
for (scenario in scenarios) {
  for (year in years) {
    # Dynamically construct the file path
    input_file <- file.path(outputPathLandscapes, paste0("LandUseClass_", scenario, "_", year, "_", gsub(" ", "_", extent), ".tif"))
    
    # Load the raster
    loaded_raster <- terra::rast(input_file)
    loaded_raster <- replace_numbers_with_names(loaded_raster, LULC_Types, LULC_Types_names)
    
    # Save the processed raster
    output_file <- file.path(outputPathLandscapes, paste0("predictionLandscapesLandUse_", scenario, "_", year, ".tif"))
    terra::writeRaster(loaded_raster, output_file, overwrite = TRUE)
    
    # Store the raster in the list
    predictionLandscapesLandUse[[paste0(scenario, "_", year)]] <- loaded_raster
  }
}

# Example plot
#plot(predictionLandscapesLandUse$ssp126_2100)

