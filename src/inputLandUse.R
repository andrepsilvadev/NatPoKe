## Name: inputLandUse.R ##
## Authors: Jorinde-M. Rieger ##
## Description: Applies functions to calculate land-use input data for the ssp126 and ssp585 scenarios in various years##
## Date: May 23rd 2025 ##

# Input variables -------------------------------------------
# Define input variables
#scenarios <- c("ssp126", "ssp585")
#scenario_names <- c("SSP1-RCP2.6", "SSP5-RCP8.5")

# Define years
#years <- c(2030, 2050, 2100)
#baseline_year <- 2015

# Define the target resolution
#target_resolution <- 0.008333333 # 1km resolution

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

# Define the biome and continents
# Tropical Biome
#biome_name <- "Tropical & Subtropical Moist Broadleaf Forests"
#extent <- "Tropical Biome"

# Boreal Biome
#biome_name <- "Boreal Forests/Taiga"
#extent <- "Boreal Biome"

# Create land-use input raster for the trainingLandscape -------------------------------------------
# Process baseline year
baseline_raster <- load_baseline_landUse(baseline_year)
#plot(baseline_raster)

# Load extent global OR biome
extent <- "Global"
extent_sf <- ne_countries(scale = "medium", returnclass = "sf") # OR load_select_biome(biome_name)
extent_sf <- st_transform(extent_sf, crs = crs(baseline_raster))
extent_sp <- vect(extent_sf)

# Crop and mask baseline raster to biome
baseline_raster_extent <- crop_mask_raster(baseline_raster, extent_sp)

# Apply land-use type mapping
mapped_baseline<- terra::app(x = baseline_raster_extent, fun = map_values_to_landUse) # takes approx. 2h for one biome
plot(mapped_baseline)

# Save the mapped baseline raster
output_file <- file.path(outputPathLandscapes, paste0("MappedLandUse_base_", baseline_year, "_", gsub(" ", "_", extent), ".tif"))
writeRaster(mapped_baseline, output_file, overwrite = TRUE)
assign(paste0("MappedLandUse_base_", baseline_year, "_", gsub(" ", "_", extent)), mapped_baseline, envir = .GlobalEnv)

# Load the mapped raster stack for the baseline year
#baseline_year_raster <- load_mapped_baseline_landUse(baseline_year)

# Apply calculateRasterClass to the baseline raster to create raster classes for the land use types
trainingLandscapesLandUse <- calculateRasterClass(
  OriginalRaster = mapped_baseline,
  extent = extent_sp,
  target_resolution = target_resolution
)

# Replace land use numbers with names
trainingLandscapesLandUse <- replace_numbers_with_names(baseline_LULC, LULC_Types, LULC_Types_names)

# Save the processed baseline raster
output_file <- file.path(outputPathLandscapes, paste0("trainingLandscapesLandUse_", baseline_year, ".tif")) # add extent name if needed "gsub(" ", "_", extent)"
writeRaster(trainingLandscapesLandUse, output_file, overwrite = TRUE)

#plot(trainingLandscapesLandUse)

# Create land-use input raster for the predictionLandscape -------------------------------------------
# Loop through the scenarios and years crop to the biome
for (scenario in scenarios) {
  for (year in years) {
    # Load the raster
    raster <- load_scenario_landUse(scenario, year)
    
    # Crop and mask the raster
    raster_extent <- crop_mask_raster(raster, extent_sp)
    
    # Save the aggregated raster
    output_file <- file.path(outputPathLandscapes, paste0("LandUse_", scenario, "_", year, "_", gsub(" ", "_", extent), ".tif"))
    writeRaster(raster_extent, output_file, overwrite = TRUE)
    
    # Assign the raster to name
    assign(paste0("LandUse_", scenario, "_", year, "_", gsub(" ", "_", extent)), raster_extent)
  }
}

# Loop through the years to create raster stacks and map land-use types
for (year in years) {
  stack_rasters(year)
  # Load the raster stack for the year
  raster_stack <- get(paste0("LandUse_scenarioStack_", year, "_", gsub(" ", "_", extent)))
  
  # Apply land-use type mapping
  mapped_scenarios <- terra::app(x = raster_stack, fun = map_values_to_landUse)
  
  # Save the mapped raster stack to disk
  output_file <- file.path(outputPathLandscapes, paste0("MappedLandUse_scenarios_", year, "_", gsub(" ", "_", extent), ".tif"))
  writeRaster(mapped_scenarios, output_file, overwrite = TRUE)
  
  # Save the mapped raster stack back to the environment
  assign(paste0("MappedLandUse_scenarios_", year, "_", gsub(" ", "_", extent)), mapped_scenarios, envir = .GlobalEnv)
}

# Access mapped land-use scenario stacks
#MappedLandUse_scenarios_2021_Boreal_Biome


# Load the mapped raster stacks for the target years
LULC_scenarios_list <- list()
for (year in years) {
  LULC_scenarios_list[[as.character(year)]] <- load_mapped_landUse(year)
}

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
    writeRaster(scenario_raster_classified, output_file, overwrite = TRUE)
    
    # Store the processed raster in the list
    processed_scenario_rasters[[scenario]] <- scenario_raster_classified
  }
  
  # Update the LULC_scenarios_list with the processed rasters
  LULC_scenarios_list[[year]] <- processed_scenario_rasters
}

# Replace land use numbers with names
LULC_scenarios_list <- replace_numbers_with_names_nested(LULC_scenarios_list, LULC_Types, LULC_Types_names)

# Create a list to store the formatted prediction landscapes
predictionLandscapesLandUse <- list()
# Loop through each year
for (year in names(LULC_scenarios_list)) {
  # Loop through each scenario
  for (scenario in names(LULC_scenarios_list[[year]])) {
    # Get the classified raster for the scenario and year
    scenario_raster <- LULC_scenarios_list[[year]][[scenario]]
    
    # Combine the layers into a single SpatRaster stack
    combined_raster <- rast(scenario_raster)
    
    # Rename the layers to match the land-use class names
    names(combined_raster) <- LULC_Types_names
    
    # Store the combined raster in the formatted list
    predictionLandscapesLandUse[[paste0(scenario, "_", year)]] <- combined_raster
  }
}

# Save the formatted prediction landscapes
for (key in names(predictionLandscapesLandUse)) {
  # Extract the scenario and year from the key
  scenario <- strsplit(key, "_")[[1]][1]
  year <- strsplit(key, "_")[[1]][2]
  
  # Get the combined raster
  predictionLandscapesLandUse <- predictionLandscapesLandUse[[key]]
  
  # Define the output file path
  output_file <- file.path(outputPathLandscapes, paste0("predictionLandscapesLandUse_", scenario, "_", year, ".tif"))
  
  # Save the raster
  writeRaster(predictionLandscapesLandUse, output_file, overwrite = TRUE)
}

# Example plot
#plot(predictionLandscapesLandUse$ssp126$`2100`)

