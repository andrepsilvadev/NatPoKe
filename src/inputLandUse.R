## Name: LandUseChange.R ##
## Authors: Jorinde-M. Rieger ##
## Description: Applies functions to calculate percentage changes over time and spatial explicit changes for a given Biome
## for the ssp126 and ssp585 scenarios in various years ##
## Date: May 4th 2025 ##

# Settings & libraries -------------------------------------------
source("~/NatPoKe9/src/libraries.R") # libraries
source("~/NatPoKe9/src/customFunctions2.R") # functions

# Input variables -------------------------------------------
# Define input variables
scenarios <- c("rcp26_ssp1", "rcp85_ssp5")
scenario_names <- c("SSP1-RCP2.6", "SSP5-RCP8.5")

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
years <- c(2021, 2030, 2050, 2070, 2100)

# Define the baseline year
baseline_year <- 2015

# Define the file paths
base_path <- "~/data/data/stitched_lulc_esa_scenarios"
output_path <- "~/data/data/stitched_lulc_esa_scenarios/outputData"
output_folder <- "~/data/output"

# Define the biome and continents
# Tropical Biome
biome_name <- "Tropical & Subtropical Moist Broadleaf Forests"
biome_name_short <- "Tropical Biome"
continent_names <- c("Central & South America", "Africa", "Asia")
continent_title <- c("Central & South America", "Africa", "Asia")

# Boreal Biome
biome_name <- "Boreal Forests/Taiga"
biome_name_short <- "Boreal Biome"
continent_names <- c("North America", "Europe")
continent_title <- c("North America", "Europe & Asia")

# Prepare the climate scenarios rasters for further calculations and graphical representation -------------------------------------------
# Process baseline year
baseline_raster <- load_baseline_landUse(baseline_year)

# Aggregate the baseline raster
baseline_raster_agg <- aggregate(baseline_raster, fact = 10, fun = mean)

# Load the selected biome, ensure CRS consistency, converst to spatial object
biome_sf <- load_select_biome(biome_name)
biome_sf <- st_transform(biome_sf, crs = crs(baseline_raster_agg))
biome_sp <- vect(biome_sf)

# Crop and mask the baseline raster
baseline_raster_biome <- crop_mask_raster(baseline_raster_agg, biome_sp)

# Apply land-use type mapping
mapped_baseline<- terra::app(x = baseline_raster_biome, fun = map_values_to_landUse)

# Save the mapped baseline raster
output_file <- file.path(output_path, paste0("Mapped_LandUseChange_baseline_", baseline_year, "_", biome_name_short, ".tif"))
writeRaster(mapped_baseline, output_file, overwrite = TRUE)
assign(paste0("Mapped_LandUseChange_baseline_", baseline_year, "_", biome_name_short), mapped_baseline, envir = .GlobalEnv)

# Loop through the scenarios and years for the biome
for (scenario in scenarios) {
  for (year in years) {
    # Load the raster
    raster <- load_scenario_landUse(scenario, year)
    
    # Aggregate the raster
    raster_agg <- aggregate(raster, fact = 10, fun = mean)
    
    # Crop and mask the raster
    raster_biome <- crop_mask_raster(raster_agg, biome_sp)
    
    # Save the aggregated raster
    output_file <- file.path(output_path, paste0("LandUseChange_", scenario, "_", year, "_agg.tif"))
    writeRaster(raster_biome, output_file, overwrite = TRUE)
    
    # Assign the raster to name
    assign(paste0("LandUseChange_", scenario, "_", year, "_", biome_name_short), raster_biome)
  }
}

# Loop through the years to create raster stacks and map land-use types
for (year in years) {
  stack_rasters(year)
  # Load the raster stack for the year
  raster_stack <- get(paste0("LandUseChange_scenarioStack_", year, "_", gsub(" ", "_", biome_name_short)))
  
  # Apply land-use type mapping
  mapped_scenarios <- terra::app(x = raster_stack, fun = map_values_to_landUse)
  
  # Save the mapped raster stack to disk
  mapped_output_file <- file.path(output_path, paste0("Mapped_LandUseChange_scenarioStack_", year, "_", biome_name_short, ".tif"))
  writeRaster(mapped_scenarios, mapped_output_file, overwrite = TRUE)
  
  # Save the mapped raster stack back to the environment
  assign(paste0("Mapped_LandUseChange_scenarioStack_", year, "_", gsub(" ", "_", biome_name_short)), mapped_scenarios, envir = .GlobalEnv)
}
#Example on how to access Land Use Change Stack
LandUseChange_scenarioStack_2021_Tropical_Biome


# Calculate spatially explicit Land Use Change -------------------------------------------
# Load the mapped raster stack for the baseline year
baseline_year_raster <- load_mapped_baseline_landUse(baseline_year)

# Load the mapped raster stacks for the target years
target_year_rasters_list <- list()
for (year in years) {
  target_year_rasters_list[[as.character(year)]] <- load_mapped_rasters_landUse(year)
}

# Load the selected biome, ensure CRS consistency, converst to spatial object
biome_sf <- load_select_biome(biome_name)
biome_sf <- st_transform(biome_sf, crs = crs(baseline_year_raster))
biome_sp <- vect(biome_sf)

# Apply calculateRasterClass to the baseline raster to create raster classes for the land use types
baseline_year_raster_classified <- calculateRasterClass(
  OriginalRaster = baseline_year_raster,  # Already aggregated and mapped raster
  extent = biome_sp
)

# Save the processed baseline raster
output_file <- file.path(output_path, paste0("LandUseChange_baseline_", baseline_year,"_", biome_name_short, "_classified.tif"))
writeRaster(baseline_year_raster_classified, output_file, overwrite = TRUE)

# Apply calculateRasterClass to the target year rasters
# Loop through the years to process each layer (scenario) in the target_year_rasters_list
for (year in names(target_year_rasters_list)) {
  # Get the raster for the year
  target_raster <- target_year_rasters_list[[year]]
  
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
      extent = biome_sp
    )
    
    # Save the processed raster
    output_file <- file.path(output_path, paste0("LandUseChange_", scenario, "_", year,"_", biome_name_short, "_classified.tif"))
    writeRaster(scenario_raster_classified, output_file, overwrite = TRUE)
    
    # Store the processed raster in the list
    processed_scenario_rasters[[scenario]] <- scenario_raster_classified
  }
  
  # Update the target_year_rasters_list with the processed rasters
  target_year_rasters_list[[year]] <- processed_scenario_rasters
}

# Replace land use numbers with names
baseline_year_raster_classified <- replace_numbers_with_names(baseline_year_raster_classified, LULC_Types, LULC_Types_names)
target_year_rasters_list <- replace_numbers_with_names_nested(target_year_rasters_list, LULC_Types, LULC_Types_names)

# Load and select the continents
continents <- load_select_continents(continent_names)

# Transform continent CRS to match the raster CRS
continents <- st_transform(continents, crs = st_crs(biome_sf))

# Define continent geometries
continent_geoms <- setNames(lapply(continent_names, function(continent) {
  continents %>% dplyr::filter(continent == !!continent)
}), continent_names)

# Intersect the biome with the continents
biome_continents <- intersect_biome_with_continents(biome_sf, continent_geoms)

# Calculate percentage changes
percentage_change_rasters_list <- calculate_percentage_changes(
  base_year_raster = baseline_year_raster_classified,
  target_year_rasters_list = target_year_rasters_list,
  years = as.character(years)
)

# Create spatially explicit Land Use Change Maps -------------------------------------------
# Crop and mask the percentage change rasters to the desired continents
cropped_rasters <- list()
for (year in names(percentage_change_rasters_list)) {
  cropped_rasters[[year]] <- list()
  
  for (scenario in names(percentage_change_rasters_list[[year]])) {
    cropped_rasters[[year]][[scenario]] <- list()
    
    for (class in names(percentage_change_rasters_list[[year]][[scenario]])) {
      percentage_change_raster <- percentage_change_rasters_list[[year]][[scenario]][[class]]
      
      # Crop and mask to continents
      cropped_rasters[[year]][[scenario]][[class]] <- lapply(continent_geoms, function(continent_geom) {
        crop_mask_continent(percentage_change_raster, continent_geom)
      })
    }
  }
}


