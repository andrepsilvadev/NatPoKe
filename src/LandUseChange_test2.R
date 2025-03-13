## Name: LandUseChange_test2.R ##
## Authors: Jorinde-M. Rieger ##
## Description: Applies functions to calculate percentage changes over time and spatial explicit changes for a given Biome
## for the ssp126 and ssp585 scenarios in various years ##
## Date: March 13th 2025 ##

# Settings & libraries -------------------------------------------
source("~/data/src/libraries.R") # libraries
source("~/data/src/customFunctions.R") # functions

# Input variables -------------------------------------------
# Define input variables
scenarios <- c("rcp26_ssp1", "rcp85_ssp5")
scenario_names <- c("SSP1-RCP2.6", "SSP5-RCP8.5")
years <- c(2021, 2030, 2050, 2070, 2100)

# Simplify and define ESA LULC types (39) to the 7 (SEALS) LULC types
# Source of ESA LULC simplification scheme in Table S.2.4.1 of Supporting Information Appendix in Johnson et al. 2023 
# (https://www.pnas.org/doi/10.1073/pnas.2220401120#supplementary-materials)
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
value_to_land_use <- list(
  "190" = 1,  # Urban
  "10" = 2, "11" = 2, "12" = 2, "20" = 2, "30" = 2,  # Cropland
  "130" = 3,  # Pasture/Grassland
  "40" = 4, "50" = 4, "60" = 4, "61" = 4, "62" = 4, "70" = 4, "71" = 4, "72" = 4, "80" = 4, "81" = 4, "82" = 4, "90" = 4, "100" = 4,  # Forest
  "110" = 5, "120" = 5, "121" = 5, "122" = 5, "140" = 5,  # Non-forest vegetation
  "210" = 6,  # Water
  "150" = 7, "151" = 7, "152" = 7, "153" = 7, "160" = 7, "170" = 7, "180" = 7, "200" = 7, "201" = 7, "202" = 7, "210" = 7, "220" = 7  # Barren or Other
)

# Define the file paths
base_path <- "~/data/data/stitched_lulc_esa_scenarios"
output_path <- "~/data/data/stitched_lulc_esa_scenarios/outputData"
output_folder <- "~/data/output"


# Define the biome and continents
biome_name <- "Tropical & Subtropical Moist Broadleaf Forests"
biome_name_short <- "Tropical Biome"
continent_names <- c("Africa", "Asia", "South America")

# Functions - later add them to CustomFunctions.R -------------------------------------------
# Function to load rasters
load_raster <- function(scenario, year) {
  file_path <- file.path(base_path, scenario, paste0("lulc_esa_gtap1_", scenario, "_", year, "_no_policy.tif"))
  rast(file_path)
}

# Function to load and select the biome shapefile
load_select_biome <- function(biome_name) {
  biome_sf <- st_read("~/data/data/Ecoregions2017/Ecoregions2017/Ecoregions2017.shp")
  biome_sf[biome_sf$BIOME_NAME == biome_name, ]
}

# Function to crop and mask rasters
crop_mask_raster <- function(raster, biome_sp) {
  mask(crop(raster, biome_sp), biome_sp)
}

# Function to stack rasters
stack_rasters <- function(year) {
  scenarios_list <- list()
  for (scenario in scenarios) {
    raster_name <- paste0("LandUseChange_", scenario, "_", year, "_", biome_name_short)
    if (exists(raster_name)) {
      scenarios_list[[paste0("scenario_", scenario, "_", year)]] <- get(raster_name)
    }
  }
  
  # Create a raster stack from the list of scenarios
  scenarios_stack <- rast(scenarios_list)
  
  # Assign names to the raster stack layers
  names(scenarios_stack) <- names(scenarios_list)
  
  # Save the raster stack
  stack_output_file <- file.path(output_path, paste0("LandUseChange_scenarioStack_", year, "_", biome_name_short, ".tif"))
  writeRaster(scenarios_stack, stack_output_file, overwrite = TRUE)
  
  # Assign the raster stack to a variable in the environment
  assign(paste0("LandUseChange_scenarioStack_", year, "_", gsub(" ", "_", biome_name_short)), scenarios_stack, envir = .GlobalEnv)
  
  return(scenarios_stack)
}

# Function to map values to land use types
map_values_to_land_use <- function(x) {
  sapply(x, function(val) {
    if (val %in% names(value_to_land_use)) {
      return(value_to_land_use[[as.character(val)]])
    } else {
      return(NA)  # Handle values that do not map to any land-use type
    }
  })
}

# Function to process and map scenarios for each year
process_and_map_scenarios <- function(year) {
  raster_stack <- get(paste0("LandUseChange_scenarioStack_", year, "_", gsub(" ", "_", biome_name_short)))
  mapped_scenarios <- terra::app(x = raster_stack, fun = map_values_to_land_use)
  return(mapped_scenarios)
}

# Function to calculate the percentages for each land-use type
calculate_land_use_percentages <- function(raster_stack, land_use_types, land_use_names, time) {
  # Create binary maps for each land-use type
  land_use_layers <- lapply(land_use_types, function(cat) {
    terra::app(raster_stack, fun = function(x) {
      return(ifelse(x == cat, 1, 0))
    })
  })
  
  # Calculate the percentages for each land-use type without NAs
  land_use_percentages <- sapply(land_use_layers, function(layer) {
    sum(values(layer), na.rm = TRUE) / sum(!is.na(values(layer))) * 100
  })
  
  # Create a data frame with the results
  percentage_df <- data.frame(
    time = time,
    landUse = land_use_names,
    variable = land_use_names,
    value = land_use_percentages
  )
  
  return(percentage_df)
}


# Prepare the climate scenarios rasters for further calculations and graphical representation -------------------------------------------
# Load the selected biome
biome_sf <- load_select_biome(biome_name)

# Loop through the scenarios, and years for the biome
for (scenario in scenarios) {
  for (year in years) {
    # Load the raster
    raster <- load_raster(scenario, year)
    
    # Aggregate the raster
    raster_agg <-  aggregate(raster, fact = 10, fun = mean)
    
    # Ensure CRS consistency
    biome_sf <- st_transform(biome_sf, crs = crs(raster_agg[[1]]))
    
    # Convert the sf to a spatial object
    biome_sp <- vect(biome_sf)
    
    # Crop and mask the raster
    raster_biome <- crop_mask_raster(raster_agg, biome_sp)
    
    # Assign the raster to a variable in the environment
    assign(paste0("LandUseChange_", scenario, "_", year, "_", biome_name_short), raster_biome)
  }
}

# Create raster stacks for each year
for (year in years) {
  stack_rasters(year)
}

# Check raster stack layer
LandUseChange_scenarioStack_2021_Tropical_Biome

# Calculate and create Climate Change graphics over time -------------------------------------------
# Define consistent color palette for the scenarios
scenario_colors <- setNames(
  c("#1f77b4", "#ff7f0e"), scenario_names)


# Process and map scenarios for each year
mapped_scenarios_list <- lapply(years, process_and_map_scenarios)

# Calculate percentages for each year
scenarios_percentages_df_list <- list()
for (i in 1:length(years)) {
  year <- years[i]
  mapped_scenarios <- mapped_scenarios_list[[i]]
  scenarios_percentages_list <- list()
  for (j in 1:nlyr(mapped_scenarios)) {
    scenario_name <- names(mapped_scenarios)[j]
    raster_layer <- mapped_scenarios[[j]]
    percentages_df <- calculate_land_use_percentages(
      raster_stack = raster_layer,
      land_use_types = LULC_Types,
      land_use_names = LULC_Types_names,
      time = year
    )
    percentages_df$Scenario <- scenario_name
    scenarios_percentages_list[[scenario_name]] <- percentages_df
  }
  scenarios_percentages_df <- do.call(rbind, scenarios_percentages_list)
  scenarios_percentages_df_list[[i]] <- scenarios_percentages_df
}

# Combine the data frames into a single data frame
scenarios_percentages_df <- do.call(rbind, scenarios_percentages_df_list)

# Remove the year suffix from scenario names
scenarios_percentages_df <- scenarios_percentages_df %>%
  mutate(Scenario = gsub("_\\d{4}$", "", Scenario))

# Plot the land use change of the different scenarios
LandUseChange_time_plot <- ggplot(scenarios_percentages_df, aes(x = time, y = value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = scenario_colors) +
  facet_wrap(~ landUse, scales = "free_y", ncol = 3) +
  labs(title = paste0("Land Use Percentages of the ",  biome_name_short, " Over Time by Scenario"),
       x = "Year",
       y = "Total Land Area (%)") +
  theme_minimal()

print(LandUseChange_time_plot)
ggsave(filename = file.path(output_folder, paste0("LandUseChange_time_", biome_name_short, ".png")),
       plot = LandUseChange_time_plot,
       dpi = 600)

