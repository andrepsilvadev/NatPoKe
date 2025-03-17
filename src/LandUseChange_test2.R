## Name: LandUseChange_test2.R ##
## Authors: Jorinde-M. Rieger ##
## Description: Applies functions to calculate percentage changes over time and spatial explicit changes for a given Biome
## for the ssp126 and ssp585 scenarios in various years ##
## Date: March 13th 2025 ##

# Input variables -------------------------------------------
# Define input variables
scenarios <- c("rcp26_ssp1", "rcp85_ssp5")
scenario_names <- c("SSP1-RCP2.6", "SSP5-RCP8.5")

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
years <- c(2021, 2030, 2050, 2070, 2100)

# Define the baseline year
baseline_year <- 2015

# Define the file paths
base_path <- "~/data/data/stitched_lulc_esa_scenarios"
output_path <- "~/data/data/stitched_lulc_esa_scenarios/outputData"
output_folder <- "~/data/output"

# Define the target resolution (based on the landUsePercentage rasters)
#target_resolution <- 0.0277 # the resolution is different form the mapping before should only be 0.027

# Define the biome and continents
biome_name <- "Tropical & Subtropical Moist Broadleaf Forests"
biome_name_short <- "Tropical Biome"
continent_names <- c("Africa", "Asia", "South America")

#biome_name <- "Boreal Forests/Taiga"
#biome_name_short <- "Boreal Biome"
#continent_names <- c("Europe", "North America")

# Functions - later add them to CustomFunctions.R -------------------------------------------
# Function to load rasters
load_baseline_raster <- function(baseline_year){
  rast("~/data/data/stitched_lulc_esa_scenarios/lulc_esa_2015.tif")
}

load_scenario_raster <- function(scenario, year) {
  # Construct the file path
  file_path <- file.path(base_path, scenario, paste0("lulc_esa_gtap1_", scenario, "_", year, "_no_policy.tif"))
  rast(file_path)
}

# load mapped baseline raster
load_mapped_baseline <- function(baseline_year){
  rast("~/data/data/stitched_lulc_esa_scenarios/outputData/Mapped_LandUseChange_baseline_2015_agg.tif")
}

# Function to load stored mapped raster stacks
load_mapped_rasters <- function(year) {
  mapped_file_path <- file.path(output_path, paste0("Mapped_LandUseChange_scenarioStack_", year, "_", biome_name_short, ".tif"))
  if (file.exists(mapped_file_path)) {
    mapped_raster_stack <- rast(mapped_file_path)
    assign(paste0("Mapped_LandUseChange_scenarioStack_", year, "_", gsub(" ", "_", biome_name_short)), mapped_raster_stack, envir = .GlobalEnv)
    return(mapped_raster_stack)
  } else {
    stop(paste("Mapped raster file for year", year, "does not exist."))
  }
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
      scenarios_list[[paste0(scenario, "_", year)]] <- get(raster_name)
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

# Define the mapping function
map_values_to_landUse <- function(x) {
  value_to_landUse <- list(
    "190" = 1,  # Urban
    "10" = 2, "11" = 2, "12" = 2, "20" = 2, "30" = 2,  # Cropland
    "130" = 3,  # Pasture/Grassland
    "40" = 4, "50" = 4, "60" = 4, "61" = 4, "62" = 4, "70" = 4, "71" = 4, "72" = 4, "80" = 4, "81" = 4, "82" = 4, "90" = 4, "100" = 4,  # Forest
    "110" = 5, "120" = 5, "121" = 5, "122" = 5, "140" = 5,  # Non-forest vegetation
    "210" = 6,  # Water
    "150" = 7, "151" = 7, "152" = 7, "153" = 7, "160" = 7, "170" = 7, "180" = 7, "200" = 7, "201" = 7, "202" = 7, "210" = 7, "220" = 7  # Barren or Other
  )
  sapply(x, function(val) {
    if (val %in% names(value_to_landUse)) {
      return(value_to_landUse[[as.character(val)]])
    } else {
      return(NA)  # Handle values that do not map to any land-use type
    }
  })
}

# Function to calculate the percentages for each land-use type
calculate_landUse_percentages <- function(raster_stack, landUse_types, landUse_names, time) {
  # Create binary maps for each land-use type
  landUse_layers <- lapply(landUse_types, function(cat) {
    terra::app(raster_stack, fun = function(x) {
      return(ifelse(x == cat, 1, 0))
    })
  })
  
  # Calculate the percentages for each land-use type without NAs
  landUse_percentages <- sapply(landUse_layers, function(layer) {
    sum(values(layer), na.rm = TRUE) / sum(!is.na(values(layer))) * 100
  })
  
  # Create a data frame with the results
  percentage_df <- data.frame(
    time = time,
    landUse = landUse_names, # I took variable away, the same as landuse
    variable = landUse_names,
    value = landUse_percentages
  )
  
  return(percentage_df)
}

process_and_map_scenarios <- function(year) {
  # Load the raster stack for the years
  mapped_raster_stack <- get(paste0("Mapped_LandUseChange_scenarioStack_", year, "_", gsub(" ", "_", biome_name_short)))
  
  # Calculate land use percentages
  scenarios_percentages_list <- list()
  for (i in 1:nlyr(mapped_raster_stack)) {
    scenario_name <- names(mapped_raster_stack)[i]
    raster_layer <- mapped_raster_stack[[i]]
    percentages_df <- calculate_landUse_percentages(
      raster_stack = raster_layer,
      landUse_types = LULC_Types,
      landUse_names = LULC_Types_names,
      time = year
    )
    percentages_df$Scenario <- scenario_name
    scenarios_percentages_list[[scenario_name]] <- percentages_df
  }
  
  # Combine the results into a single data frame
  scenarios_percentages_df <- do.call(rbind, scenarios_percentages_list)
  return(scenarios_percentages_df)
}


# Function to create binary maps and classes of land use types
calculateRasterClass <- function(OriginalRaster, extent) {
  # Crop and mask the raster to the biome's boundary
  raster <- mask(crop(OriginalRaster, extent), extent)
  
  # Define the unique land-use classes and remove NAs
  land_use_classes <- unique(values(raster))
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
  
  # Aggregate each binary raster by a factor of 10
  aggregated_rasters <- list()
  for (class in names(binary_rasters)) {
    aggregated_raster <- aggregate(binary_rasters[[class]], fact = 10, fun = function(x) sum(x > 0, na.rm = TRUE))
    masked_raster <- mask(crop(aggregated_raster, extent), extent)
    aggregated_rasters[[class]] <- masked_raster
  }
  
  # Convert the list of rasters to a SpatRaster stack
  aggregated_rasters_stack <- rast(aggregated_rasters)
  return(aggregated_rasters_stack)
}


# Function to replace numbers of LULC Types with names
replace_numbers_with_names <- function(raster_list, types, names) {
  # Replace the names of the raster layers with the corresponding land-use names
  names(raster_list) <- names[match(names(raster_list), types)]
  return(raster_list)
}

# Function to replace numbers of LULC Types with names in a nested list
replace_numbers_with_names_nested <- function(nested_list, types, names) {
  for (scenario in names(nested_list)) {
    for (year in names(nested_list[[scenario]])) {
      # Replace the names of the land-use types within each year
      names(nested_list[[scenario]][[year]]) <- names[match(names(nested_list[[scenario]][[year]]), types)]
    }
  }
  return(nested_list)
}

# Function to calculate percentage changes for each land-use class for both scenarios
calculate_percentage_changes <- function(base_year_raster, target_year_rasters_list, years) {
  percentage_change_rasters_list <- list()
  
  for (year in years) {
    percentage_change_rasters_list[[year]] <- list()
    
    for (scenario in names(target_year_rasters_list[[year]])) {
      target_raster <- target_year_rasters_list[[year]][[scenario]]
      
      # Create a list to store percentage changes for each land-use class
      percentage_change_classes <- list()
      
      for (class in names(base_year_raster)) {
        base_raster <- base_year_raster[[class]]
        target_raster_class <- target_raster[[class]]
        
        # Calculate the percentage change
        percentage_change <- (target_raster_class - base_raster) / base_raster * 100
        percentage_change_classes[[class]] <- percentage_change
      }
      
      # Store the percentage changes for the scenario
      percentage_change_rasters_list[[year]][[scenario]] <- percentage_change_classes
    }
  }
  
  return(percentage_change_rasters_list)
}


# Function to load and select continents
load_select_continents <- function(continent_names) {
  continents <- ne_countries(scale = "medium", returnclass = "sf") %>%
    dplyr::filter(continent %in% continent_names) %>% 
    group_by(continent) %>%
    summarise(geometry = st_union(geometry))
  continents
}

# Function to crop and mask the rasters to the continents
crop_mask_continent <- function(raster, continent_geom) {
  mask(crop(raster, continent_geom), continent_geom)
}

# Function to crop the biome boundaries to the continents
crop_biome_to_continent <- function(biome, continent_geom) {
  st_intersection(biome, continent_geom)
}

# Function to to create individual plots for each scenario, class, and year
plot_landUse_spatialChanges <- function(raster, biome_geom, color_ramp, fill_label, min_value, max_value) {
  raster_df <- as.data.frame(raster, xy = TRUE)
  colnames(raster_df)[3] <- "value"  # Percentage change (%)
  
  ggplot(raster_df) +
    geom_sf(data = biome_geom, fill = "lightgrey", color = "lightgrey", size = 0.2) +  # Biome and continent basemap
    geom_tile(data = raster_df, aes(x = x, y = y, fill = value)) +
    scale_fill_gradientn(name = fill_label, colors = color_ramp(seq(-100, 100, length.out = 101)), limits = c(min_value, max_value), na.value = "grey") +
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 8),
      plot.title = element_blank(),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8)
    ) +
    coord_sf()  # Use coord_sf() for spatial data
}


# Prepare the climate scenarios rasters for further calculations and graphical representation -------------------------------------------
# Process baseline year
baseline_raster <- load_baseline_raster(baseline_year)
  
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
output_file <- file.path(output_path, paste0("Mapped_LandUseChange_baseline_", baseline_year, "_agg.tif"))
writeRaster(mapped_baseline, output_file, overwrite = TRUE)
assign(paste0("Mapped_LandUseChange_baseline_", baseline_year, "_", biome_name_short), mapped_baseline, envir = .GlobalEnv)


# Loop through the scenarios and years for the biome
# L apply
for (scenario in scenarios) {
  for (year in years) {
    # Load the raster
    raster <- load_scenario_raster(scenario, year)
    
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

LandUseChange_scenarioStack_2021_Tropical_Biome

# Calculate and create Climate Change graphics over time -------------------------------------------

# Load mapped scenarios if needed
mapped_scenarios <- load_mapped_rasters(years)

  # Define consistent color palette for the scenarios
scenario_colors <- setNames(
  c("#1f77b4", "#ff7f0e"), scenario_names)

# Process and map scenarios for each year
scenarios_percentages_df_list <- lapply(years, process_and_map_scenarios)

# Combine the data frames into a single data frame
scenarios_percentages_df <- do.call(rbind, scenarios_percentages_df_list)

# Remove the year suffix from scenario names
scenarios_percentages_df <- scenarios_percentages_df %>%
  mutate(Scenario = gsub("_\\d{4}$", "", Scenario))

# This is a test function
scenarios_percentages_df <- scenarios_percentages_df %>%
  mutate(Scenario = gsub("scenario_", "", Scenario)) %>%  # Remove "scenario_"
  mutate(Scenario = gsub("_\\d{4}$", "", Scenario)) %>%  # Remove year suffix
  mutate(Scenario = recode(Scenario, !!!setNames(scenario_names, scenarios)))  # Map to human-readable names


# needs to be modified 
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

# Calculate and create spatially explicit Land Use Change Maps -------------------------------------------
# Load the mapped raster stack for the baseline year
baseline_year_raster <- load_mapped_baseline(baseline_year)

# Load the mapped raster stacks for the target years
target_year_rasters_list <- list()
for (year in years) {
  target_year_rasters_list[[as.character(year)]] <- load_mapped_rasters(year)
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
output_file <- file.path(output_path, paste0("LandUseChange_baseline_", baseline_year, "_classified.tif"))
writeRaster(baseline_year_raster_classified, output_file, overwrite = TRUE)


# Apply calculateRasterClass to the target year rasters
# Loop through the years to process each layer (scenario) in the target_year_rasters_list
for (year in names(target_year_rasters_list)) {
  # Get the raster for the year
  target_raster <- target_year_rasters_list[[year]]
  
  # Ensure the raster has two layers (one for each scenario)
  if (nlyr(target_raster) != 2) {
    stop(paste("The raster for year", year, "does not have exactly two layers."))
  }
  
  # Extract the layers for each scenario
  scenario_rasters <- list(
    rcp26_ssp1 = target_raster[[1]],  # Layer 1 corresponds to rcp26_ssp1
    rcp85_ssp5 = target_raster[[2]]   # Layer 2 corresponds to rcp85_ssp5
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
    output_file <- file.path(output_path, paste0("LandUseChange_", scenario, "_", year, "_classified.tif"))
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

# Validate the geometries, corrects geometries
biome_sf <- st_make_valid(biome_sf)

# Crop the biome boundaries to the continents
biome_continents <- setNames(lapply(continent_geoms, function(continent_geom) {
  crop_biome_to_continent(biome_sf, continent_geom)
}), names(continent_geoms))


# Calculate percentage changes
percentage_change_rasters_list <- calculate_percentage_changes(
  base_year_raster = baseline_year_raster_classified,
  target_year_rasters_list = target_year_rasters_list,
  years = as.character(years)
)

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



# Example of how to access the dynamically created variables
# change_ssp126_Forest_2050_africa
# change_ssp585_Cropland_2100_asia

# Create a custom color ramp with specified breakpoints
custom_color_ramp <- colorRamp2(c(-100, 0, 100), c("blue", "yellow", "red"))


# Create plots for each land-use class, scenario, and year
for (class in names(baseline_year_raster_classified)) {
  for (year in as.character(years)) {
    for (scenario in scenarios) {
      # Create plots for each continent and scenario
      plots_spatial <- list()
      for (continent in names(continent_geoms)) {
        # Access the raster from cropped_rasters
        raster <- cropped_rasters[[year]][[scenario]][[class]][[continent]]
        # creat the plots
        plot <- plot_landUse_spatialChanges(
          raster,
          biome_continents[[continent]],
          custom_color_ramp,
          "Change in %", 
          -100, 100
        )
        plots_spatial[[paste0(scenario, "_", tolower(continent))]] <- plot
      }
      
      # Combine the plots into a grid layout
      combined_plot_spatial <- grid.arrange(
        arrangeGrob(
          textGrob(continent_names[1], gp = gpar(fontsize = 16)),
          textGrob(continent_names[2], gp = gpar(fontsize = 16)),
          textGrob(continent_names[3], gp = gpar(fontsize = 16)),
          ncol = 3,
          heights = unit(c(0.5), "null")
        ),
        arrangeGrob(
          textGrob(scenario_names[1], rot = 90, gp = gpar(fontsize = 16)),
          plots_spatial[[paste0(scenario, "_", tolower(continent_names[1]))]], 
          plots_spatial[[paste0(scenario, "_", tolower(continent_names[2]))]], 
          plots_spatial[[paste0(scenario, "_", tolower(continent_names[3]))]],
          ncol = 4,
          widths = unit(c(0.5, 5, 5, 5), "null")
        ),
        arrangeGrob(
          textGrob(scenario_names[2], rot = 90, gp = gpar(fontsize = 16)),
          plots_spatial[[paste0(scenario, "_", tolower(continent_names[1]))]], 
          plots_spatial[[paste0(scenario, "_", tolower(continent_names[2]))]], 
          plots_spatial[[paste0(scenario, "_", tolower(continent_names[3]))]],
          ncol = 4,
          widths = unit(c(0.5, 5, 5, 5), "null")
        ),
        ncol = 1,
        heights = unit(c(0.5, 5, 5), "null"),
        top = textGrob(paste0("Land Use Change for ", class, " in the ", biome_name_short, " (", baseline_year, " vs. ", year, ")"), gp = gpar(fontsize = 18))
      )
      
      # Save the combined plot
      ggsave(filename = file.path(output_folder, paste0("LandUseChange_", class, "_spatialChanges_", year, "_", biome_name_short, ".png")), 
             plot = combined_plot_spatial, 
             width = 15, height = 6, dpi = 300)
    }
  }
}

