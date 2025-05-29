## Name: CustomFunctions2.R ##
## Authors: Jorinde-M. Rieger ##
## Description: Loads all developed customised functions for LandUseChange.R, ClimateChange.R, inputClimate.R, inputLandUse.R##
## Date: May 29th 2025 ##

#####################################
# General Functions
#####################################
# Functions to load and modify rasters-------------------------------------------
# Function to load and select the biome shapefile
load_select_biome <- function(biome_name) {
  biome_sf <- st_read("~/data/data/Ecoregions2017/Ecoregions2017/Ecoregions2017.shp")
  biome_sf[biome_sf$BIOME_NAME == biome_name, ]
}

# Function to crop and mask rasters
crop_mask_raster <- function(raster, biome_sp) {
  mask(crop(raster, biome_sp), biome_sp)
}

# Function to load and select continents
load_select_continents <- function(continent_names) {
  continents <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Initialize an empty list to store merged continents
  merged_continents <- list()
  
  # Handle merged "Central & South America"
  if ("Central & South America" %in% continent_names) {
    central_south_america <- continents %>%
      dplyr::filter(subregion %in% c("Central America", "South America", "Caribbean")) %>%
      summarise(geometry = st_union(geometry)) %>%
      mutate(continent = "Central & South America")
    
    # Ensure column consistency
    missing_columns <- setdiff(names(continents), names(central_south_america))
    for (col in missing_columns) {
      central_south_america[[col]] <- NA  # Add missing columns with NA values
    }
    central_south_america <- central_south_america[names(continents)]  # Reorder columns to match `continents`
    
    # Add to merged continents
    merged_continents[["Central & South America"]] <- central_south_america
    
    # Remove "Central & South America" from the continent names
    continent_names <- setdiff(continent_names, "Central & South America")
  }
  
  # Filter the remaining continents
  remaining_continents <- continents %>%
    dplyr::filter(continent %in% continent_names)
  
  # Combine merged and remaining continents
  all_continents <- do.call(rbind, c(merged_continents, list(remaining_continents)))
  
  # Return the final dataset
  return(all_continents)
}

# Function to intersect biome with continents
intersect_biome_with_continents <- function(biome_sf, continent_geoms) {
  # Validate and fix geometries
  biome_sf <- st_make_valid(biome_sf)
  continent_geoms <- lapply(continent_geoms, st_make_valid)
  
  # Perform intersection and handle empty geometries
  biome_continents <- setNames(lapply(continent_geoms, function(continent_geom) {
    result <- st_intersection(biome_sf, continent_geom)
    if (is.null(result) || nrow(result) == 0) {
      return(NULL)  # Return NULL if no intersection
    }
    return(result)
  }), names(continent_geoms))
  
  return(biome_continents)
}

# Function to crop and mask the rasters to the continents
crop_mask_continent <- function(raster, continent_geom) { # technically not needed, merge with crop_mask_raster
  mask(crop(raster, continent_geom), continent_geom)
}

# Function to crop the biome boundaries to the continents
crop_biome_to_continent <- function(biome, continent_geom) {
  st_intersection(biome, continent_geom)
}

# Function to extract the legend from a ggplot object
extract_legend <- function(plot) {
  gtable <- ggplotGrob(plot)
  legend <- gtable$grobs[which(sapply(gtable$grobs, function(x) x$name) == "guide-box")][[1]]
  return(legend)
}

#####################################
# Climate realated Functions #
#####################################
# Functions to load rasters -------------------------------------------
load_baseline_clim <- function(variable, baseline_year){
  file_path <- file.path(basePathClim, paste0("CHELSA_", variable, "_", baseline_year, "_V.2.1.tif"))
  rast(file_path)}

load_scenario_clim <- function(scenario, variable, year) {
  file_path <- file.path(basePathClim, scenario, paste0("CHELSA_", variable, "_", year, "_gfdl-esm4_", scenario, "_V.2.1.tif"))
  rast(file_path)}

# Function to aggregate rasters
aggregate_raster <- function(raster, aggregation_factor) {
  aggregate(raster, aggregation_factor, fun = mean)
}

# Function to stack rasters
stack_clim_rasters <- function(variable, year) {
  scenarios_list <- list(
    get(paste0("ClimateChange_", scenarios[1],"_", variable, "_", year, "_", biome_name_short)),
    get(paste0("ClimateChange_", scenarios[2],"_", variable, "_", year, "_", biome_name_short))
  )
  
  # Assign names to the list elements
  names(scenarios_list) <- c(paste0(scenario_names[1], "_", year), paste0(scenario_names[2], "_", year))
  
  # Create a raster stack from the list of scenarios
  scenarios_stack <- rast(scenarios_list)
  
  # Assign names to the raster stack layers
  names(scenarios_stack) <- names(scenarios_list)
  
  # Save the raster stack
  stack_output_file <- file.path(outputPathLandscapes, paste0("scenarios_stack_", variable, "_", year, "_", biome_name_short, ".tif"))
  writeRaster(scenarios_stack, stack_output_file, overwrite = TRUE)
  
  # Assign the raster stack to a variable in the environment
  assign(paste0("scenarios_stack_", variable, "_", year, "_", gsub(" ", "_", biome_name_short)), scenarios_stack, envir = .GlobalEnv)
  
  return(scenarios_stack)
}

# Function to extract mean values from a raster stack
extract_mean_values <- function(raster_stack, years, value_type) {
  mean_values <- sapply(1:nlyr(raster_stack), function(i) {
    mean(values(raster_stack[[i]]), na.rm = TRUE)
  })
  data.frame(
    Year = years,
    Scenario = names(raster_stack),
    Mean_Value = mean_values,
    Value_Type = value_type
  )
}

# Functions to vizualize results -------------------------------------------
# Function to create plots
plot_timeChanges <- function(mean_values_df, value_type, y_label) {
  ggplot(mean_values_df, aes(x = Year, y = Mean_Value, color = Scenario, group = Scenario)) +
    geom_line() +
    geom_point() +
    scale_color_manual(values = scenario_colors) +
    labs(
      x = "Year",
      y = y_label
    ) +
    theme_minimal()+
    theme(
      axis.title.x = element_text(size = 14, margin = margin(t = 10)),  # Increase gap for x-axis title
      axis.title.y = element_text(size = 14, margin = margin(r = 10)),   # Increase gap for y-axis title
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    )
}

# Function to calculate changes
calculate_change <- function(raster_future, raster_present) {
  raster_future - raster_present
}

plot_ClimatespatialChanges <- function(raster, biome_geom, color_ramp, fill_label, min_value, max_value) {
  # Convert raster to data frame
  raster_df <- as.data.frame(raster, xy = TRUE)
  colnames(raster_df)[3] <- "value"  # Percentage change (%)
  
  # Load country boundaries
  countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  # Exclude sovereign states and keep only relevant territories
  countries <- countries %>%
    dplyr::filter(sovereignt != "France")  # Exclude France as a sovereign state
  
  # Ensure CRS consistency
  biome_geom <- st_transform(biome_geom, crs = st_crs(countries))
  countries <- st_transform(countries, crs = st_crs(biome_geom))
  
  # Validate geometries
  countries <- st_make_valid(countries)
  biome_geom <- st_make_valid(biome_geom)
  
  # Identify countries overlapping with the biome
  overlapping_indices <- st_intersects(countries, biome_geom, sparse = TRUE)
  overlapping_countries <- countries[lengths(overlapping_indices) > 0, ]
  
  # Create the plot
  plot <- ggplot(raster_df) +
    # Add country boundaries
    geom_sf(data = overlapping_countries, aes(color = "Country Boundaries"), fill = NA, size = 0.2) +
    # Add biome boundary
    geom_sf(data = biome_geom, aes(color = "Biome"), fill = "lightgrey", size = 0.2) +
    # Add raster data
    geom_tile(data = raster_df, aes(x = x, y = y, fill = value)) +
    # Define the color scale for the raster
    scale_fill_gradientn(name = fill_label, 
                         colors = color_ramp(seq(min_value, max_value, length.out = 101)), 
                         limits = c(min_value, max_value), 
                         na.value = "grey") +
    # Define the color scale for the biome and country boundaries
    scale_color_manual(
      name = "Legend",
      values = c("Biome" = "lightgrey", "Country Boundaries" = "darkgrey"),
      breaks = c("Biome", "Country Boundaries"),  # Ensure these match the aes(color = ...) values
      labels = c("Biome", "Country Boundaries")
    ) +
    # Add labels and theme
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 14),
      plot.title = element_blank(),
      legend.title = element_text(size = 22, margin = margin(b = 10)),
      legend.text = element_text(size = 18),
      legend.key.height = unit(1, "cm"),  # Increase the height of the color ramp
      legend.spacing = unit(1, "cm")
    ) +
    coord_sf()  # Use coord_sf() for spatial data
  
  return(plot)
}

#####################################
# Land-Use Change Functions #
#####################################
# Functions to load rasters-------------------------------------------
# Baseline raster
load_baseline_landUse <- function(baseline_year){
  rast("~/data/data/stitched_lulc_esa_scenarios/lulc_esa_2015.tif")
}
# Scenario Rasters
load_scenario_landUse <- function(scenario, year) {
  # Construct the file path
  file_path <- file.path(basePathLandUse, scenario, paste0("lulc_esa_gtap1_", scenario, "_", year, "_no_policy.tif"))
  rast(file_path)
}
# Mapped baseline raster
load_mapped_baseline_landUse <- function(baseline_year){
  mapped_baseline_path <- file.path(outputPathLandscapes, paste0("MappedLandUse_base_", baseline_year, "_", gsub(" ", "_", extent), ".tif"))
  rast(mapped_baseline_path)
}
# Mapped raster stacks
load_mapped_landUse <- function(year) {
  mapped_file_path <- file.path(outputPathLandscapes, paste0("MappedLandUse_scenarios_", year, "_", gsub(" ", "_", extent), ".tif"))
  if (file.exists(mapped_file_path)) {
    mapped_raster_stack <- rast(mapped_file_path)
    assign(paste0("MappedLandUse_scenarios_", year, "_", gsub(" ", "_", extent)), mapped_raster_stack, envir = .GlobalEnv)
    return(mapped_raster_stack)
  } else {
    stop(paste("Mapped raster file for year", year, "does not exist."))
  }
}

# Functions to modify rasters-------------------------------------------
# Function to stack rasters
#stack_rasters <- function(year) {
#  scenarios_list <- list()
#  for (scenario in scenarios) {
#    raster_name <- paste0("LandUse_", scenario, "_", year, "_", gsub(" ", "_", extent))
#    if (exists(raster_name)) {
#      scenarios_list[[paste0(scenario, "_", year)]] <- get(raster_name)
#    }
#  }
  
  # Create a raster stack from the list of scenarios
#  scenarios_stack <- rast(scenarios_list)
#  names(scenarios_stack) <- names(scenarios_list)
  
#  stack_output_file <- file.path(outputPathLandscapes, paste0("LandUse_scenarioStack_", year, "_", gsub(" ", "_", extent), ".tif"))
#  writeRaster(scenarios_stack, stack_output_file, overwrite = TRUE)
  
  # Assign the raster stack to a variable in the environment
#  assign(paste0("LandUse_scenarioStack_", year, "_", gsub(" ", "_", extent)), scenarios_stack, envir = .GlobalEnv)
  
#  return(scenarios_stack)
#}

stack_rasters <- function(year, scenarios, extent, outputPathLandscapes) {
  scenarios_list <- list()
  
  for (scenario in scenarios) {
    # Dynamically construct the raster file path
    raster_file <- file.path(outputPathLandscapes, paste0("LandUse_", scenario, "_", year, "_", gsub(" ", "_", extent), ".tif"))
    
    # rasterize file
    scenarios_list[[paste0(scenario, "_", year)]] <- terra::rast(raster_file)
  }
  
  # Create a raster stack from the list of scenarios
  scenarios_stack <- terra::rast(scenarios_list)
  terra::names(scenarios_stack) <- terra::names(scenarios_list)
  
  # Save the raster stack to disk
  stack_output_file <- file.path(outputPathLandscapes, paste0("LandUse_scenarioStack_", year, "_", gsub(" ", "_", extent), ".tif"))
  writeRaster(scenarios_stack, stack_output_file, overwrite = TRUE)
  
  # Return the raster stack
  return(scenarios_stack)
}

# Define the mapping function of ESA LULC types (37) to the 7 (SEALS) LULC types
map_values_to_landUse <- function(x) {
  value_to_landUse <- list(
    "190" = 1,  # Urban
    "10" = 2, "11" = 2, "12" = 2, "20" = 2, "30" = 2, "40" = 2,   # Cropland
    "130" = 3,  # Pasture/Grassland
    "50" = 4, "60" = 4, "61" = 4, "62" = 4, "70" = 4, "71" = 4, "72" = 4, "80" = 4, "81" = 4, "82" = 4, "90" = 4, "100" = 4, "151" = 4, "160" = 4, "170" = 4, # Forest
    "110" = 5, "120" = 5, "121" = 5, "122" = 5, "140" = 5, "150" = 5, "152" = 5, "153" = 5, "180" = 5,  # Non-forest vegetation
    "210" = 6,  # Water
    "200" = 7, "201" = 7, "202" = 7, "220" = 7  # Barren or Other
  )
  sapply(x, function(val) {
    if (val %in% names(value_to_landUse)) {
      return(value_to_landUse[[as.character(val)]])
    } else {
      return(NA)  # Handles values that do not map to any land-use type
    }
  })
}


# Functions to analyze rasters-------------------------------------------
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
    value = landUse_percentages
  )
  return(percentage_df)
}

# Function to process the mapped scenarios and apply the function to calculate percentages
process_and_map_scenarios <- function(year) {
  # Load the raster stack for the years
  mapped_raster_stack <- get(paste0("Mapped_LandUseChange_scenarioStack_", year, "_", gsub(" ", "_", extent)))
  
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
    aggregated_raster <- aggregate(binary_rasters[[class]], fact = aggregation_factor, fun = function(x) sum(x > 0, na.rm = TRUE)) # change aggregation faktor to 1km
    masked_raster <- mask(crop(aggregated_raster, extent), extent)
    aggregated_rasters[[class]] <- masked_raster
  }
  
  # Convert the list of rasters to a SpatRaster stack
  aggregated_rasters_stack <- rast(aggregated_rasters)
  return(aggregated_rasters_stack)
}

# Function to replace numbers of LULC Types with names (for baseline raster)
replace_numbers_with_names <- function(raster_list, types, names) {
  # Replace the names of the raster layers with the corresponding land-use names
  names(raster_list) <- names[match(names(raster_list), types)]
  return(raster_list)
}

# Function to replace numbers of LULC Types with names in a nested list (for scenario rasters)
replace_numbers_with_names_nested <- function(nested_list, types, names) {
  for (scenario in names(nested_list)) {
    for (year in names(nested_list[[scenario]])) {
      # Replace the names of the land-use types within each year
      names(nested_list[[scenario]][[year]]) <- names[match(names(nested_list[[scenario]][[year]]), types)]
    }
  }
  return(nested_list)
}

# Function to calculate percentage changes for each land-use classes for scenarios
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
      percentage_change_rasters_list[[year]][[scenario]] <- percentage_change_classes
    }
  }
  return(percentage_change_rasters_list)
}

# Functions to vizualize results -------------------------------------------
# Function to create individual plots for each scenario, class, and year
plot_landUse_spatialChanges <- function(raster, biome_geom, color_ramp, fill_label, min_value, max_value) {
  # Convert raster to data frame
  raster_df <- as.data.frame(raster, xy = TRUE)
  colnames(raster_df)[3] <- "value"  # Percentage change (%)
  
  # Load country boundaries
  countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  # Exclude sovereign states and keep only relevant territories
  countries <- countries %>%
    dplyr::filter(sovereignt != "France")  # Exclude France as a sovereign state
  
  # Ensure CRS consistency
  biome_geom <- st_transform(biome_geom, crs = st_crs(countries))
  countries <- st_transform(countries, crs = st_crs(biome_geom))
  
  # Validate geometries
  countries <- st_make_valid(countries)
  biome_geom <- st_make_valid(biome_geom)
  
  # Identify countries overlapping with the biome
  overlapping_indices <- st_intersects(countries, biome_geom, sparse = TRUE)
  overlapping_countries <- countries[lengths(overlapping_indices) > 0, ]
  
  # Create the plot
  plot <- ggplot(raster_df) +
    # Add country boundaries
    geom_sf(data = overlapping_countries, aes(color = "Country Boundaries"), fill = NA, size = 0.2) +
    # Add biome boundary
    geom_sf(data = biome_geom, aes(color = "Biome"), fill = "lightgrey", size = 0.2) +
    # Add raster data
    geom_tile(data = raster_df, aes(x = x, y = y, fill = value)) +
    # Define the color scale for the raster
    scale_fill_gradientn(
      name = fill_label,
      colors = color_ramp(seq(-100, 100, length.out = 101)),
      limits = c(min_value, max_value),
      na.value = "grey"
    ) +
    # Define the color scale for the biome and country boundaries
    scale_color_manual(
      name = "Legend",
      values = c("Biome" = "lightgrey", "Country Boundaries" = "darkgrey"),
      breaks = c("Biome", "Country Boundaries"),  # Ensure these match the aes(color = ...) values
      labels = c("Biome", "Country Boundaries")
    ) +
    # Add labels and theme
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 18),
      plot.title = element_blank(),
      legend.title = element_text(size = 22, margin = ggplot2::margin(b = 10)),
      legend.text = element_text(size = 18),
      legend.key.height = unit(1, "cm"),  # Increase the height of the color ramp
      legend.spacing = unit(1, "cm")
    ) +
    coord_sf()  # Use coord_sf() for spatial data
  
  return(plot)
}

