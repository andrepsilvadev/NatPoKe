## Name: LandUseChange.R ##
## Authors: Jorinde-M. Rieger ##
## Description: Applies functions to calculate percentage changes over time and spatial explicit changes for a given Biome
## for the ssp126 and ssp585 scenarios in various years ##
## Date: May 4th 2025 ##

# Settings & libraries -------------------------------------------
source("~/NatPoKe9/src/libraries.R") # libraries
source("~/NatPoKe9/src/customFunctions.R") # functions

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

# Calculate and create Climate Change graphics over time -------------------------------------------
# Load mapped scenarios if needed
mapped_scenarios <- list()
for (year in years) {
  mapped_scenarios[[as.character(year)]] <- load_mapped_landUse(year)
}

# Define consistent color palette for the scenarios
scenario_colors <- setNames(
  c("#1f77b4", "#ff7f0e"), scenario_names)

# Process and map scenarios for each year
scenarios_percentages_df_list <- lapply(years, process_and_map_scenarios)

# Combine the data frames into a single data frame
scenarios_percentages_df <- do.call(rbind, scenarios_percentages_df_list)

# Remove the year suffix from scenario names
scenarios_percentages_df <- scenarios_percentages_df %>%
  mutate(Scenario = gsub("^scenario_", "", Scenario)) %>% # Remove scenario prefix
  mutate(Scenario = gsub("_\\d{4}$", "", Scenario)) %>% # Remove year suffix
  mutate(Scenario = recode(Scenario, !!!setNames(scenario_names, scenarios)))  # Map to human-readable names

# Filter out the "Water" land-use type
scenarios_percentages_df_filtered <- scenarios_percentages_df %>%
  filter(landUse != "Water")

# Plot the land use change of the different scenarios
LandUseChange_time_plot <- ggplot(scenarios_percentages_df_filtered, aes(x = time, y = value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = scenario_colors) +
  facet_wrap(~ landUse, scales = "free_y", ncol = 3) +
  labs(title = paste0("Land Use Percentages of the ",  biome_name_short, " Over Time by Scenario"),
       x = "Year",
       y = "Total Land Area (%)") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 14),  # Adjust title size
    axis.title = element_text(size = 12),  # Adjust axis title size
    axis.text = element_text(size = 10),   # Adjust axis text size
    legend.title = element_text(size = 14),  # Adjust legend title size
    legend.text = element_text(size = 12),   # Adjust legend text size
    strip.text = element_text(size = 12),    # Adjust facet label size
    plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10)  # Add margin around the entire plot
  )
print(LandUseChange_time_plot)

# Save the plot with specified dimensions and resolution
ggsave(
  filename = file.path(output_folder, paste0("LandUseChange_time_", biome_name_short, ".png")),
  plot = LandUseChange_time_plot,
  width = 10,  # Width in inches
  height = 6, # Height in inches 
  dpi = 300     # High resolution
)

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

# Create a custom color ramp with specified breakpoints
custom_color_ramp <- colorRamp2(c(-100, 0, 100), c("blue", "yellow", "red"))

# Create plots for each land-use class, scenario, and year
for (class in names(baseline_year_raster_classified)) {
  # skip the "Water" land-use type
  if (class == "Water") next
  
  for (year in as.character(years)) {
    # Create a list to store plots for both scenarios
    plots_spatial <- list()
    
    for (scenario in scenarios) {
      for (continent in names(continent_geoms)) { #continent_geoms or continent_names
        
        # Access the raster from cropped_rasters
        raster <- cropped_rasters[[year]][[scenario]][[class]][[continent]]
        # creat the plots
        plot <- plot_landUse_spatialChanges(
          raster = raster,
          biome_geom = biome_continents[[continent]],
          color_ramp = custom_color_ramp,
          fill_label = "Change in %", 
          min_value = -100,
          max_value = 100
        )+
          theme(legend.position = "none")  # Remove individual legends
        
        plots_spatial[[paste0(scenario, "_", tolower(continent))]] <- plot
      }
    }
    # Extract the legend from one of the plots
    example_plot <- plot_landUse_spatialChanges(
      raster = cropped_rasters[[year]][[scenarios[1]]][[class]][[continent_names[1]]],
      biome_geom = biome_continents[[continent_names[1]]],
      color_ramp = custom_color_ramp,
      fill_label = "Change in %",
      min_value = -100,
      max_value = 100
    )
    shared_legend <- extract_legend(example_plot)
    
    # Dynamically determine the number of continents
    num_continents <- length(continent_names)
    
    # Combine the plots into a grid layout
    combined_plot_spatial <- grid.arrange(
      arrangeGrob(
        grobs = lapply(continent_title, function(continent) {
          textGrob(continent, gp = gpar(fontsize = 22))
        }),
        ncol = num_continents,
        heights = unit(c(0.5), "null")
      ),
      arrangeGrob(
        grobs = c(
          list(textGrob(scenario_names[1], rot = 90, gp = gpar(fontsize = 22))),
          lapply(continent_names, function(continent) {
            plots_spatial[[paste0(scenarios[1], "_", tolower(continent))]]
          })
        ),
        ncol = num_continents + 1,
        widths = unit(c(0.5, rep(5, num_continents)), "null")
      ),
      arrangeGrob(
        grobs = c(
          list(textGrob(scenario_names[2], rot = 90, gp = gpar(fontsize = 22))),
          lapply(continent_names, function(continent) {
            plots_spatial[[paste0(scenarios[2], "_", tolower(continent))]]
          })
        ),
        ncol = num_continents + 1,
        widths = unit(c(0.5, rep(5, num_continents)), "null")
      ),
      heights = unit(c(0.5, 5, 5), "null")
    )
    
    # Combine the plot and legend side by side
    final_plot <- grid.arrange(
      combined_plot_spatial,
      arrangeGrob(
        grobs = list(shared_legend),
        ncol = 1
      ),
      ncol = 2,  # Two columns: one for the plot and one for the legend
      widths = unit(c(15, 3), "null"),
      top = textGrob(paste0("Land Use Change for ", class, " in the ", biome_name_short, " (", baseline_year, " vs. ", year, ")"), gp = gpar(fontsize = 24))
    )
    
    # Save the combined plot
    ggsave(filename = file.path(output_folder, paste0("LandUseChange_", class, "_spatialChanges_", year, "_", gsub(" ", "_", biome_name_short), ".png")), 
           plot = final_plot, 
           width = 24, height = 10, dpi = 300)
  }
}
