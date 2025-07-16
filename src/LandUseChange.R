## Name: LandUseChange.R ##
## Authors: Jorinde-M. Rieger ##
## Description: Applies functions to calculate percentage changes over time and spatial explicit changes for a given extent
## for the ssp126 and ssp585 scenarios in various years ##
## Date: July 15th 2025 ##

# Settings & libraries -------------------------------------------
source("~/NatPoKe9/src/libraries.R") # libraries
source("~/NatPoKe9/src/customFunctions2.R") # functions

# Input variables -------------------------------------------
# Define input variables
scenarios <- c("ssp126", "ssp585") # define socio-economic pathways
scenarios_des <- c("rcp26_ssp1", "rcp85_ssp5") # scenario names in land-use raster
scenario_names <- c("SSP1-RCP2.6", "SSP5-RCP8.5") # define socio-economic pathways names

# Define a mapping for scenario names
scenario_name_mapping <- c(
  "rcp26_ssp1" = "ssp126",
  "rcp85_ssp5" = "ssp585")

# Define years
years <- c(2030, 2050, 2100)
baseline_year <- 2015

# Define the target resolution (based on climate inputs)
target_resolution <- 0.008333333 # 1km resolution

# Define global terrestrial extent
#extent = "Global Terrestrial"
#extent_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Iberian peninsula
#extent_name <- "Iberian peninsula"
#extent_sf <- rnaturalearth::ne_countries(scale = "medium", country = c("Spain", "Portugal"), returnclass = "sf")
#extent_spain <- rnaturalearth::ne_countries(country = "Spain", returnclass = "sf")
#continent_names <- NULL
#continent_title <- NULL
#continent_geoms <- NULL

# Define outputfile for Bank of Spain Project
#outputPathLandUse <- "~/data/BoS/output/LandUseChange"
#if (!dir.exists(outputPathLandUse)) {
#  dir.create(outputPathLandUse, recursive = TRUE)
#}

# Tropical Biome
extent <- "Tropical Biome"
extent_name <- "Tropical & Subtropical Moist Broadleaf Forests"
extent_sf <- load_biome(extent_name)
continent_names <- c("Central & South America", "Africa", "Asia")
continents_sf <- load_select_continents(continent_names)

# Boreal Biome
extent <- "Boreal Biome"
extent_name <- "Boreal Forests/Taiga"
extent_sf <- load_biome(extent_name)
continent_names <- c("North America", "Europe & Asia")
continents_sf <- load_select_continents(continent_names)
continents_sf <- sf::st_wrap_dateline(continents_sf, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
continents_sf <- sf::st_make_valid(continents_sf)

# Define the file paths
basePathLandUse <- "~/data/data/stitched_lulc_esa_scenarios"
outputPathLandscapes <- "~/data/output/Landscapes"
if (!dir.exists(outputPathLandscapes)) {
  dir.create(outputPathLandscapes, recursive = TRUE)
}
outputPathLandUse <- "~/data/output/LandUseChange"
if (!dir.exists(outputPathLandUse)) {
  dir.create(outputPathLandUse, recursive = TRUE)
}

# Simplify and define ESA LULC types (39) to the 7 (SEALS) LULC types
# ESA LULC simplification scheme based on  Johnson, J. A., & Thakrar, S., (2024)
# (Code availability: https://github.com/jandrewjohnson/seals)
LULC_Types <- 1:7
LULC_Types_names <- c(
  "Urban",
  "Cropland",
  "Pasture_Grassland",
  "Forest",
  "Nonforest_vegetation",
  "Water",
  "Barren_other")

# To reclassify LandUse classes create a matrix
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
landUsematrix <- matrix(value_to_landUse, ncol = 2, byrow = TRUE )

# Create land-use input raster for the trainingLandscape -------------------------------------------
# Process baseline year
baseline_raster <- load_baseline_landUse(baseline_year)

# Ensure CRS consistency
extent_crs <- sf::st_transform(extent_sf, crs = crs(baseline_raster))

# Convert the sf to a spatial object
extent_sp <- terra::vect(extent_crs)

# Crop and mask baseline raster to extent
baseline_raster_extent <- crop_mask_raster(baseline_raster, extent_sp)

# Apply land-use type mapping
mapped_baseline <- terra::classify(baseline_raster_extent, landUsematrix, include.lowest = TRUE)
plot(mapped_baseline)

# Save the mapped baseline raster
output_file <- file.path(outputPathLandscapes, paste0("MappedLandUse_base_", baseline_year, "_", gsub(" ", "_", extent), ".tif"))
terra::writeRaster(mapped_baseline, output_file, overwrite = TRUE)
assign(paste0("MappedLandUse_base_", baseline_year, "_", gsub(" ", "_", extent)), mapped_baseline, envir = .GlobalEnv)

# Load the mapped raster stack for the baseline year
mapped_baseline <- load_mapped_baseline_landUse(outputPathLandscapes, baseline_year)

# Apply calculateRasterClass to the baseline raster to create raster classes for the land use types
trainingLandscapesLandUse <- calculateRasterClass(
  OriginalRaster = mapped_baseline,
  extent = extent_sp,
  target_resolution = target_resolution
)

# Replace land use numbers with names
trainingLandscapesLandUse <- replace_numbers_with_names(trainingLandscapesLandUse, LULC_Types, LULC_Types_names)

# Save the processed baseline raster
output_file <- file.path(outputPathLandscapes, paste0("trainingLandscapesLandUse_", baseline_year, "_", gsub(" ", "_", extent), ".tif"))
terra::writeRaster(trainingLandscapesLandUse, output_file, overwrite = TRUE)

#plot(trainingLandscapesLandUse)
gc()

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
  # Create raster stacks for years
  raster_stack <- stack_rasters(year, scenarios, extent, outputPathLandscapes)
  
  # Load the raster stack for the year
  raster_stack <- file.path(outputPathLandscapes, paste0("LandUse_scenarioStack_", year, "_", gsub(" ", "_", extent), ".tif"))
  raster_stack <- terra::rast(raster_stack)
  
  # Apply land-use type mapping
  mapped_scenarios <- terra::classify(raster_stack, landUsematrix, include.lowest = TRUE)
  
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
  LULC_scenarios_list[[as.character(year)]] <- load_mapped_landUse(outputPathLandscapes, year, extent)
}

# Apply calculateRasterClass to the target year rasters
# Loop through the years to process each layer (scenario) (takes approx 8h per landscape on a global scale)
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
    output_file <- file.path(outputPathLandscapes, paste0("predictionLandscapesLandUse_", scenario, "_", year,"_", gsub(" ", "_", extent), ".tif"))
    terra::writeRaster(loaded_raster, output_file, overwrite = TRUE)
    
    # Store the raster in the list
    predictionLandscapesLandUse[[paste0(scenario, "_", year)]] <- loaded_raster
  }
}

# Example plot
#plot(predictionLandscapesLandUse$ssp126_2100)

# Calculate and create projected percentage changes in land-use types -------------------------------------------
# Define consistent color palette for the scenarios
scenario_colors <- setNames(
  c("#1f77b4", "#ff7f0e"), scenario_names)

# Load mapped prediction LandscapeLandUse
mapped_scenarios <- list()
for (year in years) {
  mapped_scenarios[[as.character(year)]] <- load_mapped_landUse(outputPathLandscapes, year, extent)
}

# Crop and mask to continent extents
# Transform continent CRS to match the raster CRS
continents_crs <- st_transform(continents_sf, crs = st_crs(extent_sf))

# Define continent geometries
continent_geoms <- setNames(lapply(continent_names, function(continent) {
  continents_sf %>% dplyr::filter(continent == !!continent)
}), continent_names)

# Crop and mask each mapped raster stack to regions/continents
continent_scenarios <- list()
for (year in names(mapped_scenarios)) {
  for (continent in continent_names) {
    continent_raster <- crop_mask_raster(mapped_scenarios[[year]], continent_geoms[[continent]])
    continent_scenarios[[continent]][[year]] <- continent_raster
  }
}

# Calculate land use cover percentages and create a grafic
LandUseCover <- list()
if (exists("continent_scenarios") && length(continent_scenarios) > 0) {
  # Loop for continent-cropped rasters
  for (continent in names(continent_scenarios)) {
    scenarios_percentages_df_list <- lapply(years, process_and_map_scenarios, use_continent = TRUE, continent = continent)
    scenarios_percentages_df <- do.call(rbind, scenarios_percentages_df_list)
    # safe percentages in the list
    LandUseCover[[continent]] <- scenarios_percentages_df
    
    # Clean and plot
    scenarios_percentages_df <- scenarios_percentages_df %>%
      mutate(Scenario = gsub("^scenario_", "", Scenario)) %>% # Remove scenario prefix
      mutate(Scenario = gsub("_\\d{4}$", "", Scenario)) %>% # Remove year suffix
      mutate(Scenario = recode(Scenario, !!!setNames(scenario_names, scenarios)))  # Map to readable scenrio names
    
    scenarios_percentages_df_filtered <- scenarios_percentages_df %>% # Filter "Water" land-use type
      filter(landUse != "Water")
    
    LandUseChange_time_plot <- ggplot(scenarios_percentages_df_filtered, aes(x = time, y = value, color = Scenario, group = Scenario)) +
      geom_line() +
      geom_point() +
      scale_color_manual(values = scenario_colors,
                         guide = guide_legend(direction = "horizontal")) +
      facet_wrap(~ landUse, scales = "free_y", ncol = 3) +
      labs(title = paste0("Future Land Use Projections for the ",  extent, " of ", continent),
           x = "Year",
           y = "Total Land Area (%)") +
      theme_minimal()+
      theme(
        plot.title = element_text(size = 14),  # Adjust title size
        axis.title = element_text(size = 12),  # Adjust axis title size
        axis.text = element_text(size = 10),   # Adjust axis text size
        legend.title = element_text(size = 14),  # Adjust legend title size
        legend.text = element_text(size = 12),   # Adjust legend text size
        legend.position = "bottom", 
        strip.text = element_text(size = 12),    # Adjust facet label size
        plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10)  # Add margin around the entire plot
      )
    print(LandUseChange_time_plot)
    
    # Save the plot with specified dimensions and resolution
    ggsave(
      filename = file.path(outputPathLandUse, paste0("LandUseChange_time_", gsub(" ", "_", extent),"_", gsub(" ", "_", continent), ".png")),
      plot = LandUseChange_time_plot,
      width = 10,  # Width in inches
      height = 6, # Height in inches 
      dpi = 300     # High resolution
    )
  }
} else {
  # For rasters without continent adjustments
  scenarios_percentages_df_list <- lapply(years, process_and_map_scenarios, use_continent = FALSE)
  scenarios_percentages_df <- do.call(rbind, scenarios_percentages_df_list)
  
  # Clean and plot
  scenarios_percentages_df <- scenarios_percentages_df %>%
    mutate(Scenario = gsub("^scenario_", "", Scenario)) %>%
    mutate(Scenario = gsub("_\\d{4}$", "", Scenario)) %>%
    mutate(Scenario = recode(Scenario, !!!setNames(scenario_names, scenarios)))
  
  scenarios_percentages_df_filtered <- scenarios_percentages_df %>%
    filter(landUse != "Water")
  
  LandUseChange_time_plot <- ggplot(scenarios_percentages_df_filtered, aes(x = time, y = value, color = Scenario, group = Scenario)) +
    geom_line() +
    geom_point() +
    scale_color_manual(values = scenario_colors, guide = guide_legend(direction = "horizontal")) +
    facet_wrap(~ landUse, scales = "free_y", ncol = 3) +
    labs(title = paste0("Future Land Use Projections for the ", extent),
         x = "Year", y = "Total Land Area (%)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.position = "bottom",
      strip.text = element_text(size = 12),
      plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10)
    )
  print(LandUseChange_time_plot)
  
  ggsave(
    filename = file.path(outputPathLandUse, paste0("LandUseChange_time_", gsub(" ", "_", extent), ".png")),
    plot = LandUseChange_time_plot,
    width = 10,
    height = 6,
    dpi = 300
  )
}

gc()

# Load the training and prediction Landscapes, crop to extent if needed -------------------------------------------
# Load training- and predictionLandscapesLandUse for defined extent
trainingLandscapesLandUse <- file.path(outputPathLandscapes, paste0("trainingLandscapesLandUse_", baseline_year,"_", gsub(" ", "_", extent), ".tif"))
trainingLandscapesLandUse <- terra::rast(trainingLandscapesLandUse)

predictionLandscapesLandUse <- list()
for (scenario in scenarios) {
  for (year in years) {
    raster_path <- file.path(outputPathLandscapes, paste0("predictionLandscapesLandUse_", scenario, "_", year,"_", gsub(" ", "_", extent), ".tif"))
    raster <- terra::rast(raster_path)
    predictionLandscapesLandUse[[paste0(scenario, "_", year)]] <- raster
  }
}

# Load training- and predictionLandscapesLandUse for global extent and crop to defined extent
trainingLandscapesLandUse <- file.path(outputPathLandscapes, paste0("trainingLandscapesLandUse_", baseline_year, ".tif"))
trainingLandscapesLandUse <- terra::rast(trainingLandscapesLandUse)

extent_crs <- sf::st_transform(extent_sf, crs = crs(trainingLandscapesLandUse))
extent_sp <- terra::vect(extent_crs)
trainingLandscapesLandUse <- crop_mask_raster(trainingLandscapesLandUse, extent_sp)

predictionLandscapesLandUse <- list()
for (scenario in scenarios) {
  for (year in years) {
    raster_path <- file.path(outputPathLandscapes, paste0("predictionLandscapesLandUse_", scenario, "_", year, ".tif"))
    raster <- terra::rast(raster_path)
    raster <- crop_mask_raster(raster, extent_sp)
    predictionLandscapesLandUse[[paste0(scenario, "_", year)]] <- raster
  }
}

# Calculate spatial distirbution of land-use change -------------------------------------------

# Define, transform continents IF APPLICABLE
# Transform continent CRS to match the raster CRS
continents_crs <- st_transform(continents_sf, crs = st_crs(extent_sf))

# Define continent geometries
continent_geoms <- setNames(lapply(continent_names, function(continent) {
  continents_sf %>% dplyr::filter(continent == !!continent)
}), continent_names)

# Intersect the biome with the continents
extent_continents <- intersect_extent_continents(extent_sf, continent_geoms)


# Double check if this is correct, look for NA values
# Calculate the percentage changes
percentage_change_rasters_list <- calculate_percentage_changes(
  base_year_raster = trainingLandscapesLandUse,
  target_year_rasters_list = predictionLandscapesLandUse,
  scenarios = scenarios,
  years = years
)

# Create spatial distribution figures of land-use change -------------------------------------------

# Crop and mask the percentage change rasters to the desired continents IF APPLICABLE
cropped_rasters <- list()
for (year in names(percentage_change_rasters_list)) {
  cropped_rasters[[year]] <- list()
  
  for (scenario in names(percentage_change_rasters_list[[year]])) {
    cropped_rasters[[year]][[scenario]] <- list()
    
    for (class in names(percentage_change_rasters_list[[year]][[scenario]])) {
      percentage_change_raster <- percentage_change_rasters_list[[year]][[scenario]][[class]]
      
      # Crop and mask to continents
      cropped_rasters[[year]][[scenario]][[class]] <- lapply(continent_geoms, function(continent_geom) {
        crop_mask_raster(percentage_change_raster, continent_geom)
      })
    }
  }
}

# Create a custom color ramp with specified breakpoints
custom_color_ramp <- colorRamp2(c(-100, 0, 100), c("blue", "yellow", "red"))

# Set this flag to TRUE if you want continent-based plots, FALSE for global/region
use_continents <- exists("continent_names") && length(continent_names) > 0
for (class in names(trainingLandscapesLandUse)) {
  if (class == "Water") next
  
  for (year in as.character(years)) {
    plots_spatial <- list()
    
    for (scenario in scenarios) {
      if (use_continents) {
        for (continent in continent_names) {
          raster <- cropped_rasters[[year]][[scenario]][[class]][[continent]]
          coord_limits <- switch(
            continent,
            "North America" = list(xlim = c(-180, -40), ylim = c(20, 75)),
            "Europe & Asia" = list(xlim = c(-40, 180), ylim = c(20, 75)),
            NULL  # fallback
          )
          plot <- plot_landUse_spatialChanges(
            raster = raster,
            biome_geom = extent_continents[[continent]],
            color_ramp = custom_color_ramp,
            fill_label = "Change in %  ",
            min_value = -100,
            max_value = 100,
            coord_limits = coord_limits
          ) + theme(legend.position = "none")
          plots_spatial[[paste0(scenario, "_", tolower(continent))]] <- plot
        }
      } else {
        raster <- percentage_change_rasters_list[[year]][[scenario]][[class]]
        plot <- plot_landUse_spatialChanges(
          raster = raster,
          biome_geom = extent_sf, # or NULL if you don't want a boundary
          color_ramp = custom_color_ramp,
          fill_label = "Change in %  ",
          min_value = -100,
          max_value = 100
        ) + theme(legend.position = "none")
        plots_spatial[[scenario]] <- plot
      }
    }
    
    # Extract the legend from one of the plots
    example_plot <- if (use_continents) {
      plot_landUse_spatialChanges(
        raster = cropped_rasters[[year]][[scenarios[1]]][[class]][[continent_names[1]]],
        biome_geom = extent_continents[[continent_names[1]]],
        color_ramp = custom_color_ramp,
        fill_label = "Change in %  ",
        min_value = -100,
        max_value = 100
      )
    } else {
      plot_landUse_spatialChanges(
        raster = percentage_change_rasters_list[[year]][[scenarios[1]]][[class]],
        biome_geom = extent_sf,
        color_ramp = custom_color_ramp,
        fill_label = "Change in %  ",
        min_value = -100,
        max_value = 100
      )
    }
    shared_legend <- extract_legend(example_plot)
    
    # Arrange plots
    if (use_continents) {
      num_continents <- length(continent_names)
      combined_plot_spatial <- grid.arrange(
        arrangeGrob(
          grobs = lapply(continent_names, function(continent) {
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
    } else {
      combined_plot_spatial <- grid.arrange(
        grobs = lapply(seq_along(scenarios), function(i) {
          arrangeGrob(
            textGrob(scenario_names[i], gp = gpar(fontsize = 22)),
            plots_spatial[[scenarios[i]]],
            ncol = 1,
            heights = unit(c(0.5, 5), "null")
          )
        }),
        ncol = length(scenarios)
      )
    }
    
    # Combine the plot and legend side by side
    #    final_plot <- grid.arrange(
    #      combined_plot_spatial,
    #      arrangeGrob(
    #        grobs = list(shared_legend),
    #        ncol = 1
    #      ),
    #      ncol = 2,
    #      widths = unit(c(15, 3), "null"),
    #      top = textGrob(
    #        paste0("Land Use Change for ", class, " in the ", extent_name, " (", baseline_year, " vs. ", year, ")"),
    #        gp = gpar(fontsize = 24)
    #      )
    #    )
    
    # Combine the plot and legend below
    final_plot <- grid.arrange(
      combined_plot_spatial,
      shared_legend,
      ncol = 1,
      heights = unit(c(10, 2.5), "null"), #2.5 including country boundaries and area legend
      top = textGrob(
        paste0("Land-Use Change of ", class, " Areas in the ", extent_name, " (", baseline_year, " vs. ", year, ")"),
        gp = gpar(fontsize = 24)
      )
    )
    
    # Save the combined plot
    ggsave(
      filename = file.path(outputPathLandUse, paste0("LandUseSpatialChanges_", class, "_", year, "_", gsub(" ", "_", extent), ".png")),
      plot = final_plot,
      width = 1640, height = 900, dpi = 300 # adjust according to plot arrangements
    )
  }
}