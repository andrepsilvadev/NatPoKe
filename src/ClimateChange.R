## Name: ClimateChange.R ##
## Authors: Jorinde-M. Rieger ##
## Description: Applies functions to calculate spatial explicit temperature and precipitation change in a given Biome
## for the ssp126 and ssp585 scenarios in various time periods ##
## Date: June 10th 2025 ##

# Settings & libraries -------------------------------------------
source("~/NatPoKe9/src/libraries.R") # libraries
source("~/NatPoKe9/src/customFunctions2.R") # functions

# Input variables -------------------------------------------
# Define input variables
scenarios <- c("ssp126", "ssp585") # define socio-economic pathways
scenario_names <- c("SSP1-RCP2.6", "SSP5-RCP8.5") # define socio-economic pathways names

# Define climatologies
variables <- c("bio1", # mean annual air temperature
               "bio10", # mean daily mean air temperatures of the warmest quarter
               "bio11", # mean daily mean air temperatures of the coldest quarter
               "bio12", # annual precipitation amount
               "bio16", # mean monthly precipitation amount of the wettest quarter
               "bio17") # mean monthly precipitation amount of the driest quarter
variable_names <- c("Temperature (bio1)", "Temperature (bio10)", "Temperature (bio11)", 
                    "Precipitation (bio12)", "Precipitation (bio16)", "Precipitation (bio17)")
y_labels <- c("Mean annual air temperature (bio1) (°C)", 
              "Mean daily air temp. (bio10) (°C)",
              "Mean daily air temp. (bio11) (°C)",
              "Annual precipitation amount (bio12)(kg m-2 year-1)",
              "Mean monthly precip. (bio16) (kg m-2 month-1)",
              "Mean monthly precip. (bio17) (kg m-2 month-1)"
              )
value_units <- c("°C", "°C", "°C", "kg m-2 year-1", "kg m-2 month-1", "kg m-2 month-1")

# Define years
years <- c(2030, 2050, 2100)
baseline_year <- 2015
yearsOrigin <- c("2011-2040", "2041-2070", "2071-2100") # original in time periods"2011-2040", "2041-2070", "2071-2100"
baseline_yearOrigin <- "1981-2010"
# Map time periods to adapted years
yearsMapping <- setNames(years, yearsOrigin)

# Climate Models (GCMs)
models <- c("gfdl-esm4", "ipsl-cm6a-lr", "mpi-esm1-2-hr", "mri-esm2-0", "ukesm1-0-ll")

# Define extent
# Global Terrestrial
extent = "Global Terrestrial"
extent_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Iberian peninsula
extent = "Iberian peninsula"
extent_name <- "Spain & Portugal"
extent_sf <- rnaturalearth::ne_countries(scale = "medium", country = c("Spain", "Portugal"), returnclass = "sf")
continent_names <- NULL
continent_title <- NULL
continent_geoms <- NULL
outputPathClimate <- "~/data/BoS/output/ClimateChange"
if (!dir.exists(outputPathClimate)) {
  dir.create(outputPathClimate, recursive = TRUE)
}

# Tropical Biome
extent <- "Tropical & Subtropical Moist Broadleaf Forests"
extent_name <- "Tropical Biome"
extent_sf <- load_biome(extent)
continent_names <- c("Central & South America", "Africa", "Asia")
continent_title <- c("Central & South America", "Africa", "Asia")
continents_sf <- load_select_continents(continent_names)

# Boreal Biome
biome_name <- "Boreal Forests/Taiga"
biome_name_short <- "Boreal Biome"
extent_sf <- load_biome(extent)
continent_names <- c("North America", "Europe")
continent_title <- c("North America", "Europe & Asia")
continents_sf <- load_select_continents(continent_names)

# Define the file paths
basePathClim <- "~/data/data/CHELSA_gfdl-esm4_V.2.1"
outputPathLandscapes <- "~/data/output/Landscapes"
if (!dir.exists(outputPathLandscapes)) {
  dir.create(outputPathLandscapes, recursive = TRUE)
}

# Define output path
#outputPathClimate <- "~/data/output/ClimateChange"
#if (!dir.exists(outputPathClimate)) {
#  dir.create(outputPathClimate, recursive = TRUE)
#}

# Create environmental input Data (climate) as training landscapes -------------------------------------------
# Create an empty list to store climate training Landscapes
trainingLandscapesClim <- list()
# Loop through the training landscapes
for (variable in variables) {
  # Load the raster
  raster <- load_baseline_clim(variable, baseline_yearOrigin)
  
  # Ensure CRS consistency
  extent_crs <- sf::st_transform(extent_sf, crs = crs(raster))
  
  # Convert the sf to a spatial object
  extent_sp <- terra::vect(extent_crs)
  
  # Crop and mask the raster
  raster_extent <- crop_mask_raster(raster, extent_sp)
  
  trainingLandscapesClim[[variable]] <- raster_extent
}

# Convert the list into a SpatRaster stack
trainingLandscapesClim <- terra::rast(trainingLandscapesClim)

# Rename layers to match variable names
names(trainingLandscapesClim) <- variables

# Save the trainingLandscape with the adapted baseline year
output_file <- file.path(outputPathClimate, paste0("trainingLandscapesClim_", baseline_year, ".tif"))
terra::writeRaster(trainingLandscapesClim, output_file, overwrite = TRUE)

# Test the rasters
plot(trainingLandscapesClim)

# Create environmental input Data (climate) as prediction landscapes -------------------------------------------
# Register parallel backend
num_cores <- min(parallel::detectCores() - 1, 10)  # Use up to 10 cores
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Create an empty list to store all processed rasters
#all_rasters <- list()

# Create raster of averaged GCMs in a paralleled loop
foreach(scenario = scenarios, .combine = 'c', .packages = c("terra", "sf")) %:%
  foreach(yearOrigin = yearsOrigin, .combine = 'c') %dopar% {
    # Create a list for each scenario-year combination
    raster_list <- list()
    
    for (variable in variables) {
      # Load averaged raster of climate models
      raster <- average_climate_models(basePathClim, scenario, yearOrigin, variable)
      
      # Store the processed raster in the list
      raster_list[[variable]] <- raster
    }
    
    # Combine the rasters for this scenario and year into a SpatRaster stack
    #combined_raster <- terra::rast(raster_list)
    
    # Return the combined raster as a list element
    #list(paste0(scenario, "_", yearOrigin) = combined_raster)
  }

# Stop the cluster
stopCluster(cl)

# Create an empty list to store prediction landscapes
predictionLandscapesClim <- list()

for (scenario in scenarios) {
  for (yearOrigin in yearsOrigin) {
    
    # Create a list for each scenario-year combination
    raster_list <- list()
    
    for (variable in variables) {
      # Load averaged raster of climate models
      raster <- load_average_scenario_clim(basePathClim, scenario, yearOrigin, variable)
      
      # Extent
      #extent_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
      
      # Ensure CRS consistency
      #extent_crs <- sf::st_transform(extent_sf, crs = crs(raster))
      
      # Convert the sf to a spatial object
      #extent_sp <- terra::vect(extent_crs)
      
      # Crop and mask the raster
      raster_extent <- crop_mask_raster(raster, extent_sp)
      
      # Store the processed raster in the list
      raster_list[[variable]] <- raster_extent
    }
    
    # Convert the list of rasters into a SpatRaster stack
    predictionLandscapesClim[[paste0(scenario, "_", yearOrigin)]] <- terra::rast(raster_list)
  }
}

# Rename raster layers within each stack to match variable names
for (i in seq_along(predictionLandscapesClim)) {
  names(predictionLandscapesClim[[i]]) <- variables
}

# Save the climate prediction landscapes  with adapted years
for (scenario in scenarios) {
  for (yearOrigin in yearsOrigin) {
    # Get the adapted year
    year <- yearsMapping[yearOrigin]
    
    # Save the prediction landscape
    output_file <- file.path(outputPathClimate, paste0("predictionLandscapesClim_", scenario, "_", year, ".tif"))
    terra::writeRaster(predictionLandscapesClim[[paste0(scenario, "_", yearOrigin)]], output_file, overwrite = TRUE)
  }
}

# Print the structure of the final list
#print(predictionLandscapesClim)
#plot(predictionLandscapesClim[["ssp126_2071-2100"]])

# Load the training and prediction Landscapes for the global scale and crop to defined extent -------------------------------------------
trainingLandscapesClim <- file.path(outputPathLandscapes, paste0("trainingLandscapesClim_", baseline_year, ".tif"))
trainingLandscapesClim <- terra::rast(trainingLandscapesClim)

# Crop the training landscapes to the defined extent
extent_crs <- sf::st_transform(extent_sf, crs = crs(trainingLandscapesClim))
extent_sp <- terra::vect(extent_crs)

trainingLandscapesClim <- crop_mask_raster(trainingLandscapesClim, extent_sp)

#Load predictionLandscapes and crop to defined extent
predictionLandscapesClim <- list()
# Loop through scenarios and years
for (scenario in scenarios) {
  for (year in years) {
    raster_path <- file.path(outputPathLandscapes, paste0("predictionLandscapesClim_", scenario, "_", year, ".tif"))
    raster <- terra::rast(raster_path)
    raster <- crop_mask_raster(raster, extent_sp)
    predictionLandscapesClim[[paste0(scenario, "_", year)]] <- raster
  }
}

# Loop through the variables and years to extract mean values and create plots
mean_values_list <- list()
for (variable in variables) {
  for (year in years) {
    for (scenario in scenarios) {
      raster_stack <- predictionLandscapesClim[[paste0(scenario, "_", year)]]
      layer <- raster_stack[[variable]]
      mean_val <- as.numeric(terra::global(layer, fun = "mean", na.rm = TRUE))
      mean_values_list[[paste(variable, scenario, year, sep = "_")]] <- data.frame(
        Variable = variable,
        Scenario = scenario,
        Year = year,
        Mean = mean_val,
        Value_Type = variable_names[which(variables == variable)]
      )
    }
  }
}
mean_values_df <- do.call(rbind, mean_values_list)

# change name of scenarios to readable names
mean_values_df <- mean_values_df %>%
  mutate(Scenario = recode(Scenario,
                           "ssp126" = "SSP1-RCP2.6",
                           "ssp585" = "SSP5-RCP8.5"))

# HOW TO INCLUDE?
# Define consistent color palette for the scenarios
scenario_colors <- setNames(
  c("#1f77b4", "#ff7f0e"), scenario_names)

# Split data by variable for plotting
variable_dfs <- lapply(variable_names, function(vn) {
  mean_values_df %>% filter(Value_Type == vn)
})
names(variable_dfs) <- variable_names

# Automate plotting for all variables
plots <- lapply(seq_along(variable_names), function(i) {
  plot_timeChanges(variable_dfs[[i]], variable_names[i], y_labels[i])
})
names(plots) <- variable_names

# Extract the shared legend from one of the plots
shared_legend <- extract_legend(plots[["Temperature (bio1)"]])

# Arrange the 6 plots in a 2x3 grid
plots_grid <- arrangeGrob(
  grobs = lapply(plots, function(p) p + theme(legend.position = "none")), #plots + theme(legend.position = "none"),
  ncol = 3,
  nrow = 2,
  top = textGrob(extent_name, gp = gpar(fontsize = 18))
)

# Combine with the shared legend on the right
combined_plot_time <- grid.arrange(
  plots_grid,
  shared_legend,
  ncol = 1,
  heights = unit(c(10, 1), "null")
)

# Save the combined plot
ggsave(
  filename = file.path(outputPathClimate, "Climate_timeChanges_6vars_", gsub(" ", "_", extent_name), ".png"),
  plot = combined_plot_time,
  width = 18, height = 10, dpi = 600
)


# Calculate and create spatially explicit Climate Change Maps -------------------------------------------
# Set this flag to TRUE if you want continent-based plots, FALSE for global/region
use_continents <- !is.null(continent_names) && length(continent_names) > 0

if (use_continents) {
  # Transform continent CRS to match the raster CRS
  continents_crs <- st_transform(continents_sf, crs(trainingLandscapesClim))
  # Define continent geometries
  continent_geoms <- setNames(lapply(continent_names, function(continent) {
    continents_crs %>% dplyr::filter(continent == !!continent)
  }), continent_names)
  # Intersect the extent with the continents
  extent_continents <- intersect_extent_continents(extent_sf, continent_geoms)
}

# ---- Calculate changes and crop/mask ----
for (variable in variables) {
  baseline_raster <- trainingLandscapesClim[[variable]]
  for (scenario in scenarios) {
    for (year in years) {
      pred_stack <- predictionLandscapesClim[[paste0(scenario, "_", year)]]
      pred_raster <- pred_stack[[variable]]
      change_raster <- pred_raster - baseline_raster
      
      if (use_continents) {
        # Crop and mask to continents
        change_rasters <- lapply(continent_geoms, function(continent_geom) {
          crop_mask_continent(change_raster, continent_geom)
        })
        for (continent in names(change_rasters)) {
          assign(paste0("Change_", scenario, "_", variable, "_", year, "_", tolower(continent)), change_rasters[[continent]])
        }
      } else {
        # No continents: just assign the change raster
        assign(paste0("Change_", scenario, "_", variable, "_", year, "_", extent_name), change_raster)
      }
    }
  }
}

# ---- Calculate min/max values ----
min_values <- list()
max_values <- list()
for (variable in variables) {
  min_value <- Inf
  max_value <- -Inf
  for (year in years) {
    for (scenario in scenarios) {
      if (use_continents) {
        for (continent in continent_names) {
          raster_stack <- get(paste0("Change_", scenario, "_", variable, "_", year, "_", tolower(continent)))
          min_value <- min(min_value, min(values(raster_stack), na.rm = TRUE))
          max_value <- max(max_value, max(values(raster_stack), na.rm = TRUE))
        }
      } else {
        raster_stack <- get(paste0("Change_", scenario, "_", variable, "_", year, "_", extent_name))
        min_value <- min(min_value, min(values(raster_stack), na.rm = TRUE))
        max_value <- max(max_value, max(values(raster_stack), na.rm = TRUE))
      }
    }
  }
  min_values[[variable]] <- round(min_value)
  max_values[[variable]] <- round(max_value)
}

# ---- Create color ramps ----
color_ramps <- list()
for (i in seq_along(variables)) {
  if (i <= 3) {
    color_ramps[[variables[i]]] <- colorRamp2(
      c(min_values[[variables[i]]], 0, max_values[[variables[i]]]),
      c("blue", "white", "red")
    )
  } else {
    color_ramps[[variables[i]]] <- colorRamp2(
      c(min_values[[variables[i]]], 0, max_values[[variables[i]]]),
      c("saddlebrown", "yellow", "darkgreen")
    )
  }
}

# ---- Plotting ----
for (variable in variables) {
  for (year in years) {
    for (scenario in scenarios) {
      color_ramp <- color_ramps[[variable]]
      var_idx <- which(variables == variable)
      fill_label <- paste("Change in", value_units[var_idx], " ")
      min_value <- min_values[[variable]]
      max_value <- max_values[[variable]]
      
      plots_spatial <- list()
      for (scenario in scenarios) {
      if (use_continents) {
        for (continent in continent_names) {
          plot <- plot_ClimatespatialChanges(
            raster = get(paste0("Change_", scenario, "_", variable, "_", year, "_", tolower(continent))),
            extent_geom = extent_continents[[continent]],
            color_ramp = color_ramp,
            fill_label = fill_label,
            min_value = min_value,
            max_value = max_value
          ) + theme(legend.position = "none")
          plots_spatial[[paste0(scenario, "_", tolower(continent))]] <- plot
        }
      } else {
        plot <- plot_ClimatespatialChanges(
          raster = get(paste0("Change_", scenario, "_", variable, "_", year, "_", extent_name)),
          extent_geom = extent_sf,
          color_ramp = color_ramp,
          fill_label = fill_label,
          min_value = min_value,
          max_value = max_value
        ) + theme(legend.position = "none")
        plots_spatial[[scenario]] <- plot
      }
      }
      # Extract the legend
      example_plot <- if (use_continents) {
        plot_ClimatespatialChanges(
          raster = get(paste0("Change_", scenarios[1], "_", variable, "_", year, "_", tolower(continent_names[1]))),
          extent_geom = extent_continents[[continent_names[1]]],
          color_ramp = color_ramp,
          fill_label = fill_label,
          min_value = min_value,
          max_value = max_value
        )
      } else {
        plot_ClimatespatialChanges(
          raster = get(paste0("Change_", scenarios[1], "_", variable, "_", year, "_", extent_name)),
          extent_geom = extent_sf,
          color_ramp = color_ramp,
          fill_label = fill_label,
          min_value = min_value,
          max_value = max_value
        )
      }
      
      # Extract legends
      #legend_colorbar <- extract_legend(example_plot + guides(color = "none"))
      #legend_discrete <- extract_legend(example_plot + guides(fill = "none"))
      #shared_legend <- cowplot::plot_grid(legend_colorbar, legend_discrete, nrow = 1, rel_widths = c(2, 1))
      
      shared_legend <- extract_legend(example_plot)
      
      # Combine plots
      if (use_continents) {
        num_continents <- length(continent_names)
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
      } else {
#        combined_plot_spatial <- grid.arrange(
#          grobs = lapply(scenarios, function(scenario) {
#            plots_spatial[[scenario]]
#          }),
#          ncol = length(scenarios)
#        )
#      }
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
      # Combine plot and legend (legend below)
      final_plot <- grid.arrange(
        combined_plot_spatial,
        shared_legend,
        ncol = 1,
        heights = unit(c(10, 2.5), "null"), #2.5 including country boundaries and area legend
        top = textGrob(
          paste0(variable_names[var_idx], " ", fill_label, " for the ", extent_name, " (", baseline_year, " vs. ", year, ")"),
          gp = gpar(fontsize = 24)
        )
      )
      
      # Save the plot
      ggsave(filename = file.path(outputPathClimate,
                                  paste0("ClimateSpatialChange_", variable, year, "_", gsub(" ", "_", extent_name), ".png")),
             plot = final_plot,
             width = 15, height = 8, dpi = 300)
    }
  }
}
