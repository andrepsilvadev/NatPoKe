## Name: ClimateChange.R ##
## Authors: Jorinde-M. Rieger ##
## Description: Create figures: Projected changes in climatologies (bio1 & 12) for extent & Projected spatial changes in climatologies (bio1 & 12) for extent
## for the ssp126 and ssp585 scenarios for 2030, 2050, 2100 ##
## Date: August 4th 2025 ##

# Settings & libraries -------------------------------------------
#setwd("/mnt/data/jorinde") # set working directory
source("NatPoKe1/src/libraries.R") # libraries
source("NatPoKe1/src/customFunctions2.R") # functions

# Input variables -------------------------------------------
# Define input variables
scenarios <- c("ssp126", "ssp585") # define socio-economic pathways
scenario_names <- c("SSP1-RCP2.6", "SSP5-RCP8.5") # define socio-economic pathways names

# Define climatologies
variables <- c("bio1", # mean annual air temperature
               "bio12") # annual precipitation amount
variable_names <- c("Temperature (bio1)",
                    "Precipitation (bio12)")
y_labels <- c("Mean annual air temperature (bio1) (°C)",
              "Annual precipitation amount (bio12)(kg m-2 year-1)")
value_units <- c("°C", 
                 "kg m-2 year-1")

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
basePathClim <- "data/CHELSA_gfdl-esm4_V.2.1"
outputPathLandscapes <- "output/Landscapes"
if (!dir.exists(outputPathLandscapes)) {
  dir.create(outputPathLandscapes, recursive = TRUE)
}

# Define output path
outputPathClimate <- "output/ClimateChange"
if (!dir.exists(outputPathClimate)) {
  dir.create(outputPathClimate, recursive = TRUE)
}

# Create climate rasters as training landscapes (baseline) -------------------------------------------
trainingLandscapesClim <- list()
for (variable in variables) {
  raster <- load_baseline_clim(variable, baseline_yearOrigin) # Load the raster
  
  # Format extent object and crop the baseline raster
  extent_crs <- sf::st_transform(extent_sf, crs = crs(raster))
  extent_sp <- terra::vect(extent_crs)
  raster_extent <- crop_mask_raster(raster, extent_sp)
  
  # Aggregate to target resolution
  input_resolution <- terra::res(raster_extent)[1]  # Assuming square cells, take the resolution of the first dimension
  aggregation_factor <- round(target_resolution / input_resolution)
  raster_agg <- aggregate(raster_extent, fact = aggregation_factor, fun = mean)
  
  trainingLandscapesClim[[variable]] <- raster_agg
}

# Convert the list into a SpatRaster stack
trainingLandscapesClim <- terra::rast(trainingLandscapesClim)

# Rename layers to match variable names
names(trainingLandscapesClim) <- variables

# Save the trainingLandscape with the adapted baseline year
output_file <- file.path(outputPathLandscapes, paste0("trainingLandscapesClim_", baseline_year, "_", gsub(" ", "_", extent), "_5km.tif"))
terra::writeRaster(trainingLandscapesClim, output_file, overwrite = TRUE)

#plot(trainingLandscapesClim)

# Create climate rasters as prediction landscapes (future scenarios) -------------------------------------------
# Register parallel backend
num_cores <- min(parallel::detectCores() - 1, 10)  # Use up to 10 cores
cl <- parallel::makeCluster(num_cores)
doParallel::registerDoParallel(cl)

# Create raster of averaged GCMs in a paralleled loop
foreach::foreach(scenario = scenarios, .combine = 'c', .packages = c("terra", "sf")) %:%
  foreach::foreach(yearOrigin = yearsOrigin, .combine = 'c') %dopar% {
    raster_list <- list() # Create a list for each scenario-year combination
    
    for (variable in variables) {
      # average climate models and save them
      raster <- average_climate_models(basePathClim, scenario, yearOrigin, variable)
      raster_list[[variable]] <- raster
    }
  }

# Stop the cluster
stopCluster(cl)

# Create prediction landscapes for climate variables
predictionLandscapesClim <- list()

for (scenario in scenarios) {
  for (yearOrigin in yearsOrigin) {
    raster_list <- list() # Create a list for each scenario-year combination
    for (variable in variables) {
      # Load averaged raster of climate models
      raster <- load_average_scenario_clim(outputPathLandscapes, scenario, yearOrigin, variable)
      
      # Format extent object and crop the baseline raster
      extent_crs <- sf::st_transform(extent_sf, crs = crs(raster))
      extent_sp <- terra::vect(extent_crs)
      raster_extent <- crop_mask_raster(raster, extent_sp)
      
      # Aggregate to target resolution
      input_resolution <- terra::res(raster_extent)[1]  # Assuming square cells, take the resolution of the first dimension
      aggregation_factor <- round(target_resolution / input_resolution)
      raster_agg <- aggregate(raster_extent, fact = aggregation_factor, fun = mean)
      
      # Store the processed raster in the list
      raster_list[[variable]] <- raster_agg
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
    year <- yearsMapping[yearOrigin] # Get the adapted year
    
    # Save the prediction landscape
    output_file <- file.path(outputPathLandscapes, paste0("predictionLandscapesClim_", scenario, "_", year,"_", gsub(" ", "_", extent), "_5km.tif"))
    terra::writeRaster(predictionLandscapesClim[[paste0(scenario, "_", yearOrigin)]], output_file, overwrite = TRUE)
  }
}

#plot(predictionLandscapesClim[["ssp126_2071-2100"]])

# Load the training and prediction Landscapes for climate variables -------------------------------------------
trainingLandscapesClim <- file.path(outputPathLandscapes, paste0("trainingLandscapesClim_", baseline_year, "_", gsub(" ", "_", extent),"_5km.tif"))
trainingLandscapesClim <- terra::rast(trainingLandscapesClim)

# Load predictionLandscapesClim
predictionLandscapesClim <- list()
# Loop through scenarios and years
for (scenario in scenarios) {
  for (year in years) {
    raster_path <- file.path(outputPathLandscapes, paste0("predictionLandscapesClim_", scenario, "_", year,"_", gsub(" ", "_", extent), "_5km.tif"))
    raster <- terra::rast(raster_path)
    predictionLandscapesClim[[paste0(scenario, "_", year)]] <- raster
  }
}

# Calculate and create Figure: Projected changes in climatologies (bio1 & 12) for extent -------------------------------------------
# Loop through the variables and years to extract mean values
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

# Create labels (a) and (b)
label_grobs <- gridExtra::arrangeGrob(
  grobs = list(
    textGrob("(a)", gp = gpar(fontsize = 16), hjust = 0.5),
    textGrob("(b)", gp = gpar(fontsize = 16), hjust = 0.5)
  ),
  ncol = 2
)

# Arrange the 2 plots in a 1x2 grid
plots_grid <- gridExtra::arrangeGrob(
  label_grobs,
  gridExtra::arrangeGrob(
    grobs = lapply(plots, function(p) p + theme(legend.position = "none")),
    ncol = 2,
    nrow = 1
  ),
  ncol = 1,
  heights = unit(c(0.5, 5), "null"),
  top = textGrob(extent_name, gp = gpar(fontsize = 18))
)

# Combine with the shared legend on the right
combined_plot_time <- gridExtra::grid.arrange(
  plots_grid,
  shared_legend,
  ncol = 1,
  heights = unit(c(5, 1), "null")
)

# Save the combined plot
ggplot2::ggsave(
  filename = file.path(outputPathClimate, "ClimateChange_time_", gsub(" ", "_", extent), ".png"),
  plot = combined_plot_time,
  width = 10, height = 6, dpi = 600
)


# Calculate and create spatially explicit Climate Change Maps -------------------------------------------
# Set this flag to TRUE if you want continent-based plots, FALSE for global/region
use_continents <- !is.null(continent_names) && length(continent_names) > 0

if (use_continents) {
  # transform, crop and mask the rasters to the continent geometries
  continents_crs <- sf::st_transform(continents_sf, crs(trainingLandscapesClim))
  continent_geoms <- setNames(lapply(continent_names, function(continent) {
    continents_crs %>% dplyr::filter(continent == !!continent)
  }), continent_names)
  extent_continents <- intersect_extent_continents(extent_sf, continent_geoms) # Intersect the extent with the continents
}

# Calculate changes and crop/mask
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

# Calculate min/max values for colour scale
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

# Create color ramps
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

# Plotting
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
      shared_legend <- extract_legend(example_plot)
      
      # Combine plots
      if (use_continents) {
        num_continents <- length(continent_names)
        combined_plot_spatial <- gridExtra::grid.arrange(
          gridExtra::arrangeGrob(
            grobs = lapply(continent_title, function(continent) {
              textGrob(continent, gp = gpar(fontsize = 22))
            }),
            ncol = num_continents,
            heights = unit(c(0.5), "null")
          ),
          gridExtra::arrangeGrob(
            grobs = c(
              list(textGrob(scenario_names[1], rot = 90, gp = gpar(fontsize = 22))),
              lapply(continent_names, function(continent) {
                plots_spatial[[paste0(scenarios[1], "_", tolower(continent))]]
              })
            ),
            ncol = num_continents + 1,
            widths = unit(c(0.5, rep(5, num_continents)), "null")
          ),
          gridExtra::arrangeGrob(
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
        combined_plot_spatial <- gridExtra::grid.arrange(
          grobs = lapply(seq_along(scenarios), function(i) {
            gridExtra::arrangeGrob(
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
      final_plot <- gridExtra::grid.arrange(
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
      ggplot2::ggsave(filename = file.path(outputPathClimate,
                                           paste0("ClimateSpatialChange_", variable, year, "_", gsub(" ", "_", extent_name), ".png")),
                      plot = final_plot,
                      width = 15, height = 8, dpi = 300)
    }
  }
}
gc()