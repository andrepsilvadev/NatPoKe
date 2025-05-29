## Name: ClimateChange.R ##
## Authors: Jorinde-M. Rieger ##
## Description: Applies functions to calculate spatial explicit temperature and precipitation change in a given Biome
## for the ssp126 and ssp585 scenarios in various time periods ##
## Date: May 4th 2025 ##

# Settings & libraries -------------------------------------------
source("~/NatPoKe9/src/libraries.R") # libraries
source("~/NatPoKe9/src/customFunctions2.R") # functions

# Input variables -------------------------------------------
# Define input variables
scenarios <- c("ssp126", "ssp585")
scenario_names <- c("SSP1-RCP2.6", "SSP5-RCP8.5")
variables <- c("bio1", "bio12")
variable_names <- c("Temperature", "Precipitation")
y_labels <- c("Annual Daily Mean Air Temperatures (°C)", "Annual Mean Precipitation Amount (kg m-2 year-1)")
value_units <- c("°C", "kg m-2 year-1")
years <- c("2011-2040", "2041-2070", "2071-2100") # first year/timeperiod will be used as a baseline for change calculation

# Define the file paths
basePathClim <- "~/data/data/CHELSA_gfdl-esm4_V.2.1"
outputPathClim <- "~/data/data/CHELSA_gfdl-esm4_V.2.1/outputData"
output_folder <- "~/data/output"

# Define the target resolution (based on the landUsePercentage rasters)
target_resolution <- 0.277

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
# Load the selected biome
biome_sf <- load_select_biome(biome_name)

# Loop through the scenarios, variables, and years for the biome
# L apply
for (scenario in scenarios) {
  for (variable in variables) {
    for (year in years) {
      # Load the raster
      raster <- load_scenario_clim(scenario, variable, year)
      
      # Get the original resolution from the raster
      original_resolution <- res(raster)[1]
      
      # Calculate the aggregation factor
      aggregation_factor <- target_resolution / original_resolution
      
      # Aggregate the raster
      raster_agg <- aggregate_raster(raster, aggregation_factor)
      
      # Ensure CRS consistency
      biome_sf <- st_transform(biome_sf, crs = crs(raster_agg))
      
      # Convert the sf to a spatial object
      biome_sp <- vect(biome_sf)
      
      # Crop and mask the raster
      raster_biome <- crop_mask_raster(raster_agg, biome_sp)
      
      # Save the aggregated raster
      output_file <- file.path(outputPathClim, paste0("ClimateChange_", scenario, "_", variable, "_", year, "_", biome_name_short, ".tif"))
      writeRaster(raster_biome, output_file, overwrite = TRUE)
      
      # Assign the raster to a variable dynamically
      assign(paste0("ClimateChange_", scenario, "_", variable, "_", year, "_", biome_name_short), raster_biome)
    }
  }
}

# Loop through the variables and years to create raster stacks
for (variable in variables) {
  for (year in years) {
    stack_clim_rasters(variable, year)
  }
}

# Calculate and create Climate Change graphics over time -------------------------------------------
# Define consistent color palette for the scenarios
scenario_colors <- setNames(
  c("#1f77b4", "#ff7f0e"), scenario_names)

# Loop through the variables and years to extract mean values and create plots
mean_values_list <- list()
for (variable in variables) {
  for (year in years) {
    raster_stack <- get(paste0("scenarios_stack_", variable, "_", year, "_", gsub(" ", "_", biome_name_short)))
    if (variable == variables[1]) {
      mean_values_list[[paste0(variable, "_", year)]] <- extract_mean_values(raster_stack, rep(year, each = nlyr(raster_stack)), variable_names[1])
    } else {
      mean_values_list[[paste0(variable, "_", year)]] <- extract_mean_values(raster_stack, rep(year, each = nlyr(raster_stack)), variable_names[2])
    }
  }
}

# Combine the data frames into a single data frame
mean_values_df <- do.call(rbind, mean_values_list)

# Remove the year suffix from scenario names
mean_values_df <- mean_values_df %>%
  mutate(Scenario = gsub("_\\d{4}-\\d{4}$", "", Scenario))

# Split the data frame into data frames according to variables
variable1_df <- mean_values_df %>% filter(Value_Type == variable_names[1])
variable2_df <- mean_values_df %>% filter(Value_Type == variable_names[2])

# Create plots
scenarios_variable1_plot <- plot_timeChanges(variable1_df, variable_names[1], y_labels[1])
scenarios_variable2_plot <- plot_timeChanges(variable2_df, variable_names[2], y_labels[2])

# Extract the shared legend from one of the plots
shared_legend <- extract_legend(scenarios_variable1_plot)

# Arrange the plots side by side with the shared legend
combined_plot_time <- grid.arrange(
  arrangeGrob(
    arrangeGrob(
      textGrob("(a)", gp = gpar(fontsize = 16)),
      textGrob("(b)", gp = gpar(fontsize = 16)),
      ncol = 2,
      heights = unit(c(0.5), "null")
    ),
    arrangeGrob(
      scenarios_variable1_plot + theme(legend.position = "none"),
      scenarios_variable2_plot + theme(legend.position = "none"), 
      ncol = 2
    ),
    ncol = 1,
    heights = unit(c(0.5, 5), "null")
  ),
  arrangeGrob(
    grobs = list(shared_legend),  # Add the shared legend
    ncol = 1
  ),
  ncol = 2,  # Two columns: one for the plots and one for the legend
  widths = unit(c(10, 2), "null"),  # Adjust the width ratio between plots and legend
  top = textGrob(biome_name_short, gp = gpar(fontsize = 18))
)

# Save the combined plot
ggsave(filename = file.path(output_folder, 
                            paste0("ClimateChange_", variable, "_timeChanges_", year, "_", gsub(" ", "_", biome_name_short), ".png")),
       plot = combined_plot_time,
       width = 14, height = 7, dpi = 600)


# Calculate and create spatially explicit Climate Change Maps -------------------------------------------
# Load and select the continents
continents <- load_select_continents(continent_names)

# Transform continent CRS to match the raster CRS
continents <- st_transform(continents, crs(load_raster(scenarios[1], variables[1], years[1])))

# Define continent geometries
continent_geoms <- setNames(lapply(continent_names, function(continent) {
  continents %>% dplyr::filter(continent == !!continent)
}), continent_names)

# Validate the geometries, corrects geometries
#biome_sf <- st_make_valid(biome_sf)

# Intersect the biome with the continents
biome_continents <- intersect_biome_with_continents(biome_sf, continent_geoms)

# Calculate changes and crop/mask to continents
for (variable in variables) {
  for (scenario in scenarios) {
    for (year in years[-1]) {
      # Calculate changes
      change_raster <- calculate_change(
        get(paste0("ClimateChange_", scenario, "_", variable, "_", year, "_", biome_name_short)),
        get(paste0("ClimateChange_", scenario, "_", variable, "_", years[1], "_", biome_name_short))
      )
      
      # Crop and mask to continents
      change_rasters <- lapply(continent_geoms, function(continent_geom) {
        crop_mask_continent(change_raster, continent_geom)
      })
      
      # Assign the cropped/masked rasters to variables dynamically
      for (continent in names(change_rasters)) {
        assign(paste0("change_", scenario, "_", variable, "_", year, "_", tolower(continent)), change_rasters[[continent]])
      }
    }
  }
}

# Calculate the minimum and maximum values for the variables
min_values <- list()
max_values <- list()

for (variable in variables) {
  min_value <- Inf
  max_value <- -Inf
  for (year in years[-1]) {
    for (scenario in scenarios) {
      for (continent in continent_names) {
        raster_stack <- get(paste0("change_", scenario, "_", variable, "_", year, "_", tolower(continent)))
        min_value <- min(min_value, min(values(raster_stack), na.rm = TRUE))
        max_value <- max(max_value, max(values(raster_stack), na.rm = TRUE))
      }
    }
  }
  min_values[[variable]] <- round(min_value)
  max_values[[variable]] <- round(max_value)
}

# Create custom color ramps based on the calculated min and max values
color_ramps <- list(
  var1 = colorRamp2(c(min_values[[variables[1]]], 0, max_values[[variables[1]]]), c("blue", "white", "red")),
  var2 = colorRamp2(c(min_values[[variables[2]]], 0, max_values[[variables[2]]]), c("saddlebrown", "yellow", "darkgreen"))
)

# Create plots for each scenario, variable, and year
for (variable in variables) {
  for (year in years[-1]) {
    for (scenario in scenarios) {
      # Define the color ramp and fill label based on the variable
      if (variable == variables[1]) {
        color_ramp <- color_ramps[["var1"]]
        fill_label <- paste("Change in", value_units[1])
        min_value <- min_values[[variables[1]]]
        max_value <- max_values[[variables[1]]]
      } else {
        color_ramp <- color_ramps[["var2"]]
        fill_label <- paste("Change in", "\n", value_units[2])
        min_value <- min_values[[variables[2]]]
        max_value <- max_values[[variables[2]]]
      }
      
      # Create plots for each continent and scenario
      plots_spatial <- list()
      for (scenario in scenarios) {
        for (continent in names(continent_geoms)) {
          plot <- plot_ClimatespatialChanges(
            raster = get(paste0("change_", scenario, "_", variable, "_", year, "_", tolower(continent))), 
            biome_geom = biome_continents[[continent]], 
            color_ramp = color_ramp, 
            fill_label = fill_label, 
            min_value = min_value, 
            max_value = max_value
          )+
            theme(legend.position = "none")  # Remove individual legends
          
          plots_spatial[[paste0(scenario, "_", tolower(continent))]] <- plot
        }
      }
      # Extract the legend dynamically for the current variable
      example_plot <- plot_ClimatespatialChanges(
        raster = get(paste0("change_", scenarios[1], "_", variable, "_", year, "_", tolower(continent_names[1]))),
        biome_geom = biome_continents[[continent_names[1]]],
        color_ramp = color_ramp,
        fill_label = fill_label,
        min_value = min_value,
        max_value = max_value
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
          widths = unit(c(15, 4), "null"),
          top = textGrob(
            paste0(ifelse(variable == variables[1], variable_names[1], variable_names[2]), " ", fill_label, " for the ", biome_name_short, " (", years[1], " vs. ", year,")"), 
            gp = gpar(fontsize = 24)
        )
      )
      
      # Save the combined plot
      ggsave(filename = file.path(output_folder, 
                                  paste0("ClimateChange_", variable, "_spatialChanges_", year, "_", gsub(" ", "_", biome_name_short), ".png")), 
             plot = final_plot, 
             width = 15, height = 6, dpi = 300)
    }
  }
}
