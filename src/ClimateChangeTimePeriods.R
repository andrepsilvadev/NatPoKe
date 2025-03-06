## Name: ClimateChangeTimePeriods.R ##
## Authors: Jorinde-M. Rieger ##
## Description: Applies functions to calculate spatial explicit temperature and precipitation change in a given Biome
  ## for the ssp126 and ssp585 scenarios in various time periods ##
## Date: March 5th 2025 ##

# Settings & libraries -------------------------------------------
source("~/data/src/libraries.R") # libraries
source("~/data/src/customFunctions.R") # functions

# Input variables -------------------------------------------
# Define input variables of CHELSA rasters
scenarios <- c("ssp126", "ssp585")
scenario_names <- c("SSP1-RCP2.6", "SSP5-RCP8.5")
variables <- c("bio1", "bio12")
variable_names <- c("Temperature", "Precipitation")
y_labels <- c("Annual Daily Mean Air Temperatures (°C)", "Annual Mean Precipitation Amount (kg m-2 year-1)")
years <- c("2011-2040", "2041-2070", "2071-2100") # first year/timeperiod will be used as a baseline for change calculation

# Define the file paths
base_path <- "~/data/data/CHELSA_gfdl-esm4_V.2.1"
output_path <- "~/data/data/CHELSA_gfdl-esm4_V.2.1/outputData"
output_folder <- "~/data/output"

# Define the target resolution (based on the landUsePercentage rasters)
target_resolution <- 0.277

# Define the biome and continents
biome_name <- "Tropical & Subtropical Moist Broadleaf Forests"
continent_names <- c("Africa", "Asia", "South America")


# Functions - later add them to CustomFunctions.R -------------------------------------------
# Function to load rasters
load_raster <- function(scenario, variable, year) {
  file_path <- file.path(base_path, scenario, paste0("CHELSA_", variable, "_", year, "_gfdl-esm4_", scenario, "_V.2.1.tif"))
  rast(file_path)
}

# Function to aggregate rasters
aggregate_raster <- function(raster, aggregation_factor) {
  aggregate(raster, aggregation_factor, fun = mean)
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

# Function to calculate changes
calculate_change <- function(raster_future, raster_present) {
  raster_future - raster_present
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
    theme_minimal()
}

# Function to load and select continents
# add/create a list function
load_select_continents <- function(continent_names) {
  continents <- ne_countries(scale = "medium", returnclass = "sf") %>%
    dplyr::filter(continent %in% continent_names) %>% 
    group_by(continent) %>%
    summarise(geometry = st_union(geometry))
  continents
}

# Function to crop and mask the rasters to the continents
crop_and_mask_continent <- function(raster, continent_geom) {
  mask(crop(raster, continent_geom), continent_geom)
}

# Function to crop the biome boundaries to the continents
crop_biome_to_continent <- function(biome, continent_geom) {
  st_intersection(biome, continent_geom)
}

# Function to plot the changes
plot_spatialChanges <- function(raster, biome_geom, color_ramp, fill_label, min_value, max_value) {
  raster_df <- as.data.frame(raster, xy = TRUE)
  colnames(raster_df)[3] <- "value"  # Ensure the column name is "value"
  
  ggplot() +
    geom_sf(data = biome_geom, fill = NA, color = "lightgrey", size = 0.2) +  # Biome and continent basemap
    geom_tile(data = raster_df, aes(x = x, y = y, fill = value)) +
    scale_fill_gradientn(name = fill_label, colors = color_ramp(seq(min_value, max_value, length.out = 101)), limits = c(min_value, max_value), na.value = "grey") +
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
# Load and select the biome
biome_sf <- load_select_biome(biome_name)

# Loop through the scenarios, variables, and years for the biome
# L apply
for (scenario in scenarios) {
  for (variable in variables) {
    for (year in years) {
      # Load the raster
      raster <- load_raster(scenario, variable, year)
      
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
      output_file <- file.path(output_path, paste0("ClimateChange_", scenario, "_", variable, "_", year, "_agg.tif"))
      writeRaster(raster_biome, output_file, overwrite = TRUE)
      
      # Assign the raster to a variable dynamically
      assign(paste0("ClimateChange_", scenario, "_", variable, "_", year, "_", biome_name), raster_biome)
    }
  }
}

# create a list as output?

# Function to stack rasters
stack_rasters <- function(variable, year) {
  scenarios_list <- list(
    get(paste0("ClimateChange_", scenarios[1],"_", variable, "_", year, "_", biome_name)),
    get(paste0("ClimateChange_", scenarios[2],"_", variable, "_", year, "_", biome_name))
  )
  
  # Assign names to the list elements
  names(scenarios_list) <- c(paste0(scenario_names[1], "_", year), paste0(scenario_names[2], "_", year))
  
  # Create a raster stack from the list of scenarios
  scenarios_stack <- rast(scenarios_list)
  
  # Assign names to the raster stack layers
  names(scenarios_stack) <- names(scenarios_list)
  
  # Save the raster stack
  stack_output_file <- file.path(output_path, paste0("scenarios_stack_", variable, "_", year, "_", biome_name, ".tif"))
  writeRaster(scenarios_stack, stack_output_file, overwrite = TRUE)
  
  return(scenarios_stack)
}

# Loop through the variables and years to create raster stacks
for (variable in variables) {
  for (year in years) {
    stack_rasters(variable, year)
  }
}

# Calculate and create Climate Change graphics over time -------------------------------------------
# Define consistent color palette for the scenarios
scenario_colors <- c(
  scenario_names[1] = "#1f77b4",
  scenario_names[2] = "#ff7f0e"
)

# Loop through the variables and years to extract mean values and create plots
mean_values_list <- list()
for (variable in variables) {
  for (year in years) {
    raster_stack <- get(paste0("scenarios_stack_", variable, "_", year, "_", biome_name))
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
  mutate(Scenario = gsub("_\\d{4}_\\d{4}$", "", Scenario))

# Split the data frame into temperature and precipitation data frames
mean_temps_df <- mean_values_df %>% filter(Value_Type == variable_names[1])
mean_preci_df <- mean_values_df %>% filter(Value_Type == variable_names[2])

# Create plots
scenarios_temps_plot <- plot_timeChanges(mean_temps_df, variable_names[1], y_labels[1])
scenarios_preci_plot <- plot_timeChanges(mean_preci_df, variable_names[2], y_labels[2])

# Arrange the plots side by side
combined_plot_time <- grid.arrange(scenarios_temps_plot, scenarios_preci_plot, ncol = 2,
                              top = textGrob(biome_name))

# Save the combined plot
ggsave(filename = file.path(output_folder, 
                           paste0("ClimateChange_", variable, "_timeChanges_", year, "_", biome_name, ".png")),
       plot = combined_plot_time,
       width = 14, height = 7, dpi = 600)


# Calculate and create spatially explicit Climate Change Maps -------------------------------------------
# Load and select the continents
continents <- load_select_continents(continent_names)

# Transform continent CRS to match the raster CRS
continents <- st_transform(continents, crs(load_raster(scenarios[1], variables[1], years[1])))

# Define continent geometries
# create a list that is automated
continent_geoms <- list(
  Africa = continents %>% filter(continent == "Africa"),
  South_America = continents %>% filter(continent == "South America"),
  Asia = continents %>% filter(continent == "Asia")
)

# Validate the geometries, corrects geometries
biome_sf <- st_make_valid(biome_sf)

# Crop the biome boundaries to the continents
biome_continents <- lapply(continent_geoms, function(continent_geom) {
  crop_biome_to_continent(biome_sf, continent_geom)
})

# Calculate changes and crop/mask to continents
for (variable in variables) {
    for (scenario in scenarios) {
      for (year in years[-1]) {
      # Calculate changes
      change_raster <- calculate_change(
        get(paste0("ClimateChange_", scenario, "_", variable, "_", year, "_", biome_name)),
        get(paste0("ClimateChange_", scenario, "_", variable, "_", years[1], "_", biome_name))
      )
      
      # Crop and mask to continents
      change_rasters <- lapply(continent_geoms, function(continent_geom) {
        crop_and_mask_continent(change_raster, continent_geom)
      })
      
      # Assign the cropped/masked rasters to variables dynamically
      for (continent in names(change_rasters)) {
        assign(paste0("change_", scenario, "_", variable, "_", year, "_", tolower(continent)), change_rasters[[continent]])
      }
    }
  }
}

# Create custom color ramps
# fix scale external, min, max based rasters max min for temp and precipitation
#minimum = min(change_raster - temperature)

custom_color_ramp_temp <- colorRamp2(c(-5, 0, 5), c("blue","white", "red")) # this could be adapted dynamically to the max and min of all data for a given biome
custom_color_ramp_precip <- colorRamp2(c(-950, 0, 950), c("saddlebrown", "yellow", "darkgreen")) # this could be adapted dynamically to the max and min of all data for a given biome

# Create plots for each scenario, variable, and year
# L apply
# make plot names general, how can I access that?
for (variable in variables) {
  for (year in years[-1]) {
    for (scenario in scenarios) {
      # Define the color ramp and fill label based on the variable
      if (variable == variables[1]) {
        color_ramp <- custom_color_ramp_temp
        fill_label <- "Change in °C"
        min_value <- -5
        max_value <- 5
      } else {
        color_ramp <- custom_color_ramp_precip
        fill_label <- "Change in kg m-2 year-1"
        min_value <- -950
        max_value <- 950
      }
      
      # Create plots for each continent and scenario
      plots_spatial <- list()
      for (scenario in scenarios) {
        for (continent in names(continent_geoms)) {
          plot <- plot_spatialChanges(get(paste0("change_", scenario, "_", variable, "_", year, "_", tolower(continent))), biome_continents[[continent]], color_ramp, fill_label, min_value, max_value)
          plots_spatial[[paste0(scenario, "_", tolower(continent))]] <- plot
        }
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
          plots_spatial[[scenarios[1],"_", tolower(continent)[1]]], plots_spatial[[scenarios[1],"_", tolower(continent)[2]]], plots_spatial[[scenarios[1],"_", tolower(continent)[3]]],
          ncol = 4,
          widths = unit(c(0.5, 5, 5, 5), "null")
        ),
        arrangeGrob(
          textGrob(scenario_names[2], rot = 90, gp = gpar(fontsize = 16)),
          plots_spatial[[scenarios[2],"_", tolower(continent)[1]]], plots_spatial[[scenarios[2],"_", tolower(continent)[2]]], plots_spatial[[scenarios[2],"_", tolower(continent)[3]]],
          ncol = 4,
          widths = unit(c(0.5, 5, 5, 5), "null")
        ),
        ncol = 1,
        heights = unit(c(0.5, 5, 5), "null"),
        top = textGrob(paste0(ifelse(variable == variables[1],variable_names[1], variable_names[2]), " ", fill_label, " for the ", biome_name, " (", years[1], " vs. ", year,")"), gp = gpar(fontsize = 18))
      )
      
      # Save the combined plot
      ggsave(filename = file.path(output_folder, 
                                  paste0("ClimateChange_", variable, "_spatialChanges_", year, "_", biome_name, ".png")), 
             plot = combined_plot_spatial, 
             width = 15, height = 6, dpi = 300)
    }
  }
}
