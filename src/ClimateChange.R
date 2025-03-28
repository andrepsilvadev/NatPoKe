## Name: ClimateChangeTimePeriods.R ##
## Authors: Jorinde-M. Rieger ##
## Description: Applies functions to calculate spatial explicit temperature and precipitation change in a given Biome
## for the ssp126 and ssp585 scenarios in various time periods ##
## Date: March 19th 2025 ##

# Settings & libraries -------------------------------------------
source("./src/libraries.R") # libraries
source("./src/customFunctions.R") # functions

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
base_path <- "~/data/data/CHELSA_gfdl-esm4_V.2.1"
output_path <- "~/data/data/CHELSA_gfdl-esm4_V.2.1/outputData"
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
continent_names <- c("Europe", "North America")
continent_title <- c("North America", "Europe & Asia")

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

# Function to stack rasters
stack_rasters <- function(variable, year) {
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
  stack_output_file <- file.path(output_path, paste0("scenarios_stack_", variable, "_", year, "_", biome_name_short, ".tif"))
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


# Function to load and select continents
#load_select_continents <- function(continent_names) {
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

# Function to calculate changes
calculate_change <- function(raster_future, raster_present) {
  raster_future - raster_present
}

# Function to extract the legend from a ggplot object
extract_legend <- function(plot) {
  gtable <- ggplotGrob(plot)
  legend <- gtable$grobs[which(sapply(gtable$grobs, function(x) x$name) == "guide-box")][[1]]
  return(legend)
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
      axis.text = element_text(size = 18),
      plot.title = element_blank(),
      legend.title = element_text(size = 22, margin = margin(b = 10)),
      legend.text = element_text(size = 18),
      legend.key.height = unit(1, "cm"),  # Increase the height of the color ramp
      legend.spacing = unit(1, "cm")
    ) +
    coord_sf()  # Use coord_sf() for spatial data
  
  return(plot)
}

# Function to plot the changes
#plot_spatialChanges <- function(raster, biome_geom, color_ramp, fill_label, min_value, max_value) {
  raster_df <- as.data.frame(raster, xy = TRUE)
  colnames(raster_df)[3] <- "value"  # Ensure the column name is "value"
  
  ggplot() +
    geom_sf(data = biome_geom, fill = "lightgrey", color = "lightgrey", size = 0.2) +  # Biome and continent basemap
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
# Load the selected biome
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
      output_file <- file.path(output_path, paste0("ClimateChange_", scenario, "_", variable, "_", year, "_", biome_name_short, ".tif"))
      writeRaster(raster_biome, output_file, overwrite = TRUE)
      
      # Assign the raster to a variable dynamically
      assign(paste0("ClimateChange_", scenario, "_", variable, "_", year, "_", biome_name_short), raster_biome)
    }
  }
}
# create a list as output?


# Loop through the variables and years to create raster stacks
for (variable in variables) {
  for (year in years) {
    stack_rasters(variable, year)
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
                            paste0("ClimateChange_", variable, "_timeChanges_", year, "_", biome_name_short, ".png")),
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


# Crop the biome boundaries to the continents
#biome_continents <- lapply(continent_geoms, function(continent_geom) {
  crop_biome_to_continent(biome_sf, continent_geom)
})

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
        crop_and_mask_continent(change_raster, continent_geom)
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
        fill_label <- paste("Change in", value_units[2])
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
            textGrob(continent, gp = gpar(fontsize = 16))
          }),
          ncol = num_continents,
          heights = unit(c(0.5), "null")
        ),
        arrangeGrob(
          grobs = c(
            list(textGrob(scenario_names[1], rot = 90, gp = gpar(fontsize = 16))),
            lapply(continent_names, function(continent) {
              plots_spatial[[paste0(scenarios[1], "_", tolower(continent))]]
            })
          ),
          ncol = num_continents + 1,
          widths = unit(c(0.5, rep(5, num_continents)), "null")
        ),
        arrangeGrob(
          grobs = c(
            list(textGrob(scenario_names[2], rot = 90, gp = gpar(fontsize = 16))),
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
          top = textGrob(
            paste0(ifelse(variable == variables[1], variable_names[1], variable_names[2]), " ", fill_label, " for the ", biome_name_short, " (", years[1], " vs. ", year,")"), 
            gp = gpar(fontsize = 18)
        )
      )
      
      # Save the combined plot
      ggsave(filename = file.path(output_folder, 
                                  paste0("ClimateChange_", variable, "_spatialChanges_", year, "_", gsub(" ", "_", biome_name_short), ".png")), 
             plot = combined_plot_spatial, 
             width = 15, height = 6, dpi = 300)
    }
  }
}
