## Name: inputClimate.R ##
## Authors: Jorinde-M. Rieger ##
## Description: Applies functions to calculate environmental input data (temperature and precipitation)
## for the ssp126 and ssp585 scenarios in various time periods ##
## Date: April 1st 2025 ##

# Input variables -------------------------------------------
# Define input variables
scenarios <- c("ssp126", "ssp585")
scenario_names <- c("SSP1-RCP2.6", "SSP5-RCP8.5")
variables <- c("bio1", "bio12")

years <- c("2011-2040", "2041-2070", "2071-2100") 
baseline_year <- "1981-2010"

# Define the file paths
base_path <- "~/data/data/CHELSA_gfdl-esm4_V.2.1"
output_path <- "~/data/data/CHELSA_gfdl-esm4_V.2.1/outputData"
output_folder <- "~/data/output"

# Define the target resolution (based on the landUsePercentage rasters)
target_resolution <- 0.277


# Functions - later add them to CustomFunctions.R -------------------------------------------
# Function to load rasters
load_baseline_raster <- function(variable, baseline_year){
  file_path <- file.path(base_path, paste0("CHELSA_", variable, "_", baseline_year, "_V.2.1.tif"))
  rast(file_path)}

load_raster <- function(scenario, variable, year) {
  file_path <- file.path(base_path, scenario, paste0("CHELSA_", variable, "_", year, "_gfdl-esm4_", scenario, "_V.2.1.tif"))
  rast(file_path)}

# Function to aggregate rasters
aggregate_raster <- function(raster, aggregation_factor) {
  aggregate(raster, aggregation_factor, fun = mean)
}

# Function to crop and mask rasters
crop_mask_raster <- function(raster, land) {
  mask(crop(raster, land), land)
}

# Function to stack training landscape rasters
stack_trainingLandscape <- function(variable) {
  variable_list <- list(
    get(paste0("trainingLandscape_", variables[1])),
    get(paste0("trainingLandscape_", variables[2]))
  )
  # Assign names to the list elements
  names(variable_list) <- c(paste0(variables))
  
  # Create a raster stack from the list of scenarios
  variable_stack <- rast(variable_list)
  
  # Assign names to the raster stack layers
  names(variable_stack) <- names(variable_list)
  
  # Save the raster stack
  stack_output_file <- file.path(output_path, paste0("trainingLandscape_stack", ".tif"))
  writeRaster(variable_stack, stack_output_file, overwrite = TRUE)
  
  # Assign the raster stack to a variable in the environment
  assign(paste0("trainingLandscape_stack"), variable_stack, envir = .GlobalEnv)
  
  return(variable_stack)
}

# Function to stack prediction landscape rasters
stack_predictionLandscape <- function(variable, year) {
  variable_list <- list(
    get(paste0("predictionLandscape_", scenario,"_", variables[1], "_", year)),
    get(paste0("predictionLandscape_", scenario,"_", variables[2], "_", year))
  )
  
  # Assign names to the list elements
  names(variable_list) <- c(paste0(variables[1]), paste0(variables[2]))
  
  # Create a raster stack from the list of scenarios
  variable_stack <- rast(variable_list)
  
  # Assign names to the raster stack layers
  names(variable_stack) <- names(variable_list)
  
  # Save the raster stack
  stack_output_file <- file.path(output_path, paste0("predictionLandscape_stack_", scenario, "_", year, ".tif"))
  writeRaster(variable_stack, stack_output_file, overwrite = TRUE)
  
  # Assign the raster stack to a variable in the environment
  assign(paste0("predictionLandscape_stack_", scenario, "_", year), variable_stack, envir = .GlobalEnv)
  
  return(variable_stack)
}

# Create environmental input Data (climate) as training and predition landscapes-------------------------------------------
# Loop through the training landscapes
for (variable in variables) {
    # Load the raster
    raster <- load_baseline_raster(variable, baseline_year)
    
    # Get the original resolution from the raster
    original_resolution <- res(raster)[1]
    
    # Calculate the aggregation factor
    aggregation_factor <- target_resolution / original_resolution
    
    # Aggregate the raster
    raster_agg <- aggregate_raster(raster, aggregation_factor)
    
    # Load terrestrial extent
    land <- ne_countries(scale = "medium", returnclass = "sf")
    
    # Ensure CRS consistency
    land <- st_transform(land, crs = crs(raster_agg))
    
    # Convert the sf to a spatial object
    land <- vect(land)
    
    # Crop and mask the raster
    raster_land <- crop_mask_raster(raster_agg, land)
    
    # Save the aggregated rasters
    output_file <- file.path(output_path, paste0("trainingLandscape_", variable, ".tif"))
    writeRaster(raster_land, output_file, overwrite = TRUE)
    
    assign(paste0("trainingLandscape_", variable), raster_land)
}

# Create trainingLandscape_stack
for (variable in variables) {
    stack_trainingLandscape(variable)
  }
# Test the rasters
print(trainingLandscape_stack)
plot(trainingLandscape_stack)


# Loop through the prediction landscapes
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
      
      # load the terrestrial extent
      land <- ne_countries(scale = "medium", returnclass = "sf")
      
      # Ensure CRS consistency
      land <- st_transform(land, crs = crs(raster_agg))
      
      # Convert the sf to a spatial object
      land <- vect(land)
      
      # Crop and mask the raster
      raster_land <- crop_mask_raster(raster_agg, land)
      
      # Save the aggregated raster
      output_file <- file.path(output_path, paste0("predictionLandscape_", scenario, "_", variable, "_", year, ".tif"))
      writeRaster(raster_land, output_file, overwrite = TRUE)
      
      # Assign the raster to a variable dynamically
      assign(paste0("predictionLandscape_", scenario, "_", variable, "_", year), raster_land)
    }
  }
}

# Creat prediction landscapes stacks
for (scenario in scenarios) {
  for (variable in variables) {
   for (year in years) {
    stack_predictionLandscape(variable, year)
    }
  }
}


# Create an empty list to store prediction landscapes
predictionLandscape <- list()

# Loop through the prediction landscapes
for (scenario in scenarios) {
  for (year in years) {
    
    # Create a list for each scenario-year combination
    raster_list <- list()
    
    for (variable in variables) {
      # Load the raster
      raster <- load_raster(scenario, variable, year)
      
      # Get the original resolution from the raster
      original_resolution <- res(raster)[1]
      
      # Calculate the aggregation factor
      aggregation_factor <- target_resolution / original_resolution
      
      # Aggregate the raster
      raster_agg <- aggregate_raster(raster, aggregation_factor)
      
      # Load the terrestrial extent
      land <- ne_countries(scale = "medium", returnclass = "sf")
      
      # Ensure CRS consistency
      land <- st_transform(land, crs = crs(raster_agg))
      
      # Convert the sf object to a spatial vector
      land <- vect(land)
      
      # Crop and mask the raster
      raster_land <- crop_mask_raster(raster_agg, land)
      
      # Store the processed raster in the list
      raster_list[[variable]] <- raster_land
    }
    
    # Convert the list of rasters into a SpatRaster stack
    predictionLandscape[[paste0(scenario, "_", year)]] <- rast(raster_list)
  }
}

# Rename raster layers within each stack to match variable names
for (i in seq_along(predictionLandscape)) {
  names(predictionLandscape[[i]]) <- variables
}

# Print the structure of the final list
print(predictionLandscape)

# Test
print(`predictionLandscape_stack_ssp585_2071-2100`)
plot(`predictionLandscape_stack_ssp585_2071-2100`)
print(`predictionLandscape_stack_ssp126_2071-2100`)

