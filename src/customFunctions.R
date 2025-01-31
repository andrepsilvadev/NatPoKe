## Name: Custom functions ##
## Authors: Andre P. Silva & Jorinde-M. Rieger ##
## Description: Loads all developed customised functions ##

# Function to map ESA LULC values to the 7 LULC types
map_values_to_land_use <- function(x) {
  sapply(x, function(val) {
    if (val %in% names(value_to_land_use)) {
      return(value_to_land_use[[as.character(val)]])
    } else {
      return(NA)  # Handle values that do not map to any land-use type
    }
  })
}
# Function to calculate the percentages for each land-use type
calculate_land_use_percentages <- function(raster_stack, land_use_types, land_use_names,time) {
  # Create binary maps for each land-use type
  land_use_layers <- lapply(land_use_types, function(cat) {
    terra::app(raster_stack, fun = function(x) {
      return(ifelse(x == cat, 1, 0))
    })
  })
  
  # Calculate the percentages for each land-use type without NAs
  land_use_percentages <- sapply(land_use_layers, function(layer) {
    sum(values(layer), na.rm = TRUE) / sum(!is.na(values(layer))) * 100
  })
  
  # Create a data frame with the results
  percentage_df <- data.frame(
    time = time,
    landuse = land_use_names,
    variable = land_use_names,
    value = land_use_percentages
  )
  
  return(percentage_df)
}
