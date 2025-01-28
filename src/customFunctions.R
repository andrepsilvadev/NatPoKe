## Name: Custom functions ##
## Authors: Andre P. Silva ##
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
