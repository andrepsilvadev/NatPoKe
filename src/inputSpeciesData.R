## Input Species Data ##
## Jorinde-M. Rieger ##
## April 2nd, 2025 ##

# Load species occurence data
speciesData <- read.csv(
  "~/data/data/trait_datasets/GBIF_mammal_30+occurrences_speciesTest.csv")
print(head(speciesData))

# Remove NAs and filter out records older than 2015
speciesData <- speciesData %>%
  drop_na(decimalLongitude,decimalLatitude, year)%>%
  filter(year >= 2015)

## This code addresses spatial bias and selects one record per grid cell
# Load raster to define grid cells
env_raster <- trainingLandscapes

# Extract cell ID for each occurrence
speciesData$cell <- terra::cellFromXY(env_raster, cbind(speciesData$decimalLongitude, speciesData$decimalLatitude))

# Function to select one occurrence per grid cell
removeSpeciesDuplicatesbyCellID <- function (dataframe) {
  SpeciesDataOcc <- dataframe %>%
    drop_na(cell) %>%
    group_by(species, cell) %>%
    slice_max(year, with_ties = FALSE) %>%  # Keep most recent record per species-cell
    ungroup() %>%
    distinct(species, cell, year, .keep_all = TRUE) %>%  # Ensure unique species-cell-year  
    arrange(species, cell, year)
  return(SpeciesDataOcc)
}

# Keep only one occurrence per unique grid cell
speciesDataOcc <- removeSpeciesDuplicatesbyCellID(speciesData) # Remove duplicate records per cell
View(speciesDataOcc)

# Save the filtered data
readr::write_csv(speciesDataOcc, 
                 file =  "~/data/data/filtered_occurrences.csv")
