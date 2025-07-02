## Name: InputSpeciesData.R ##
## Author: Jorinde-M. Rieger ##
## Description: Cleans species occurrence data, one species per grid cell, filters by year ##
## Date: May 22nd, 2025 ##

# Define and load species occurence data -----------------------------------------------------------------
# Mammal occurrences
#species_group <- "mammals"
#speciesData <- read.csv(
#  "~/data/data/trait_datasets/GBIF_mammal_30+occurrences.csv") # all targeted mammals
#print(head(speciesData))

# Mammal occurrences that has trait data sets
species_group <- "mammalsWTrait"
speciesData <- read.csv(file = paste0("~/data/data/trait_datasets/GBIF_",species_group, "_30+occurrences_", extent,".csv"))

# Mammal occurrence with trait data set, subset for Iberian peninsula #NOT DONE YET
#species_group <- "mammals"
#speciesData <- read.csv("~/data/data/trait_datasets/GBIF_mammals_30+occurrences_Iberian peninsula.csv")
#print(head(speciesData))

# Birds occurrence subset, Iberian peninsula
#species_group <- "birds"
#speciesData <- read.csv("~/data/data/trait_datasets/GBIF_birds_30+occurrences_Iberian peninsula.csv")
#print(head(speciesData))


# Format species occurrence data -----------------------------------------------------------------
# Remove NAs and filter out records older than 2015
speciesData <- speciesData %>%
  drop_na(decimalLongitude,decimalLatitude, year)%>%
  filter(year >= 2015)

## This code addresses spatial bias and selects one record per grid cell
# Load raster to define grid cells
env_raster <- trainingLandscapes[[1]]

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
#table(speciesDataOcc$species)

# Save the filtered data
#readr::write_csv(speciesDataOcc, 
#                 file = paste0("~/data/data/trait_datasets/GBIF_", species_group, extent, "_OccPerCell.csv"))
rm(speciesData)

# Format species occurrence to true presence and NAs with corresponding coordinates
# Get all cell indices and coordinates from the raster
all_cells <- data.frame(cell = 1:terra::ncell(env_raster))
coords <- terra::xyFromCell(env_raster, all_cells$cell)
all_cells$x <- coords[,1]
all_cells$y <- coords[,2]

# Get list of target species
#species_list <- unique(speciesDataOcc$species) # all species
#targetSpecies <- c("Alces alces", "Canis lupus", "Tragelaphus scriptus")
#species_list <- intersect(unique(speciesDataOcc$species), targetSpecies)

# Group species in 10 by number of occurences (Balanced Groups)
species_counts <- speciesDataOcc %>%
  count(species) %>%
  arrange(desc(n))

species_list <- species_counts$species
group_size <- 5
n_groups <- ceiling(length(species_list) / group_size)

species_groups <- split(species_list, 
                        rep(1:n_groups, each = group_size, length.out = length(species_list)))


# create data frames for each species group
targetSpecies <- species_groups[[1]]
species_list <- intersect(unique(speciesDataOcc$species), targetSpecies)

# For each species, mark presence (1) in the cell, NA otherwise
presence_matrix <- sapply(species_list, function(sp) {
  pres_cells <- speciesDataOcc$cell[speciesDataOcc$species == sp]
  as.integer(all_cells$cell %in% pres_cells)
})
presence_matrix[presence_matrix == 0] <- NA  # Convert 0 to NA

# Combine into a data frame
speciesDataInput <- data.frame(
  x = all_cells$x,
  y = all_cells$y,
  cell = all_cells$cell,
  presence_matrix
)
colnames(speciesDataInput)[-(1:3)] <- species_list

# Filter presence only data
SpeciesPresences <- speciesDataInput %>%
  filter(if_any(all_of(targetSpecies), ~ . == 1))
#head(SpeciesPresences)

# Save the presence/absence grid for SDM input
readr::write_csv(
  speciesDataInput%>%
    mutate(
      x = format(x, digits = 7, scientific = FALSE),
      y = format(y, digits = 7, scientific = FALSE)
    ),
  file = paste0("~/data/data/trait_datasets/speciesPresences_", species_group, extent, "_PresenceGrid.csv")
)

rm(all_cells, coords, presence_matrix, speciesDataOcc, speciesDataInput)
gc()



# Check the mammal trait data set with Spain mammals
mammalTrait<- readr::read_csv(
  paste0("~/data/data/trait_datasets/mammalTraits_2025-02-03.csv")
)

unique(mammalTrait$Species)
#targetSpecies <- spain species
mammalstraitSpain <- intersect(unique(mammalTrait$species), targetSpecies)
# subset by species, dont worry about biome doubles
