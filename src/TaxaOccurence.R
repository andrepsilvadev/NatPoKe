## Name: Taxa occurrences ##
## Author: Inês Silva, Andre P. Silva & Afonso Barrocal & Jorinde-M. Rieger ##
## Date: January 17th, 2026 ##
## Description: Download and filter GBIF occurrence data for multiple species ##

## Script Instructions

# STEP 1. Extract species occurrence records from GBIF
# STEP 2. Filter by data quality and minimum sample size
# STEP 3. Spatially restrict occurrences to IUCN range polygons
# STEP 4. Allow for manual taxonomic vetting (subspecies, synonyms)
# STEP 5. Export a clean CSV file ("GBIF_occurrences_mammals.csv").

# load packages & functions
source("./src/libraries.R")
source("./src/customFunctions.R")
source("./src/customFunctions2.R")


##########
# STEP 1 # Retrieve occurrences from GBIF using user-defined function
##########

# Use get_taxa_occurrences() function from (customFunctions2.R)
# Downloads GBIF occurrence records for a set of species, applies quality
# filters, and crops occurrences to IUCN range polygons. Retruns: An sf object
# of GBIF occurrences spatially restricted to IUCN ranges.

# load species list
species_table <- read_excel("./data/traitData/MammalSpecies_selection.xlsx",
                            sheet = "CompleteSpeciesDf")
# pull species names
target_species <- unique(species_table$sci_name)

# load GBIF credentials
keys <- yaml::read_yaml("./config/api_keys.yml")

# run get_taxa_occurrences() function for mammals
GBIF_data <- get_taxa_occurrences(
  iucn_shapefile = "./data/externaldata/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp",
  taxa_filter = target_species,
  gbif_user = keys$gbif_user,
  gbif_pwd = keys$gbif_pwd,
  gbif_email = keys$gbif_email)
invisible(gc())

##########
# STEP 2 # Manual taxonomic check
##########

# This portion of the script should be curated manually.
# We go through the table below and assess whether subspecies
# are being included in the recovered occurrences.

# summarise number of records per species and verbatim name
species_name_check <- GBIF_data %>%
  group_by(species, verbatimScientificName) %>%
  summarise(n_records = n(), .groups = "drop") %>%
  st_drop_geometry() %>% 
  arrange(species, desc(n_records))
invisible(gc())

# save .csv for manual review
write.csv(species_name_check,
          "./data/mammal_species_verbatim_check.csv",
          row.names = FALSE)

# print warning message
message("STOP & ANALYSE - Please check species vs. verbatim names before going further...")

## APPLY TAXONOMIC EXCLUSIONS ##
# now manually review the taxonomic list names in 'data/species_verbatim_check.csv'
# if suspect names or subspecies are included add 'exclude' to column C
# re-import the file and clean records (works as a blacklist)

exclusions <- read.csv("./data/species_verbatim_check.csv") %>% 
  dplyr::filter(exclude == "exclude")

# filter the GBIF occurrences
GBIF_data_df <- GBIF_data %>%
  # remove unwanted subspecies, domestic animals, or problematic names
  dplyr::anti_join(
    exclusions,
    by = c("species", "verbatimScientificName")
  ) %>% 
  # recheck if species remaining have over 30 occ
  dplyr::filter(n() > 30) %>% 
  # get coordinates again
  mutate(
    decimalLongitude = st_coordinates(.)[,1],
    decimalLatitude = st_coordinates(.)[,2]) %>%
  # drop geometry
  st_set_geometry(NULL)

##########
# STEP 3 # Export screened occurrence dataset to .csv file 
##########

invisible(gc(rm(GBIF_data, species_name_check)))

# occurrences file
write.csv(GBIF_data_df[, c("species", "decimalLatitude", "decimalLongitude", "year")],
          "./data/GBIF_occurrences_mammals.csv")

message("✅ Screened GBIF  mammals data exported to 'GBIF_occurrences_mammals.csv'.")

message("Following species had range polygons but not enough records:\n",
  paste(setdiff(target_species, unique(GBIF_data_df$species)), collapse = "\n")
  )
