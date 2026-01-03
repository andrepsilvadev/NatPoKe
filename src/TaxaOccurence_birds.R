## Name: taxaOccurrences_birds.R ##
## Author: Inês Silva ##
## Date: 03 Jan 2026
## Description: Download and filter bird sps occurrence data from GBIF ##

## Script Instructions
## STEP 1 – Load Bird Life ranges and extract species list.
## STEP 2 – Match species to GBIF taxon keys.
## STEP 3 – Download GBIF occurrence records with coordinates & after 2015.
## STEP 4 – Keep only species with >30 occurrence points.
## STEP 5 – Crop GBIF occurrences within each species’ Bird Life range.
## STEP 6 – Manually review species name matches (script pauses for manual check).
## STEP 7 – Export final cleaned dataset ("GBIF_occurrences.csv").

# load packages & functions
source("./src/libraries.R")
source("./src/customFunctions.R")
source("./src/customFunctions2.R")


get_taxa_occurrences <- function(
                                iucn_shapefile, # spatial file with sps distribution ranges
                                taxa_filter = NULL, # use in case we want to subset specific species
                                gbif_user = "", # GBIF username
                                gbif_pwd = "", # GBIF password
                                gbif_email = "") # GBIF account email
  {
  
  ##########
  # STEP 1 # Read IUCN ranges and species list
  ##########
  
  # progess message
  message("Selecting species names from IUCN ranges file")
  
  IUCN_ranges <- sf::st_read(iucn_shapefile)
  #IUCN_ranges <- readRDS(iucn_shapefile)
  invisible(gc())
  
  # keep unique species names
  taxa_spp <- unique(IUCN_ranges$sci_name)
  
  # filter taxa if provided
  if (!is.null(taxa_filter)) {
    taxa_spp <- taxa_spp[taxa_spp %in% taxa_filter]}
  
  # for testing only
  #taxa_spp <- taxa_spp[1:10]
  #taxa_spp <- c("Lophaetus occipitalis", "Ardea goliath", "Podica senegalensis")
  
  # progess message
  message("All selected species have a known IUCN ranges polygon available")
  
  ##########
  # STEP 2 # Get GBIF taxon keys
  ##########
  
  gbif_taxon_keys <- name_backbone_checklist(name = taxa_spp) %>%
    filter(matchType == "EXACT") %>%
    pull(usageKey)
  invisible(gc())
  
  ##########
  # STEP 3 # Download occurrences with coordinates only
  ##########
  
  download_key <- occ_download(
    pred_in("taxonKey", gbif_taxon_keys), 
    pred("OCCURRENCE_STATUS","PRESENT"),
    pred_in("basisOfRecord", c("HUMAN_OBSERVATION", "OBSERVATION", "MACHINE_OBSERVATION", "OCCURRENCE")),
    pred_gte("year", 2015),
    pred("hasCoordinate", TRUE),
    pred_notnull("decimalLatitude"),
    pred_notnull("decimalLongitude"),
    format = "SIMPLE_CSV",
    user = gbif_user,
    pwd = gbif_pwd,
    email = gbif_email
  )
  
  # wait until finished
  message("Downloading occurrences file from GBIF. Please wait...")
  occ_download_wait(download_key)
  invisible(gc())
  
  # retrieve and import download
  d <- occ_download_get(key = download_key)
  gbif_data <- occ_download_import(d)
  rm(d)
  invisible(gc())
  
  ##########
  # STEP 4 # Filter species with >30 records
  ##########
  
  gbif_data_filtered <- gbif_data %>%
    drop_na(decimalLatitude, decimalLongitude, year) %>% 
    group_by(species) %>% 
    # keep only species with over 30 occ
    dplyr::filter(n() > 30) %>% 
    ungroup()
  
  # are there species with no occurrence records match?
  missing_spp <- setdiff(taxa_spp,  unique(gbif_data_filtered$species))
  
  # Check and report
  if (length(missing_spp) > 0) {
    message("!! WARNING !! The following species do not have enough records:")
    message(paste(missing_spp, collapse = ", "))
  } else {
    message("All taxa_spp species have more than 30 occurrence records! Continue...")
  }
  
  ##########
  # STEP 5 # Crop occurrences within IUCN range
  ##########
  
  # progress message
  message("Cropping occurrences by species IUCN range...")
  
  gbif_sf <- st_as_sf(
    gbif_data_filtered,
    coords = c("decimalLongitude", "decimalLatitude"),
    crs = st_crs(IUCN_ranges))
  invisible(gc())
  
  sf_use_s2(FALSE)
  occurrences_in_range <- bind_rows(lapply(taxa_spp, function(sp) {
    message(sp)
    poly <- IUCN_ranges %>% filter(sci_name == sp)
    pts  <- gbif_sf %>% filter(species == sp)
    if (nrow(poly) == 0 || nrow(pts) == 0) return(NULL)
    return(st_join(pts, poly, join = st_within, left = FALSE))
  }))
  
  # final output
  return(occurrences_in_range)
}


## USE FUNCTION ----------------------------------------------------------------

# get GBIF keys
keys <- yaml::read_yaml("./config/api_keys.yml")

# test species
test_sps <- read_excel("data/BirdSpecies_selection.xlsx", 
                       sheet = "CompleteSpeciesDf") %>% 
  dplyr::pull(sci_name)



# retrieve and filter bird occurrences
GBIF_data <- get_taxa_occurrences(
  iucn_shapefile = "./data/BOTW_2024_2.gpkg",
  taxa_filter = test_sps[1:10] ,
  gbif_user = keys$gbif_user,
  gbif_pwd = keys$gbif_pwd,
  gbif_email = keys$gbif_email)
invisible(gc())

##########
# STEP 6 # Manual taxonomic check
##########

# This portion of the script should be curated manually.
# We go through the table below and assess whether subspecies
# are being included in the recovered occurrences.

invisible(gc(rm(IUCN_ranges, gbif_taxon_keys, keys)))

# summarise number of records per species and verbatim name
species_name_check <- GBIF_data %>%
  # dplyr::filter(
  #   # Condition 1: Keep all records for Gorilla species
  #   species %in% c("Gorilla gorilla", "Gorilla beringei") |
  #     
  #     # Condition 2: OR (if not a Gorilla species), keep only those where taxonRank is "SPECIES"
  #     (taxonRank == "SPECIES")
  # ) %>% 
  group_by(species, verbatimScientificName) %>%
  summarise(n_records = n(), .groups = "drop") %>%
  st_drop_geometry() %>% 
  arrange(species, desc(n_records))
invisible(gc())

# print warning message
message("STOP & ANALYSE - Please check species vs. verbatim names before going further...")

# open table for manual check
View(species_name_check)
# save .csv for detailed review
write.csv(species_name_check,
          "./data/species_verbatim_check.csv",
          row.names = FALSE)

# STOP script review is done
readline(prompt = "Press [Enter] to continue with filtering once you've reviewed the species name matches...")

# filter the GBIF occurrences
GBIF_data_df <- GBIF_data %>%
  # # remove unwanted subspecies, domestic animals, or problematic names
  # dplyr::filter(!verbatimScientificName %in% c(
  #                                             # Possible coyote record
  #                                             "Canis latrans spp",
  #                                             # Red-Deer subspecies
  #                                             "Cervus elaphus hippelaphus",
  #                                             "Cervus elaphus montanus",
  #                                             # Wildbeast subspecies
  #                                             "Connochaetes mearnsi", 
  #                                             "Connochaetes johnstoni",
  #                                             "Connochaetes albojubatus",
  #                                             #Gorilla subspecies
  #                                             "Gorilla beringei graueri",
  #                                             "Gorilla gorilla diehli",
  #                                             # Lynx lynx subspecies
  #                                             "Lynx lynx dinniki",
  #                                             "Lynx lynx wrangeli",
  #                                             # Chimpanzee subspecies
  #                                             "Pan troglodytes verus",
  #                                             # leopard subsspecies
  #                                             "Panthera pardus tulliana",
  #                                             #sri lanka boar
  #                                             "Sus scrofa affinis",
  #                                             # domestic pigs
  #                                             "Sus domesticus Erxleben, 1777",
  #                                             "Sus domesticus",
  #                                             "Sus attila",
  #                                             "Sus domesticus forma",
  #                                             "Sus scrofa baeticus",
  #                                             "Sus setosus",
  #                                             # Gobi bear
  #                                             "Ursus arctos gobiensis",
  #                                             # american red fox
  #                                             "Vulpes fulva",
  #                                             # subspecies of fox
  #                                             "Vulpes vulpes patwin",
  #                                             # Domestic dog / dingo
  #                                             "Canis familiaris",
  #                                             "Canis familiaris (Linnaeus, 1758)",
  #                                             "Canis familiaris Linnaeus, 1758",
  #                                             "Canis familiaris familiaris",
  #                                             "Canis   familiaris",
  #                                             "Canis familiaris dingo",
  #                                             # Red wolf
  #                                             "Canis rufus",
  #                                             # Italian wolf
  #                                             "Canis lupus italicus",
  #                                             # Iberian wolf
  #                                             "Canis lupus signatus")) %>%
  # get coordinates again
  mutate(
    decimalLongitude = st_coordinates(.)[,1],
    decimalLatitude = st_coordinates(.)[,2]) %>%
  # drop geometry
  st_set_geometry(NULL)

##########
# STEP 7 # Export screened data to .csv & save taxonomic info table
##########

invisible(gc(rm(GBIF_data, species_name_check)))

# occurrences file
write.csv(GBIF_data_df[, c("species", "decimalLatitude", "decimalLongitude", "year")],
          "./data/GBIF_occurrences_birds.csv")

# taxonomic info file
# sps_names <- GBIF_data_df %>% 
#   dplyr::select(species, order, family, genus) %>% 
#   st_drop_geometry() %>% 
#   unique()
# write.csv(sps_names, paste0("output/27October25/mammalSpeciesTaxonomy.csv"), row.names = FALSE)


message("✅ Screened GBIF data exported to 'GBIF_occurrences.csv'.")

##############################
# QUICK CHEKUP TO SHOW ANDRÉ #
##############################

# world map as sf
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey70") +
  geom_point(
    data = GBIF_data_df,
    aes(
      x = decimalLongitude,
      y = decimalLatitude,
      color = species
    ),
    size = 1,
    alpha = 0.7
  ) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  labs(
    x = "Longitude",
    y = "Latitude",
    color = "Species"
  )
