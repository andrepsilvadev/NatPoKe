## Name: Taxa occurrences ##
## Author: Andre P. Silva & Afonso Barrocal & Jorinde-M. Rieger & Inês Silva ##
## Date: April 2nd, 2025 ##
## Description: Download and filter GBIF occurrence data for multiple species ##

## Script Instructions
## STEP 1 – Load IUCN ranges and extract species list.
## STEP 2 – Match species to GBIF taxon keys.
## STEP 3 – Download GBIF occurrence records with coordinates.
## STEP 4 – Keep only species with >30 occurrence points.
## STEP 5 – Crop GBIF occurrences within each species’ IUCN range.
## STEP 6 – Manually review species name matches (script pauses for manual check).
## STEP 7 – Export final cleaned dataset ("GBIF_occurrences.csv").

source("./src/libraries.R")

get_taxa_occurrences <- function(
                                iucn_shapefile, # IUCN spatial file
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
  invisible(gc())
  
  # keep unique species names
  taxa_spp <- unique(IUCN_ranges$sci_name)
  
  # filter taxa if provided
  if (!is.null(taxa_filter)) {
    taxa_spp <- taxa_spp[taxa_spp %in% taxa_filter]}
  
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
    pred("hasCoordinate", TRUE),
    pred_gte("year", 2015),
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

# these are taregt sps for NatPoKe
targetSpecies <- c(## BOREAL SPS ##
  "Alces alces", "Canis lupus"
  # , "Bison bonasus", "Cervus elaphus", 
  # "Sus scrofa", "Vulpes vulpes", "Canis latrans", "Lynx rufus",
  # "Martes americana", "Taxidea taxus", "Ursus americanus", "Panthera tigris",
  # "Lynx lynx", "Ursus arctos", "Rangifer tarandus",
  # "Puma concolor", "Bison bison",
  # 
  # ## TROPICAL SPS ##
  # "Leontopithecus caissara", # has only 4 occurences
  # "Leopardus pardalis", "Nasua nasua", "Aepyceros melampus",
  # "Colobus angolensis", "Daubentonia madagascariensis",
  # "Diceros bicornis", "Erythrocebus patas", "Gorilla beringei",
  # "Gorilla gorilla", "Orycteropus afer", "Pan paniscus",
  # "Pan troglodytes", "Papio anubis", "Papio ursinus", "Cervus nippon",
  # "Cuon alpinus", "Felis chaus", "Macaca fuscata", "Pongo abelii",
  # "Pongo pygmaeus",  "Panthera onca", "Crocuta crocuta", "Mandrillus sphinx",
  # "Panthera pardus", "Syncerus caffer", "Acinonyx jubatus",
  # "Panthera leo", "Connochaetes taurinus", "Loxodonta africana",
  # "Puma concolor"
)

keys <- yaml::read_yaml("./config/api_keys.yml")


GBIF_data <- get_taxa_occurrences(
  iucn_shapefile = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/externalData/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp",
  taxa_filter = targetSpecies,
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
  dplyr::filter(
    # Condition 1: Keep all records for Gorilla species
    species %in% c("Gorilla gorilla", "Gorilla beringei") |
      
      # Condition 2: OR (if not a Gorilla species), keep only those where taxonRank is "SPECIES"
      (taxonRank == "SPECIES")
  ) %>% 
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
  # remove unwanted subspecies, domestic animals, or problematic names
  dplyr::filter(!verbatimScientificName %in% c(
                                              # Possible coyote record
                                              "Canis latrans spp",
                                              # Red-Deer subspecies
                                              "Cervus elaphus hippelaphus",
                                              "Cervus elaphus montanus",
                                              # Wildbeast subspecies
                                              "Connochaetes mearnsi", 
                                              "Connochaetes johnstoni",
                                              "Connochaetes albojubatus",
                                              #Gorilla subspecies
                                              "Gorilla beringei graueri",
                                              "Gorilla gorilla diehli",
                                              # Lynx lynx subspecies
                                              "Lynx lynx dinniki",
                                              "Lynx lynx wrangeli",
                                              # Chimpanzee subspecies
                                              "Pan troglodytes verus",
                                              # leopard subsspecies
                                              "Panthera pardus tulliana",
                                              #sri lanka boar
                                              "Sus scrofa affinis",
                                              # domestic pigs
                                              "Sus domesticus Erxleben, 1777",
                                              "Sus domesticus",
                                              "Sus attila",
                                              "Sus domesticus forma",
                                              "Sus scrofa baeticus",
                                              "Sus setosus",
                                              # Gobi bear
                                              "Ursus arctos gobiensis",
                                              # american red fox
                                              "Vulpes fulva",
                                              # subspecies of fox
                                              "Vulpes vulpes patwin",
                                              # Domestic dog / dingo
                                              "Canis familiaris",
                                              "Canis familiaris (Linnaeus, 1758)",
                                              "Canis familiaris Linnaeus, 1758",
                                              "Canis familiaris familiaris",
                                              "Canis   familiaris",
                                              "Canis familiaris dingo",
                                              # Red wolf
                                              "Canis rufus",
                                              # Italian wolf
                                              "Canis lupus italicus",
                                              # Iberian wolf
                                              "Canis lupus signatus")) %>%
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
          "./data/GBIF_occurrences_mammals.csv")

# taxonomic info file
# sps_names <- GBIF_data_df %>% 
#   dplyr::select(species, order, family, genus) %>% 
#   st_drop_geometry() %>% 
#   unique()
# write.csv(sps_names, paste0("output/27October25/mammalSpeciesTaxonomy.csv"), row.names = FALSE)


message("✅ Screened GBIF data exported to 'GBIF_occurrences.csv'.")
