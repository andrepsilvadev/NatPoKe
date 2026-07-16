## Name: taxaOccurrences_birds.R ##
## Author: Inês Silva ##
## Date: 03 Jan 2026
## Description: Download and filter bird sps occurrence data from GBIF ##

## Script Instructions
# STEP 1. Extract species occurrence records from GBIF
# STEP 2. Filter by data quality and minimum sample size
# STEP 3. Spatially restrict occurrences to IUCN range polygons
# STEP 4. Allow for manual taxonomic vetting (subspecies, synonyms)
# STEP 5. Export a clean CSV file ("GBIF_occurrences_mammals.csv").

# load packages & functions
source("./src/libraries.R")
#source("./src/customFunctions.R")
source("./src/customFunctions2.R")

get_taxa_occurrences <- function(
    iucn_shapefile, # IUCN or BIRDLIFE spatial file
    taxa_filter = NULL, # taxa names to use in case we want to subset specific species
    gbif_user = "", # GBIF username
    gbif_pwd = "", # GBIF password
    gbif_email = "", # GBIF account email
    ncores = 6 # number of cores to use in case parallelisation is available and worth it
) {
  
  require(sf)
  require(dplyr)
  require(rgbif)
  require(future.apply)
  
  ##########
  # STEP 1 # Read IUCN ranges and species list
  ##########
  
  message("Selecting species names from IUCN ranges file")
  
  IUCN_ranges <- sf::st_read(iucn_shapefile, quiet = TRUE)
  sf::sf_use_s2(FALSE)
  invisible(gc())
  # keep unique species names
  taxa_spp <- unique(IUCN_ranges$sci_name)
  # filter for selected taxa if provided
  if (!is.null(taxa_filter)) {
    taxa_spp <- taxa_spp[taxa_spp %in% taxa_filter]
  }
  
  ##########
  # STEP 2 # Get GBIF taxon keys
  ##########
  
  gbif_taxon_keys <- name_backbone_checklist(name = taxa_spp) %>%
    dplyr::filter(matchType == "EXACT") %>%
    dplyr::pull(usageKey)
  
  invisible(gc())
  
  ##########
  # STEP 3 # Download occurrences with coordinates only Download GBIF data
  ##########
  
  download_key <- occ_download(
    pred_in("taxonKey", gbif_taxon_keys),
    # living species
    pred("OCCURRENCE_STATUS","PRESENT"),
    # human or machine occurrences
    pred_in("basisOfRecord",
            c("HUMAN_OBSERVATION","OBSERVATION",
              "MACHINE_OBSERVATION","OCCURRENCE")),
    # with coordinates values
    pred("hasCoordinate", TRUE),
    pred_notnull("decimalLatitude"),
    pred_notnull("decimalLongitude"),
    # frmo 2015 onwards
    pred_gte("year", 2015),
    # file format
    format = "SIMPLE_CSV",
    # specify gbif credentials
    user = gbif_user,
    pwd = gbif_pwd,
    email = gbif_email
  )
  
  message("Downloading occurrences file from GBIF. Please wait...")
  occ_download_wait(download_key)
  
  # retrieve and import downloaded file
  d <- occ_download_get(key = download_key) #"0071953-251120083545085")
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
  missing_spp <- setdiff(taxa_spp, unique(gbif_data_filtered$species))
  # Check and report
  if (length(missing_spp) > 0) {
    message("!! WARNING !! Species with insufficient records:")
    message(paste(missing_spp, collapse = ", "))
  }
  
  ########
  # STEP # Crop GBIF occurrences within IUCN range
  ########
  
  # convert GBIF occ points to and sf object
  gbif_sf <- st_as_sf(
    gbif_data_filtered,
    coords = c("decimalLongitude", "decimalLatitude"),
    crs = st_crs(IUCN_ranges)
  )
  invisible(gc())
  
  # split files (range and occ) per species 
  ## this is to increase speed and save save RAM space in works when running parallelisation)
  message("Splitting datasets by species...")
  poly_list <- split(IUCN_ranges, IUCN_ranges$sci_name)
  pts_list  <- split(gbif_sf, gbif_sf$species)
  
  # work only with species present in both
  taxa_spp2 <- intersect(names(poly_list), names(pts_list))
  
  rm(IUCN_ranges, gbif_sf)
  invisible(gc())
  
  message("Cropping occurrences by species IUCN range...")
  # ---- Windows - sequential ----
  if (.Platform$OS.type == "windows") {
    
    message("Windows detected → running sequentially")
    
    occurrences_list <- lapply(
      taxa_spp2,
      function(sp) {
        message(sp)
        st_join(
          pts_list[[sp]],
          poly_list[[sp]],
          join = st_within,
          left = FALSE
        )
      }
    )
    
  } else {
    # ---- Linux - parallellisation ----   
    message("Unix-like OS detected → running multicore (", ncores, " workers)")
    
    future::plan(multicore, workers = ncores)
    
    occurrences_list <- future_lapply(
      taxa_spp2,
      function(sp) {
        st_join(
          pts_list[[sp]],
          poly_list[[sp]],
          join = st_within,
          left = FALSE
        )
      }
    )
    
    future::plan(sequential)
  }
  
  # combine results
  occurrences_in_range <- dplyr::bind_rows(occurrences_list)
  invisible(gc())
  return(occurrences_in_range)
}
##########
# STEP 1 # Retrieve occurrences from GBIF using user-defined function
##########

# Use get_taxa_occurrences() function from (customFunctions2.R)
# Downloads GBIF occurrence records for a set of species, applies quality
# filters, and crops occurrences to IUCN range polygons. Retruns: An sf object
# of GBIF occurrences spatially restricted to IUCN ranges.

# load species list
species_table <- read_excel("data/traitData/BirdSpecies_selection.xlsx", 
                       sheet = "CompleteSpeciesDf") 
# pull species names
target_species <- unique(species_table$sci_name)

# load GBIF credentials
keys <- yaml::read_yaml("./config/api_keys.yml")


# run get_taxa_occurrences() function for birds
GBIF_data <- get_taxa_occurrences(
  iucn_shapefile = "./data/externaldata/BOTW_2024_2.gpkg",
  taxa_filter = target_species,
  gbif_user = keys$gbif_user,
  gbif_pwd = keys$gbif_pwd,
  gbif_email = keys$gbif_email)

# get citation for the occurrences file being retrieved
gbif_citation(download_key) # using the downloadkey

# clean memory space
invisible(gc())
invisible(gc(rm(IUCN_ranges, gbif_taxon_keys, keys)))

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

# print warning message
message("STOP & ANALYSE - Please check species vs. verbatim names before going further...")

# save .csv for manual review
write.csv(species_name_check,
          "./data/bird_species_verbatim_check.csv",
          row.names = FALSE)

# print warning message
message("STOP & ANALYSE - Please check species vs. verbatim names before going further...")

## APPLY TAXONOMIC EXCLUSIONS ##
# now manually review the taxonomic list names in 'data/species_verbatim_check.csv'
# if suspect names or subspecies are included add 'exclude' to column C
# re-import the file and clean records (works as a blacklist)

exclusions <- read.csv("./data/bird_species_verbatim_check.csv") %>% 
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


##############################
# QUICK CHEKUP TO SHOW ANDRÉ #
##############################

# # world map as sf
# world <- ne_countries(scale = "medium", returnclass = "sf")
# 
# ggplot() +
#   geom_sf(data = world, fill = "grey95", color = "grey70") +
#   geom_point(
#     data = GBIF_data_df,
#     aes(
#       x = decimalLongitude,
#       y = decimalLatitude,
#       color = species
#     ),
#     size = 1,
#     alpha = 0.7
#   ) +
#   coord_sf(expand = FALSE) +
#   theme_minimal() +
#   labs(
#     x = "Longitude",
#     y = "Latitude",
#     color = "Species"
#   )
