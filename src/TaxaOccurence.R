## Taxa occurrences ##
## Andre P. Silva & Afonso Barrocal ## Jorinde-M. Rieger ##
## Description: Downloads taxa occurence for multiple species from GBIF Database ##
## May 19th, 2025 ##

# Use IUCN species names and ranges
# downloaded manually, later find a way to download automatically through R
sps <- sf::st_read("~/data/data/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")
sps_names <- unique(sps$sci_name)
species_group <- "mammals"

# for testing only
#speciesTest <- c("Alces alces", "Canis lupus")
#sps_names <- as.data.frame(sps_names) %>% dplyr::filter(sps_names %in% speciesTest)

# Bird species
sps <- read.csv("~/data/data/trait_datasets/birdTraits_withNAs(in).csv")
sps_names <- unique(sps$sci_name)
species_group <- "birds"

# Extract occurrences available in GBIF (e.g. mammals). Filter species >30 records
gbif_taxon_keys <-
  as.data.frame(sps_names) %>%
  pull("sps_names") %>% #use the sps names from the file
  name_backbone_checklist() %>% #match to backbone
  filter(!matchType == "NONE") %>% #get matched names
  pull(usageKey) #get the GBIF taxon keys

# to download datasets from gbif credentials are necessary.
# Register at https://www.gbif.org/user/profile

test <- occ_download( # creates key
  pred_in("taxonKey", gbif_taxon_keys),
  format = "SIMPLE_CSV",
  user = "jorinde_rgr", # ADD USERNAME HERE
  pwd = "NatPoKe2025", # ADD PASSWORD HERE
  email = "jorinde.rieger@su.se") # ADD EMAIL ASSOCIATE WITH ACCOUNT HERE

# check if download is finished
occ_download_wait('0006748-250515123054153') # ADD download key from "test"

# retrieve the download from GBIF to local computer
d <- occ_download_get(
  key = '0006748-250515123054153', # Exchange with download key "test"
  path = "~/data/data/trait_datasets"
)

# with the download key we can go directly to gbif and download the folder with
# the data without running the script again

# import download to current session
gbif_data <- occ_download_import(d)

GBIF_sps <- 
  gbif_data %>%
  # remove occurrences without coordinates
  drop_na(c(decimalLatitude, decimalLongitude)) %>% 
  group_by(species) %>%
  # filter individuals with more than 30 occurrences
  dplyr::filter(n() > 30)

# Write species occurences, with the subselection of variables
write.csv(
  GBIF_sps[, c("species", "decimalLatitude", "decimalLongitude", "year")],
  file = paste0("~/data/data/trait_datasets/GBIF_",species_group, "_30+occurrences_062025.csv"),
  row.names = FALSE
)
invisible(gc())

# View the unique species
unique_species <- unique(GBIF_sps$species)
# Convert to a data frame
unique_species_df <- data.frame(species = unique_species)

# Write to a CSV file
write.csv(
  unique_species_df,
  file = paste0("~/data/data/trait_datasets/GBIF_", species_group, "_species_names.csv"),
  row.names = FALSE
)