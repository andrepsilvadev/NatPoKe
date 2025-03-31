## Taxa occurrences ##
## Andre P. Silva & Afonso Barrocal ##
## January 7th, 2025 ##

# Use IUCN species names and ranges
# downloaded manually, later find a way to download automatically through R
IUCN_mammals <- sf::st_read("~/data/data/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")
mammal_sps <- unique(IUCN_mammals$sci_name)

# for testing only
#speciesTest <- c("Alces alces", "Canis lupus")
#mammal_sps <- as.data.frame(mammal_sps) %>% dplyr::filter(mammal_sps %in% speciesTest)

# Extract occurrences available in GBIF (e.g. mammals). Filter species >30 records
gbif_taxon_keys <-
  as.data.frame(mammal_sps) %>%
  pull("mammal_sps") %>% #use the sps names from the file
  name_backbone_checklist() %>% #match to backbone
  filter(!matchType == "NONE") %>% #get matched names
  pull(usageKey) #get the GBIF taxon keys

test <- occ_download( # creates key
  pred_in("taxonKey", gbif_taxon_keys),
  format = "SIMPLE_CSV",
  user = "jorinde_rgr", # needs registration before #"maria_ines_silva"
  pwd = "NatPoKe2025", #"SRIT2024!"
  email = "jorinde.rieger@su.se") #"ncisines@gmail.com"

# check if download is finished
occ_download_wait('0008317-250325103851331')
invisible(gc())

# retrieve the download from GBIF to local computer
d <- occ_download_get(
  key = '0008317-250325103851331',
  path = "~/data/data/trait_datasets"
)

# import download to current session
gbif_data <- occ_download_import(d)

GBIF_mammal_sps <- 
  gbif_data %>%
  # remove occurrences without coordinates
  drop_na(c(decimalLatitude, decimalLongitude)) %>% 
  group_by(species) %>%
  # filter individuals with more than 30 occurrences
  dplyr::filter(n() > 30)

# Write species occurences, with the subselection of variables
write.csv(
  GBIF_mammal_sps[, c("species", "decimalLatitude", "decimalLongitude", "year")],
  "~/data/data/trait_datasets/GBIF_mammal_30+occurrences_speciesTest.csv",
  row.names = FALSE
)
invisible(gc())
