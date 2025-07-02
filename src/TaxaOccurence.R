## TaxaOccurrence.R ##
## Jorinde-M. Rieger ##
## Description: Downloads taxa occurence for multiple species from GBIF Database ##
## June 11th, 2025 ##

# Use IUCN species names and ranges
# downloaded manually, later find a way to download automatically through R
#sps <- sf::st_read("~/data/data/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")
#species_group <- "mammals"
#sps_names <- unique(sps$sci_name)

# Mammals trait data set in Boreal and Tropical biome
sps<- readr::read_csv(
  paste0("~/data/data/trait_datasets/mammalTraits_2025-02-03.csv")
)
species_group <- "mammalsWTrait"
sps_names <- unique(sps$Species)

# for testing only
#species_group <- "testMammals"
#testSpecies <- c("Alces alces", "Canis lupus", "Tragelaphus scriptus")
#sps_names <- as.data.frame(sps_names) %>% dplyr::filter(sps_names %in% testSpecies)

# Bird species IUCN
#sps <- read.csv("~/data/data/trait_datasets/birdTraits_withNAs(in).csv")
#sps_names <- unique(sps$sci_name)
#species_group <- "birds"

# Bird species BirdLife Iberian peninsula
# downloaded manually from https://www.birdlife.org/ subset of Iberian peninsula
#sps <- read.csv("~/data/data/trait_datasets/BirdLife_Iberian_peninsula.csv")
#species_group <- "birds"

# Subset according to IUCN Red list category: vulnerable (VU), endangered (EN), critically endangered (CR)
# Check column names to specify the category and specifications
#threatened_status <- c("VU", "EN", "CR")
#sps_threatened <- sps %>%
#  filter(category %in% threatened_status)

#sps_threatened <- sps%>%
#  filter(RL.Category %in% threatened_status)
#sps_names <- unique(sps_threatened$Scientific.name)
#length(unique(sps_threatened$Scientific.name))

# Safe as Excel sheet if needed
#writexl::write_xlsx(sps_threatened, path = paste0("~/data/data/trait_datasets/SpeciesIUCNCategory", species_group, extent,".xlsx"))

# Overlay IUCN data with extent e.g. Iberian peninsula, take list of intersection
#extent = "Iberian peninsula"
#extent_name <- "Iberian peninsula"
#extent_sf <- rnaturalearth::ne_countries(scale = "medium", country = c("Spain", "Portugal"), returnclass = "sf")
#sf::sf_use_s2(FALSE) # disable s2
#sps_extent <- sf::st_intersection(sps_threatened, extent_sf) # or sps_threatened
#length(unique(sps_extent$sci_name))
#sps_names <- unique(sps_extent$sci_name) #Macaca sylvanus is non-native species

# Safe as Excel sheet if needed
#writexl::write_xlsx(sps_extent, path = paste0("~/data/data/trait_datasets/SpeciesIUCNCategory", species_group, extent,".xlsx"))

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
occ_download_wait('0064707-250525065834625') # ADD download key from "test"

# retrieve the download from GBIF to local computer
d <- occ_download_get(
  key = '0064707-250525065834625', # Exchange with download key "test"
  path = "~/data/data/trait_datasets", 
  overwrite = TRUE
)

# with the download key we can go directly to gbif and download the folder with
# the data without running the script again
# Mammals global: '0030856-250525065834625'
# Mammals globale with Trait data: '0064707-250525065834625'
# Mammals global test species: '0050651-250525065834625'
# Mammals threatened subset Iberian peninsula: '0042465-250525065834625'
# Birds threatened subset Iberian peninsula: '0042559-250525065834625'

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
  file = paste0("~/data/data/trait_datasets/GBIF_",species_group, "_30+occurrences_", extent,".csv"),
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
  file = paste0("~/data/data/trait_datasets/GBIF_", species_group, extent, "_sps_names.csv"),
  row.names = FALSE
)
