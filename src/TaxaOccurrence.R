## TaxaOccurrence.R ##
## Jorinde-M. Rieger ##
## Description: Downloads taxa occurrence for multiple species from GBIF Database ##
## Date: August 4th 2025 ##

# Define input species -----------------------------------------------------------------
# Use IUCN species names and ranges
# downloaded manually, later find a way to download automatically through R
#sps <- sf::st_read("data/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp") 
#species_group <- "mammals"

# Mammals selected for NatPoKe project based on trait and occurrence data availability
species_group <- "NatPoKeMammals"
sps_names <- c("Sus scrofa", "Vulpes vulpes", "Alces alces", "Canis latrans", "Lynx rufus", "Martes americana", "Taxidea taxus", "Ursus americanus", 
               "Leontopithecus caissara", "Leopardus pardalis", "Nasua nasua", "Aepyceros melampus", "Colobus angolensis", "Daubentonia madagascariensis",
               "Diceros bicornis", "Erythrocebus patas", "Gorilla beringei", "Gorilla gorilla", "Orycteropus afer", "Pan paniscus", "Pan troglodytes",
               "Papio anubis", "Papio ursinus", "Cervus nippon", "Cuon alpinus", "Felis chaus", "Macaca fuscata", "Pongo abelii", "Pongo pygmaeus",
               "Lynx lynx", "Ursus arctos", "Canis lupus", "Rangifer tarandus", "Puma concolor", "Bison bison", "Ursus arctos", "Panthera onca",
               "Crocuta crocuta", "Mandrillus sphinx", "Panthera pardus", "Syncerus caffer", "Acinonyx jubatus", "Panthera leo", "Connochaetes taurinus",
               "Loxodonta africana", "Acinonyx jubatus", "Panthera tigris")


# Bird species IUCN
#sps <- read.csv("data/trait_datasets/birdTraits_withNAs(in).csv")
#sps_names <- unique(sps$sci_name)
#species_group <- "birds"

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
  user = "USERNAME", # ADD USERNAME HERE
  pwd = "PASSWORD", # ADD PASSWORD HERE
  email = "EMAIL") # ADD EMAIL ASSOCIATE WITH ACCOUNT HERE

# check if download is finished
occ_download_wait('0021647-250717081556266') # ADD download key from "test"

# retrieve the download from GBIF to local computer
d <- occ_download_get(
  key = '0021647-250717081556266', # Exchange with download key "test"
  path = "data/trait_datasets", 
  overwrite = TRUE
)

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
  file = paste0("data/trait_datasets/GBIF_",species_group, "_30+occurrences_.csv"),
  row.names = FALSE
)

# View the target species and add them to SDMRun.R
targetSpecies <- unique(GBIF_sps$species)

invisible(gc())
