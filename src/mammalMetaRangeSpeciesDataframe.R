##################################
# FORMATING MAMMAL SPS DATAFRAME #
##################################
# Ines Silva
# 04 Feb 2025

## Select Target Biome (choose one)
#target_biome <- "Boreal Forests/Taiga" # Options: "Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga"

## Select Target Region (choose one)
#target_region <- "Europe" # Options: "North America", "South America", "Europe", "Asia", "Africa"

## Select scenario
#scenario <- "ssp126"

# # target species
# targetSpecies <- c(## BOREAL SPS ##
#   "Alces alces", "Canis lupus", "Bison bonasus", "Cervus elaphus", 
#   "Sus scrofa", "Vulpes vulpes", "Canis latrans", "Lynx rufus",
#   "Martes americana", "Taxidea taxus", "Ursus americanus", "Panthera tigris",
#   "Lynx lynx", "Ursus arctos", "Rangifer tarandus",
#   "Puma concolor", "Bison bison"#,
#   
#   ## TROPICAL SPS ##
#   #"Leontopithecus caissara", # has only 4 occurences
#   #"Leopardus pardalis", "Nasua nasua", "Aepyceros melampus",
#   #"Colobus angolensis", "Daubentonia madagascariensis",
#   #"Diceros bicornis", "Erythrocebus patas", "Gorilla beringei",
#   #"Gorilla gorilla", "Orycteropus afer", "Pan paniscus",
#   #"Pan troglodytes", "Papio anubis", "Papio ursinus", "Cervus nippon",
#   #"Cuon alpinus", "Felis chaus", "Macaca fuscata", "Pongo abelii",
#   #"Pongo pygmaeus",  "Panthera onca", "Crocuta crocuta", "Mandrillus sphinx",
#   #"Panthera pardus", "Syncerus caffer", "Acinonyx jubatus",
#   #"Panthera leo", "Connochaetes taurinus", "Loxodonta africana",
#   #"Puma concolor"
# )

##########
# Step 1 # Import Trait Dataframe 
##########

combined_traits_data <- read_csv(here("data", "mammalTraits_2025-03-17.csv")) %>% 
  # filter for prefered area & species
  dplyr::filter(BIOME_NAME %in% gsub("[/& ]", "", target_biome)) %>% 
  dplyr::filter(CONTINENT %in% if (gsub("[/& ]", "", target_region) == "Europe") {
    c("Europe", "Asia")
  } else {
    gsub("[/& ]", "", target_region)
  }
  ) %>% 
  dplyr::filter(sci_name %in% target_species) 


##########
# Step 2 # Format dataframe for metaRange
##########

species_traits <- tibble(
  # species index
  Index = 1:nrow(combined_traits_data), 
  # BIOME
  #Biome = combined_traits_data$BIOME_NAME,
  # scientific name WITHOUT spaces
  Species = stringr::str_replace_all(combined_traits_data$sci_name, " ", "."), 
  # family
  Family = combined_traits_data$family.x,
  # order
  Order = combined_traits_data$order_,
  # trophic level (with 3 factors)
  TrophicLevel = combined_traits_data$trophic_level, 
  # taxa
  Taxa = "Mammal",
  # body mass (kg)
  BodyMass = combined_traits_data$Mass.g / 1000, 
  # cell area (km2)
  CellResolution = (0.04166667*111)^2,
  # modelling resolution based on the sps mean HomeRange (km)
  ##ModellingRes = ceiling(sqrt(2/as.numeric(combined_traits_data$IndsHaCell))), # ANDRE'S MODELLING RES
  #ModellingRes = ceiling(sqrt(combined_traits_data$Mean_HomeRange_km2)), 
  ModellingRes = 25,
  #ProjRes = ModellingRes*1000,
  # initial number of individuals per cell (from PredMd, in Ind/km2, Santini et al. 2022)
  initialAbundance = ceiling(as.numeric(combined_traits_data$PredMd)*(ModellingRes^2)), 
  # maximum number of individuals per cell (from up75, in Ind/km2, Santini et al. 2022)
  carryingCapacity = ceiling(as.numeric(combined_traits_data$up75)*(ModellingRes^2)), 
  # net reproduction rate
  reproductionRate = combined_traits_data$litter_size_n * (combined_traits_data$MaxAge - combined_traits_data$AgeFirstReproduction),
  # Mean dispersal distance according to Schloss et al. 2012 (based on trophic level)
  dispersalDistance = ifelse(
    combined_traits_data$trophic_level == "Carnivore", pmax((3.45 * BodyMass^0.89)/ModellingRes, ModellingRes), 
    ifelse(combined_traits_data$trophic_level == "Herbivore", pmax((1.45 * BodyMass^0.54)/ModellingRes, ModellingRes),
           ifelse(combined_traits_data$trophic_level == "Omnivore", pmax((1.45 * BodyMass^0.54)/ModellingRes, ModellingRes), NA))), # If the computed value is smaller than the modelling resolution, it is adjusted to be at least (ModellingRes + 1).
  # Maximum long-distance dispersal according to Schloss et al. 2012 (based on trophic level)
  dispersalMaxDistance = ceiling(ifelse(
    combined_traits_data$trophic_level == "Carnivore", pmax((40.7 * BodyMass^0.81)/ModellingRes,ModellingRes),
    ifelse(combined_traits_data$trophic_level == "Herbivore", pmax((3.31 * BodyMass^0.65)/ModellingRes, ModellingRes),
           ifelse(combined_traits_data$trophic_level == "Omnivore", pmax((3.31 * BodyMass^0.65)/ModellingRes, ModellingRes), NA)))),
  # yearly survival rate (from mortality rate based on McCarthy 2008 and Savage 2004)
  yearlySurvivalRate = 1 - (BodyMass^-0.25)
) %>% 
  distinct(Species, .keep_all = TRUE) %>% drop_na() 

# check NA's
sapply(species_traits, function(x) sum(is.na(x))) # number NA per column
sapply(species_traits, function(x) sum(is.na(x)/length(x))) # proportion NA per column


# write table to .csv file
write_csv(species_traits, file = file.path(dirinput,"metaRangeSpeciesDataframe.csv"))

# remove unecessary objects
#rm(rast_obj, res_x, res_y, filename, file)
rm(combined_traits_data)

# final message with species codes
message(
  "✅ Species dataframe save successfully!\n",
  "It contains trait data for ", paste(length(unique(species_traits$Species)), " species. "),
  paste(unique(species_traits$Species), collapse = ", "))
