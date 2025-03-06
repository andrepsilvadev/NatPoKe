#####################################
# FORMATING SPECIES INPUT DATAFRAME #
#####################################
# Ines Silva
# 04 Feb 2025


##########
# STEP 1 # Define area and species to model
##########

selected_biome <- "Boreal Forests/Taiga" # Tropical & Subtropical Moist Broadleaf Forests OR Boreal Forests/Taiga

selected_continent <- "Europe" # "North America" OR "South America" OR "Europe" OR "Asia" OR "Antarctica" OR "Africa" OR "Australia" OR "Oceania"     

selected_species <- c("Alces alces", "Cervus elaphus", "Lynx lynx", "Rangifer tarandus")

##########
# STEP 2 # Import Trait Dataframe 
##########

combined_traits_data <- read_csv(here("data", "mammalTraits_2025-03-06.csv")) %>% 
  # filter for prefered area & species
  filter(BIOME_NAME == selected_biome & CONTINENT == selected_continent & Species %in% selected_species) %>% 
  mutate(
    Trophic = case_when(
      # based on Schloss 2012
      Diet.Meat >= 90 ~ "Carnivore",
      Diet.Plant >= 90 ~ "Herbivore",
      TRUE ~ NA_character_
    ),
    trophic_level = case_when(
      # from original database
      trophic_level == 1 ~ "Herbivore",
      trophic_level == 2 ~ "Omnivore",
      trophic_level == 3 ~ "Carnivore",
      TRUE ~ as.character(trophic_level) 
    )
  )

##########
# STEP 3 # Format dataframe for metaRange
##########

species_traits <- tibble(
  Index = 1:nrow(combined_traits_data), # species index
  Species = stringr::str_replace_all(combined_traits_data$sci_name, " ", ""), # scientific name WITHOUT spaces
  Family = combined_traits_data$family.x, # family
  Order = combined_traits_data$order_, # order
  TrophicLevel = combined_traits_data$trophic_level, # trophic level with 3 factors
  Taxa = "Mammal",
  BodyMass = combined_traits_data$Mass.g / 1000, # species body mass (kg)
  CellResolution = 3.076948*3.076948, # Cell area (km2)
  #ModellingRes = ceiling(sqrt(2/as.numeric(combined_traits_data$IndsHaCell))), # ANDRE'S MODELLING RES
  ModellingRes = ceiling(sqrt(combined_traits_data$Mean_HomeRang_km2)), # STEFAN'S MODELLING RES
  #ProjRes = ModellingRes*1000,
  initialAbundance = ceiling(as.numeric(combined_traits_data$PredMd)*(ModellingRes^2)), # initial number of individuals per cell (from PredMd, in Ind/km2, Santini et al. 2022)
  carryingCapacity = ceiling(as.numeric(combined_traits_data$up75)*(ModellingRes^2)), # maximum number of individuals per cell (from up75, in Ind/km2, Santini et al. 2022)
  reproductionRate = combined_traits_data$litter_size_n, # Litter size
  dispersalDistance = ifelse(
    combined_traits_data$trophic_level == "Carnivore", pmax(3.45 * BodyMass^0.89, ModellingRes + 1), # Mean dispersal distance according to Schloss et al. 2012 (based on trophic level)
    ifelse(combined_traits_data$trophic_level == "Herbivore", pmax(1.45 * BodyMass^0.54, ModellingRes + 1), NA)), # If the computed value is smaller than the modelling resolution, it is adjusted to be at least (ModellingRes + 1).
  dispersalMaxDistance = case_when(
    combined_traits_data$trophic_level == "Carnivore" ~ 40.7 * BodyMass^0.81,
    combined_traits_data$trophic_level == "Herbivore" ~ 3.31 * BodyMass^0.65, TRUE ~ NA_real_), # Maximum long-distance dispersal according to Schloss et al. 2012 (based on trophic level)
  yearlySurvivalRate = 1 - (BodyMass^-0.25)  # based on McCarthy 2008 and Savage 2004
  ) %>%
  drop_na()  

# check NA's
sapply(species_traits, function(x) sum(is.na(x))) # number NA per column
sapply(species_traits, function(x) sum(is.na(x)/length(x))) # proportion NA per column

# write table to .csv file
write_csv(species_traits, file = file.path(dirinput,"metaRangeSpeciesDataframe.csv"))

# remove unecessary objects
#rm(rast_obj, res_x, res_y, filename, file)
rm(combined_traits_data)

