#######################################
# FORMATTING BIRD SPS TRAIT DATAFRAME #
#######################################
# 19 March 2025

##########
# STEP 1 # Define area and species to model
##########

selected_biome <- "Tropical & Subtropical Moist Broadleaf Forests" # Tropical & Subtropical Moist Broadleaf Forests OR Boreal Forests/Taiga

selected_continent <- c("South America", "Africa") # "North America" OR "South America" OR "Europe" OR "Asia" OR "Antarctica" OR "Africa" OR "Australia" OR "Oceania"     

selected_species <- c("Ramphastos toco", "Jynx torquilla")

##########
# STEP 2 # Import Trait Dataframe & Filter for target species
##########

combined_traits_data <- read_csv(here("birdtraits.csv")) %>% 
  # filter for prefered area & species
  filter(BIOME_NAME == selected_biome & CONTINENT %in% selected_continent & sci_name %in% selected_species)


##########
# STEP 3 # Format dataframe for metaRange
##########

birdSpeciesTraits <- tibble(
  # species index
  Index = 1:nrow(combined_traits_data), 
  # scientific name WITHOUT spaces
  Species = stringr::str_replace_all(combined_traits_data$sci_name, " ", ""), 
  # family
  Family = combined_traits_data$family,
  # order
  Order = combined_traits_data$order,
  # trophic level (with 3 factors)
  TrophicLevel = combined_traits_data$Trophic.Level, 
  # taxa
  Taxa = "Birds",
  # body mass (kg)
  BodyMass = combined_traits_data$combined_bodyMass_g / 1000, 
  # cell area (km2)
  #CellResolution = 3.076948*3.076948,
  ModellingRes = 5,
  #ModellingRes = ceiling(sqrt(combined_traits_data$Mean_HomeRange_km2)), 
  # initial number of individuals per cell (from PredMd, in Ind/km2, Santini et al. 2023)
  initialAbundance = ceiling(as.numeric(combined_traits_data$Predicted_Density_n_km2)), 
  # maximum number of individuals per cell (from up75, in Ind/km2, Santini et al. 2022)
  carryingCapacity = ceiling(as.numeric(combined_traits_data$Q75)), 
  # net reproduction rate
  reproductionRate = combined_traits_data$Clutch * (combined_traits_data$combined_longevity - (combined_traits_data$combined_age_at_maturity/365)),
  # Mean dispersal distance according to Sutherland et al 2000 (based on trophic level & Body Mass)
  dispersalDistance = ifelse(
    combined_traits_data$Trophic.Level == "Carnivore", pmax((36.4 * BodyMass^0.62)/ModellingRes, ModellingRes), 
    ifelse(combined_traits_data$Trophic.Level == "Herbivore", pmax((2.1 * BodyMass^0.18)/ModellingRes, ModellingRes),
           ifelse(combined_traits_data$Trophic.Level == "Omnivore", pmax((2.1 * BodyMass^0.18)/ModellingRes, ModellingRes),NA))), # If the computed value is smaller than the modelling resolution, it is adjusted to be at least (ModellingRes + 1).
  # Maximum long-distance dispersal according to Sutherland et al 2000 (based on trophic level & Body Mass)
  dispersalMaxDistance = ifelse(
    combined_traits_data$Trophic.Level == "Carnivore", pmax((199.5 * BodyMass^0.59)/ModellingRes, ModellingRes),
    ifelse(combined_traits_data$Trophic.Level == "Herbivore", pmax((36.4 * BodyMass^0.14)/ModellingRes, ModellingRes),
           ifelse(combined_traits_data$Trophic.Level == "Omnivore", pmax((36.4 * BodyMass^0.14)/ModellingRes, ModellingRes),NA))),
  # yearly survival rate (from mortality rate based on McCarthy 2008 and Savage 2004)
  yearlySurvivalRate = 1 - BodyMass^-0.21
) %>%
  drop_na()  

