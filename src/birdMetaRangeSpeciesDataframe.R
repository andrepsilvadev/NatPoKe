#################################
# FORMATTING BIRD SPS DATAFRAME #
#################################
# Inês Silva
# 19 March 2025

##########
# Step 1 # Import Trait Dataframe 
##########

combined_traits_data <- read_csv(here("data", "birdTraits_2025-07-19.csv")) %>% 
#   # filter for prefered area & species
   dplyr::filter(BIOME_NAME %in% target_biome)
#%>% 
#   dplyr::filter(CONTINENT %in% target_region) %>% 
#   dplyr::filter(sci_name %in% target_species)

target_biome <- c("Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga")
target_region <- c("South America", "Africa")
target_species <- c("Ramphastos toco", "Jynx torquilla")

##########
# Step 2 # Format dataframe for metaRange
##########

birdSpeciesTraits <- tibble(
  # species index
  Index = 1:nrow(combined_traits_data), 
  # scientific name WITHOUT spaces
  Species = stringr::str_replace_all(combined_traits_data$sci_name, " ", ""), 
  # family
  Family = combined_traits_data$Family,
  # order
  Order = combined_traits_data$Order,
  # trophic level (with 3 factors)
  TrophicLevel = combined_traits_data$Trophic.Level, 
  # taxa
  Taxa = "Birds",
  # body mass (kg)
  BodyMass = combined_traits_data$adult_body_mass_g / 1000, 
  # cell area (km2)
  #CellResolution = 3.076948*3.076948,
  ModellingRes = 5,
  #ModellingRes = ceiling(sqrt(combined_traits_data$Mean_HomeRange_km2)), 
  # initial number of individuals per cell (from PredMd, in Ind/km2, Santini et al. 2023)
  initialAbundance = ceiling(as.numeric(combined_traits_data$Predicted_Density_n_km2)), 
  # maximum number of individuals per cell (from up75, in Ind/km2, Santini et al. 2022)
  carryingCapacity = ceiling(as.numeric(combined_traits_data$Q75)), 
  # net reproduction rate
  reproductionRate = combined_traits_data$Clutch * (combined_traits_data$Maximum_longevity_M - (combined_traits_data$Age_at_first_reproduction_M/365)),
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
  yearlySurvivalRate = combined_traits_data$Adult_survival_M #1 - BodyMass^-0.21
) %>%
  drop_na()  
# check NA's
sapply(species_traits, function(x) sum(is.na(x))) # number NA per column
sapply(species_traits, function(x) sum(is.na(x)/length(x))) # proportion NA per column


# write table to .csv file
write_csv(birdSpeciesTraits, file = file.path("./birdMetaRangeSpeciesDataframe.csv"))

# remove unecessary objects
rm(combined_traits_data)
getwd()
