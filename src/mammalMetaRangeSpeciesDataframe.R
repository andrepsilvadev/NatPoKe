##################################
# FORMATING MAMMAL SPS DATAFRAME #
##################################
# Ines Silva
# 04 Feb 2025 # Updates on 30th of March 2026

## Select Target Biome (choose one)
#target_biome <- "Boreal Forests/Taiga" # Options: "Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga"

## Select Target Region (choose one)
#target_region <- "Europe" # Options: "North America", "South America", "Europe", "Asia", "Africa"

# species_table <- read.csv(
#   file.path(data_dir, "species_by_region.csv"),
#   stringsAsFactors = FALSE)
# 
# target_species <- species_table |>
#   dplyr::filter(
#     BIOME_NAME == target_biome,
#     CONTINENT  == target_region
#   ) |>
#   dplyr::pull(sci_name) |>
#   unique()

##########
# Step 1 # Import Trait Dataframe 
##########

combined_traits_data <- read_csv(here("data", "traitData", "CompleteMammalSpsDataframe_2025-12-20.csv")) %>% 
  # filter for prefered area & species
  dplyr::filter(BIOME_NAME %in% target_biome) %>% 
  dplyr::filter(CONTINENT %in% if (target_region == "Europe") {
    c("Europe", "Asia")
  } else {
    target_region
  }
  ) %>% 
  dplyr::filter(sci_name %in% target_species) 


##########
# Step 2 # Format dataframe for metaRange
##########

# Because landscapes being used as of (30th of March of 2026 are in **meters**
# the traits' values should also be in meters so a conversion must be made
# (Vasco V. noticed this!)

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
  # Maximum age (years)
  MaxAge = combined_traits_data$max_longevity_d / 365,
  # age at first reproduction (years)
  AgeFirstReproduction = combined_traits_data$age_first_reproduction_d / 365,
  # fixed modelling resolution to aggregate landscapes (m)
  ModellingRes = 25000,
  # orignal landscape's cell resolution (m)
  CellResolution =  0.04166667*111139,
  # modelling resolution based on the sps mean HomeRange (km)
  # initial number of individuals per cell (from PredMd, in Ind/km2, Santini et al. 2022)
  initialAbundance = ceiling(as.numeric(combined_traits_data$PredMd) * ((ModellingRes/1000)^2)), 
  # maximum number of individuals per cell (from up75, in Ind/km2, Santini et al. 2022)
  carryingCapacity = ceiling(as.numeric(combined_traits_data$up75) * ((ModellingRes/1000)^2)), 
  # net reproduction rate
  reproductionRate = combined_traits_data$litter_size_n2 * (MaxAge - AgeFirstReproduction),
  # Mean dispersal distance according to Schloss et al. 2012 (based on trophic level)
  dispersalDistance = {
    disp_km <- ifelse(
      combined_traits_data$trophic_level == "Carnivore", (3.45 * BodyMass^0.89),
      ifelse(combined_traits_data$trophic_level %in% c("Herbivore","Omnivore"), (1.45 * BodyMass^0.54), NA)
    )
    pmax(1, (disp_km * 1000) / ModellingRes)   # cells
  },
  # Maximum long-distance dispersal according to Schloss et al. 2012 (based on trophic level)
  dispersalMaxDistance = ceiling({
    max_km <- ifelse(
      combined_traits_data$trophic_level == "Carnivore", (40.7 * BodyMass^0.81),
      ifelse(combined_traits_data$trophic_level %in% c("Herbivore","Omnivore"), (3.31 * BodyMass^0.65), NA)
    )
    as.integer(pmax(1, ceiling((max_km * 1000) / ModellingRes)))  # cells (integer)
  }),
  # yearly survival rate (from mortality rate based on McCarthy 2008 and Savage 2004)
  yearlySurvivalRate = 1 - (BodyMass^-0.25)
) %>% 
  distinct(Species, .keep_all = TRUE) %>%
  drop_na() 

# check NA's
sapply(species_traits, function(x) sum(is.na(x))) # number NA per column
sapply(species_traits, function(x) sum(is.na(x)/length(x))) # proportion NA per column

##########
# STEP 3 # Write final trait dataframe
##########

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
