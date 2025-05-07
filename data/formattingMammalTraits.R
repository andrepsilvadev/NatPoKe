########################################
# FURTHER FORMATTING MAMMAL TRAIT DATA #
########################################
# Inês Silva
# 05 May 2024

source("./src/libraries.R")
source("./src/customFunctions.R")  

combined_traits_data <- read_csv(here("data", "mammalTraits_2025-03-17.csv")) %>% 
  #dplyr::filter(BIOME_NAME %in% c("Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga")) %>% 
  mutate(
    BIOME_NAME = gsub("[/& ]", "", BIOME_NAME),
    CONTINENT = gsub("[/& ]", "", CONTINENT),
    Trophic = case_when(
      # based on Schloss 2012
      Diet.Meat >= 90 ~ "Carnivore",
      Diet.Plant >= 90 ~ "Herbivore",
      TRUE ~ NA_character_),
    trophic_level = case_when(
      # from original database
      trophic_level == 1 ~ "Herbivore",
      trophic_level == 2 ~ "Omnivore",
      trophic_level == 3 ~ "Carnivore",
      TRUE ~ as.character(trophic_level)))

write.csv(combined_traits_data, 
          file = "data/mammalTraits_2025-03-17.csv")
