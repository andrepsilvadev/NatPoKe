## checkup of modelling resolution ##
## Inês Silva ##
## 05Feb2026 ##


## The intent of this script is just to confirm and check the modelling resolution
## being used to model mammal species with metaRange. Modelling Resolution, available
## in the CompleteMammalSpsDataframe_2025-12-20.csv (with all species with complete trait data),
## is based on the square root of species HomeRange_km2 values, which were
## obtained from the HomeRange package keeping only isopleths values over 90%

# target mammal species
species_table <- read.csv("./data/traitData/CompleteMammalSpsDataframe_2025-12-20.csv",
                          stringsAsFactors = FALSE)

# average modelling resolution values per biome&region
avg_ModellingRes_perRegion <- species_table %>% 
  group_by(BIOME_NAME, CONTINENT) %>% 
  dplyr::summarise(avg_modellingRes = mean(ModellingRes, na.rm = TRUE),
                   # add number of unique species considered
                   n_species = length(unique(sci_name)))

# A tibble: 5 × 4
# # Groups:   BIOME_NAME [2]
# BIOME_NAME                                     CONTINENT     avg_modellingRes n_species
# <chr>                                          <chr>                    <dbl>     <int>
# 1 Boreal Forests/Taiga                           Europe+Asia              25.1         15
# 2 Boreal Forests/Taiga                           North America            19.2         18
# 3 Tropical & Subtropical Moist Broadleaf Forests Africa                   11.3         39
# 4 Tropical & Subtropical Moist Broadleaf Forests Asia                      9.71        41
# 5 Tropical & Subtropical Moist Broadleaf Forests South America             5.7         20

# average modelling resolution across all species
mean(species_table$ModellingRes, na.rm = TRUE)
# [1] 13.37324