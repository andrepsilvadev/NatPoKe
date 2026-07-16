## checkup of number of families of mammals used ##
## Inês Silva ##
## 21April2026 ##

MammalSpecies_selection <- read_excel("data/traitData/MammalSpecies_selection.xlsx", 
                                      sheet = "CompleteSpeciesDf")
View(MammalSpecies_selection)
str(MammalSpecies_selection)

library(dplyr)
library(tidyr)

family_summary <- MammalSpecies_selection %>%
  group_by(family.x) %>%
  summarise(
    n_species = n_distinct(sci_name)
  ) %>%
  arrange(desc(n_species))

family_summary

family_biome_summary <- MammalSpecies_selection %>%
  distinct(family.x, BIOME_NAME, sci_name) %>%
  count(family.x, BIOME_NAME, name = "n_species") %>% 
  pivot_wider(names_from = BIOME_NAME, values_from = n_species)
