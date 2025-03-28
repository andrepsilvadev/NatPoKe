######################################################
# MAMMALS WITH COMPLETE TRAITS AND MODELLINGRES PLOT #
######################################################
# Inês Silva
# 27 March 2025

# packages
library(tidyverse)
library(xlsx)
library(patchwork)
library(openxlsx)

# import csv
combined_traits_data <- read_csv(here("data", "mammalTraits_2025-03-17.csv")) %>% 
  mutate(Trophic = case_when(
    # based on Schloss 2012
    Diet.Meat >= 90 ~ "Carnivore", Diet.Plant >= 90 ~ "Herbivore",
    TRUE ~ NA_character_),
    trophic_level = case_when(
      # from original database
      trophic_level == 1 ~ "Herbivore", trophic_level == 2 ~ "Omnivore", trophic_level == 3 ~ "Carnivore",
      TRUE ~ as.character(trophic_level)),
    # maximum age (years)
    MaxAge = max_longevity_d / 365,
    # age at first reproduction
    AgeFirstReproduction = age_first_reproduction_d / 365
  ) %>% 
  distinct()

species_traits <- tibble(
  # species index
  Index = 1:nrow(combined_traits_data), 
  # BIOME
  Biome = combined_traits_data$BIOME_NAME,
  #Continent
  Continent = combined_traits_data$CONTINENT,
  # scientific name WITHOUT spaces
  Species = stringr::str_replace_all(combined_traits_data$sci_name, " ", ""), 
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
  CellResolution = 3.076948*3.076948,
  # modelling resolution based on the sps mean HomeRange (km)
  ##ModellingRes = ceiling(sqrt(2/as.numeric(combined_traits_data$IndsHaCell))), # ANDRE'S MODELLING RES
  #ModellingRes = ceiling(sqrt(combined_traits_data$Mean_HomeRange_km2)), 
  ModellingRes = 10,
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
  drop_na()

# delete weird column and keep only one row per species
fileToSave <- species_traits %>% 
  distinct() %>% 
  as.data.frame()

# write into a csv
write.csv(fileToSave, "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/traits/all_mammals_available_for_metaRange_model.csv", row.names = FALSE)

# write into a xlsx
write.xlsx(fileToSave, "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/traits/all_mammals_available_for_metaRange_model.xlsx", row.names = FALSE)

# add this to my organised sheet
wb <- loadWorkbook(file = "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/traits/all_mammals_available_before_trait_formatting.xlsx")
addWorksheet(wb, sheetName = "formatted_trait_data")
writeData(wb, sheet = "formatted_trait_data", x = fileToSave)
# Save the changes to the workbook
saveWorkbook(wb, "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/traits/all_mammals_available_before_trait_formatting.xlsx", overwrite = TRUE)

###############################
# CHECK MODELLING RESOLUTIONS #
###############################

sps_modelling_res <- species_traits %>%
  # filter for tropical and boreal forests
  dplyr::filter(Biome %in% c("Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga")) %>% 
  # select only species and their modelling resolutions
  dplyr::select(Species, ModellingRes) %>% 
  # keep one row per species
  distinct()

# plot all modelling resolutions
free <- ggplot(sps_modelling_res, aes(x = "", y = ModellingRes)) + 
  geom_boxplot(outlier.shape = NA, fill = "lightblue", alpha = 0.5) +  # Box plot
  geom_jitter(aes(color = Species), size = 2, alpha = 0.7, position = position_jitter(seed = 1)) + # Points for each species
  geom_text(aes(label = Species), hjust = -0.1, size = 3, check_overlap = TRUE, position = position_jitter(seed = 1)) + # Labels next to points
  labs(y = "Modelling Resolution", x = "", title = "Species Modelling Resolution") + 
  theme_minimal() +
  theme(legend.position = "none") 

# limit yy axis to see better where most modelling resolutions are
limited <- ggplot(sps_modelling_res, aes(x = "", y = ModellingRes)) + 
  geom_boxplot(outlier.shape = NA, fill = "lightblue", alpha = 0.5) +  # Box plot
  geom_jitter(aes(color = Species), size = 2, alpha = 0.7, position = position_jitter(seed = 1)) + # Points for each species
  geom_text(aes(label = Species), hjust = -0.1, size = 3, check_overlap = TRUE, position = position_jitter(seed = 1)) + # Labels next to points
  labs(y = "Modelling Resolution", x = "") + 
  ylim(0,10) +
  theme_minimal() +
  theme(legend.position = "none")  


# calculate how many species are outside the boxplot area
# step 1 - compute Q1, Q3, and IQR
Q1 <- quantile(sps_modelling_res$ModellingRes, 0.25, na.rm = TRUE)
Q3 <- quantile(sps_modelling_res$ModellingRes, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
# step 2- define lower and upper fences for outliers
lower_fence <- Q1 - 1.5 * IQR
upper_fence <- Q3 + 1.5 * IQR
# step 3 - count outliers outside boxplot area
outlier_count <- sum(sps_modelling_res$ModellingRes < lower_fence | 
                       sps_modelling_res$ModellingRes > upper_fence, na.rm = TRUE)
# step 4 - calculate the same but in %
outlier_percentage <- (outlier_count / nrow(sps_modelling_res)) * 100


# build figure to send to Stefan and André
sps_modellingRes <- free + limited + plot_annotation(caption = paste0("Percentage of species outside the boxplot: ", round(outlier_percentage, 2), "%\n"))
# save figure
ggsave(plot = sps_modellingRes,
       file = "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/all_mammals_resolution.tiff",
       bg = 'white', width = 400, height = 200, units = "mm", dpi = 1200, compression = "lzw")
