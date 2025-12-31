## Name: speciesSelection.R ##
## Author: Inês Silva ##
## Description: CheckPoints to assess species available for modelling pipeline
## Date: December 05th 2025 ##

# Data source: SRIT trait database (ADD Zenodo LINK HERE OR GitHub LINKr)
#
# Expected folder structure:
# project_root/
#   data/
#        mammalTraits_2025-12-11.csv
#        birdTraits_2025-12-11.csv
#   src/
#     01_selectSpecies.R
#     (...)
#
# How to run:
#   1. Download SRIT-database from Zenodo/GitHub
#   2. Place it in /data/SRIT-database/
#   3. Run this script top-to-bottom

source("./src/libraries.R")

##########
# STEP 0 # Path & Parameters set up
##########

# specify target biomes
target_biomes <- c("Boreal Forests/Taiga",
                   "Tropical & Subtropical Moist Broadleaf Forests")

# specify target regions
target_regions <- c("Europe",
                    "North America",
                    "South America",
                    "Asia",
                    "Africa")

#####################
# MAMMALS SELECTION # -----------------------------------------------------------
#####################

message("Processing trait data for MAMMAL SPECIES ")

##########
# STEP 1 # Load raw trait dataset $ select target traits
##########

raw_mammalTraits <- read.csv("./data/mammalTraits_2025-12-11.csv")

# check available traits names
#colnames(raw_mammalTraits)  

# specify target traits (metaRange specific)
mammal_target_traits <- c("sci_name", # scientific species name (from IUCN)
                   "family.x", "order_", # taxonomic info
                   "BIOME_NAME", "CONTINENT", # ecosystem typology
                   "trophic_level", # trophic level (herbivore, carnivore and omnivore)
                   "Mass.g", # weight in grams
                   "max_longevity_d", # maximum longevity (days)
                   "age_first_reproduction_d", # age of first reproduction (days)
                   "litter_size_n", # number of offspring 
                   "litters_per_year_n", # number of repriductive events
                   "PredMd", # predicted density (ind/km2)
                   "up75", # upper percentile of estimated density (ind/km2)
                   "Mean_HomeRange_km2" # mean value of home range area (km2)
)

##########
# STEP 2 # Process Trait data
##########

mammalTraits_processed <- raw_mammalTraits %>% 
  # filter for target biomes & regions
  dplyr::filter(BIOME_NAME %in% target_biomes &
                  CONTINENT  %in% target_regions) %>% 
  # select target trait columns
  dplyr::select(mammal_target_traits) %>% 
  # estimate modelling resolution for all species from home range area
  mutate(ModellingRes = ceiling(sqrt(Mean_HomeRange_km2)),
         ResClass = case_when(
           ModellingRes < 2 ~ "<2 km",
           ModellingRes >= 2 & ModellingRes <= 10 ~ "2_10 km",
           ModellingRes > 10 ~ ">10 km",
           TRUE ~ "Unknown" ),
         trophic_level = case_when(
           trophic_level == 1 ~ "Herbivore",
           trophic_level == 2 ~ "Omnivore",
           trophic_level == 3 ~ "Carnivore",
           TRUE ~ as.character(trophic_level))
  ) %>%
  # remove species if all of the following traits are empty
  dplyr::filter(
    !if_any(
      c("trophic_level", "Mass.g", "max_longevity_d",
        "age_first_reproduction_d", "litter_size_n",
        "litters_per_year_n", "PredMd", "up75"),
      is.na
    )
  ) 

##########
# STEP 3 # Add species occurrences (from GBIF with rgbif pckg)
##########

mammalSpecies <- unique(mammalTraits_processed$sci_name)

message("Querying GBIF for ", length(mammalSpecies), " species...")
pb <- utils::txtProgressBar(min = 0, max = length(mammalSpecies), style = 3)
occ_total <- integer(length(mammalSpecies))
occ_2015  <- integer(length(mammalSpecies))

for (i in seq_along(mammalSpecies)) {
  
  # Total occurrences (with coordinates)
  occ_total[i] <- rgbif::occ_count(
    scientificName = mammalSpecies[i],
    hasCoordinate  = TRUE,
    basisOfRecord = "HUMAN_OBSERVATION;OBSERVATION;OCCURRENCE;MACHINE_OBSERVATION"
  )
  
  # Occurrences from 2015 onwards
  occ_2015[i] <- rgbif::occ_count(
    scientificName = mammalSpecies[i],
    hasCoordinate  = TRUE,
    basisOfRecord = "HUMAN_OBSERVATION;OBSERVATION;OCCURRENCE;MACHINE_OBSERVATION",
    year           = "2015,2024"
  )
  # add progress bar as this step might take a while
  utils::setTxtProgressBar(pb, i)
}
close(pb)

# format gbif output as a dataframe
occ_df <- data.frame(
  sci_name       = mammalSpecies,
  occ_count_all  = occ_total,
  occ_count_2015 = occ_2015,
  row.names      = NULL
)


##########
# STEP 4 # Join traits with occ counts numbers & MAKE SELECTION
##########

# In the case of mammals species are going to be filtered by:
## HomeRange sizes over 2km
## Body masses over 5 kg
## Number of occurrences above 30 records with coordinates
## If many species result from here a mannual selection can be done by carefully
## analysing the .csv and .xlsx files produce in STEP 5

mammalTraits_processed <- mammalTraits_processed %>% 
  # join with occ numbers
  left_join(occ_df, by = "sci_name") %>% 
  distinct(sci_name, BIOME_NAME, CONTINENT, .keep_all = TRUE) %>% 
  mutate(CONTINENT = case_when(BIOME_NAME == "Boreal Forests/Taiga" & CONTINENT %in% c("Europe", "Asia") ~ "Europe+Asia",
      TRUE ~ CONTINENT)) %>% 
  dplyr::filter(
    # filter out tropical forests in north america (not our goal here)
    !(BIOME_NAME == "Tropical & Subtropical Moist Broadleaf Forests" & CONTINENT == "North America"),
    # homeRange (ResClass) over 2km
    ResClass %in% c("2_10 km", ">10 km"),
    # number of occurrences over 30 with coordinates
    occ_count_2015 >= 30,
    # body mass over 5 kg (= 5000 g)
    Mass.g >= 5000
  )

##########
# STEP 5 # .csv AND .xslx FILES PRODUCTION 
##########

# write complete table to a .csv file (data folder)
write.csv(mammalTraits_processed,
          file = paste0("./data/CompleteMammalSpsDataframe_", Sys.Date(), ".csv"),
          row.names = FALSE)

# check number of available sps for target areas
biome_cont_troph_spp <- mammalTraits_processed %>%
  dplyr::count(BIOME_NAME, CONTINENT, trophic_level, name = "n_species") %>%
  arrange(desc(n_species))
message(paste0("Number of species per biome × continent × trophic level: ", length(unique(mammalTraits_processed$sci_name))))
print(biome_cont_troph_spp)

# split dfs per continent to better view
selectedMammals_perContinent <- split(mammalTraits_processed,
                                      mammalTraits_processed$CONTINENT)
#selectedMammals_perContinent$Europe

# write .xlsx 
wb <- openxlsx::createWorkbook()
# README file
addWorksheet(wb, "README")
writeData(
  wb,
  "README",
  c(
    "README",
    "",
    "This Excel file contains trait data for mammal species available for metaRange that meet the following criteria: over 30 occ records in GBIF with coordinates (2015-2024), over 5000 g body mass and a homerange size of at least 2 km2",
    "",
    "Sheets:",
    paste0("- CompleteSpeciesDf: complete filtered dataset for ", length(unique(mammalTraits_processed$sci_name)), " species"),
    "- BiomeContTrophic_SPP: species counts per biome, continent and trophic level",
    "- One sheet per continent with species meeting filter criteria"
  ))
# Complete dataset
addWorksheet(wb, "CompleteSpeciesDf")
writeData(wb, "CompleteSpeciesDf", mammalTraits_processed)
# Summary table
addWorksheet(wb, "BiomeContTropphic_SPP")
writeData(wb, "BiomeContTropphic_SPP", biome_cont_troph_spp)
# One sheet per continent
for (ct in names(selectedMammals_perContinent)) {
  addWorksheet(wb, ct)
  writeData(wb, ct, selectedMammals_perContinent[[ct]])
}

# Save file
saveWorkbook(wb, "./data/MammalSpecies_selection.xlsx", overwrite = TRUE)

# final message 
message("✅ Traits selection for mammals species done!\n\nCheck MammalSpecies_selection.xlsx and CompleteMammalSpsDataframe.csv")

###################
# BIRDS SELECTION # -----------------------------------------------------------
###################

message("Processing trait data for BIRD SPECIES")

##########
# STEP 1 # Load raw trait dataset & select target traits
##########

raw_birdTraits <- read.csv("./data/birdTraits_2025-07-19.csv")

# check available traits names
colnames(raw_birdTraits)  

# specify target traits (metaRange specific)
bird_target_traits <- c("sci_name", # scientific species name (from IUCN)
                   "Family", "Order", # taxonomic info
                   "BIOME_NAME", "CONTINENT", # ecosystem typology
                   "Trophic.Level",
                   "adult_body_mass_g", # adult weight (grams)
                   "Maximum_longevity_M", # maximum longevity (months)
                   "Age_at_first_reproduction_M", # age of first reproduction (months)
                   "Clutch", # clutch size (egg number) 
                   "Adult_survival_M",
                   "Predicted_Density_n_km2", # predicted density (ind/km2)
                   "Q75" # upper percentile of estimated density (ind/km2)
)

 

##########
# STEP 2 # Process Trait data
##########

birdTraits_processed <- raw_birdTraits %>% 
  # filter for target biomes & regions
  dplyr::filter(BIOME_NAME %in% target_biomes &
                  CONTINENT  %in% target_regions) %>% 
  # select target trait columns
  dplyr::select(bird_target_traits) %>%
  # remove species if all of the following traits are empty
  dplyr::filter(
    !if_any(
      bird_target_traits,
      is.na
    )
  ) 


birdSpecies <- unique(birdTraits_processed$sci_name)

message("Querying GBIF for ", length(birdSpecies), " species...")
pb <- utils::txtProgressBar(min = 0, max = length(birdSpecies), style = 3)
occ_totalb <- integer(length(birdSpecies))
occ_2015b  <- integer(length(birdSpecies))

for (i in seq_along(birdSpecies)) {
  
  # Total occurrences (with coordinates)
  occ_totalb[i] <- rgbif::occ_count(
    scientificName = birdSpecies[i],
    hasCoordinate  = TRUE,
    basisOfRecord = "HUMAN_OBSERVATION;OBSERVATION;OCCURRENCE;MACHINE_OBSERVATION"
  )
  
  # Occurrences from 2015 onwards
  occ_2015b[i] <- rgbif::occ_count(
    scientificName = birdSpecies[i],
    hasCoordinate  = TRUE,
    basisOfRecord = "HUMAN_OBSERVATION;OBSERVATION;OCCURRENCE;MACHINE_OBSERVATION",
    year           = "2015,*"
  )
  # add progress bar as this step might take a while
  utils::setTxtProgressBar(pb, i)
}
close(pb)

# format gbif output as a dataframe
occ_df <- data.frame(
  sci_name       = birdSpecies,
  occ_count_all  = occ_totalb,
  occ_count_2015 = occ_2015b,
  row.names      = NULL
)

##########
# STEP 4 # Join traits with occ counts numbers & MAKE SELECTION
##########

# In the case of bird species, these are going to be filtered by:
## Body masses over 0.5 kg
## Number of occurrences above 30 records with coordinates since 2015

birdTraits_processed <- birdTraits_processed %>% 
  # join with occ numbers
  left_join(occ_df, by = "sci_name") %>% 
  distinct(sci_name, BIOME_NAME, CONTINENT, .keep_all = TRUE) %>% 
  mutate(CONTINENT = case_when(BIOME_NAME == "Boreal Forests/Taiga" & CONTINENT %in% c("Europe", "Asia") ~ "Europe+Asia",
                               TRUE ~ CONTINENT)) %>% 
  # split sci_name but keep it
  separate(
    sci_name,
    into = c("genus", "species"),
    sep = " ",
    remove = FALSE
  ) %>% 
  dplyr::filter(
    # filter out tropical forests in north america (not our goal here)
    !(BIOME_NAME == "Tropical & Subtropical Moist Broadleaf Forests" & CONTINENT == "North America"),
    # number of occurrences over 30 with coordinates
    occ_count_2015 >= 30,
    # body mass over 5 kg (= 5000 g)
    adult_body_mass_g >= 500
  )

##########
# STEP 5 # .csv AND .xslx FILES PRODUCTION 
##########

# write complete table to a .csv file (data folder)
write.csv(birdTraits_processed,
          file = paste0("./data/CompleteBirdSpsDataframe_", Sys.Date(), ".csv"),
          row.names = FALSE)

# check number of available sps for target areas
biome_cont_troph_spp_birds <- birdTraits_processed %>%
  dplyr::count(BIOME_NAME, CONTINENT, Trophic.Level, genus, name = "n_species") %>%
  arrange(desc(n_species))
message(paste0("Number of species per biome × continent × trophic level: ", length(unique(birdTraits_processed$sci_name))))
print(biome_cont_troph_spp_birds)

# split dfs per continent to better view
selectedBirds_perContinent <- split(birdTraits_processed,
                                      birdTraits_processed$CONTINENT)

# write .xlsx 
wb <- openxlsx::createWorkbook()
# README file
addWorksheet(wb, "README")
writeData(
  wb,
  "README",
  c(
    "README",
    "",
    "This Excel file contains trait data for bird species available for metaRange with all traits values, adult body mass over 500 g (0.5 kg) and more than 30 occurrences since 2015.",
    "",
    "Sheets:",
    paste0("- CompleteSpeciesDf: complete filtered dataset for ", length(unique(birdTraits_processed$sci_name)), " species"),
    "- BiomeContTrophic_SPP: species counts per biome, continent and trophic level",
    "- One sheet per continent with species meeting filter criteria"
  ))
# Complete dataset
addWorksheet(wb, "CompleteSpeciesDf")
writeData(wb, "CompleteSpeciesDf", birdTraits_processed)
# Summary table
addWorksheet(wb, "BiomeContTropphic_SPP")
writeData(wb, "BiomeContTropphic_SPP", biome_cont_troph_spp_birds)
# One sheet per continent
for (ct in names(selectedBirds_perContinent)) {
  addWorksheet(wb, ct)
  writeData(wb, ct, selectedBirds_perContinent[[ct]])
}

# Save file
saveWorkbook(wb, "./data/BirdSpecies_selection.xlsx", overwrite = TRUE)

# final message 
message("✅ Traits selection for bird species done!\n\nCheck BirdSpecies_selection.xlsx and CompleteBirdpsDataframe.csv")

##########
# STEP 6 # write a .csv file with all mammals and bird species per region
##########

df <- bind_rows(mammalTraits_processed %>%
                               dplyr::select(BIOME_NAME, CONTINENT, sci_name),
                             birdTraits_processed %>%
                               dplyr::select(BIOME_NAME, CONTINENT, sci_name))
write.csv(df, 
          file = "./data/species_by_region.csv",
          row.names = FALSE)

