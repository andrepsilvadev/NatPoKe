###################################
# UPDATED SPATIALLY EXPLICIT MAPS #
###################################
# Ines Silva
# 28 March 2025

##########
# Step 1 # Set up, load needed packages & functions
##########

library(here)
source(here("src", "libraries.R"))
source(here("src", "customFunctions.R"))

##########
# Step 2 # Load abundance rasters
##########

# specifically look for the rasters in Robinson projection (better looking maps)

# list all directories with outputs to map
europe <- "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/28Mar2025_EuropeRobinson/Outputs"
northAmerica <- "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/28Mar2025_NorthAmericaRobinson/Outputs"
southAmerica <- "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/28Mar2025_SouthAmericaRobinson/Outputs"
africa <- "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/28Mar2025_AfricaRobinson/Outputs"
asia <- "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/28Mar2025_AsiaRobinson/Outputs"
# all directories
directories <- c(europe, asia, africa, southAmerica, northAmerica)

# get every species that was modeled for the outputs
target_species <- c("Alcesalces", "Lynxlynx", "Canislupus", "Susscrofa", "Rangifertarandus", "Odocoileusvirginianus", "Cervuselaphus", "Damadama", "Lynxrufus", "Crocutacrocuta", "Pantheraleo", "Pantheratigris", "Pumaconcolor", "Callithrix jacchus", "Nasua nasua")


# Initialize an empty list to store final dataframes
all_final_data <- list()

# Loop through each directory
for (dir in directories) {
  # Initialize an empty list to store dataframes for the current directory
  dir_data <- list()
  
  # Loop through each species within the directory
  for (target_sps in target_species) {
    # Import abundance raster for timestep 101
    sps101_files <- list.files(path = dir,
                               pattern = paste0(".*101_", target_sps, "_abundance\\.tif"),
                               full.names = TRUE)
    if (length(sps101_files) > 0) {
      sps101 <- rast(sps101_files)
      sps101_df <- as.data.frame(sps101, xy = TRUE) %>%
        mutate(timestep = 101, species = target_sps)
      dir_data[[paste(target_sps, "101", sep = "_")]] <- sps101_df
    }
    
    # Import abundance raster for timestep 125
    sps125_files <- list.files(path = dir,
                               pattern = paste0(".*125_", target_sps, "_abundance\\.tif"),
                               full.names = TRUE)
    if (length(sps125_files) > 0) {
      sps125 <- rast(sps125_files)
      sps125_df <- as.data.frame(sps125, xy = TRUE) %>%
        mutate(timestep = 125, species = target_sps)
      dir_data[[paste(target_sps, "125", sep = "_")]] <- sps125_df
    }
  }
  
  # Combine all dataframes for the current directory into a single dataframe
  final_df <- bind_rows(dir_data)
  
  # Extract the second-to-last folder name as the key
  folder_names <- strsplit(dir, "/")[[1]] #split path into each folder name.
  short_dir_name <- folder_names[length(folder_names) - 1] #get the second to last.
  
  # Store the final dataframe in the all_final_data list, using the directory path as the name
  all_final_data[[short_dir_name]] <- final_df
}


##########
# Step 3 # Calculate Shannon's Index & the Change per cell
##########

Shannon_indexes <- list()

for (dir_name in names(all_final_data)) { 
  df <- all_final_data[[dir_name]] 
  
  Shannon_index_df <- df %>% 
    dplyr::filter(lyr1 != 0) %>% # keep only cells where species exist 
    group_by(timestep, x, y) %>%
    dplyr::mutate(p_i = lyr1 / sum(lyr1),
                  # calculate proportion of individuals of species i
                  ln_p_i = ifelse(p_i > 0, log(p_i), 0)) %>%  # in case pi is 0
    # up until here the table has values for each species, then info is summarised
    dplyr::summarize(Shannon_Wiener_Index = -sum(p_i * ln_p_i)) %>%   # calculate the Shannon-Wiener index
    group_by(x, y) %>% #group only by x and y for the change calculation.
    mutate(
      Shannon_change = (Shannon_Wiener_Index - Shannon_Wiener_Index[timestep == 101])
    ) %>% 
    dplyr::filter(timestep == 125)
  
  Shannon_indexes[[dir_name]] <- Shannon_index_df # Store the result per region
}

##########
# Step 4 # Calculate Functional Diversity Index & the Change per cell
##########

# import again the mammal traits so I can get the functional level of each species
mammalTraits <- read_csv(here("data", "mammalTraits_2025-03-17.csv")) %>% 
  dplyr::filter(BIOME_NAME %in% c("Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga")) %>% 
  mutate(Trophic = case_when(
                              # based on Schloss 2012
                              Diet.Meat >= 90 ~ "Carnivore",
                              Diet.Plant >= 90 ~ "Herbivore",
                              TRUE ~ NA_character_),
    trophic_level = case_when(# from original database
                              trophic_level == 1 ~ "Herbivore",
                              trophic_level == 2 ~ "Omnivore",
                              trophic_level == 3 ~ "Carnivore",
                              TRUE ~ as.character(trophic_level)),
    sci_name = stringr::str_replace_all(sci_name, " ", "")) %>% 
  distinct()
      

# left join each dataframe in the list with the mammal traits
all_final_data_funct <- map(all_final_data, ~ suppressWarnings(left_join(.x, mammalTraits %>%
                                             select("sci_name", "trophic_level"), by = c("species"="sci_name"))))
# carefull if changing the code, I suppressed the warnings

functionalDiv_index <- list()
# go through each dataset to calculate fucntional diversity index and its change
for (dir_name in names(all_final_data_funct)) { 
  df <- all_final_data_funct[[dir_name]] 

  functionalDiv_index_df <- df %>% 
    dplyr::filter(lyr1 != 0) %>% # keep only cells where species exist
    group_by(timestep, trophic_level, x, y) %>%
    dplyr::summarise(Fmean_TNIND = mean(lyr1, na.rm = TRUE)) %>%
    group_by(timestep, x, y) %>%
    dplyr::mutate(Fp_i = Fmean_TNIND / sum(Fmean_TNIND),
                  # calculate proportion of individuals of fucntional group i
                  Fln_p_i = ifelse(Fp_i > 0, log(Fp_i), 0)) %>%  # in case Fpi is 0
    # up until here the table has values for each functional group, then info is summarised
    dplyr::summarize(Funct_diversity_Index = -sum(Fp_i * Fln_p_i)) %>%   # calculate the functional diversity index
    # calculate the Shannon-Wiener index
    group_by(x, y) %>% #group only by x and y for the change calculation.
    mutate(
      Functional_change = (Funct_diversity_Index - Funct_diversity_Index[timestep == 101])
    ) %>% 
    dplyr::filter(timestep == 125)
  
  functionalDiv_index[[dir_name]] <- functionalDiv_index_df # Store the result per region
}

##########
# Step 5 # Prepare & build maps insets for better visualisation 
##########

regions <- c("Europe", "North America", "South America", "Africa", "Asia")
# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")
unique(world$continent)

region_sfs <- list()
# Loop through each region
for (region in regions) {
  # Filter the world map for the current region
  region_sf <- world[world$continent == region,] %>% 
    dplyr::select(continent, geometry)
  region_sfs[[region]] <- st_transform(region_sf, crs = "ESRI:54030")
}

# work on flat earth
sf_use_s2(FALSE) 
# function to load and select the biome shapefile
load_select_biome <- function(biome_name) {
  biome_sf <- st_read(here("data/Ecoregions2017", "Ecoregions2017.shp"))
  biome_sf[biome_sf$BIOME_NAME == biome_name, ] %>% 
    group_by(BIOME_NAME) %>% 
    summarise(geometry = st_union(geometry))
}

# function to load and select continents
load_select_continents <- function(continent_names) {
  continents <- ne_countries(scale = "medium", returnclass = "sf") %>%
    dplyr::filter(continent %in% continent_names) %>% 
    group_by(continent) %>%
    summarise(geometry = st_union(geometry))
}

# function to crop the biome boundaries to the continents
crop_biome_to_continent <- function(biome, continent_geom) {
  st_intersection(biome, continent_geom)
}

# Asia forests
asia_trop <- crop_biome_to_continent(biome = load_select_biome(biome_name = "Tropical & Subtropical Moist Broadleaf Forests"),
                                     continent_geom = load_select_continents(continent_names = "Asia"))
# South America forests
southAmerica_trop <- crop_biome_to_continent(biome = load_select_biome(biome_name = "Tropical & Subtropical Moist Broadleaf Forests"),
                                             continent_geom = load_select_continents(continent_names = "South America"))
# Africa forests
africa_trop <- crop_biome_to_continent(biome = load_select_biome(biome_name = "Tropical & Subtropical Moist Broadleaf Forests"),
                                       continent_geom = load_select_continents(continent_names = "Africa"))
# North America forests
northAmerica_bor <- crop_biome_to_continent(biome = load_select_biome(biome_name = "Boreal Forests/Taiga"),
                                            continent_geom = load_select_continents(continent_names = "North America"))
# Europe forests
europe_bor <- crop_biome_to_continent(biome = load_select_biome(biome_name = "Boreal Forests/Taiga"),
                                      continent_geom = load_select_continents(continent_names = "Europe"))

# south america inset - continent + tropical forests
southamerica_plot <- ggplot() +
  geom_sf(data = region_sfs[["South America"]], color = "black", fill = "gray95") + 
  geom_sf(data = southAmerica_trop, fill = "gray20") +
  coord_sf(crs = st_crs("ESRI:54030")) +
  theme_void()

# africa inset - continent + tropical forests
africa_plot <- ggplot() +
  geom_sf(data = region_sfs[["Africa"]], color = "black", fill = "gray95") + 
  geom_sf(data = africa_trop, fill = "gray20") +
  coord_sf(crs = st_crs("ESRI:54030")) +
  theme_void()

# asia inset - continent + boreal forests
asia_plot <- ggplot() +
  geom_sf(data = region_sfs[["Asia"]], color = "black", fill = "gray95") + 
  geom_sf(data = asia_trop, fill = "gray20") +
  coord_sf(crs = st_crs("ESRI:54030")) +
  theme_void()

# europe inset - continent + tropical forests
europe_plot <- ggplot() +
  geom_sf(data = region_sfs[["Europe"]], color = "black", fill = "gray95") + 
  geom_sf(data = europe_bor, fill = "gray20") +
  coord_sf(crs = st_crs("ESRI:54030"), xlim = c(-2984101.5843,13538200), ylim = c(3825520.3916,7850400)) +
  theme_void()

# north america inset - continent + tropical forests
northamerica_plot <- ggplot() +
  geom_sf(data = region_sfs[["North America"]], color = "black", fill = "gray95") + 
  geom_sf(data = northAmerica_bor, fill = "gray20") +
  coord_sf(crs = st_crs("ESRI:54030"), xlim = c(-15030000, -1500000), ylim = c(3031000, 8134000)) +
  theme_void()


##########
# Step 6 # Build actual SHANNON'S INDEX change maps
##########

## SOUTH AMERICA ##
southamerica_shannon <- ggplot() +
  geom_tile(data = Shannon_indexes$`28Mar2025_SouthAmericaRobinson`, aes(x = x, y = y, fill = Shannon_change)) +
  scale_fill_viridis_c(name = "Shannon's Index\nChange", limits = c(-0.5, 0.5)) +
  ylim(-1207500,-1200500) + 
  labs(x = "Latitude", y = "Longitude" , title = "South America") +
  #scale_fill_gradientn(colors = rev(brewer.pal(11, "RdYlBu")), limits = c(-0.5, 0.5), na.value = "transparent", name = "Shannon Index\nChange") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

################################################################################
################################################################################

# ADD THIS TOMORROW!!!!!!!!!!!!!!!!
#panel.border = element_rect(colour = "black", fill=NA, linewidth=5)
## https://www.google.com/search?sca_esv=cf65d87ff3121f3e&sxsrf=AHTn8zoSmRkFmTLjSuW9L4w7bR52yo4K6g:1745953204951&q=shared+socioeconomic+pathway+ssp+scenarios+maps&udm=2&fbs=ABzOT_CXl75FCxE-ABa2Bmjosysol6fG0rtSzgy4yP8EvogSGKBDYjWFRwUGWattUZ3lI872XWDECKqw28n-aR24Drtbj-KqRCevFFaHabPnZGrOAxJOfY762rSnSIMtKYNC63VTtpwWcNiyIGsE9BtTLVBiPzR1udH2MGdJbkNUCu6f0EttXvzyOAYMbGnCSrPnZiWlyjU_&sa=X&ved=2ahUKEwj_lc3E9v2MAxU7cKQEHbYMEUgQtKgLegQIExAB&biw=1536&bih=825&dpr=2#imgrc=hbAfyY60yD7d8M&imgdii=AdILw7oRiNs52M


################################################################################
################################################################################


# build final south america plot
southAmerica_Shannon <- southamerica_shannon + inset_element(southamerica_plot, 0.6, 0.7, 1, 1)
# # save plot
# ggsave(plot = southAmerica_Shannon,
#        file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/NatPoke_figures/SouthAmericaShannonIndexMap.tif",
#        bg = 'white', width = 250, height = 300, units = "mm", dpi = 1200, compression = "lzw")


## AFRICA ##
africa_shannon <- ggplot() +
  geom_tile(data = Shannon_indexes$`28Mar2025_AfricaRobinson`, aes(x = x, y = y, fill = Shannon_change)) +
  xlim(1734000, 1742000) +
  ylim(-1176000, -1171000)+
  scale_fill_viridis_c(name = "Shannon's Index\nChange", limits = c(-0.5, 0.5)) +
  labs(x = "Latitude", y = "Longitude" , title = "Africa") +
  #scale_fill_gradientn(colors = rev(brewer.pal(11, "RdYlBu")), limits = c(-0.5, 0.5), na.value = "transparent", name = "Shannon Index\nChange") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# build final africa plot
Africa_Shannon <- africa_shannon + inset_element(africa_plot, 0.7, 0.7, 1, 1)
# # save plot
# ggsave(plot = Africa_Shannon,
#        file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/NatPoke_figures/AfricaShannonIndexMap.tif",
#        bg = 'white', width = 300, height = 200, units = "mm", dpi = 1200, compression = "lzw")


## ASIA ##
asia_shannon <- ggplot() +
  geom_tile(data = Shannon_indexes$`28Mar2025_AsiaRobinson`, aes(x = x, y = y, fill = Shannon_change)) +
  scale_fill_viridis_c(name = "Shannon's Index\nChange", limits = c(-0.5, 0.5)) +
  labs(x = "Latitude", y = "Longitude" , title = "Asia") +
  #scale_fill_gradientn(colors = rev(brewer.pal(11, "RdYlBu")), limits = c(-0.5, 0.5), na.value = "transparent", name = "Shannon Index\nChange") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# build final asia plot
Asia_Shannon <- asia_shannon + inset_element(asia_plot, 0.7, 0.7, 1, 1)
# save plot
# ggsave(plot = Asia_Shannon,
#        file = "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/AsiaShannonIndexMap.tif",
#        bg = 'white', width = 300, height = 180, units = "mm", dpi = 1200, compression = "lzw")


# Tropical Forests -------------------------------------------------------------
tropical_forests <- southAmerica_Shannon +
  Africa_Shannon +
  Asia_Shannon +
  # increase asia and africa's widths
  plot_layout(widths = c(1, 2, 2)) +
  plot_annotation(title = 'Tropical & Subtropical Moist Broadleaf Forests', theme = theme(plot.title = element_text(size = 16, hjust = 0.5))) +
  # ensure one color scale
  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
# # save tropical forests
# ggsave(plot = tropical_forests,
#        file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/NatPoke_figures/TropicalForestsShannonIndex.tif",
#        bg = 'white', width = 700, height = 250, units = "mm", dpi = 1200, compression = "lzw")


## EUROPE ##
europe_shannon <- ggplot() +
  geom_tile(data = Shannon_indexes$`28Mar2025_EuropeRobinson`, aes(x = x, y = y, fill = Shannon_change)) +
  scale_fill_viridis_c(name = "Shannon's Index\nChange", limits = c(-0.5, 0.5)) +
  labs(x = "Latitude", y = "Longitude" , title = "Europe") +
  #scale_fill_gradientn(colors = rev(brewer.pal(11, "RdYlBu")), limits = c(-0.5, 0.5), na.value = "transparent", name = "Shannon Index\nChange") +
  ylim(6130500,6134000)+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

europe_Shannon <- europe_shannon + inset_element(europe_plot, 0.6, 0.7, 1, 1)
# # save europe shannon
# ggsave(plot = europe_Shannon,
#       file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/NatPoke_figures/europeShannonIndex.tif",
#         bg = 'white', width = 400, height = 200, units = "mm", dpi = 1200, compression = "lzw")


## NORTH AMERICA
nortamerica_shannon <- ggplot() +
  geom_tile(data = Shannon_indexes$`28Mar2025_NorthAmericaRobinson`, aes(x = x, y = y, fill = Shannon_change)) +
  scale_fill_viridis_c(name = "Shannon's Index\nChange", limits = c(-0.5, 0.5)) +
  labs(x = "Latitude", y = "Longitude" , title = "North America") +
  #scale_fill_gradientn(colors = rev(brewer.pal(11, "RdYlBu")), limits = c(-0.5, 0.5), na.value = "transparent", name = "Shannon Index\nChange") +
  ylim(6080800, 6084000) +
  #xlim(15030000, 1500000) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

NorthAmerica_Shannon <- nortamerica_shannon + inset_element(northamerica_plot, 0.6, 0.7, 1, 1)
# save europe shannon
ggsave(plot = NorthAmerica_Shannon,
       file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/NatPoke_figures/northAmericaShannonIndex.tif",
       bg = 'white', width = 400, height = 200, units = "mm", dpi = 1200, compression = "lzw")

# Boreal Forests ---------------------------------------------------------------

# all regions combined
boreal_forests <- NorthAmerica_Shannon + europe_Shannon +
  plot_annotation(title = 'Boreal Forests/Taiga', theme = theme(plot.title = element_text(size = 16, hjust = 0.5))) +
  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')

# save boreal forests shannon
ggsave(plot = boreal_forests,
       file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/NatPoke_figures/BorealForestsShannonIndex.tif",
       bg = 'white', width = 700, height = 250, units = "mm", dpi = 1200, compression = "lzw")


##########
# Step 7 # Build actual FUNCTIONAL DIVERSITY INDEX change maps
##########

## SOUTH AMERICA ##
southamerica_funct <- ggplot() +
  geom_tile(data = functionalDiv_index$`28Mar2025_SouthAmericaRobinson`, aes(x = x, y = y, fill = Functional_change)) +
  scale_fill_viridis_c(name = "Functional Diversity\n Index Change", limits = c(-0.5, 0.5)) +
  ylim(-1207500,-1200500) + 
  labs(x = "Latitude", y = "Longitude" , title = "South America") +
  #scale_fill_gradientn(colors = rev(brewer.pal(11, "RdYlBu")), limits = c(-0.5, 0.5), na.value = "transparent", name = "Shannon Index\nChange") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# build final south america plot
southAmerica_FunctinalDiv <- southamerica_funct + inset_element(southamerica_plot, 0.6, 0.7, 1, 1)
# save plot
ggsave(plot = southAmerica_FunctinalDiv,
       file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/NatPoke_figures/SouthAmericaFunctionalDiversityMap.tif",
       bg = 'white', width = 250, height = 300, units = "mm", dpi = 1200, compression = "lzw")


## AFRICA ##
africa_funct <- ggplot() +
  geom_tile(data = functionalDiv_index$`28Mar2025_AfricaRobinson`, aes(x = x, y = y, fill = Functional_change)) +
  xlim(1734000, 1742000) +
  ylim(-1176000, -1171000)+
  scale_fill_viridis_c(name = "Functional Diversity\n Index Change", limits = c(-0.5, 0.5)) +
  labs(x = "Latitude", y = "Longitude" , title = "Africa") +
  #scale_fill_gradientn(colors = rev(brewer.pal(11, "RdYlBu")), limits = c(-0.5, 0.5), na.value = "transparent", name = "Shannon Index\nChange") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# build final africa plot
Africa_FunctinalDiv <- africa_funct + inset_element(africa_plot, 0.7, 0.7, 1, 1)
# save plot
ggsave(plot = Africa_FunctinalDiv,
       file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/NatPoke_figures/AfricaFunctionalDiversityMap.tif",
       bg = 'white', width = 300, height = 200, units = "mm", dpi = 1200, compression = "lzw")


## ASIA ##
asia_funct <- ggplot() +
  geom_tile(data = functionalDiv_index$`28Mar2025_AsiaRobinson`, aes(x = x, y = y, fill = Functional_change)) +
  scale_fill_viridis_c(name = "Functional Diversity\n Index Change", limits = c(-0.5, 0.5)) +
  labs(x = "Latitude", y = "Longitude" , title = "Asia") +
  #scale_fill_gradientn(colors = rev(brewer.pal(11, "RdYlBu")), limits = c(-0.5, 0.5), na.value = "transparent", name = "Shannon Index\nChange") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# build final asia plot
Asia_FunctinalDiv <- asia_funct + inset_element(asia_plot, 0.7, 0.7, 1, 1)
# save plot
ggsave(plot = Asia_FunctinalDiv,
       file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/NatPoke_figures/AsiaFunctionalDiversityMap.tif",
       bg = 'white', width = 300, height = 180, units = "mm", dpi = 1200, compression = "lzw")

# Tropical Forests -------------------------------------------------------------
tropical_forests_functional <- southAmerica_FunctinalDiv +
  Africa_FunctinalDiv +
  Asia_FunctinalDiv +
  # increase asia and africa's widths
  plot_layout(widths = c(1, 2, 2)) +
  plot_annotation(title = 'Functional Diversity Index Change - Tropical & Subtropical Moist Broadleaf Forests', theme = theme(plot.title = element_text(size = 16, hjust = 0.5))) +
  # ensure one color scale
  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
# save tropical forests
ggsave(plot = tropical_forests_functional,
       file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/NatPoke_figures/TropicalForestsFunctionalDiversity.tif",
       bg = 'white', width = 700, height = 250, units = "mm", dpi = 1200, compression = "lzw")

## EUROPE ##
europe_funct <- ggplot() +
  geom_tile(data = functionalDiv_index$`28Mar2025_EuropeRobinson`, aes(x = x, y = y, fill = Functional_change)) +
  scale_fill_viridis_c(name = "Functional Diversity\n Index Change", limits = c(-0.5, 0.5)) +
  labs(x = "Latitude", y = "Longitude" , title = "Europe") +
  #scale_fill_gradientn(colors = rev(brewer.pal(11, "RdYlBu")), limits = c(-0.5, 0.5), na.value = "transparent", name = "Shannon Index\nChange") +
  ylim(6130500,6134000)+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

europe_FunctinalDiv <- europe_funct + inset_element(europe_plot, 0.6, 0.7, 1, 1)
# # save europe shannon
# ggsave(plot = europe_Shannon,
#       file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/NatPoke_figures/europeShannonIndex.tif",
#         bg = 'white', width = 400, height = 200, units = "mm", dpi = 1200, compression = "lzw")


## NORTH AMERICA
nortamerica_funct <- ggplot() +
  geom_tile(data = functionalDiv_index$`28Mar2025_NorthAmericaRobinson`, aes(x = x, y = y, fill = Functional_change)) +
  scale_fill_viridis_c(name = "Functional Diversity\n Index Change", limits = c(-0.5, 0.5)) +
  labs(x = "Latitude", y = "Longitude" , title = "North America") +
  #scale_fill_gradientn(colors = rev(brewer.pal(11, "RdYlBu")), limits = c(-0.5, 0.5), na.value = "transparent", name = "Shannon Index\nChange") +
  ylim(6080800, 6084000) +
  #xlim(15030000, 1500000) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

NorthAmerica_FunctinalDiv <- nortamerica_funct + inset_element(northamerica_plot, 0.6, 0.7, 1, 1)
# save europe shannon
ggsave(plot = NorthAmerica_Shannon,
       file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/NatPoke_figures/northAmericaShannonIndex.tif",
       bg = 'white', width = 400, height = 200, units = "mm", dpi = 1200, compression = "lzw")

# Boreal Forests ---------------------------------------------------------------

# all regions combined
boreal_forests_FunctinalDiv <- NorthAmerica_FunctinalDiv + europe_FunctinalDiv +
  plot_annotation(title = 'Boreal Forests/Taiga', theme = theme(plot.title = element_text(size = 16, hjust = 0.5))) +
  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')

# save boreal forests shannon
ggsave(plot = boreal_forests_FunctinalDiv,
       file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/NatPoke_figures/BorealForestsFunctionalDiversity.tif",
       bg = 'white', width = 700, height = 250, units = "mm", dpi = 1200, compression = "lzw")

