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
# Step 4 # Prepare maps insets for better visualisation 
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

# Asia
asia_trop <- crop_biome_to_continent(biome = load_select_biome(biome_name = "Tropical & Subtropical Moist Broadleaf Forests"),
                                     continent_geom = load_select_continents(continent_names = "Asia"))
# South America
southAmerica_trop <- crop_biome_to_continent(biome = load_select_biome(biome_name = "Tropical & Subtropical Moist Broadleaf Forests"),
                                             continent_geom = load_select_continents(continent_names = "South America"))
# Africa
africa_trop <- crop_biome_to_continent(biome = load_select_biome(biome_name = "Tropical & Subtropical Moist Broadleaf Forests"),
                                       continent_geom = load_select_continents(continent_names = "Africa"))
# North America
northAmerica_bor <- crop_biome_to_continent(biome = load_select_biome(biome_name = "Boreal Forests/Taiga"),
                                            continent_geom = load_select_continents(continent_names = "North America"))
# Europe
europe_bor <- crop_biome_to_continent(biome = load_select_biome(biome_name = "Boreal Forests/Taiga"),
                                      continent_geom = load_select_continents(continent_names = "Europe"))

##########
# Step 5 # Build actual Shannon's Index change maps
##########

## SOUTH AMERICA ##

# south america inset - continent + tropical forests
southamerica_plot <- ggplot() +
  geom_sf(data = region_sfs[["South America"]], color = "black", fill = "gray95") + 
  geom_sf(data = southAmerica_trop, fill = "gray20") +
  coord_sf(crs = st_crs("ESRI:54030")) +
  theme_void()

southamerica_shannon <- ggplot() +
  geom_tile(data = Shannon_indexes$`28Mar2025_SouthAmericaRobinson`, aes(x = x, y = y, fill = Shannon_change)) +
  scale_fill_viridis_c(name = "Shannon's Index\nChange", limits = c(-0.5, 0.5)) +
  ylim(-1207500,-1200500) + 
  labs(x = "Latitude", y = "Longitude" , title = "South America") +
  #scale_fill_gradientn(colors = rev(brewer.pal(11, "RdYlBu")), limits = c(-0.5, 0.5), na.value = "transparent", name = "Shannon Index\nChange") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# build final south america plot
southAmerica_Shannon <- southamerica_shannon + inset_element(southamerica_plot, 0.6, 0.7, 1, 1)
# # save plot
# ggsave(plot = southAmerica_Shannon,
#        file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/NatPoke_figures/SouthAmericaShannonIndexMap.tif",
#        bg = 'white', width = 250, height = 300, units = "mm", dpi = 1200, compression = "lzw")


## AFRICA ##

# africa inset - continent + tropical forests
africa_plot <- ggplot() +
  geom_sf(data = region_sfs[["Africa"]], color = "black", fill = "gray95") + 
  geom_sf(data = africa_trop, fill = "gray20") +
  coord_sf(crs = st_crs("ESRI:54030")) +
  theme_void()

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

# asia inset - continent + boreal forests
asia_plot <- ggplot() +
  geom_sf(data = region_sfs[["Asia"]], color = "black", fill = "gray95") + 
  geom_sf(data = asia_trop, fill = "gray20") +
  coord_sf(crs = st_crs("ESRI:54030")) +
  theme_void()

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


####################
# Tropical Forests #
####################

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

# europe inset - continent + tropical forests
europe_plot <- ggplot() +
  geom_sf(data = region_sfs[["Europe"]], color = "black", fill = "gray95") + 
  geom_sf(data = europe_bor, fill = "gray20") +
  coord_sf(crs = st_crs("ESRI:54030"), xlim = c(-2984101.5843,13538200), ylim = c(3825520.3916,7850400)) +
  theme_void()

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

# north america inset - continent + tropical forests
northamerica_plot <- ggplot() +
  geom_sf(data = region_sfs[["North America"]], color = "black", fill = "gray95") + 
  geom_sf(data = northAmerica_bor, fill = "gray20") +
  coord_sf(crs = st_crs("ESRI:54030"), xlim = c(-15030000, -1500000), ylim = c(3031000, 8134000)) +
  theme_void()

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

##################
# Boreal Forests #
##################

# all regions combined
boreal_forests <- NorthAmerica_Shannon + europe_Shannon +
  plot_annotation(title = 'Boreal Forests/Taiga', theme = theme(plot.title = element_text(size = 16, hjust = 0.5))) +
  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')

# save boreal forests shannon
ggsave(plot = boreal_forests,
       file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/NatPoke_figures/BorealForestsShannonIndex.tif",
       bg = 'white', width = 700, height = 250, units = "mm", dpi = 1200, compression = "lzw")


