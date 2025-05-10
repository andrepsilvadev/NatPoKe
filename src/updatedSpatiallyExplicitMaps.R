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
# Step 1 # Prepare & build maps insets for better visualisation 
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

# load biome shapefile
biome_sf <- st_read(here("data/Ecoregions2017", "Ecoregions2017.shp"))

# adapt Jorinde's functions
get_biome <- function(biome_name) {
  biome_sf[biome_sf$BIOME_NAME == biome_name, ] %>%
    group_by(BIOME_NAME) %>%
    summarise(geometry = st_union(geometry))
}

get_continent <- function(continent_name) {
  ne_countries(scale = "medium", returnclass = "sf") %>%
    filter(continent == continent_name) %>%
    group_by(continent) %>%
    summarise(geometry = st_union(geometry))
}

crop_biome <- function(biome_name, continent_name) {
  st_intersection(get_biome(biome_name), get_continent(continent_name))
}

# define a tasks list
tasks <- list(
  asia_trop = c("Tropical & Subtropical Moist Broadleaf Forests", "Asia"),
  southAmerica_trop = c("Tropical & Subtropical Moist Broadleaf Forests", "South America"),
  africa_trop = c("Tropical & Subtropical Moist Broadleaf Forests", "Africa"),
  northAmerica_bor = c("Boreal Forests/Taiga", "North America"),
  europe_bor = c("Boreal Forests/Taiga", "Europe")
)

# run those tasks all at once (much cleaner than before)
results <- lapply(tasks, function(x) crop_biome(biome_name = x[1], continent_name = x[2]))
list2env(results, .GlobalEnv)  # Optional: assign each result to a named object
results$asia_trop



# south america inset - continent + tropical forests ---------------------------
southamerica_plot <- ggplot() +
  geom_sf(data = region_sfs[["South America"]], color = "black", fill = "gray95") + 
  geom_sf(data = southAmerica_trop, fill = "gray20") +
  coord_sf(crs = st_crs("ESRI:54030")) +
  theme_void() +
  annotate("text", x = -8000000, y = -2200000, label = "Target Area", size = 4, color = "black") +
  annotate("segment", x = -8000000, y = -2000000, xend = -7000000, yend = -1000000,
           arrow = arrow(length = unit(0.3, "cm")), color = "black", size = 1)

# africa inset - continent + tropical forests ----------------------------------
africa_plot <- ggplot() +
  geom_sf(data = region_sfs[["Africa"]], color = "black", fill = "gray95") + 
  geom_sf(data = africa_trop, fill = "gray20") +
  coord_sf(crs = st_crs("ESRI:54030")) +
  theme_void() +
  annotate("text", x = 400000, y = -1000000, label = "Target Area", size = 4, color = "black") +
  annotate("segment", x = 400000, y = -900000, xend = 900000, yend = -500000,
           arrow = arrow(length = unit(0.3, "cm")), color = "black", size = 1)

# asia inset - continent + boreal forests --------------------------------------
asia_plot <- ggplot() +
  geom_sf(data = region_sfs[["Asia"]], color = "black", fill = "gray95") + 
  geom_sf(data = asia_trop, fill = "gray20") +
  coord_sf(crs = st_crs("ESRI:54030")) +
  theme_void() +
  annotate("text", x = 6000000, y = 300000, label = "Target Area", size = 4, color = "black") +
  annotate("segment", x = 6100000, y = 400000, xend = 8000000, yend = 1900000,
           arrow = arrow(length = unit(0.3, "cm")), color = "black", size = 1)

# europe inset - continent + tropical forests ----------------------------------
europe_plot <- ggplot() +
  geom_sf(data = region_sfs[["Europe"]], color = "black", fill = "gray95") + 
  geom_sf(data = europe_bor, fill = "gray20") +
  coord_sf(crs = st_crs("ESRI:54030"), xlim = c(-2984101.5843,13538200), ylim = c(3825520.3916,7850400)) +
  theme_void() +
  annotate("text", x = 8000000, y = 4500000, label = "Target Area", size = 4, color = "black") +
  annotate("segment", x = 7500000, y = 5000000, xend = 6000000, yend = 6500000,
           arrow = arrow(length = unit(0.3, "cm")), color = "black", size = 1)

# north america inset - continent + tropical forests ---------------------------
northamerica_plot <- ggplot() +
  geom_sf(data = region_sfs[["North America"]], color = "black", fill = "gray95") + 
  geom_sf(data = northAmerica_bor, fill = "gray20") +
  coord_sf(crs = st_crs("ESRI:54030"), xlim = c(-15030000, -1500000), ylim = c(3031000, 8134000)) +
  theme_void()


##########
# Step 2 # list all directories with outputs to map SSP5
##########

# IF I HAVE MORE RUNS (e.g. DIFF SSP RUNS) JUST MAKE SURE THAT IS IN THE RUN NAME

europe_SSP5 <- "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/28Mar2025_EuropeRobinson/Outputs"
northAmerica_SSP5 <- "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/28Mar2025_NorthAmericaRobinson/Outputs"
southAmerica_SSP5 <- "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/28Mar2025_SouthAmericaRobinson/Outputs"
africa_SSP5 <- "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/28Mar2025_AfricaRobinson/Outputs"
asia_SSP5 <- "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/28Mar2025_AsiaRobinson/Outputs"

# all directories
directories <- c(europe_SSP5, asia_SSP5, africa_SSP5, southAmerica_SSP5, northAmerica_SSP5)

# get every species that was modeled for the outputs
target_species <- c("Alcesalces", "Lynxlynx", "Canislupus", "Susscrofa", "Rangifertarandus", "Odocoileusvirginianus", "Cervuselaphus", "Damadama", "Lynxrufus", "Crocutacrocuta", "Pantheraleo", "Pantheratigris", "Pumaconcolor", "Callithrixjacchus", "Nasuanasua")

##########
# Step 3 # transform rasters
##########

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

unique(all_final_data$`28Mar2025_EuropeRobinson`$species)

##########
# Step 4 # Calculate Shannon index change **per functional group**
##########

Shannon_indexes <- list()

# call combined trait data to get trophic levels
combined_traits_data <- read_csv(here("data", "mammalTraits_2025-03-17.csv")) %>% 
  mutate(sci_name = gsub("[/& ]", "",sci_name))

# going trhough each scenario+region
for (dir_name in names(all_final_data)) { 
  df <- all_final_data[[dir_name]]
  
  # join with combined triats for trophic levels
  df <- df %>%
    left_join(combined_traits_data, by = c("species" = "sci_name"), relationship = "many-to-many") %>% # to silence warning, careful if it does not apply in the future!!
    dplyr::filter(!is.na(trophic_level))  # keep only species with known trophic level
  invisible(gc())
  
  # start a sublist for the scenario+region
  Shannon_indexes[[dir_name]] <- list()
  
  # go through each trophic level in each scenario+region
  for (troph in unique(df$trophic_level)) {
    troph_df <- df %>%
      dplyr::filter(trophic_level == troph, lyr1 != 0)
    
    # get the sps names used in that specific trophic group
    species_used <- unique(troph_df$species)
    
    # print a message saying which species are being used
    message("Scenario: ", dir_name, " | Trophic level: ", troph, " | Species: ", paste(species_used, collapse = ", "))
    
    # calculate Shannon Wiener index
    troph_df <- troph_df %>%
      group_by(timestep, x, y) %>%
      dplyr::mutate(
        p_i = lyr1 / sum(lyr1),
        ln_p_i = ifelse(p_i > 0, log(p_i), 0)
      ) %>%
      dplyr::summarize(
        Shannon_Wiener_Index = -sum(p_i * ln_p_i),
        .groups = "drop"
      ) %>%
      # calculate the change in realtion to the first equilibrium timestep
      group_by(x, y) %>%
      mutate(
        Shannon_change = Shannon_Wiener_Index - Shannon_Wiener_Index[timestep == 101]
      ) %>%
      dplyr::filter(timestep == 125)
    
    invisible(gc())
    
    # save the df into a nested list (per scenario+region and trophic level)
    Shannon_indexes[[dir_name]][[troph]] <- troph_df
    invisible(gc())
  }
}

# check results
#Shannon_indexes$`28Mar2025_EuropeRobinson`$Herbivore

##########
# Step 5 # Build actual SHANNON'S INDEX change maps
##########

# start empty list for plots
all_plots <- list()

# go through each scenario+region
for (region in names(Shannon_indexes)) {
  
  # go through the trophic levels (functional groups) in the scenario+region
  for (troph in names(Shannon_indexes[[region]])) {
    
    df <- Shannon_indexes_[[region]][[troph]]
    
    # make the plot
    p <- ggplot() +
      geom_tile(data = df, aes(x = x, y = y, fill = Shannon_change)) +
      scale_fill_viridis_c(name = "Shannon's Index\nChange", limits = c(-0.5, 0.5), na.value = "transparent") +
      labs(x = "Longitude", y = "Latitude", 
           title = paste(region, "-", troph)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    # save to list
    all_plots[[paste(region, troph, sep = "_")]] <- p
    
    # save directly to a .tiff file
    # ggsave(filename = paste0("plots/", region, "_", troph, "_ShannonChange.tif"),
            # plot = p,
            # bg = 'white', width = 250, height = 300, units = "mm", dpi = 1200, compression = "lzw")
  }
}

# check results
#all_plots$`28Mar2025_EuropeRobinson_Herbivore`

##############
# EXTRA STEP # Organise plots in a figure
##############


# The idea, as of 10 May 2025, is to have one figure per trophic group showing
# both scenarios (SSP1 and SSP5), beacuse in total using only mammals we will
# have 30 different maps (3 functional groups, 2 scenarios and 5 continents)


# aa <- ((southamerica_shannon_SSP5 + southamerica_shannon_SSP5 + southamerica_plot) /(africa_shannon_SSP5 + africa_shannon_SSP5 + africa_plot)/ (asia_shannon_SSP5 + asia_shannon_SSP5 + asia_plot)) +
#   plot_layout(guides = 'collect', widths = 1) +
#   plot_annotation(tag_levels = list(c("A", "B", "", "C", "D", "", "E", "F", ""))) & theme(legend.position = 'bottom', plot.tag = element_text(size = 12))
# 
# ggsave(plot = aa,
#        file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/figures_20250427/figure3_test.png",
#        bg = 'white', width = 400, height = 500, units = "mm", dpi = 1200, #compression = "lzw"
#        )