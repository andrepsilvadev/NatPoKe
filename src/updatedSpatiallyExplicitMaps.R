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
source(here("src", "customFunctions2.R"))

##########
# Step 1 # Prepare & build maps insets for better visualisation 
##########

regions <- c("Europe", "North America", "South America", "Africa", "Asia")
# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")
unique(world$continent)


# turn off s2 geometry (like before)
sf_use_s2(FALSE)

# define your tasks list (biome, continent pairs)
tasks <- list(
  asia_trop        = c("Tropical & Subtropical Moist Broadleaf Forests", "Asia"),
  southAmerica_trop = c("Tropical & Subtropical Moist Broadleaf Forests", "South America"),
  africa_trop      = c("Tropical & Subtropical Moist Broadleaf Forests", "Africa"),
  northAmerica_bor = c("Boreal Forests/Taiga", "North America"),
  europe_bor       = c("Boreal Forests/Taiga", "Europe")
)

region_sfs <- list()

for (nm in names(tasks)) {
  biome_name     <- tasks[[nm]][1]
  continent_name <- tasks[[nm]][2]
  
  # load biome and continent geometries for this task
  biome_sf     <- load_biome(biome_name = biome_name)
  continent_sf <- load_select_continents(continent_names = continent_name)
  
  # crop biome to continent
  region_sfs[[nm]] <- crop_biome_to_continent(biome_sf, continent_sf)%>%
    summarise(geometry = st_union(geometry))
}

# optional: unpack into the global environment
list2env(region_sfs, .GlobalEnv)
plot(region_sfs$asia_trop)

plot(region_sfs$southAmerica_trop)

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
# Boreal Forests ---------------------------------------------------------------

## Europe SSP5
europe_SSP5 <- "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/13Sep_Europe_ssp585/Outputs"
## Europe SSP1
europe_SSP1 <- "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/13Sep_Europe_ssp126/Outputs"

## North America SSP5
northAmerica_SSP5 <- "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/13Sep_NorthAmerica_ssp585/Outputs" 

## North America SSP1
northAmerica_SSP1 <- "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/13Sep_NorthAmerica_ssp126/Outputs"


# Tropical Moist Forests -------------------------------------------------------

## Asia SSP5
asia_SSP5 <- "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/13Sep_Asia_ssp585/Outputs"
## Asia SSP1
asia_SSP1 <- "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/13Sep_Asia_ssp126/Outputs"
## Africa SSP5
africa_SSP5 <- "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/13Sep_Africa_ssp585/Outputs"
## Africa SSP1
africa_SSP1 <- "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/13Sep_Africa_ssp126/Outputs"


## South America SSP5
southAmerica_SSP5 <- "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/13Sep_SouthAmerica_ssp585/Outputs"
## South America SSP1
southAmerica_SSP1 <- "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/13Sep_SouthAmerica_ssp126/Outputs"
invisible(gc())

# all directories
directories <- c(europe_SSP5, asia_SSP5, africa_SSP5, southAmerica_SSP5, northAmerica_SSP5,
                 europe_SSP1, asia_SSP1, africa_SSP1, southAmerica_SSP1, northAmerica_SSP1)

# get every species that was modeled for the outputs
target_species <- c("Alces alces",
                    "Bison bonasus", "Cervus elaphus", "Sus scrofa", 
                    "Lynx rufus", "Canis lupus", "Rangifer tarandus",
                    "Gorilla gorilla", "Orycteropus afer", "Pan troglodytes", 
                    "Panthera onca", "Crocuta crocuta", "Syncerus caffer",
                    "Panthera leo","Loxodonta africana","Puma concolor")
target_species <- gsub(" ", ".", target_species)

##########
# Step 3 # transform rasters
##########

# Initialize an empty list to store final dataframes
all_final_data <- list()

#dir <- "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/13Sep_Europe_ssp585/Outputs" 

all_final_data <- list()

# Loop through each directory
for (dir in directories) {
  
  dir_data <- list()
  
  # Loop through each species
  for (target_sps in target_species) {
    
    for (timestep in c(101, 235)) {
      
      # list all rasters for this species & timestep (handles replicates)
      sps_files <- list.files(path = dir,
                              pattern = paste0(".*", timestep, "_", target_sps, "_abundance\\.tif"),
                              full.names = TRUE)
      
      if (length(sps_files) > 0) {
        # import all replicates as a SpatRaster
        sps_stack <- rast(sps_files)
        
        # compute mean across replicates
        sps_mean <- app(sps_stack, mean, na.rm = TRUE)
        
        # convert to dataframe with xy coordinates
        sps_df <- as.data.frame(sps_mean, xy = TRUE) %>%
          mutate(timestep = timestep,
                 species = target_sps)
        
        # store in the directory list
        dir_data[[paste(target_sps, timestep, sep = "_")]] <- sps_df
      }
      
    }
    
  }
  
  # Combine all species & timesteps into a single dataframe
  final_df <- bind_rows(dir_data)
  
  # Extract second-to-last folder name as key
  folder_names <- strsplit(dir, "/")[[1]]
  short_dir_name <- folder_names[length(folder_names) - 1]
  
  # Store in the master list
  all_final_data[[short_dir_name]] <- final_df
}


#all_final_data$`13Sep_Europe_ssp585`
#summary(all_final_data$`13Sep_Europe_ssp585`)
#unique(all_final_data$`13Sep_Europe_ssp585`$species)

##########
# Step 4 # Calculate Shannon index change **per functional group**
##########

Shannon_indexes <- list()

# call combined trait data to get trophic levels
combined_traits_data <- read_csv(here("data", "mammalTraits_2025-03-17.csv")) %>% 
  mutate(sci_name = gsub("[/& ]", ".",sci_name))

#dir_name <- "13Sep_Europe_ssp585"

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
      dplyr::filter(trophic_level == troph, mean != 0)
    
    # get the sps names used in that specific trophic group
    species_used <- unique(troph_df$species)
    
    # print a message saying which species are being used
    message("Scenario: ", dir_name, " | Trophic level: ", troph, " | Species: ", paste(species_used, collapse = ", "))
    
    # calculate Shannon Wiener index
    troph_df <- troph_df %>%
      group_by(timestep, x, y) %>%
      dplyr::mutate(
        p_i = mean / sum(mean),
        ln_p_i = ifelse(p_i > 0, log(p_i), 0)
      ) %>%
      dplyr::summarize(
        Shannon_Wiener_Index = -sum(p_i * ln_p_i),
        .groups = "drop"
      ) %>%
      # keep only what you need
      select(x, y, timestep, Shannon_Wiener_Index) %>%
      # reshape wide by timestep
      pivot_wider(names_from = timestep, values_from = Shannon_Wiener_Index, names_prefix = "t") %>%
      # compute change (t235 - t101), automatically NA if one is missing
      mutate(Shannon_change = t235 - t101)
    
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
    
    df <- Shannon_indexes[[region]][[troph]]
    
    # make the plot
    p <- ggplot() +
      geom_tile(data = df, aes(x = x, y = y, fill = Shannon_change)) +
      scale_fill_viridis_c(name = "Shannon's Index\nChange",
                           #limits = c(-0.5, 0.5), na.value = "transparent"
                           ) +
      labs(x = "Longitude", y = "Latitude", 
           title = paste(region, "-", troph)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    # save to list
    all_plots[[paste(region, troph, sep = "_")]] <- p
    
    # save directly to a .tiff file
    #ggsave(filename = paste0("./output/", region, "_", troph, "_ShannonChange.tif"),
     #   plot = p,
      #  bg = 'white', width = 250, height = 300, units = "mm", dpi = 1200, compression = "lzw")
  }
}

df <- Shannon_indexes[["13Sep_Africa_ssp126"]][["Herbivore"]]


# get African country polygons
africa <- ne_countries(continent = "Africa", scale = "medium", returnclass = "sf")

ggplot() +
  # raster layer
  geom_tile(data = df, aes(x = x, y = y, fill = Shannon_change)) +
  # country borders
  geom_sf(data = africa, fill = NA, color = "black", linewidth = 0.3) +
  # color scale
  scale_fill_viridis_c(
    name = "Shannon's Index\nChange"
    #, limits = c(-0.5, 0.5), na.value = "transparent"
  ) +
  labs(x = "Longitude", y = "Latitude",
       title = paste(region, "-", troph)) +
  # Robinson projection (EPSG:54030)
  coord_sf(crs = "+proj=robin") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


all_plots$`13Sep_Africa_ssp585_Herbivore`
all_plots$`13Sep_Europe_ssp585_Herbivore`
all_plots$`13Sep_Asia_ssp585_Omnivore`
all_plots$`13Sep_Asia_ssp126_Omnivore`
all_plots$`13Sep_Asia_ssp585_Carnivore`
all_plots$`13Sep_Africa_ssp126_Herbivore`
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