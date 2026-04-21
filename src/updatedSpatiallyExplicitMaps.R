## Name: UpdatedSpatiallyExplicitMaps.R ##
## Authors: Inês Silva ##
## Description: calculate Shannon-Index Change and build maps per scenario and trophic group ##
## Date: March 28th 2025 updated on November 25th 2025


# Set up, load needed packages & functions
library(here)
source(here("src", "libraries.R"))
source(here("src", "customFunctions2.R"))

##########
# STEP 1 # list all directories with outputs to map SSP5
##########

outputFolder_paths <- character(0)
runs <- read.csv("./data/run_table.csv", stringsAsFactors = FALSE)

for (i in seq_len(nrow(runs))) {
  
  target_region <- runs$region[i]
  target_biome <- runs$biome[i]
  future_scenario <- runs$scenario[i]
  
  runname <- paste(
    target_region,
    future_scenario,
    "20260405",
    #format(Sys.time(), "%Y%m%d"),
    sep = "_"
  )
  
  output_folder <- file.path(
    "D:/metaRange_April26", 
    runname, "Outputs")
  
  
  if (!file.exists(output_folder)) {
    warning("Output folder not found (skipping): ", output_folder)
    next
  }
  
  outputFolder_paths[runname] <- output_folder
}

# clean up
rm(output_folder)
invisible(gc())

# get target species
TNIND_yr <- read.csv("D:/metaRange_April26/completeMetaRangeRun_20260405.csv")
target_species <- unique(TNIND_yr$species)
target_species <- gsub(" ", ".", target_species)

##########
# STEP 2 # Transform rasters
##########

# Initialize an empty list to store final dataframes
all_final_data <- list()

# Loop through each directory
for (dir in outputFolder_paths) {
  
  dir_data <- list()
  
  # Loop through each species
  for (target_sps in target_species) {
    
    message(paste0("Working on ", target_sps))
    
    for (timestep in c(26, 136)) {
      
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
# STEP 3 # Calculate Shannon index change **per functional group**
##########

Shannon_indexes <- list()

# call combined trait data to get trophic levels
combined_traits_data <- read_csv(here("data", "externaldata", "mammalTraits_2025-12-11.csv")) %>% 
  mutate(sci_name = gsub("[/& ]", ".",sci_name)) %>% 
  dplyr::filter(BIOME_NAME %in% c("Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga")) %>% 
  mutate(
    CONTINENT = case_when(
      BIOME_NAME == "Boreal Forests/Taiga" & CONTINENT == "Europe" ~ "Europe+Asia",
      TRUE ~ CONTINENT),
    trophic_level = case_when(
      trophic_level == 1 ~ "Herbivore",
      trophic_level == 2 ~ "Omnivore",
      trophic_level == 3 ~ "Carnivore",
      TRUE ~ as.character(trophic_level))
  )

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
      mutate(Shannon_change = t136 - t26)
    
    invisible(gc())
    
    # save the df into a nested list (per scenario+region and trophic level)
    Shannon_indexes[[dir_name]][[troph]] <- troph_df
    invisible(gc())
  }
}


# check results
Shannon_indexes$Asia_ssp126_20260405


##########
# STEP 4 # Build actual SHANNON'S INDEX change maps
##########

world <- ne_countries(scale = "medium", returnclass = "sf")

# get icons for each taxonomic group plot
uuid_carnivores <- get_uuid(name = "Panthera leo", n = 5)[[5]]
uuid_herbivores <- get_uuid(name = "Cervus elaphus", n = 1)
uuid_omnivores <- get_uuid(name = "Sus scrofa", n = 5)[[2]]


## SSP5-8.5 --------------------------------------------------------------------

###################################
# Global Plot - SSP585 Herbivores #
###################################

herb_ssp585 <-  ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80",color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = Shannon_indexes$`North America_ssp585_20260405`$Herbivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Europe+Asia_ssp585_20260405`$Herbivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`South America_ssp585_20260405`$Herbivore, aes(x = x, y = y, fill =  Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Africa_ssp585_20260405`$Herbivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Asia_ssp585_20260405`$Herbivore, aes(x = x, y = y, fill = Shannon_change)) +
  add_phylopic(uuid = uuid_herbivores, x = -15325223, y = 0.25, height = 2000000) +
  annotate("text", x = -12325223, y = 12, label = "Herbivores", size = 3.5) +
  # color scale
  scale_fill_gradientn(
    colors = c("#8F0D14",  # strong negative
               "#D13C16",  # moderate negative
               "#F7DDA0",  # neutral
               "#5CA4B3",  # moderate positive
               "#1E4E79"   # strong positive
    ), name = NULL) +
  #scale_fill_scico(palette = "lapaz") +
  #scale_fill_viridis_c(name = "Shannon's Index\nChange") +
  labs(x = "", y = "",
       #title = "Herbivores"
       ) +
  # Robinson projection
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        # legend inside, left bottom corner (adjust as needed)
        legend.position = c(0.1, 0.25),
        # keep vertical color scale
        legend.direction = "vertical",
        legend.key.height = unit(0.5, 'cm'), 
        legend.key.width = unit(0.5, 'cm'),
        # smaller box
        legend.background = element_rect(fill = NA,
                                         color = NA),
        # draw a rectangle frame around the map
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )

###################################
# Global Plot - SSP585 Carnivores #
###################################

carn_ssp585 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = Shannon_indexes$`North America_ssp585_20260405`$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Europe+Asia_ssp585_20260405`$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`South America_ssp585_20260405`$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Africa_ssp585_20260405`$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Asia_ssp585_20260405`$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  add_phylopic(uuid = uuid_carnivores, x = -15325223, y = 0.25, height = 1200000) +
  annotate("text", x = -12325223, y = -223267, label = "Carnivores", size = 3.5) +
  # color scale
  scale_fill_gradientn(
    colors = c("#8F0D14",  # strong negative
               "#D13C16",  # moderate negative
               "#F7DDA0",  # neutral
               "#5CA4B3",  # moderate positive
               "#1E4E79"   # strong positive
    ), name = NULL) +
  
  #scale_fill_scico(palette = "lapaz") +
  #scale_fill_viridis_c(name = "Shannon's Index\nChange") +
  labs(x = "", y = "",
      # title = "Carnivores"
      ) +
  # Robinson projection
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        # legend inside, left bottom corner (adjust as needed)
        legend.position = c(0.1, 0.25),
        # keep vertical color scale
        legend.direction = "vertical",
        legend.key.height = unit(0.5, 'cm'), 
        legend.key.width = unit(0.5, 'cm'),
        # smaller box
        legend.background = element_rect(fill = NA,
                                         color = NA),
        # draw a rectangle frame around the map
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )

###################################
# Global Plot - SSP585 Omnivores #
###################################

omni_ssp585 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = Shannon_indexes$`North America_ssp585_20260405`$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Europe+Asia_ssp585_20260405`$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`South America_ssp585_20260405`$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Africa_ssp585_20260405`$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Asia_ssp585_20260405`$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  add_phylopic(uuid = uuid_omnivores, x = -15325223, y = 0.25, height = 1500000) +
  annotate("text", x = -12325223, y = 12, label = "Omnivores", size = 3.5) +
  # color scale
  scale_fill_gradientn(
    colors = c("#8F0D14",  # strong negative
               "#D13C16",  # moderate negative
               "#F7DDA0",  # neutral
               "#5CA4B3",  # moderate positive
               "#1E4E79"   # strong positive
    ), name = NULL) +
  #scale_fill_scico(palette = "lapaz") +
  #scale_fill_viridis_c(name = "Shannon's Index\nChange") +
  labs(x = "", y = "",
       #title = "Omnivores"
       ) +
  # Robinson projection
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        # legend inside, left bottom corner (adjust as needed)
        legend.position = c(0.1, 0.25),
        # keep vertical color scale
        legend.direction = "vertical",
        legend.key.height = unit(0.5, 'cm'), 
        legend.key.width = unit(0.5, 'cm'),
        # smaller box
        legend.background = element_rect(fill = NA,
                                         color = NA),
        # draw a rectangle frame around the map
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )

## SSP1-2.6 --------------------------------------------------------------------

###################################
# Global Plot - SSP126 Herbivores #
###################################

herb_ssp126 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = Shannon_indexes$`North America_ssp126_20260405`$Herbivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Europe+Asia_ssp126_20260405`$Herbivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`South America_ssp126_20260405`$Herbivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Africa_ssp126_20260405`$Herbivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Asia_ssp126_20260405`$Herbivore, aes(x = x, y = y, fill = Shannon_change)) +
  add_phylopic(uuid = uuid_herbivores, x = -15325223, y = 0.25, height = 2500000) +
  annotate("text", x = -12325223, y = 12, label = "Herbivores", size = 3.5) +
  # color scale
  scale_fill_gradientn(
    colors = c("#8F0D14",  # strong negative
               "#D13C16",  # moderate negative
               "#F7DDA0",  # neutral
               "#5CA4B3",  # moderate positive
               "#1E4E79"   # strong positive
    ), name = NULL) +
  
  #scale_fill_scico(palette = "lapaz") +
  #scale_fill_viridis_c(name = "Shannon's Index\nChange") +
  labs(x = "", y = "",
      # title = "Herbivores"
      ) +
  # Robinson projection
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        # legend inside, left bottom corner (adjust as needed)
        legend.position = c(0.1, 0.25),
        # keep vertical color scale
        legend.direction = "vertical",
        legend.key.height = unit(0.5, 'cm'), 
        legend.key.width = unit(0.5, 'cm'),
        # smaller box
        legend.background = element_rect(fill = NA,
                                         color = NA),
        # draw a rectangle frame around the map
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )

###################################
# Global Plot - SSP126 Carnivores #
###################################

carn_ssp126 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = Shannon_indexes$`North America_ssp126_20260405`$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Europe+Asia_ssp126_20260405`$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`South America_ssp126_20260405`$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Africa_ssp126_20260405`$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Asia_ssp126_20260405`$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  add_phylopic(uuid = uuid_carnivores, x = -15325223, y = 0.25, height = 1200000) +
  annotate("text", x = -12325223, y = -223267, label = "Carnivores", size = 3.5) +
  # color scale
  scale_fill_gradientn(
    colors = c("#8F0D14",  # strong negative
               "#D13C16",  # moderate negative
               "#F7DDA0",  # neutral
               "#5CA4B3",  # moderate positive
               "#1E4E79"   # strong positive
    ), name = NULL) +
  #scale_fill_scico(palette = "lapaz") +
  #scale_fill_viridis_c(name = "Shannon's Index\nChange") +
  labs(x = "", y = "",
       #title = "Carnivores"
       ) +
  # Robinson projection
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        # legend inside, left bottom corner (adjust as needed)
        legend.position = c(0.1, 0.25),
        # keep vertical color scale
        legend.direction = "vertical",
        legend.key.height = unit(0.5, 'cm'), 
        legend.key.width = unit(0.5, 'cm'),
        # smaller box
        legend.background = element_rect(fill = NA,
                                         color = NA),
        # draw a rectangle frame around the map
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )

###################################
# Global Plot - SSP126 Omnivores #
###################################

omni_ssp126 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = Shannon_indexes$`North America_ssp126_20260405`$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Europe+Asia_ssp126_20260405`$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`South America_ssp126_20260405`$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Africa_ssp126_20260405`$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Asia_ssp126_20260405`$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  add_phylopic(uuid = uuid_omnivores, x = -15325223, y = 0.25, height = 1500000) +
  annotate("text", x = -12325223, y = 12, label = "Omnivores", size = 3.5) +
   # color scale
  scale_fill_gradientn(
    colors = c("#8F0D14",  # strong negative
               "#D13C16",  # moderate negative
               "#F7DDA0",  # neutral
               "#5CA4B3",  # moderate positive
               "#1E4E79"   # strong positive
    ), name = NULL) +
  #scale_fill_scico(palette = "lapaz") +
  #scale_fill_viridis_c(name = "Shannon's Index\nChange") +
  labs(x = "", y = "",
       #title = "Omnivores"
       ) +
  # Robinson projection
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        # legend inside, left bottom corner (adjust as needed)
        legend.position = c(0.1, 0.25),
        # keep vertical color scale
        legend.direction = "vertical",
        legend.key.height = unit(0.5, 'cm'), 
        legend.key.width = unit(0.5, 'cm'),
        # smaller box
        legend.background = element_rect(fill = NA,
                                         color = NA),
        # draw a rectangle frame around the map
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )





final_plot2 <-  (carn_ssp126 + carn_ssp585) /
  (herb_ssp126 + herb_ssp585) /
  (omni_ssp126 + omni_ssp585) &
  theme(plot.margin = ggplot2::margin(-2, -2, -2, -2, "pt"))


row1 <- carn_ssp126 + carn_ssp585
row2 <- herb_ssp126 + herb_ssp585
row3 <- omni_ssp126 + omni_ssp585

final_plot2 <-
  row1 /
  plot_spacer() /
  row2 /
  plot_spacer() /
  row3 +
  plot_layout(heights = c(4, -0.2, 4, -0.2, 4))

final_plot2 <-
  final_plot2 + 
  plot_annotation(tag_levels = 'A') &
  theme(
    plot.tag = element_text(size = 12),
    #plot.tag = element_blank(),
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = ggplot2::margin(0,0.2,0,0,"cm"),
    panel.spacing = grid::unit(0, "cm")
  )
#+
  #plot_layout(heights = c(0.08, 1, 0.08, 1)) #& theme(plot.margin = margin(0,0,0,0))

ggsave("D:/Figures/Figure4_ShannonIndexChangeMaps.png",
       final_plot2,
       width = 320, height = 320, units = "mm", dpi = 900)







#### LIXO #####






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

df <- Shannon_indexes[["13Sep_Africa_ssp585"]][["Herbivore"]]

df <- Shannon_indexes$Africa_ssp585_31Oct25$Herbivore
# get African country polygons
africa <- ne_countries(continent = "Africa", scale = "medium", returnclass = "sf")

ggplot() +
  # raster layer
  geom_tile(data = df, aes(x = x, y = y, fill = Shannon_change)) +
  # country borders
  geom_sf(data = africa, fill = "#505050", color = "#505050", linewidth = 0.3) +
  # raster layer
  geom_tile(data = df, aes(x = x, y = y, fill = Shannon_change)) +
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