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
    "20260517",
    #format(Sys.time(), "%Y%m%d"),
    sep = "_"
  )
  
  output_folder <- file.path(
    "E:/metaRange_May26", 
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
TNIND_yr <- read.csv("E:/metaRange_May26/completeMetaRangeRun_20260517.csv")
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
    
    # convert df to raster
    r <- rast(troph_df[, c("x", "y", "Shannon_change")],
              type = "xyz")
    
    # save as raster file
    writeRaster(r, filename = file.path("E:/metaRange_May26/Shannon_output",
                                     paste0(dir_name, "_", troph, "_change.tif")),
                overwrite = TRUE)
  }
}


library(data.table)
library(dplyr)

# create output folder if needed
dir.create("E:/metaRange_May26/Shannon_output", showWarnings = FALSE)

for (dir_name in names(Shannon_indexes)) {
  
  # combine all trophic groups for that scenario/region
  combined_df <- bind_rows(
    lapply(names(Shannon_indexes[[dir_name]]), function(troph) {
      
      Shannon_indexes[[dir_name]][[troph]] %>%
        mutate(trophic_level = troph)
      
    })
  )
  
  # save one csv per directory
  fwrite(
    combined_df,
    file = file.path("E:/metaRange_May26/Shannon_output",
                     paste0(dir_name, "_Shannon.csv"))
  )
  
  rm(combined_df)
  invisible(gc())
}


# check results
#Shannon_indexes$`South America_ssp126_20260517`$Herbivore


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
  geom_tile(data = Shannon_indexes$`North America_ssp585_20260517`$Herbivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Europe+Asia_ssp585_20260517`$Herbivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`South America_ssp585_20260517`$Herbivore, aes(x = x, y = y, fill =  Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Africa_ssp585_20260517`$Herbivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Asia_ssp585_20260517`$Herbivore, aes(x = x, y = y, fill = Shannon_change)) +
  add_phylopic(uuid = uuid_herbivores, x = -15325223, y = 0.25, height = 2000000) +
  annotate("text", x = -12325215, y = 11, label = "Herbivores", size = 3.5) +
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
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5),
    
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    # legend inside plot (bottom left corner)
    legend.position = c(0.1, 0.25),
    legend.direction = "vertical",
    legend.key.height = unit(0.7, 'cm'),
    legend.key.width = unit(0.7, 'cm'),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    # rectangular frame around plot
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )

ggsave( "E:/metaRange_May26/WBF_FIGURES/herb_ssp585.png",
  herb_ssp585,
  bg = "transparent",
  width = 10,
  height = 5,
  dpi = 1200)

###################################
# Global Plot - SSP585 Carnivores #
###################################

carn_ssp585 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = Shannon_indexes$`North America_ssp585_20260517`$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Europe+Asia_ssp585_20260517`$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`South America_ssp585_20260517`$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Africa_ssp585_20260517`$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Asia_ssp585_20260517`$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
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
  labs(x = "", y = "" ) +
  # Robinson projection
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5),
    
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    # legend inside plot (bottom left corner)
    legend.position = c(0.1, 0.25),
    legend.direction = "vertical",
    legend.key.height = unit(0.7, 'cm'),
    legend.key.width = unit(0.7, 'cm'),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    # rectangular frame around plot
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )

ggsave( "E:/metaRange_May26/WBF_FIGURES/carn_ssp585.png",
        carn_ssp585,
        bg = "transparent",
        width = 10,
        height = 5,
        dpi = 1200)

###################################
# Global Plot - SSP585 Omnivores #
###################################

omni_ssp585 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = Shannon_indexes$`North America_ssp585_20260517`$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Europe+Asia_ssp585_20260517`$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`South America_ssp585_20260517`$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Africa_ssp585_20260517`$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Asia_ssp585_20260517`$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  add_phylopic(uuid = uuid_omnivores, x = -15325223, y = 0.25, height = 1500000) +
  annotate("text", x = -12325223, y = 12, label = "Omnivores", size = 3.5) +
  # color scale
  scale_fill_gradientn(
    colors = c("#8F0D14",  # strong negative
               "#D13C16",  # moderate negative
               "#F7DDA0",  # neutral
               "#5CA4B3",  # moderate positive
               "#1E4E79"   # strong positive
    ), name = NULL,
    limits = c(-0.55, 0.55),
    breaks = c(-0.5, -0.25, 0, 0.25, 0.5),
    labels = c("-0.5", "-0.25", "0", "0.25", "0.5")) +
  #scale_fill_scico(palette = "lapaz") +
  #scale_fill_viridis_c(name = "Shannon's Index\nChange") +
  labs(x = "", y = "",
       #title = "Omnivores"
       ) +
  # Robinson projection
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  theme_minimal()  +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5),
    
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    # legend inside plot (bottom left corner)
    legend.position = c(0.1, 0.25),
    legend.direction = "vertical",
    legend.key.height = unit(0.7, 'cm'),
    legend.key.width = unit(0.7, 'cm'),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    # rectangular frame around plot
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )

ggsave( "E:/metaRange_May26/WBF_FIGURES/omni_ssp585.png",
        omni_ssp585,
        bg = "transparent",
        width = 10,
        height = 5,
        dpi = 1200)

## SSP1-2.6 --------------------------------------------------------------------

###################################
# Global Plot - SSP126 Herbivores #
###################################

herb_ssp126 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = Shannon_indexes$`North America_ssp126_20260517`$Herbivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Europe+Asia_ssp126_20260517`$Herbivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`South America_ssp126_20260517`$Herbivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Africa_ssp126_20260517`$Herbivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Asia_ssp126_20260517`$Herbivore, aes(x = x, y = y, fill = Shannon_change)) +
  add_phylopic(uuid = uuid_herbivores, x = -15325223, y = 0.25, height = 2500000) +
  annotate("text", x = -12325223, y = 12, label = "Herbivores", size = 3.5) +
  # color scale
  scale_fill_gradientn(
    colors = c("#8F0D14",  # strong negative
               "#D13C16",  # moderate negative
               "#F7DDA0",  # neutral
               "#5CA4B3",  # moderate positive
               "#1E4E79"   # strong positive
    ), name = NULL,
    limits = c(-0.65, 0.65),
    breaks = c(-0.6, -0.3, 0, 0.3, 0.6),
    labels = c("-0.6", "-0.3", "0", "0.3", "0.6")) +
  #scale_fill_scico(palette = "lapaz") +
  #scale_fill_viridis_c(name = "Shannon's Index\nChange") +
  labs(x = "", y = "",
      # title = "Herbivores"
      ) +
  # Robinson projection
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  theme_minimal()  +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5),
    
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    # legend inside plot (bottom left corner)
    legend.position = c(0.1, 0.25),
    legend.direction = "vertical",
    legend.key.height = unit(0.7, 'cm'),
    legend.key.width = unit(0.7, 'cm'),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    # rectangular frame around plot
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )

ggsave( "E:/metaRange_May26/WBF_FIGURES/herb_ssp126.png",
        herb_ssp126,
        bg = "transparent",
        width = 10,
        height = 5,
        dpi = 1200)

###################################
# Global Plot - SSP126 Carnivores #
###################################

carn_ssp126 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = Shannon_indexes$`North America_ssp126_20260517`$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Europe+Asia_ssp126_20260517`$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`South America_ssp126_20260517`$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Africa_ssp126_20260517`$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Asia_ssp126_20260517`$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
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
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5),
    
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    # legend inside plot (bottom left corner)
    legend.position = c(0.1, 0.25),
    legend.direction = "vertical",
    legend.key.height = unit(0.7, 'cm'),
    legend.key.width = unit(0.7, 'cm'),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    # rectangular frame around plot
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )

ggsave("E:/metaRange_May26/WBF_FIGURES/carn_ssp126.png",
        carn_ssp126,
        bg = "transparent",
        width = 10,
        height = 5,
        dpi = 1200)

###################################
# Global Plot - SSP126 Omnivores #
###################################

omni_ssp126 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = Shannon_indexes$`North America_ssp126_20260517`$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Europe+Asia_ssp126_20260517`$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`South America_ssp126_20260517`$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Africa_ssp126_20260517`$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$`Asia_ssp126_20260517`$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  add_phylopic(uuid = uuid_omnivores, x = -15325223, y = 0.25, height = 1500000) +
  annotate("text", x = -12325223, y = 12, label = "Omnivores", size = 3.5) +
  # color scale
  scale_fill_gradientn(
    colors = c("#8F0D14",  # strong negative
               "#D13C16",  # moderate negative
               "#F7DDA0",  # neutral
               "#5CA4B3",  # moderate positive
               "#1E4E79"   # strong positive
    ), name = NULL,
    limits = c(-0.55, 0.55),
    breaks = c(-0.5, -0.25, 0, 0.25, 0.5),
    labels = c("-0.5", "-0.25", "0", "0.25", "0.5")) +
  #scale_fill_scico(palette = "lapaz") +
  #scale_fill_viridis_c(name = "Shannon's Index\nChange") +
  labs(x = "", y = "",
       #title = "Omnivores"
  ) +
  # Robinson projection
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  theme_minimal()  +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5),
    
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    # legend inside plot (bottom left corner)
    legend.position = c(0.1, 0.25),
    legend.direction = "vertical",
    legend.key.height = unit(0.7, 'cm'),
    legend.key.width = unit(0.7, 'cm'),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    # rectangular frame around plot
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )

ggsave( "E:/metaRange_May26/WBF_FIGURES/omni_ssp126.png",
        omni_ssp126,
        bg = "transparent",
        width = 10,
        height = 5,
        dpi = 1200)


## building the final plot

# final_plot2 <-  (carn_ssp126 + carn_ssp585) /
#   (herb_ssp126 + herb_ssp585) /
#   (omni_ssp126 + omni_ssp585) &
#   theme(plot.margin = ggplot2::margin(-2, -2, -2, -2, "pt"))


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

ggsave("E:/metaRange_May26/FigureAndMetrics/Figure3_ShannonIndexChangeMaps.png",
       final_plot2,
       bg = "transparent", width = 320, height = 320, units = "mm", dpi = 1200)


################################################################################
######################## ALL DIVERISTY INDEXES TOGETHER ########################
################################################################################

Diversity_indexes <- list()

# going trhough each scenario+region
for (dir_name in names(all_final_data)) { 
  df <- all_final_data[[dir_name]]
  
  # join with combined traits for trophic levels
  # call combined trait data to get trophic levels
  combined_traits_data <- read_csv(here("data", "externaldata", "mammalTraits_2025-12-11.csv")) %>% 
    mutate(sci_name = gsub("[/& ]", ".",sci_name)) %>% 
    dplyr::filter(BIOME_NAME %in% c("Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga")) %>% 
    mutate(
      CONTINENT = case_when(
        BIOME_NAME == "Boreal Forests/Taiga" &
          CONTINENT %in% c("Europe", "Asia") ~ "Europe+Asia",
        TRUE ~ CONTINENT),
      trophic_level = case_when(
        trophic_level == 1 ~ "Herbivore",
        trophic_level == 2 ~ "Omnivore",
        trophic_level == 3 ~ "Carnivore",
        TRUE ~ as.character(trophic_level))) %>% 
    #fix sps with missing trophic_level
    mutate(trophic_level = case_when(sci_name == "Ursus.thibetanus" ~ "Omnivore",
                                     sci_name == "Panthera.tigris" ~ "Carnivore",
                                     TRUE ~ as.character(trophic_level))) %>% 
    dplyr::filter(CONTINENT %in% str_split(dir_name, "_")[[1]][1]) %>%
    dplyr::select(sci_name, trophic_level) %>% 
    unique()
  
  # join with combined triats for trophic levels
  df <- df %>%
    left_join(combined_traits_data, by = c("species" = "sci_name"))  # keep only species with known trophic level
  invisible(gc())
  
  # start a sublist for the scenario+region
  Diversity_indexes[[dir_name]] <- list()
  
  # go through each trophic level in each scenario+region
  for (troph in unique(df$trophic_level)) {
    troph_df <- df %>%
      dplyr::filter(trophic_level == troph, mean != 0)
    
    # get the sps names used in that specific trophic group
    species_used <- unique(troph_df$species)
    
    # print a message saying which species are being used
    message("Scenario: ", dir_name, " | Trophic level: ", troph, " | Species: ", paste(species_used, collapse = ", "))
    
    # calculate biodiversity metrics per pixel
    troph_df <- troph_df %>%
      group_by(timestep, x, y) %>%
      dplyr::mutate(
        p_i = mean / sum(mean),
        ln_p_i = ifelse(p_i > 0, log(p_i), 0)
      ) %>%
      dplyr::summarize(
        # Shannon diversity
        Shannon_Wiener_Index = -sum(p_i * ln_p_i),
        
        # Species richness (species with abundance > 0)
        Richness = sum(mean > 0, na.rm = TRUE),
        
        .groups = "drop"
      ) %>%
      mutate(
        # Pielou's evenness
        Evenness = ifelse(Richness > 1,
                          Shannon_Wiener_Index / log(Richness),
                          NA_real_)
      ) %>%
      
      # keep needed columns
      select(x, y, timestep,
             Shannon_Wiener_Index,
             Richness,
             Evenness) %>%
      
      # reshape wide by timestep
      pivot_wider(
        names_from = timestep,
        values_from = c(Shannon_Wiener_Index, Richness, Evenness),
        names_prefix = "t"
      ) %>%
      
      # compute change between timesteps
      mutate(
        Shannon_change = Shannon_Wiener_Index_t136 - Shannon_Wiener_Index_t26,
        Richness_change = Richness_t136 - Richness_t26,
        Evenness_change = Evenness_t136 - Evenness_t26
      )
    
    invisible(gc())
    
    # save the df into a nested list (per scenario+region and trophic level)
    Diversity_indexes[[dir_name]][[troph]] <- troph_df
    invisible(gc())
  }
}

plot_diversity <- function(data_list, troph, metric, label, icon_uuid, text_label,
                           x_icon = -15325223, y_text = 12, icon_height = 2000000) {
  
  ggplot() +
    geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
    
    # loop-like behaviour via purrr::map_dfr equivalent
    lapply(data_list, function(df) {
      geom_tile(
        data = df[[troph]],
        aes(x = x, y = y, fill = .data[[metric]])
      )
    }) +
    
    add_phylopic(uuid = icon_uuid, x = x_icon, y = 0.25, height = icon_height) +
    annotate("text", x = x_icon + 3000000, y = y_text, label = text_label, size = 3.5) +
    
    scale_fill_gradientn(
      colors = c("#8F0D14", "#D13C16", "#F7DDA0", "#5CA4B3", "#1E4E79"),
      name = NULL
    ) +
    
    coord_sf(crs = "+proj=robin", expand = FALSE) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = c(0.1, 0.25),
      legend.direction = "vertical",
      legend.key.height = unit(0.5, "cm"),
      legend.key.width = unit(0.5, "cm"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
    )
}


plots <- list()
scenarios <- c("ssp585", "ssp126")
trophs <- c("Herbivore", "Carnivore", "Omnivore")
metrics <- c("Shannon_change", "Richness_change", "Evenness_change")

icons <- list(
  Herbivore = uuid_herbivores,
  Carnivore = uuid_carnivores,
  Omnivore  = uuid_omnivores
)

for (sc in scenarios) {
  for (t in trophs) {
    for (m in metrics) {
      
      key <- paste(sc, t, m, sep = "_")
      
      plots[[key]] <- plot_diversity(
        data_list = Diversity_indexes,
        troph = t,
        metric = m,
        label = sc,
        icon_uuid = icons[[t]],
        text_label = t
      )
    }
  }
}

evenness <- (plots$ssp126_Herbivore_Evenness_change + plots$ssp585_Herbivore_Evenness_change)/
  (plots$ssp585_Carnivore_Evenness_change + plots$ssp126_Carnivore_Evenness_change)/
  (plots$ssp585_Omnivore_Evenness_change + plots$ssp126_Omnivore_Evenness_change)

ggsave("D:/metaRange_April26/FigureAndMetrics/Figure4_EvennessChangeMaps.png",
       evenness,
       width = 320, height = 320, units = "mm", dpi = 900)


########################################
## MSA INDEX (MEAN SPECIES ABUNDANCE) ##
########################################

MSA <- list()

# going trhough each scenario+region
for (dir_name in names(all_final_data)) { 
  df <- all_final_data[[dir_name]]
  
  # join with combined traits for trophic levels
  # call combined trait data to get trophic levels
  combined_traits_data <- read_csv(here("data", "externaldata", "mammalTraits_2025-12-11.csv")) %>% 
    mutate(sci_name = gsub("[/& ]", ".",sci_name)) %>% 
    dplyr::filter(BIOME_NAME %in% c("Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga")) %>% 
    mutate(
      CONTINENT = case_when(
        BIOME_NAME == "Boreal Forests/Taiga" &
          CONTINENT %in% c("Europe", "Asia") ~ "Europe+Asia",
        TRUE ~ CONTINENT),
      trophic_level = case_when(
        trophic_level == 1 ~ "Herbivore",
        trophic_level == 2 ~ "Omnivore",
        trophic_level == 3 ~ "Carnivore",
        TRUE ~ as.character(trophic_level))) %>% 
    #fix sps with missing trophic_level
    mutate(trophic_level = case_when(sci_name == "Ursus.thibetanus" ~ "Omnivore",
                                     sci_name == "Panthera.tigris" ~ "Carnivore",
                                     TRUE ~ as.character(trophic_level))) %>% 
    dplyr::filter(CONTINENT %in% str_split(dir_name, "_")[[1]][1]) %>%
    dplyr::select(sci_name, trophic_level) %>% 
    unique()
  
  df <- df %>%
    left_join(combined_traits_data, by = c("species" = "sci_name"))  # keep only species with known trophic level
  invisible(gc())
  
  # start a sublist for the scenario+region
  MSA[[dir_name]] <- list()
  
  # go through each trophic level in each scenario+region
  for (troph in unique(df$trophic_level)) {
    troph_df <- df %>%
      dplyr::filter(trophic_level == troph)
    
    # calculate MSA values
    MSA_df <- troph_df %>% 
      pivot_wider(names_from = timestep, values_from = mean, names_prefix = "t_") %>% 
      mutate(
        # calculate the proportion between disturbed/reference for each species
        prop_abundance = case_when(
          # keep only rows where the sps abundance didn't increase 
          t_26 > 0 ~ pmin(t_136 / t_26, 1),
          # remove true NA's
          TRUE ~ NA_real_)) %>%
      group_by(x, y) %>%
      summarise(
        # calculate the number of sps per cell (richness)
        richness_ref = sum(t_26 > 0, na.rm = TRUE),
        # sum all the sps proportions (top portion of the formula)
        sum_prop_abundance = sum(prop_abundance, na.rm = TRUE),
        # calculate the complete index
        MSA_index = sum_prop_abundance / richness_ref,
        .groups = "drop"
      )
    
    invisible(gc())
    
    # save the df into a nested list (per scenario+region and trophic level)
    MSA[[dir_name]][[troph]] <- MSA_df
    invisible(gc())
  }
}

##########################
## Build MSA INDEX maps ##
##########################

world <- ne_countries(scale = "medium", returnclass = "sf")

# get icons for each taxonomic group plot
uuid_carnivores <- get_uuid(name = "Panthera leo", n = 5)[[5]]
uuid_herbivores <- get_uuid(name = "Cervus elaphus", n = 1)
uuid_omnivores <- get_uuid(name = "Sus scrofa", n = 5)[[2]]


## SSP5-8.5 --------------------------------------------------------------------

#######################################
# MSA Global Plot - SSP585 Herbivores #
#######################################

MSA_herb_ssp585 <-  ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80",color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = MSA$`North America_ssp585_20260517`$Herbivore, aes(x = x, y = y, fill = MSA_index)) +
  geom_tile(data = MSA$`Europe+Asia_ssp585_20260517`$Herbivore, aes(x = x, y = y, fill = MSA_index)) +
  geom_tile(data = MSA$`South America_ssp585_20260517`$Herbivore, aes(x = x, y = y, fill =  MSA_index)) +
  geom_tile(data = MSA$`Africa_ssp585_20260517`$Herbivore, aes(x = x, y = y, fill = MSA_index)) +
  geom_tile(data = MSA$`Asia_ssp585_20260517`$Herbivore, aes(x = x, y = y, fill = MSA_index)) +
  add_phylopic(uuid = uuid_herbivores, x = -15325223, y = 0.25, height = 2000000) +
  annotate("text", x = -12325223, y = 12, label = "Herbivores", size = 3.5) +
  scale_fill_viridis_c(limits = c(0,1), na.value = "transparent", name = "MSA") +
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

#######################################
# MSA Global Plot - SSP585 Carnivores #
#######################################

MSA_carn_ssp585 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = MSA$`North America_ssp585_20260517`$Carnivore, aes(x = x, y = y, fill = MSA_index)) +
  geom_tile(data = MSA$`Europe+Asia_ssp585_20260517`$Carnivore, aes(x = x, y = y, fill = MSA_index)) +
  geom_tile(data = MSA$`South America_ssp585_20260517`$Carnivore, aes(x = x, y = y, fill = MSA_index)) +
  geom_tile(data = MSA$`Africa_ssp585_20260517`$Carnivore, aes(x = x, y = y, fill = MSA_index)) +
  geom_tile(data = MSA$`Asia_ssp585_20260517`$Carnivore, aes(x = x, y = y, fill = MSA_index)) +
  add_phylopic(uuid = uuid_carnivores, x = -15325223, y = 0.25, height = 1200000) +
  annotate("text", x = -12325223, y = -223267, label = "Carnivores", size = 3.5) +
  # color scale
  scale_fill_viridis_c(limits = c(0,1), na.value = "transparent", name = "MSA") +
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

######################################
# MSA Global Plot - SSP585 Omnivores #
######################################

MSA_omni_ssp585 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = MSA$`North America_ssp585_20260517`$Omnivore, aes(x = x, y = y, fill = MSA_index)) +
  geom_tile(data = MSA$`Europe+Asia_ssp585_20260517`$Omnivore, aes(x = x, y = y, fill = MSA_index)) +
  geom_tile(data = MSA$`South America_ssp585_20260517`$Omnivore, aes(x = x, y = y, fill = MSA_index)) +
  geom_tile(data = MSA$`Africa_ssp585_20260517`$Omnivore, aes(x = x, y = y, fill = MSA_index)) +
  geom_tile(data = MSA$`Asia_ssp585_20260517`$Omnivore, aes(x = x, y = y, fill = MSA_index)) +
  add_phylopic(uuid = uuid_omnivores, x = -15325223, y = 0.25, height = 1500000) +
  annotate("text", x = -12325223, y = 12, label = "Omnivores", size = 3.5) +
  # color scale
  scale_fill_viridis_c(limits = c(0,1), na.value = "transparent", name = "MSA") +
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

#######################################
# MSA Global Plot - SSP126 Herbivores #
#######################################

MSA_herb_ssp126 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = MSA$`North America_ssp126_20260517`$Herbivore, aes(x = x, y = y, fill = MSA_index)) +
  geom_tile(data = MSA$`Europe+Asia_ssp126_20260517`$Herbivore, aes(x = x, y = y, fill = MSA_index)) +
  geom_tile(data = MSA$`South America_ssp126_20260517`$Herbivore, aes(x = x, y = y, fill = MSA_index)) +
  geom_tile(data = MSA$`Africa_ssp126_20260517`$Herbivore, aes(x = x, y = y, fill = MSA_index)) +
  geom_tile(data = MSA$`Asia_ssp126_20260517`$Herbivore, aes(x = x, y = y, fill = MSA_index)) +
  add_phylopic(uuid = uuid_herbivores, x = -15325223, y = 0.25, height = 2500000) +
  annotate("text", x = -12325223, y = 12, label = "Herbivores", size = 3.5) +
  # color scale
  scale_fill_viridis_c(limits = c(0,1), na.value = "transparent", name = "MSA") +
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

#######################################
# MSA Global Plot - SSP126 Carnivores #
#######################################

MSA_carn_ssp126 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = MSA$`North America_ssp126_20260517`$Carnivore, aes(x = x, y = y, fill = MSA_index)) +
  geom_tile(data = MSA$`Europe+Asia_ssp126_20260517`$Carnivore, aes(x = x, y = y, fill = MSA_index)) +
  geom_tile(data = MSA$`South America_ssp126_20260517`$Carnivore, aes(x = x, y = y, fill = MSA_index)) +
  geom_tile(data = MSA$`Africa_ssp126_20260517`$Carnivore, aes(x = x, y = y, fill = MSA_index)) +
  geom_tile(data = MSA$`Asia_ssp126_20260517`$Carnivore, aes(x = x, y = y, fill = MSA_index)) +
  add_phylopic(uuid = uuid_carnivores, x = -15325223, y = 0.25, height = 1200000) +
  annotate("text", x = -12325223, y = -223267, label = "Carnivores", size = 3.5) +
  # color scale
  scale_fill_viridis_c(limits = c(0,1), na.value = "transparent", name = "MSA") +
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

######################################
# MSA Global Plot - SSP126 Omnivores #
######################################

MSA_omni_ssp126 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = MSA$`North America_ssp126_20260517`$Omnivore, aes(x = x, y = y, fill = MSA_index)) +
  geom_tile(data = MSA$`Europe+Asia_ssp126_20260517`$Omnivore, aes(x = x, y = y, fill = MSA_index)) +
  geom_tile(data = MSA$`South America_ssp126_20260517`$Omnivore, aes(x = x, y = y, fill = MSA_index)) +
  geom_tile(data = MSA$`Africa_ssp126_20260517`$Omnivore, aes(x = x, y = y, fill = MSA_index)) +
  geom_tile(data = MSA$`Asia_ssp126_20260517`$Omnivore, aes(x = x, y = y, fill = MSA_index)) +
  add_phylopic(uuid = uuid_omnivores, x = -15325223, y = 0.25, height = 1500000) +
  annotate("text", x = -12325223, y = 12, label = "Omnivores", size = 3.5) +
  # color scale
  scale_fill_viridis_c(limits = c(0,1), na.value = "transparent", name = "MSA") +
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

row1 <- MSA_carn_ssp126 + MSA_carn_ssp585
row2 <- MSA_herb_ssp126 + MSA_herb_ssp585
row3 <- MSA_omni_ssp126 + MSA_omni_ssp585

final_plot3 <-
  row1 /
  plot_spacer() /
  row2 /
  plot_spacer() /
  row3 +
  plot_layout(heights = c(4, -0.2, 4, -0.2, 4))

final_plot3 <-
  final_plot3 + 
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

ggsave("D:/Figures/Figure4_MSA_fromGLOBIO_Maps.png",
       final_plot3,
       width = 320, height = 320, units = "mm", dpi = 900)


####################################################
### MEAN PROPORTION OF CHANGE SPATIALLY EXPLICIT ###
####################################################

abundance_change <- list()

# going trhough each scenario+region
for (dir_name in names(all_final_data)) { 
  df <- all_final_data[[dir_name]]
  
  # join with combined traits for trophic levels
  # call combined trait data to get trophic levels
  combined_traits_data <- read_csv(here("data", "externaldata", "mammalTraits_2025-12-11.csv")) %>% 
    mutate(sci_name = gsub("[/& ]", ".",sci_name)) %>% 
    dplyr::filter(BIOME_NAME %in% c("Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga")) %>% 
    mutate(
      CONTINENT = case_when(
        BIOME_NAME == "Boreal Forests/Taiga" &
          CONTINENT %in% c("Europe", "Asia") ~ "Europe+Asia",
        TRUE ~ CONTINENT),
      trophic_level = case_when(
        trophic_level == 1 ~ "Herbivore",
        trophic_level == 2 ~ "Omnivore",
        trophic_level == 3 ~ "Carnivore",
        TRUE ~ as.character(trophic_level))) %>% 
    #fix sps with missing trophic_level
    mutate(trophic_level = case_when(sci_name == "Ursus.thibetanus" ~ "Omnivore",
                                     sci_name == "Panthera.tigris" ~ "Carnivore",
                                     TRUE ~ as.character(trophic_level))) %>% 
    dplyr::filter(CONTINENT %in% str_split(dir_name, "_")[[1]][1]) %>%
    dplyr::select(sci_name, trophic_level) %>% 
    unique()
  
  # join data with combined traits to get trophic levels
  df <- df %>%
    left_join(combined_traits_data, by = c("species" = "sci_name")#, relationship = "many-to-many"
              )
  
  invisible(gc())
  
  # start a sublist for the scenario+region
  abundance_change[[dir_name]] <- list()
  
  for (troph in unique(df$trophic_level)) {
    
    # filter for that trophic level
    df <- all_final_data[[dir_name]]
    
    df <- df %>% 
    pivot_wider(names_from = timestep, values_from = mean, names_prefix = "t_") %>% 
    # remove cells where the species didn't exist before and also cells it colonised (otherwise maths does make sense, we can't divide by zero)
    filter(!is.na(t_26), t_26 > 1) %>% 
    # calculate proportion of abundance chnage per species
    mutate(abundance_change = (t_136 - t_26) / t_26) %>% 
    # average prop change across trophic level
    group_by(x,y) %>% 
    summarise(abundance_change_acrossSps = mean(abundance_change, na.rm = TRUE))
  
    # save the df into a nested list (per scenario+region and trophic level)
    abundance_change[[dir_name]][[troph]] <- df
    invisible(gc())
 
  }
}
  

######################################
## Build PROP ABUNDANCE CHNAGE maps ##
######################################

world <- ne_countries(scale = "medium", returnclass = "sf")

# get icons for each taxonomic group plot
uuid_carnivores <- get_uuid(name = "Panthera leo", n = 5)[[5]]
uuid_herbivores <- get_uuid(name = "Cervus elaphus", n = 1)
uuid_omnivores <- get_uuid(name = "Sus scrofa", n = 5)[[2]]


## SSP5-8.5 --------------------------------------------------------------------

#####################################################
# prop abund change Global Plot - SSP585 Herbivores #
####################################################

PROPAbund_herb_ssp585 <-  ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80",color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = abundance_change$`North America_ssp585_20260405`$Herbivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  geom_tile(data = abundance_change$`Europe+Asia_ssp585_20260405`$Herbivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  geom_tile(data = abundance_change$`South America_ssp585_20260405`$Herbivore, aes(x = x, y = y, fill =  abundance_change_acrossSps)) +
  geom_tile(data = abundance_change$`Africa_ssp585_20260405`$Herbivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  geom_tile(data = abundance_change$`Asia_ssp585_20260405`$Herbivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  add_phylopic(uuid = uuid_herbivores, x = -15325223, y = 0.25, height = 2000000) +
  annotate("text", x = -12325223, y = 12, label = "Herbivores", size = 3.5) +
  scale_fill_viridis_c(#limits = c(-1,1),
    na.value = "transparent", name = "Mean Prop.\nAbundance Change") +
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
        legend.position = c(0.15, 0.25),
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

#######################################
# prop abund change Global Plot - SSP585 Carnivores #
#######################################

PROPAbund_carn_ssp585 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = abundance_change$`North America_ssp585_20260405`$Carnivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  geom_tile(data = abundance_change$`Europe+Asia_ssp585_20260405`$Carnivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  geom_tile(data = abundance_change$`South America_ssp585_20260405`$Carnivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  geom_tile(data = abundance_change$`Africa_ssp585_20260405`$Carnivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  geom_tile(data = abundance_change$`Asia_ssp585_20260405`$Carnivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  add_phylopic(uuid = uuid_carnivores, x = -15325223, y = 0.25, height = 1200000) +
  annotate("text", x = -12325223, y = -223267, label = "Carnivores", size = 3.5) +
  # color scale
  scale_fill_viridis_c(#limits = c(-1,1),
    na.value = "transparent", "Mean Prop.\nAbundance Change") +
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
        legend.position = c(0.15, 0.25),
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

######################################
# prop abund change Global Plot - SSP585 Omnivores #
######################################

PROPAbund_omni_ssp585 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = abundance_change$`North America_ssp585_20260405`$Omnivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  geom_tile(data = abundance_change$`Europe+Asia_ssp585_20260405`$Omnivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  geom_tile(data = abundance_change$`South America_ssp585_20260405`$Omnivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  geom_tile(data = abundance_change$`Africa_ssp585_20260405`$Omnivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  geom_tile(data = abundance_change$`Asia_ssp585_20260405`$Omnivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  add_phylopic(uuid = uuid_omnivores, x = -15325223, y = 0.25, height = 1500000) +
  annotate("text", x = -12325223, y = 12, label = "Omnivores", size = 3.5) +
  # color scale
  scale_fill_viridis_c(#limits = c(-1,1),
    na.value = "transparent", name = "Mean Prop.\nAbundance Change") +
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
        legend.position = c(0.15, 0.25),
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

#######################################
# prop abund change Global Plot - SSP126 Herbivores #
#######################################

PROPAbund_herb_ssp126 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = abundance_change$`North America_ssp126_20260405`$Herbivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  geom_tile(data = abundance_change$`Europe+Asia_ssp126_20260405`$Herbivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  geom_tile(data = abundance_change$`South America_ssp126_20260405`$Herbivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  geom_tile(data = abundance_change$`Africa_ssp126_20260405`$Herbivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  geom_tile(data = abundance_change$`Asia_ssp126_20260405`$Herbivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  add_phylopic(uuid = uuid_herbivores, x = -15325223, y = 0.25, height = 2500000) +
  annotate("text", x = -12325223, y = 12, label = "Herbivores", size = 3.5) +
  # color scale
  scale_fill_viridis_c(limits = c(-1,1), na.value = "transparent", name = "Mean Prop.\nAbundance Change") +
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
        legend.position = c(0.15, 0.25),
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

#######################################
# prop abund change Global Plot - SSP126 Carnivores #
#######################################

PROPAbund_carn_ssp126 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = abundance_change$`North America_ssp126_20260405`$Carnivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  geom_tile(data = abundance_change$`Europe+Asia_ssp126_20260405`$Carnivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  geom_tile(data = abundance_change$`South America_ssp126_20260405`$Carnivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  geom_tile(data = abundance_change$`Africa_ssp126_20260405`$Carnivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  geom_tile(data = abundance_change$`Asia_ssp126_20260405`$Carnivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  add_phylopic(uuid = uuid_carnivores, x = -15325223, y = 0.25, height = 1200000) +
  annotate("text", x = -12325223, y = -223267, label = "Carnivores", size = 3.5) +
  # color scale
  scale_fill_viridis_c(limits = c(-1,1), na.value = "transparent", name = "Mean Prop.\nAbundance Change") +
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
        legend.position = c(0.15, 0.25),
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

######################################
# prop abund change Global Plot - SSP126 Omnivores #
######################################

PROPAbund_omni_ssp126 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = abundance_change$`North America_ssp126_20260405`$Omnivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  geom_tile(data = abundance_change$`Europe+Asia_ssp126_20260405`$Omnivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  geom_tile(data = abundance_change$`South America_ssp126_20260405`$Omnivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  geom_tile(data = abundance_change$`Africa_ssp126_20260405`$Omnivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  geom_tile(data = abundance_change$`Asia_ssp126_20260405`$Omnivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  add_phylopic(uuid = uuid_omnivores, x = -15325223, y = 0.25, height = 1500000) +
  annotate("text", x = -12325223, y = 12, label = "Omnivores", size = 3.5) +
  # color scale
  scale_fill_viridis_c(limits = c(-1,1), na.value = "transparent", name = "Mean Prop.\nAbundance Change" ) +
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
        legend.position = c(0.15, 0.25),
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

row1 <- PROPAbund_carn_ssp126 + PROPAbund_carn_ssp585
row2 <- PROPAbund_herb_ssp126 + PROPAbund_herb_ssp585
row3 <- PROPAbund_omni_ssp126 + PROPAbund_omni_ssp585

final_plot4 <-
  row1 /
  plot_spacer() /
  row2 /
  plot_spacer() /
  row3 +
  plot_layout(heights = c(4, -0.2, 4, -0.2, 4))

final_plot4 <-
  final_plot4 + 
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

ggsave("D:/Figures/Figure4_MeanPropAbundChange_Maps.png",
       final_plot4,
       width = 320, height = 320, units = "mm", dpi = 900)


ggplot() +
  # borders on top
  #geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = abundance_change$`South America_ssp126_20260405`$Herbivore, aes(x = x, y = y, fill = abundance_change_acrossSps)) +
  # Robinson projection
  coord_sf(crs = "+proj=robin", expand = FALSE)
