## Name: onemetricisnotenough.R ##
## Author: Inês Silva
## Date: 08 Sept 2026
## Goal? Produce a figure to show changes overall. When suitability is low,
## population numbers may still increase. We need to produce a figure to show
## that analysing just one metric is not enough to measure how populations are doing

source("./src/libraries.R")
source("./src/customFunctions2.R")  

##########
# STEP 1 # Get metaRange runs together with suitability per timestep
##########

# import complete runs dataset
completeRun <- read.csv("E:/metaRange_May26/outputs/completeMetaRangeRun_20260517.csv")

inputFolder_paths <- character(0)
runs <- read.csv("./data/run_table.csv", stringsAsFactors = FALSE)

for (i in seq_len(nrow(runs))) {
  
  target_region <- runs$region[i]
  target_biome <- runs$biome[i]
  future_scenario <- runs$scenario[i]
  
  runname <- paste(target_region, future_scenario, "20260517",
                   #format(Sys.time(), "%Y%m%d"),
                   sep = "_")
  
  input_folder <- file.path("E:/metaRange_May26/outputs", runname, "Inputs")
  
  if (!file.exists(input_folder)) {
    warning("Input folder not found (skipping): ", input_folder)
    next
  }
  
  inputFolder_paths[runname] <- input_folder
}

# clean up
rm(input_folder)
invisible(gc())

# start an empty list to store suitability values
sps_suitability_ALL <- list()

for (dir in inputFolder_paths) {
  
  # list tif files in this folder
  files <- list.files(dir, pattern = "_reprojectedm\\.tif$", full.names = TRUE)
  
  if (length(files) == 0) next
  
  message("Processing suitability for: ", dir)
  
  # STEP 1 # get region, biome and scenario from the folder name
  
  folder_name <- basename(dirname(dir))
  # e.g. Asia_ssp126_20260517
  
  # split folder names into pieces
  folder_parts <- str_split(folder_name, "_", simplify = TRUE)
  
  # first piece if region
  target_region <- folder_parts[1]
  # second piece is scenario
  future_scenario <- folder_parts[2]
  
  # get biome from `runs`
  target_biome <- runs %>%
    filter(region == target_region,
           scenario == future_scenario) %>%
    pull(biome) %>%
    first()
  
  # STEP 2 # extract mean suitability per layer for all species

    folder_df <- lapply(files, function(f) {
    
    fname <- basename(f)
    
    # extract species name
    species_name <- str_extract(fname, "^(.*?)_(?=(boreal|tropical))")
    
    species_name <- gsub("_$", "", species_name)
    
    # load raster
    r <- rast(f)
    
    # calculate mean suitability for every timestep
    df <- global(r, "mean", na.rm = TRUE) %>%
      as.data.frame()
    
    # add timestep
    df$timestep <- seq_len(nrow(df))
    
    # add species
    df$species <- suppressMessages(pretty_species_names(species_name))
    
    df
    }) %>%
      # bind evrything together and add timstep number
    bind_rows()
  
  # STEP 3 # Add biome, region and scenario info as columns
  
  folder_df <- folder_df %>%
    mutate(region = target_region,
           biome = target_biome,
           future_scenario = future_scenario) %>%
    rename(suitability = mean) %>%
    dplyr::select(biome, region, future_scenario, species, timestep, suitability)
  
  # store
  sps_suitability_ALL[[folder_name]] <- folder_df
  
  invisible(gc())
}

# bind all rows together
suitability_ALL <- bind_rows(sps_suitability_ALL)


# suitability layers only have 111 timesteps however the metaRange adds 25
# additional steps as burnin so in completeRun we have 136 timesteps so we have
# to duplicate the forts layers an additional 25 times to match it

# repeat the first suitability timestep 25 additional times
suitability_initial <- suitability_ALL %>%
  group_by(biome, region, future_scenario, species) %>%
  filter(timestep == min(timestep)) %>%
  slice(rep(1, 26)) %>%       # original + 25 repetitions
  mutate(timestep = 1:26) %>%
  ungroup()

# shift the remaining suitability timesteps
suitability_future <- suitability_ALL %>%
  group_by(biome, region, future_scenario, species) %>%
  filter(timestep > min(timestep)) %>%
  mutate(timestep = timestep + 25) %>%
  ungroup()

# combine burn in suitability plus other timesteps values
suitability_ALL <- bind_rows(suitability_initial, suitability_future) %>%
  arrange(biome, region, future_scenario, species, timestep)

# combine the complete run with suitability values
completeRun2 <- completeRun %>%
  # average across metaRange replicates
  group_by(future_scenario, biome, region, species, trophic_level, timestep) %>%
  summarise(across(c(TNIND, MNIND, mean_repRate, mean_carrCap, occupancy),
                   ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  # join with suitability values
  left_join(suitability_ALL, by = c("biome", "region", "future_scenario",
                                    "species", "timestep"))

##########
# STEP 2 # Calculate change values for each metric
##########

change_data <- completeRun2 %>%
  # keep only 2015 and 2100
  filter(timestep %in% c(26, 136)) %>%  
  # average across species
  group_by(future_scenario, biome, region, trophic_level, timestep) %>% 
  summarise(across(c(TNIND, MNIND, mean_repRate, mean_carrCap, occupancy, suitability),
                   ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>% 
  dplyr::select(future_scenario, biome, region, trophic_level, 
                timestep, TNIND, occupancy, mean_repRate, mean_carrCap, suitability) %>%
  # go from long to wide format and keep timesteps number associated
  pivot_wider(names_from = timestep, values_from = c(TNIND, occupancy,
                                                     mean_repRate, mean_carrCap,
                                                     suitability), names_glue = "{.value}_{timestep}") %>%
  # calculate the proportion of chnage for each metric
  mutate(
    # Population size change
    TNIND_change = (TNIND_136 - TNIND_26) / TNIND_26 * 100,
    
    # Occupancy change
    occupancy_change = (occupancy_136 - occupancy_26) / occupancy_26 * 100,
    
    # Mean Reproduction Rate chnage
    mean_repRate_change = (mean_repRate_136 - mean_repRate_26) / mean_repRate_26 * 100,
    
    # Mean Carrying capacity change
    mean_carrCap_change = (mean_carrCap_136 - mean_carrCap_26) / mean_carrCap_26 * 100,
    
    # Mean Suitability Change 
    suitability_change = (suitability_136 - suitability_26) / suitability_26 * 100)


change_long <- change_data %>%
  # keep only chnage columns
  dplyr::select(future_scenario, biome, region, trophic_level, ends_with("_change")) %>%
  # transform back to long format for plotting
  pivot_longer(cols = ends_with("_change"), names_to = "metric", values_to = "change") %>%
  # better names
  mutate(metric = recode(metric, TNIND_change = "Population size",
                         occupancy_change = "Occupancy",
                         mean_repRate_change = "Reproductive rate",
                         mean_carrCap_change = "Carrying capacity",
                         suitability_change = "Habitat suitability"))

##########
# STEP 3 # Plot multiple metrics per region and trophic group
##########

# first attempt
change_long %>% 
  dplyr::filter(future_scenario %in% "ssp126") %>% 
  ggplot(aes(x= region, y = change, colour = metric, fill = metric)) +
  geom_col( position = position_dodge(width = 0.8),
            width = 0.7) +
  facet_grid2(trophic_level~biome, scales = "free", independent = "y")


# pretty attempt

## SSP1-2.6 --------------------------------------------------------------------

ssp126_metrics <- change_long %>% 
  filter(future_scenario == "ssp126") %>% 
  mutate(metric = factor(metric,
      levels = c("Habitat suitability", "Carrying capacity", "Reproductive rate", "Occupancy", "Population size"))) %>% 
  ggplot(aes(x = region, y = change, fill = metric)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, colour = "white", 
           linewidth = 0.2) +
  geom_text(aes(label = sprintf("%.1f", change),
      vjust = ifelse(change >= 0, -0.4, 1.4)),
    position = position_dodge(width = 0.8), size = 3,
    colour = "grey20") +
  geom_hline(yintercept = 0, colour = "grey30",
    linewidth = 0.5) +
  facet_grid2(trophic_level ~ biome, scales = "free", independent = "y") +
  scale_fill_manual(values = c(
      "Reproductive rate"     = "#0072B2",
      "Carrying capacity"   = "#78966F",
      "Habitat suitability" = "#D55E00",
      "Occupancy"   = "#E69F00",
      "Population size"       = "#56B4E9"), name = "Metric") +
  labs(x = NULL, y = "Change from 2015 to 2125 (%)") + theme_minimal(base_size = 11) +
  theme(
    # Remove unnecessary gridlines
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    # Keep horizontal reference structure very subtle
    panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
    strip.text.x = element_text(face = "italic", size = 9.5, colour = "grey20"),
    strip.text.y = element_text(face = "bold", size = 10.5, colour = "grey15"),
    strip.background = element_blank(),
    strip.placement = "outside",
    # Axes
    axis.title = element_text(colour = "grey15"),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    axis.text = element_text(colour = "grey25"),
    axis.text.x = element_text(#angle = 45,
      hjust = 1, vjust = 1),
    # Legend
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9),
    legend.key = element_blank(),
    legend.key.width = unit(1.2, "cm"),
    # Facet spacing
    panel.spacing = unit(0.8, "lines"),
    # Plot margins
    plot.margin = margin(8, 8, 8, 8))


## SSP5-8.5 --------------------------------------------------------------------

ssp585_metrics <- change_long %>% 
  filter(future_scenario == "ssp585") %>% 
  mutate(metric = factor(metric,
                         levels = c("Habitat suitability", "Carrying capacity", "Reproductive rate", "Occupancy", "Population size"))) %>% 
  ggplot(aes(x = region, y = change, fill = metric)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, colour = "white", 
           linewidth = 0.2) +
  geom_text(aes(label = sprintf("%.1f", change),
                vjust = ifelse(change >= 0, -0.4, 1.4)),
            position = position_dodge(width = 0.8), size = 3,
            colour = "grey20") +
  geom_hline(yintercept = 0, colour = "grey30",
             linewidth = 0.5) +
  facet_grid2(trophic_level ~ biome, scales = "free", independent = "y") +
  scale_fill_manual(values = c(
    "Reproductive rate"     = "#0072B2",
    "Carrying capacity"   = "#78966F",
    "Habitat suitability" = "#D55E00",
    "Occupancy"   = "#E69F00",
    "Population size"       = "#56B4E9"), name = "Metric") +
  labs(x = NULL, y = "Change from 2015 to 2125 (%)") + theme_minimal(base_size = 11) +
  theme(
    # Remove unnecessary gridlines
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    # Keep horizontal reference structure very subtle
    panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
    strip.text.x = element_text(face = "italic", size = 9.5, colour = "grey20"),
    strip.text.y = element_text(face = "bold", size = 10.5, colour = "grey15"),
    strip.background = element_blank(),
    strip.placement = "outside",
    # Axes
    axis.title = element_text(colour = "grey15"),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    axis.text = element_text(colour = "grey25"),
    axis.text.x = element_text(#angle = 45,
      hjust = 1, vjust = 1),
    # Legend
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9),
    legend.key = element_blank(),
    legend.key.width = unit(1.2, "cm"),
    # Facet spacing
    panel.spacing = unit(0.8, "lines"),
    # Plot margins
    plot.margin = margin(8, 8, 8, 8))


## COMBINE INTO BIG FIGURE -----------------------------------------------------

figure <- ssp126_metrics /ssp585_metrics +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom")

# save figure
ggsave("metrics_SSP126_SSP585_A4_landscape.png", figure,
  width = 15, height = 17, units = "in", dpi = 600, bg = "white")
