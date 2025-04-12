################################
# UPDATED SENSITIVITY ANALYSIS #
################################
# Inês Silva
# 07 Apr 2025

## !!!!!!!! CAREFULL !!!!!!!! need to have acolumn in the data for replicate
## then summarize across replicates

# THIS SCRIPT NEED TO HAVE DATA FROM EACH REGION IMPORTED SEPARETLY!!
# Data comes from the rasters

##########
# Step 1 # Specify which scenario, biome and species the sensitivity analysis is being done
##########

target_scenario <- "BAU"
target_biome <- "Boreal Forest/Taiga"
target_region <- "Europe"
target_species <- c("Alcesalces", "Lynxlynx",
                    "Cervuselaphus", "Rangifertarandus",
                    "Susscrofa", "Damadama",
                    "Canislupus")

# directories with data
baseline_dir <- "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/26Mar2025_Europe/Outputs"
sens095_dir <- "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/10April_Europe_abund0.95/Outputs"
sens105_dir <- "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/10April_Europe_abund1.05/Outputs"

##########
# Step 2 # Select data from folders and convert raster to dfs
##########

# baseline ---------------------------------------------------------------------

# create an empty list
abundance_list <- list()

for (target_sps in target_species) {
  # find raster for timestep 101
  abund101 <- rast(list.files(baseline_dir,
                              pattern = paste0("101_", target_sps, "_abundance\\.tif"), full.names = TRUE))
  
  abundance101_df <- as.data.frame(abund101, xy = TRUE) %>% 
    mutate(species = target_sps,
           scenario = target_scenario,
           biome = target_biome,
           region = target_region,
           simulation = "baseline")
  
  # add each df to a list
  abundance_list[[target_sps]] <- abundance101_df
}

# combine all dfs together
abundance101_all <- bind_rows(abundance_list) %>% 
  rename(TNIND_per_cell = lyr1) # ATENTION HERE for baseline scenario the TNIND col name is different
invisible(gc())

# sensitivity run 095 ----------------------------------------------------------

# create an empty list
abundance_list <- list()

for (target_sps in target_species) {
  # find raster for timestep 101
  abund101 <- rast(list.files(path = sens095_dir,
                              pattern = paste0("101_", target_sps, "_abundance\\.tif"), full.names = TRUE))
  
  abundance101_df <- as.data.frame(abund101, xy = TRUE) %>% 
    mutate(species = target_sps,
           scenario = target_scenario,
           biome = target_biome,
           region = target_region, 
           simulation = "abund095")
  
  # add each df to a list
  abundance_list[[target_sps]] <- abundance101_df
}

# combine all dfs together
abundance101_095 <- bind_rows(abundance_list) %>% 
  rename(TNIND_per_cell = lyr1)
invisible(gc())

# sensitivity run 105 ----------------------------------------------------------

# create an empty list
abundance_list <- list()

for (target_sps in target_species) {
  # find raster for timestep 101
  abund101 <- rast(list.files(path = sens105_dir,
                              pattern = paste0("101_", target_sps, "_abundance\\.tif"), full.names = TRUE))
  
  abundance101_df <- as.data.frame(abund101, xy = TRUE) %>% 
    mutate(species = target_sps,
           scenario = target_scenario,
           biome = target_biome,
           region = target_region, 
           simulation = "abund105")
  
  # add each df to a list
  abundance_list[[target_sps]] <- abundance101_df
}

# combine all dfs together
abundance101_105 <- bind_rows(abundance_list) %>% 
  rename(TNIND_per_cell = lyr1)

# remove unecessary objects
rm(abundance101_df, abund101, abundance_list)
invisible(gc())

##########
# Step 3 # Combine all datasets
##########

all_runs <- abundance101_all %>% 
  bind_rows(abundance101_095) %>% 
  bind_rows(abundance101_105) %>% 
  group_by(scenario, species, biome, region, simulation) %>% 
  summarise(sum_TNIND = sum(TNIND_per_cell), # Total number of individuals in the landscape
            mean_TNIND_per_cell = mean(TNIND_per_cell)) # Mean number of indiivduals per cell
invisible(gc())

##########
# Step 4 # Calculate the proportions
##########

baseline_data <- all_runs %>% 
  filter(simulation %in% "baseline") %>% 
  rename(simulation_baseline = simulation,
         sum_TNIND_baseline = sum_TNIND,
         mean_TNIND_per_cell_baseline = mean_TNIND_per_cell)

sensitivity_data <- all_runs %>% 
  filter(simulation %in% c("abund095", "abund105")) %>% 
  right_join(baseline_data, by = c("scenario", "species", "biome", "region")) %>% 
  mutate(prop_TNIND = sum_TNIND/sum_TNIND_baseline,
         prop_TNIND_per_cell = mean_TNIND_per_cell/mean_TNIND_per_cell_baseline)

#write.csv(TNIND_yr_sensitivity, file = "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/sensitivityData.csv")

##########
# Step 5 # Build sensitivity plot
##########

# format data for boxplot
sensitivity_plotData <- sensitivity_data %>% 
  dplyr::select(!c("sum_TNIND", "mean_TNIND_per_cell", "sum_TNIND_baseline", "mean_TNIND_per_cell", "mean_TNIND_per_cell_baseline","simulation_baseline")) %>% 
  pivot_longer(cols = !c("biome", "region", "species", "scenario", "simulation")) 

# prep labels
plot_labels <- c("prop_TNIND" = "Total Number of Individuals",
                 "prop_TNIND_per_cell" = "Mean Number of Individuals")

# plot data per metric
ggplot(sensitivity_plotData, aes(x = simulation, y = value)) + 
  geom_boxplot(outlier.shape = NA) +
  #geom_hline(yintercept=1.20, linetype="dashed", color = "red") +
  #geom_hline(yintercept=0.80, linetype="dashed", color = "red") +
  facet_wrap(~name, labeller = as_labeller(plot_labels)) +
  geom_jitter(shape = 16, position = position_jitter(0.2), aes(colour = species)) +
  scale_colour_viridis(discrete = TRUE) +
  labs(y = "Simulation", color = "Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "bottom")



################################################################################

# OPTION USING THE .CSV FILE FROM THE MODEL INSTEAD OF THE RASTERS
## COMPUTATIONALLY LESS INTENSE

# # directories with data
# baseline_dir <- "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/26Mar2025_Europe/Outputs"
# sens095_dir <- "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/10April_Europe_abund0.95/Outputs"
# sens105_dir <- "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/10April_Europe_abund1.05/Outputs"
# 
# # baseline
# baseline_pops <- read_csv(file = file.path(baseline_dir, "TNIND_yr_26Mar2025_Europe.csv")) %>% 
#   filter(Timestep %in% "101") %>% 
#   mutate(Region = "Europe",
#          Biome = "BorealForestsTaiga",
#          simulation_baseline = "baseline") %>% # fixing a mistake deleteLater
#   rename(TNIND_baseline = TNIND,
#          timestep = Timestep,
#          biome = Biome,
#          region = Region,
#          species = Species)
# 
# # sensitivity 095
# sens095_pops <- read_csv(file = file.path(sens095_dir, "TNIND_yr_10April_Europe_abund0.95.csv")) %>% 
#   filter(timestep %in% "101") %>% 
#   mutate(simulation = "abund095")
# 
# # sensitivity 105
# sens105_pops <- read_csv(file = file.path(sens105_dir, "TNIND_yr_10April_Europe_abund1.05.csv")) %>% 
#   filter(timestep %in% "101") %>% 
#   mutate(simulation = "abund105")
# 
# 
# all_data <- sens095_pops %>% 
#   bind_rows(sens105_pops) %>% 
#   select(!timestep) %>% 
#   right_join(baseline_pops, by = c("biome", "region", "species")) %>% 
#   select(!timestep) %>%  # yes remove it again!
#   mutate(prop_TNIND = TNIND/TNIND_baseline)
# 
# 
# # format data for boxplot
# sensitivity_plotData <- all_data %>% 
#   dplyr::select(!c("TNIND", "TNIND_baseline", "simulation_baseline")) %>% 
#   pivot_longer(cols = !c("biome", "region", "species", "simulation")) 
# 
# # prep labels
# plot_labels <- c("prop_TNIND" = "Total Number of Individuals")
# 
# # plot data per metric
# ggplot(sensitivity_plotData, aes(x = simulation, y = value)) + 
#   geom_boxplot(outlier.shape = NA) +
#   #geom_hline(yintercept=1.20, linetype="dashed", color = "red") +
#   #geom_hline(yintercept=0.80, linetype="dashed", color = "red") +
#   facet_wrap(~name, labeller = as_labeller(plot_labels)) +
#   geom_jitter(shape = 16, position = position_jitter(0.2), aes(colour = species)) +
#   scale_colour_viridis(discrete = TRUE) +
#   labs(y = "Simulation", color = "Species") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
#         legend.position = "bottom")
# 


 
