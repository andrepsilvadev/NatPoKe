################################
# UPDATED SENSITIVITY ANALYSIS #
################################
# Inês Silva
# 16 Apr 2025

##########
# Step 1 # Specify which scenario, biome and species the sensitivity analysis is being done
##########

target_scenario <- "BAU"
target_biome <- "Boreal Forest Taiga"
target_region <- "Europe"
target_species <- c("Alcesalces", "Lynxlynx",
                    "Cervuselaphus", "Rangifertarandus",
                    "Susscrofa", "Damadama",
                    "Canislupus")

# directories with data
baseline_dir <- "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/26Mar2025_Europe/Outputs"

##########
# Step 2 # Select baseline data and convert raster to dfs
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
baselineData <- bind_rows(abundance_list) %>% 
  rename(TNIND_per_cell = lyr1) %>% 
  group_by(scenario, species, biome, region, simulation) %>%
  # calculate metrics
  summarise(sum_TNIND = sum(TNIND_per_cell), # Total number of individuals in the landscape
            mean_TNIND_per_cell = mean(TNIND_per_cell)) %>% # Mean number of individuals per cell
  rename(simulation_baseline = simulation,
         sum_TNIND_baseline = sum_TNIND,
         mean_TNIND_per_cell_baseline = mean_TNIND_per_cell)

invisible(gc())

##########
# Step 3 # Select sensitivity runs data and convert raster to dfs 
##########

# directories path for sensitivity runs
sens095_dir <- "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/10April_Europe_abund0.95/Outputs"
sens105_dir <- "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/10April_Europe_abund1.05/Outputs"


sensitivityRuns <- function(dir_path, species, scenario, biome, region) {
  # This function creates a dataframe for each directory provided with TNIND per
  # cell for target species
  abundance_list <- list()
  
  for (target_sps in species) {
    # find Yeat 101 rasters for each target species
    abund101 <- rast(list.files(path = dir_path,
                                pattern = paste0("101_", target_sps, "_abundance\\.tif"), 
                                full.names = TRUE))
    # convert to dataframe
    abundance101_df <- as.data.frame(abund101, xy = TRUE) %>% 
      # add extra columns
      mutate(species = target_sps,
             scenario = scenario,
             biome = biome,
             region = region,
             simulation = basename(dirname(dir_path)))
    
    abundance_list[[target_sps]] <- abundance101_df
  }
  
  bind_rows(abundance_list) %>%
    rename(TNIND_per_cell = lyr1)
}

# specify the sensitivity runs directories
simulation_dirs <- c(sens095_dir, sens105_dir)

# use an anonymous function to have directories as a list of multiple and the other arguments just as one
all_results <- lapply(simulation_dirs, function(dir) {
  sensitivityRuns(dir, species = target_species, scenario = target_scenario, biome = target_biome, region = target_region)
})

# combine all results
sensitivityData <- bind_rows(all_results) 

# modify sensitivity data to be per species intead of per cell
sensitivityData <- sensitivityData %>% 
  group_by(scenario, species, biome, region, simulation) %>% 
  summarise(sum_TNIND = sum(TNIND_per_cell), # Total number of individuals in the landscape
            mean_TNIND_per_cell = mean(TNIND_per_cell)) # Mean number of indiivduals per cell

##########
# Step 4 # Calculate the proportions
##########

all_runs <- baselineData %>% 
  right_join(sensitivityData, by = c("scenario", "species", "biome", "region")) %>% 
  mutate(prop_TNIND = sum_TNIND/sum_TNIND_baseline,
         prop_TNIND_per_cell = mean_TNIND_per_cell/mean_TNIND_per_cell_baseline)

#write.csv(all_runs,
#          file = "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/sensitivityData.csv")

##########
# Step 5 # Build sensitivity plot
##########

# As of April 16th we decided to use only mean number of individuals to do the
# sensitivity analysis. If we go to pg 207 of André's PhD thesis we can see that
# both TNIND and MEAN NUMBER OF INDIV PER CELL have a similar trend, and we think 
# that MEAN NUMBER OF INDIV PER CELL might be more "sensitive" to changes in parameters
# TNIND seems to coarse...

# format data for boxplot
sensitivity_plotData <- all_runs %>% 
  dplyr::select(!c("prop_TNIND", # if we change our minds about the above comment remove this from here
                   "sum_TNIND", "mean_TNIND_per_cell", "sum_TNIND_baseline", "mean_TNIND_per_cell", "mean_TNIND_per_cell_baseline", "simulation_baseline")) %>% 
  pivot_longer(cols = !c("biome", "region", "species", "scenario", "simulation")) 

# plots facet's pretty labels
plot_labels <- c("prop_TNIND" = "Total Number of Individuals",
                 "prop_TNIND_per_cell" = "Mean Number of Individuals")
# x axis (run names) pretty labels
simulation_labels <- c("10April_Europe_abund0.95" = "abund095",
                       "10April_Europe_abund1.05" = "abund095")

# plot data per metric
sensitivity_plot <- ggplot(sensitivity_plotData, aes(x = simulation, y = value)) + 
  geom_boxplot(outlier.shape = NA) +
  #geom_hline(yintercept=1.20, linetype="dashed", color = "red") +
  #geom_hline(yintercept=0.80, linetype="dashed", color = "red") +
  facet_wrap(~name, labeller = as_labeller(plot_labels)) +
  scale_x_discrete(labels = simulation_labels)+
  geom_jitter(shape = 16, position = position_jitter(0.2), aes(colour = species)) +
  scale_colour_viridis(discrete = TRUE) +
  labs(y = "Simulation", color = "Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "bottom")

# saving the plot
#ggsave(plot = sensitivity_plot,
#       file = "./september_plots/without_2_sps/Figure3_Abundance_mismatch_Sept_without2sps.tiff", 
#       bg = 'white', width = 250, height = 230, units = "mm", dpi = 1200, compression="lzw")

################################################################################

# From this point forward the code does the same as above but from the .csv file
# produced by the metaRange model. It computationally less intense but I have a 
# feeling it is not as precise as the rasters... not sure
# It only supports the TNIND and still does not have mean number of individuals
# per cell but that is completely possible and easy to add (in the mammalModel script)

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


 
