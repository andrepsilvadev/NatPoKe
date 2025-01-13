## NatPoKe ##
#### MIS ####
# 09 JAN 25 #

# Packages
library(readr)
library(dplyr)
library(tidyr) # for pivot_wider()
library(ggplot2)
library(viridis)
#library(ggsci)
library(rnaturalearth) # for world maps
library(rnaturalearthdata) # for world maps
library(sf)


# import data
example_01_res_df <- read_csv(
  "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/example_01_res_df.csv")

# Define scenarios, biomes, species, and taxa
scenarios <- c("BAU", "policy A", "policy B", "policy C", "policy D", "policy E", "policy F")
biomes <- c("Tropical forests", "Boreal forests")
species <- c("SpeciesA", "SpeciesB", "SpeciesC", "SpeciesD", "SpeciesE")
taxa <- data.frame(
  species = species,
  taxa = c("Mammal", "Mammal", "Bird", "Insect", "Insect")
)

# Create dummy_dataset of scenarios and biomes
dummy_dataset <- expand.grid(
  scenario = scenarios,
  biome = biomes
)

# Repeat each combination for time steps (1 to 20) and cell IDs (01 to 10)
dummy_dataset <- dummy_dataset[rep(1:nrow(dummy_dataset), each = 20 * 10), ]
dummy_dataset$time <- rep(rep(1:20, each = 10), times = nrow(dummy_dataset) / (20 * 10))
dummy_dataset$cell_id <- sprintf("%02d", rep(1:10, times = nrow(dummy_dataset) / 10))

# Add regions based on biome
dummy_dataset <- dummy_dataset[rep(1:nrow(dummy_dataset), each = 3), ]
dummy_dataset$region <- ifelse(
  dummy_dataset$biome == "Boreal forests",
  rep(c("North America", "Europe"), length.out = nrow(dummy_dataset)),
  rep(c("South America", "Africa", "South Asia"), length.out = nrow(dummy_dataset))
)

# Repeat for all species
dummy_dataset <- dummy_dataset[rep(1:nrow(dummy_dataset), each = length(species)), ]
dummy_dataset$species <- rep(species, times = nrow(dummy_dataset) / length(species))

# Add taxa based on species
dummy_dataset <- merge(dummy_dataset, taxa, by = "species")

# Add random abundance values
set.seed(123) # For reproducibility
dummy_dataset$n_abundance <- round(runif(nrow(dummy_dataset), min = 5, max = 50))

# Reorder columns for clarity
dummy_dataset <- dummy_dataset[, c("scenario", "biome", "region", "time", "cell_id", "species", "taxa", "n_abundance")]
invisible(gc())

#####################
# CALCULATE METRICS #
#####################

t_burnin <- 2
t_policy <- 5

# calculate post policy mean value for the recovery time metric (to be possible in one go with the other metrics)
post_disturbance_values <- dummy_dataset %>%
  filter(time >= t_burnin) %>% # remove burn-in period
  mutate(period = ifelse(time >= t_burnin &
                           time <= t_policy, "Pre", "Post")) %>%  # code pre and post policy periods
  group_by(biome, species, scenario, period, taxa) %>%
  filter(period == "Post") %>% # filter for the post policy period only
  summarise(mean_post = mean(n_abundance, na.rm = TRUE))
invisible(gc())

# calculate all stability metrics per biome, policy & species
stability_sps <- dummy_dataset %>%
    filter(time >= t_burnin) %>% # remove burn-in period
  mutate(period = ifelse(time >= t_burnin & time <= t_policy, "Pre", "Post")) %>%  # code pre and post policy
  left_join(post_disturbance_values,by = c("biome", "species", "scenario", "period", "taxa")) %>%
  group_by(biome, species, scenario, period, taxa) %>%
  summarise(mean = mean(n_abundance, na.rm = TRUE),
            # find mean nº of individuals
            min = min(n_abundance, na.rm = TRUE),
            # find min. nº of individuals
            max = max(n_abundance, na.rm = TRUE),
            # find max. nº of individuals
            impact_year = time[which.min(n_abundance)],
            # find the year the pop. reaches a min. value in the post policy period
            recovery_year = ifelse(any(time > t_policy & n_abundance >= mean_post),
                                   min(time[time > t_policy & n_abundance >= mean_post], na.rm = TRUE), # find the year where n_abundance is equal or smaller than the post policy mean 
                                   NA), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = c(mean, min, max, impact_year, recovery_year)) %>%
  dplyr::select(!c(impact_year_Pre, recovery_year_Pre)) %>% # remove year of min. nº of individuals in the pre policy period and the year in which the nº ind is equal to the mean values of the post policy period
  mutate(impact = ifelse( mean_Post > mean_Pre,
                          (max_Post - mean_Pre) / mean_Pre,
                          (min_Post - mean_Pre) / mean_Pre),
         time_impact = impact_year_Post - t_policy,
         recovery = ifelse(impact <= 0,
                           (mean_Post - mean_Pre) / mean_Pre,
                           NA),
         time_recovery = recovery_year_Post - t_policy)
invisible(gc())


# average stability metrics across species
stability_avg <- stability_sps %>%
  group_by(biome, scenario, taxa) %>%
  dplyr::summarize(
    impact_avg = mean(impact, na.rm = TRUE),
    #impact_sd = sd(impact, na.rm = TRUE),
    recovery_avg = mean(recovery, na.rm = TRUE),
    #recovery_sd = sd(recovery, na.rm = TRUE),
    time_impact_avg = mean(time_impact, na.rm = TRUE),
    #time_impact_sd = sd(time_impact, na.rm = TRUE),
    time_recovery_avg = mean(time_recovery, na.rm = TRUE)
    #time_recovery_sd = sd(time_recovery, na.rm = TRUE),
    )
invisible(gc())

#stability_avg

# stability metrics in long format for plots
stability_avg_long <- stability_avg %>%
  pivot_longer(
    cols = -c(biome, scenario, taxa),
    names_to = "metric",
    values_to = "value")

############
# FIGURE 1 # Impact and Recovery per taxa for both biomes
############

# new facet label names
metric.labs <- c("Impact (units)", "Recovery (units)", "Time to Impact (years)" , "Time to recovery (years)")
names(metric.labs) <- c("impact_avg",
                        "recovery_avg",
                        "time_impact_avg",
                        "time_recovery_avg")

# Custom color palette
custom_colors <- c("Bird" = "#38b2fe", "Mammal" = "#ffab27", "Insect" = "#99cc00")

# Updated plot
stability_avg_long %>%
  dplyr::filter(metric %in% c("impact_avg", "recovery_avg")) %>%
  ggplot(aes(x = scenario, y = value, fill = taxa)) +
  geom_bar(stat = "identity", position = position_dodge(0.6), width = 0.6) +
  facet_grid(metric ~ biome, scales = "free", labeller = labeller(metric = metric.labs), switch = "y") +
  geom_hline(yintercept = 0) +
  # use custom colors for taxa
  scale_fill_manual(values = custom_colors) +
  ylab("") +
  xlab("\nEconomic policy scenario") +
  theme_minimal() +
  theme(
    # remove gridlines 
    panel.grid = element_blank(),
    # add subtle horizontal lines 
    panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
    # modify facet labels
    strip.text = element_text(face = "bold", size = rel(1)),
    strip.placement = "outside",
    # adjust legend
    legend.position = "right",
    # modify y & x-axis text
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title = element_text(face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
    # remove panel borders
    panel.border = element_blank(),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(2, "lines"))
invisible(gc())

######################
# SUP MATERIAL FIG 1 # Time to impact and Time to recovery per taxa for both biomes
######################

# Updated plot
stability_avg_long %>%
  dplyr::filter(metric %in% c("time_impact_avg", "time_recovery_avg")) %>%
  ggplot(aes(x = scenario, y = value, fill = taxa, shape = biome)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  facet_grid(metric ~ biome, scales = "free_y", labeller = labeller(metric = metric.labs), switch = "y") +
  geom_hline(yintercept = 0) +
  # use custom colors for taxa
  scale_fill_manual(values = custom_colors) +
  ylab("") +
  xlab("\nEconomic policy scenario") +
  theme_minimal() +
  theme(
    # remove gridlines 
    panel.grid = element_blank(),
    # add subtle horizontal lines 
    panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
    # modify facet labels
    strip.text = element_text(face = "bold", size = rel(1)),
    strip.placement = "outside",
    # adjust legend
    legend.position = "right",
    # modify y & x-axis text
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title = element_text(face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
    # remove panel borders
    panel.border = element_blank(),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(2, "lines"))
invisible(gc())


# Community metrics ------------------------------------------------------------

### Tpecies richness per cell in the landscape

community_df <- dummy_dataset %>%
  group_by(biome, scenario, time, cell_id, region, taxa) %>%
  dplyr::summarize(Sps_richness = n_distinct(species))# calculate species richness by counting the nº of species in each group

### Species Diversity (Shannon_Wiener_Index) -----------------------------------

# calculate the Shannon index
Shannon_index <- dummy_dataset %>%
  group_by(biome, scenario, time, cell_id) %>%
  dplyr::mutate(p_i = n_abundance / sum(n_abundance),
                # calculate proportion of individuals of species i
                ln_p_i = ifelse(p_i > 0, log(p_i), 0)) %>%  # in case pi is 0
  # up until here the table has values for each species, then info is summarised
  dplyr::summarize(Shannon_Wiener_Index = -sum(p_i * ln_p_i))  # calculate the Shannon-Wiener index

### Functional Diversity (Funct_diversity_Index) -------------------------------
trophic_levels <- c("herbivore", "carnivore", "omnivore")


Funct_diversity <- dummy_dataset %>%
  mutate(trophic_level = ifelse(dummy_dataset$species %in% c("SpeciesA", "SpeciesB"), "herbivore",
                                ifelse(dummy_dataset$species == "SpeciesC", "omnivore",
                                       ifelse(dummy_dataset$species %in% c("SpeciesD", "SpeciesE"), "carnivore", NA)))) %>%
  group_by(biome, scenario, time, cell_id, trophic_level) %>%
  dplyr::summarize(Total_abundance = sum(n_abundance, na.rm = TRUE)) %>%
  group_by(biome, scenario, time, cell_id, trophic_level) %>%
  dplyr::summarise(Fmean_abundance = mean(Total_abundance, na.rm = TRUE)) %>%
  group_by(biome, scenario, time, cell_id) %>%
  dplyr::mutate(Fp_i = Fmean_abundance / sum(Fmean_abundance),
                # calculate proportion of individuals of fucntional group i
                Fln_p_i = ifelse(Fp_i > 0, log(Fp_i), 0)) %>%  # in case Fpi is 0
  # up until here the table has values for each functional group, then info is summarised
  dplyr::summarize(Funct_diversity_Index = -sum(Fp_i * Fln_p_i))  # calculate the functional diversity index

community_df <- community_df %>%
  group_by(biome, scenario, time, cell_id) %>%
  left_join(select(Shannon_index, Shannon_Wiener_Index, biome, scenario, time, cell_id),
    by = c("biome", "scenario", "time", "cell_id")) %>%
  left_join(select(Funct_diversity,
                   Funct_diversity_Index,
                   biome,
                   scenario,
                   time,
                   cell_id),
            by = c("biome", "scenario", "time", "cell_id"))

# community metrics per year only
community_df_year <- community_df %>%
  group_by(biome, scenario, time, taxa) %>%
  dplyr::summarise(
    mean_Sps_richness_yr = mean(Sps_richness, na.rm = TRUE),
    mean_Shannon_Index_yr = mean(Shannon_Wiener_Index, na.rm = TRUE),
    mean_Funct_Div_yr = mean(Funct_diversity_Index, na.rm = TRUE))

# community metrics per year in long format for plots
community_df_year_long <- community_df_year %>%
  pivot_longer(cols = c("mean_Sps_richness_yr", "mean_Shannon_Index_yr", "mean_Funct_Div_yr"),
               names_to = 'variables',
               values_to = 'values') %>%
  dplyr::filter(time >= t_burnin) # remove burn-in period

############
# FIGURE 2 # Community metrics per policy in both biomes per (one figure per taxa)
############

taxas <- unique(dummy_dataset$taxa)
biome_names <- c("Tropical forests" = "Tropical forests", "Boreal forests" = "Boreal forests")
vars_names <- c("mean_Sps_richness_yr" = "Species \n Richness", "mean_Shannon_Index_yr" = "Shannon Wienner \nIndex", "mean_Funct_Div_yr" = "Functional \nDiversity")

unique(community_df_year_long$variables)

# create an empty list to store the plots
plot_list <- list()

# Loop through each scenario
for (taxa in taxas) {
  
  # Filter data for the current taxa
  taxa_data <- community_df_year_long[community_df_year_long$taxa == taxa,]
  
  comm_composition_time <- ggplot(data = taxa_data,
                                  aes(x = time, y = values, color = scenario)) +
    geom_line() +
    facet_wrap(biome~variables, scales = "free", labeller = labeller(biome = as_labeller(biome_names), variables = as_labeller(vars_names))) +
    xlab("Time") +
    ylab("Metric value") +
    scale_color_discrete("Economic policy \nscenario") +
    theme_minimal() +
    theme(
      # remove gridlines 
      panel.grid = element_blank(),
      # add subtle horizontal lines 
      panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
      # modify facet labels
      strip.text = element_text(face = "bold", size = rel(1)),
      strip.placement = "outside",
      # adjust legend
      legend.position = "bottom",
      # modify x-axis text
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      # remove panel borders
      panel.border = element_blank()) +
    geom_vline(xintercept = t_policy, linetype = "dotted", color = "black", size = 0.8) 
  
    # save each plot in the list
        plot_list[[taxa]] <- comm_composition_time
        
        # save each scenario map as a separate image
        #ggsave(paste0("comm_composition_time", taxa, ".tiff"), comm_composition_time, bg = 'white', width = 230, height = 210, units = "mm", dpi = 1200, compression = "lzw")
}

plot_list$Mammal

############
# FIGURE 3 # Spatially explicit maps per region in each biome (Shannon's index change only)
############

# Calculate Shannon Wiener INdex Change bewtween each time step and time step 3
dummy_map_dataset <- community_df %>%
  group_by(biome, scenario, region, taxa) %>% 
  mutate(
    Shannon_change = Shannon_Wiener_Index  - Shannon_Wiener_Index [time == 1],
    Richness_change = Sps_richness - Sps_richness[time == 1],
    Funct_Div_change = Funct_diversity_Index - Funct_diversity_Index[time == 1]
  )


# THESE PLOTS SHOULD CONSIDER HAVING INTERVAL SCALE INSTEAD OF CONTINUOS SO WE CAN VISUALISE BETTER

#https://www.researchgate.net/publication/324339168_Myxomycete_diversity_in_Costa_Rica/figures?lo=1

# Load world map data
world_map <- ne_countries(scale = "medium", returnclass = "sf")

dummy_map_dataset_t20 <- dummy_map_dataset %>%
  dplyr::filter(time == 20) %>%
  #split cell_id column into two separate coordinates x and y
  separate(cell_id, into = c("x", "y"), sep = 1)


regions <- unique(dummy_dataset$region)
taxas <- unique(dummy_dataset$taxa)

# split the dataset by region
region_splits <- split(dummy_dataset, dummy_dataset$region)

# loop through each region
for (region_name in names(region_splits)) {
  
  # extract the data for the current region
  region_data <- region_splits[[region_name]]
  
  # extract unique taxa for the current region
  region_taxas <- unique(region_data$taxa)
  
  # loop through each taxa within the region
  for (taxa in region_taxas) {
    
    # filter data for the current region and taxa
    region_taxa_data <- region_data[region_data$taxa == taxa, ] 
    
    # create the plot
    spatial_maps <- ggplot() +
      # geom_sf(data = world_map, 
      #          color = "black", 
      #          size = 0.2) +  # World map outline
      geom_raster(data = region_taxa_data, aes(x = x, y = y, fill = Shannon_change)) +
      facet_wrap( ~ scenario) +
      scale_fill_viridis_c(option = "D") +
      #coord_sf() +  # Use coord_sf for compatibility with geom_sf
      theme_minimal() +
      theme(legend.position = "bottom",
            # bold, slightly larger facet titles
            strip.text = element_text(face = "bold", size = rel(1.2))) +
      labs(x = "Longitude",
           y = "Latitude",
           fill = "Shannon-Wiener\nIndex change")
    
    # Save the plot
    ggsave(paste0("spatial_maps_", region_name, "_", taxa, ".tiff"), 
           spatial_maps, 
           bg = 'white', 
           width = 230, 
           height = 210, 
           units = "mm", 
           dpi = 1200, 
           compression = "lzw")
  }
}