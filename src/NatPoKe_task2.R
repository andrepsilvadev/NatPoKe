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

# this function creates a dummy dataset from stefan's metaRange example output
generate_dummy_dataset <- function(existing_df, 
                                   taxa = c("Taxa1", "Taxa2", "Taxa3"), 
                                   biomes = c("Tropical forests", "Boreal forests"), 
                                   regions = paste0("Region", 1:5), 
                                   scenarios = c("BAU", "policy A", "policy B", "policy C", "policy D", "policy E", "policy F"), 
                                   cell_ids = sprintf("%02d", 1:10)) {
  
  # ensure species names and abundance values exist
  if (!all(c("species", "n_abundance") %in% colnames(existing_df))) {
    stop("Input dataframe must have 'species' and 'n_abundance' columns.")
  }
  
  # assign each species to a random taxa but do not repeat the same sps for different taxa
  species_to_taxa <- data.frame(
    species = unique(existing_df$species),
    taxa = sample(taxa, length(unique(existing_df$species)), replace = TRUE)
  )
  
  # assign species to biomes (species can occur in one or both biomes)
  species_to_biomes <- data.frame(
    species = rep(unique(existing_df$species), each = length(biomes)),
    biome = rep(biomes, times = length(unique(existing_df$species))),
    occurs_in_biome = sample(c(TRUE, FALSE), length(unique(existing_df$species)) * length(biomes), replace = TRUE)
  )
  species_to_biomes <- subset(species_to_biomes, occurs_in_biome)
  
  # generate dummy data for regions, scenarios, and cell IDs
  dummy_data <- expand.grid(
    region = regions,
    scenario = scenarios,
    cell_id = cell_ids,
    stringsAsFactors = FALSE
  )
  
  # duplicate cell IDs for each region
  dummy_data <- dummy_data[rep(1:nrow(dummy_data), times = length(regions)), ]
  dummy_data$region <- rep(regions, each = nrow(dummy_data) / length(regions))
  invisible(gc())
  
  # combine everything into a full dataset
  full_dataset <- merge(dummy_data, species_to_taxa, by = NULL)
  full_dataset <- merge(full_dataset, species_to_biomes, by = "species")
  invisible(gc())
  
  # merge with existing dataframe to keep all original variables
  full_dataset <- merge(full_dataset, existing_df, by = "species")
  invisible(gc())
  
  # assign abundance values for each species in each cell_id
  full_dataset$n_abundance <- round(runif(nrow(full_dataset), min = 0.5, max = 1.5) * full_dataset$n_abundance)
  
  # reorder columns for clarity
  full_dataset <- full_dataset[, c("scenario", "biome", "region", "cell_id", "species", "taxa", colnames(existing_df)[-which(colnames(existing_df) == "species")])]
  
  return(full_dataset)
}


# use function
dummy_dataset <- generate_dummy_dataset(example_01_res_df)
#head(dummy_dataset)
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
            recovery_year = ifelse(any(time > t_policy & n_abundance >= mean_post), min(time[time > t_policy & n_abundance >= mean_post], na.rm = TRUE), # find the year where n_abundance is equal or smaller than the post policy mean 
                                   NA), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = c(mean, min, max, impact_year, recovery_year)) %>%
  dplyr::select(!c(impact_year_Pre, recovery_year_Pre)) %>% # remove year of min. nº of individuals in the pre policy period and the year in which the nº ind is equal to the mean values of the post policy period
  mutate(impact = ifelse( mean_Post > mean_Pre,
                          (max_Post - mean_Pre) / mean_Pre,
                          (min_Post - mean_Pre) / mean_Pre),
         time_impact = impact_year_Post - t_policy,
         recovery = (mean_Post - mean_Pre) / mean_Pre,
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
    values_to = "value"
  )

############
# FIGURE 1 #
############

# new facet label names
metric.labs <- c("Impact (units)", "Recovery (units)", "Time to Impact (years)" , "Time to recovery (years)")
names(metric.labs) <- c("impact_avg",
                        "recovery_avg",
                        "time_impact_avg",
                        "time_recovery_avg")

# Custom color palette
custom_colors <- c("Taxa1" = "#38b2fe", "Taxa2" = "#ffab27", "Taxa3" = "#99cc00")

# Updated plot
stability_avg_long %>%
  dplyr::filter(metric %in% c("impact_avg", "recovery_avg")) %>%
  ggplot(aes(x = scenario, y = value, fill = taxa, shape = biome)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  facet_grid(metric ~ biome, scales = "free_y", labeller = labeller(metric = metric.labs), switch = "y") +
  geom_hline(yintercept = 0) +
  # use custom colors for taxa
  scale_fill_manual(values = custom_colors) +
  ylab("") +
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
    # modify x-axis text
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    # remove panel borders
    panel.border = element_blank()
  )
invisible(gc())

######################
# SUP MATERIAL FIG 1 #
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
    # modify x-axis text
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    # remove panel borders
    panel.border = element_blank()
  )
invisible(gc())


# Community metrics ------------------------------------------------------------

### Tpecies richness per cell in the landscape

community_df <- dummy_dataset %>%
  group_by(biome, scenario, time, cell_id, taxa) %>%
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
