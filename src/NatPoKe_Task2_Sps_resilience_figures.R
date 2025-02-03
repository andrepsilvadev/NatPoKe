## NatPoKe Figure 1 #
######## MIS ########
##### 13 JAN 25 #####

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

###########################################
# CREATE DUMMY DATASET BASED ON MetaRange #
###########################################

# import data
example_01_res_df <- read_csv(
  "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/example_01_res_df.csv")

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
#write.csv(dummy_dataset, "~/NatPoKe/data/dummy_dataset_Jan2025.csv")
invisible(gc())



################################
# CALCULATE RESILIENCE METRICS #
################################

t_burnin <- 2
t_policy <- 5

# calculate post policy mean value for the recovery time metric 
# to be possible in one go with the other metrics)
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
         # WORTH CALCULATING RECOVERY IF IMPACT IS POSITIVE? See metrics explanation canva
         recovery = ifelse(impact <= 0,
                           (mean_Post - mean_Pre) / mean_Pre,
                           NA),
         time_recovery = recovery_year_Post - t_policy)
invisible(gc())


# average stability metrics ACROSS TAXA
stability_avg <- stability_sps %>%
  group_by(biome, scenario, taxa) %>%
  dplyr::summarize(
    impact_avg = mean(impact, na.rm = TRUE),
    impact_sd = sd(impact, na.rm = TRUE),
    recovery_avg = mean(recovery, na.rm = TRUE),
    recovery_sd = sd(recovery, na.rm = TRUE),
    timeimpact_avg = mean(time_impact, na.rm = TRUE),
    timeimpact_sd = sd(time_impact, na.rm = TRUE),
    timerecovery_avg = mean(time_recovery, na.rm = TRUE),
    timerecovery_sd = sd(time_recovery, na.rm = TRUE)
  )
invisible(gc())

#stability_avg
library(grr)
# stability metrics in long format for plots
stability_avg_long <- stability_avg %>%
  pivot_longer(
    cols = matches("_avg$|_sd$"),
    names_to = c("metric", ".value"),
    names_sep = "_")

############
# FIGURE 1 # Impact and Recovery per taxa for both biomes
############

# new facet label names
metric.labs <- c("Impact (units)", "Recovery (units)", "Time to Impact (years)" , "Time to recovery (years)")
names(metric.labs) <- c("impact",
                        "recovery",
                        "timeimpact",
                        "timerecovery")

# Custom color palette
custom_colors <- c("Bird" = "#38b2fe", "Mammal" = "#ffab27", "Insect" = "#99cc00")

# Updated plot
figure1 <- stability_avg_long %>%
  dplyr::filter(metric %in% c("impact", "recovery")) %>%
  ggplot(aes(x = scenario, y = avg, fill = taxa)) +
  geom_bar(stat = "identity", position = position_dodge(0.6), width = 0.6) +
  geom_errorbar(aes(ymin = avg-sd, ymax = avg+sd), width = 0.2, colour = "black", alpha = 0.9, size = 0.4, position = position_dodge(0.6)) +
  facet_grid(metric ~ biome, scales = "free", labeller = labeller(metric = metric.labs), switch = "y") +
  geom_hline(yintercept = 0) +
  # use custom colors for taxa
  scale_fill_manual("Taxa", values = custom_colors, ) +
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
    legend.title = element_text(face = "bold"),
    # modify y & x-axis text
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title = element_text(face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
    # remove panel borders
    panel.border = element_blank(),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(2, "lines"))
figure1
invisible(gc())

#ggsave(plot = figure1, file = "~/NatPoKe/output/dummy_figures/Figure1_Impact&Recovery.tiff", bg = 'white', width = 200, height = 180, units = "mm", dpi = 1200, compression = "lzw")


######################
# SUP MATERIAL FIG 1 # Time to impact and Time to recovery per taxa for both biomes
######################

# Updated plot
suplementary_figure1 <- stability_avg_long %>%
  dplyr::filter(metric %in% c("timeimpact", "timerecovery")) %>%
  ggplot(aes(x = scenario, y = avg, fill = taxa)) +
  geom_bar(stat = "identity", position = position_dodge(0.6), width = 0.6) +
  geom_errorbar(aes(ymin = avg-sd, ymax = avg+sd), width = 0.2, colour = "black", alpha = 0.9, size = 0.4, position = position_dodge(0.6)) +
  facet_grid(metric ~ biome, scales = "free", labeller = labeller(metric = metric.labs), switch = "y") +
  geom_hline(yintercept = 0) +
  # use custom colors for taxa
  scale_fill_manual("Taxa", values = custom_colors) +
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
    legend.title = element_text(face = "bold"),
    # modify y & x-axis text
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title = element_text(face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
    # remove panel borders
    panel.border = element_blank(),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(2, "lines"))
suplementary_figure1
invisible(gc())

#ggsave(plot = suplementary_figure1, file = "~/NatPoKe/output/dummy_figures/Suplementary_figure1_TimeImpact&TimeRecovery.tiff", bg = 'white', width = 200, height = 180, units = "mm", dpi = 1200, compression = "lzw")
