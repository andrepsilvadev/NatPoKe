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
library(here)
library(data.table)

###############
# IMPORT DATA #
###############

### DO NOT FORGET ###
# When the model is workning we need to add a simple pice of code combining
# several runs together before this
### DO NOT FORGET ###

totalDataset <- fread(file.path(dirout, "metaRangeOutputs24Mar2025_afternoon10km.csv")) %>% 
  mutate(scenario = "BAU")
invisible(gc())


###############################
# TOTAL NUMBER OF INDIVIDUALS #
###############################

t_burnin <- 100
t_policy <- 110

# !!! BE CAREFULL !!! #
## Total number of individuals is different from mean number of individuals
## In a landscape with three cells where cell 1 has two moose, cell 2 has three
## moose and cell 3 has no moose, the total number of individuals in the landscape
## would be 2+3+0 = 5 moose. But if we wanted the mean abundance of moose in those
## three cells it would be (2+3)/2 = 2.5 moose

# Total number of individuals (TNIND) per year and cellid
######### AT THE MOMENT WE STILL DON'T HAVE DIFFERENT REPLICATES ###############
TNIND <- totalDataset %>%
  group_by(species, Taxa, biome, scenario, timestep) %>% # ADD HERE WHEN IT EXISTS THE REP VARIABLE (REP FOR REPLICATES)
  dplyr::summarize(sum_TNIND = sum(abundance, na.rm = TRUE), # n individuals in each cell in each group (per replicate basically)
                   n = n()) %>% 
  dplyr::select(!n) %>% 
  group_by(species, Taxa, biome, scenario, timestep) %>% # KEEP SIM BUT REMOVE REP HERE
  dplyr::summarize(mean_TNIND = mean(sum_TNIND, na.rm = TRUE))
head(TNIND)

# Total number of individuals per year
TNIND_yr <- TNIND %>% # n cells used for the calculus
  group_by(species, Taxa, biome, scenario, timestep) %>%
  dplyr::summarize(mean_yr = mean(mean_TNIND, na.rm = TRUE), # cell mean 
                   sd_yr = sd(mean_TNIND, na.rm = TRUE),
                   n = n()) %>% 
  dplyr::select(!n) %>% 
  dplyr::filter(timestep > t_burnin)

TNIND_per_year <- ggplot(data = TNIND_yr, aes(x = timestep, y = mean_yr, group = species)) + 
  geom_line() + 
  facet_wrap(scenario~ species, scales = "free_y", ncol = 4) +
  labs(y = "Total number of individuals") +
  theme_minimal() +
  geom_vline(xintercept = t_policy, linetype = "dotted", color = "black", size = 0.8)  # add line at time of disturbance

ggsave(plot = TNIND_per_year,
       file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/27Feb2025/TotalNumberIndividualsTwoBadRuns.tiff",
       bg = 'white', width = 200, height = 180, units = "mm", dpi = 1200, compression = "lzw")


################################
# CALCULATE RESILIENCE METRICS #
################################

# calculate post policy mean value for the recovery time metric 
# to be possible in one go with the other metrics)
post_disturbance_values <- TNIND_yr %>%
  #filter(timestep > t_burnin) %>% # remove burn-in period
  mutate(period = ifelse(timestep > t_burnin &
                           timestep <= t_policy, "Pre", "Post")) %>%  # code pre and post policy periods
  group_by(biome, species, scenario, period, Taxa) %>%
  filter(period == "Post") %>% # filter for the post policy period only
  summarise(mean_post = mean(mean_yr, na.rm = TRUE))
invisible(gc())

# calculate all stability metrics per biome, policy & species
stability_sps <- TNIND_yr %>%
  filter(timestep > t_burnin) %>% # remove burn-in period
  mutate(period = ifelse(timestep >= t_burnin & timestep <= t_policy, "Pre", "Post")) %>%  # code pre and post policy
  left_join(post_disturbance_values,by = c("biome", "species", "scenario", "period", "Taxa")) %>%
  group_by(biome, species, scenario, period, Taxa) %>%
  summarise(mean = mean(mean_yr, na.rm = TRUE),
            # find mean nº of individuals
            min = min(mean_yr, na.rm = TRUE),
            # find min. nº of individuals
            max = max(mean_yr, na.rm = TRUE),
            # find max. nº of individuals
            impact_year = timestep[which.min(mean_yr)],
            # find the year the pop. reaches a min. value in the post policy period
            recovery_year = ifelse(any(timestep > t_policy & mean_yr >= mean_post),
                                   min(timestep[timestep > t_policy & mean_yr >= mean_post], na.rm = TRUE), # find the year where n_abundance is equal or smaller than the post policy mean 
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
  group_by(biome, scenario, Taxa) %>%
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
  ggplot(aes(x = scenario, y = avg, fill = Taxa)) +
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

ggsave(plot = figure1,
       file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/Figure1_Impact&RecoveryTwoBadRuns.tiff",
       bg = 'white', width = 200, height = 180, units = "mm", dpi = 1200, compression = "lzw")



######################
# SUP MATERIAL FIG 1 # Time to impact and Time to recovery per taxa for both biomes
######################

# Updated plot
suplementary_figure1 <- stability_avg_long %>%
  dplyr::filter(metric %in% c("timeimpact", "timerecovery")) %>%
  ggplot(aes(x = scenario, y = avg, fill = Taxa)) +
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

ggsave(plot = suplementary_figure1,
        file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/SuplementaryFig1_TimeImpact&TimeRecoveryTwoBadRuns.tiff",
        bg = 'white', width = 200, height = 180, units = "mm", dpi = 1200, compression = "lzw")
