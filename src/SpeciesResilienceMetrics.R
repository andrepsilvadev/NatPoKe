####################################
# MULTI-SPECIES RESILIENCE METRICS #
####################################
# Inês Silva
# 23 April 2025 updated on 06 May 2025, 06 Nov 2025

source("./src/libraries.R")
source("./src/customFunctions.R")  

##########
# STEP 1 # Read complete MetaRange run from 31 Oct 2025
##########

# all runs were previously compiled into one .csv file stored in the outputs folder
TNIND_yr <- fread("./output/completeMetaRangeRun_31Oct25.csv")

##########
# STEP 2 # Calculate metrics
##########

length(unique(TNIND_yr$timestep))

t_burnin <- 100 # burn-in years
t_policy <- 135 # final timestep

# calculate post policy mean value --------------------------------------------- 
# for the recovery time metric
post_disturbance_values <- TNIND_yr %>%
  dplyr::filter(timestep > t_burnin) %>% # remove burn-in period
  mutate(period = ifelse(timestep > t_burnin &
                           timestep <= t_policy, "Pre", "Post")) %>%  # code pre and post policy periods
  dplyr::filter(period == "Post") %>% # filter for the post policy period only
  group_by(biome, scenario, period, trophic_level, species, rep_num) %>% 
  summarise(across(c(TNIND, MNIND, mean_repRate, mean_carrCap, occupancy),
                   ~ mean(.x, na.rm = TRUE),
                   .names = "mean_{.col}"),
            .groups = "drop") %>% 
  # collapse across replicates
  group_by(biome, scenario, period, trophic_level, species) %>%
  summarise(mean_post = mean(mean_TNIND, na.rm = TRUE))
invisible(gc())

# calculate metrics per scenario, biome, functional group & sps ----------------
stability_sps <- TNIND_yr %>%
  dplyr::filter(timestep > t_burnin) %>% # remove burn-in period
  mutate(period = ifelse(timestep >= t_burnin & timestep <= t_policy, "Pre", "Post")) %>%  # code pre and post policy
  left_join(post_disturbance_values,by = c("biome", "species", "scenario", "period", "trophic_level")) %>%
  group_by(biome, scenario, period, trophic_level, species) %>%
  summarise(
    # find mean nº of individuals
    mean = mean(TNIND, na.rm = TRUE),
    # find min. nº of individuals
    min = min(TNIND, na.rm = TRUE),
    # find max. nº of individuals
    max = max(TNIND, na.rm = TRUE),
    # find the year the pop. reaches a min. value in the post policy period
    impact_year = timestep[which.min(TNIND)],
    # find year the pop bounces back to the same value in the pre period or even surpasses it
    recovery_year = ifelse(mean == 0, 
                           NA, 
                           ifelse(any(timestep > impact_year & TNIND >= mean_post),
                                  min(timestep[timestep > impact_year & TNIND >= mean_post], na.rm = TRUE), 
                                  NA)), 
    .groups = "drop") %>%
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

# write metrics per species to csv file (SUPPLEMENTARY TABLE X)
write.csv(stability_sps %>% 
            dplyr::select(biome, scenario, species, trophic_level, mean_Post, mean_Pre, impact, time_impact, recovery, time_recovery),
          file = "./output/resilienceMetricsPerSpecies.csv",
          row.names = FALSE)
invisible(gc())

# average stability metrics across functional groups ---------------------------
stability_avg <- stability_sps %>%
  dplyr::filter(!species == "Bison bonasus") %>% 
  group_by(biome, scenario, trophic_level) %>%
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

# new column labels
lookup <- c("impact_avg" = "Mean Impact",
            "impact_sd" = "Impact SD",
            "recovery_avg" = "Mean Recovery",
            "recovery_sd" = "Recovery SD",
            "timeimpact_avg" = "Mean Time to Impact",
            "timeimpact_sd" = "Time to Impact SD",
            "timerecovery_avg" = "Mean Time to Recovery",
            "timerecovery_sd" = "Time to Recovery SD")

# (Table x to support **FIGURE 1**)
write.csv(stability_avg %>%
            rename_with(~ lookup[.x], .cols = names(lookup)),
          file = "./output/resilienceMetricsAveraged.csv",
          row.names = FALSE)
invisible(gc())

# transform into long format for plots -----------------------------------------
stability_avg_long <- stability_avg %>%
  pivot_longer(
    cols = matches("_avg$|_sd$"),
    names_to = c("metric", ".value"),
    names_sep = "_")

##########
# STEP 3 # build plot for impact and recovery
##########

# new facet label names
metric.labs <- c("Impact\n (proportion of individuals lost)", "Recovery\n (proportion of individuals recovered)", "Time to Impact (years)" , "Time to recovery (years)")
names(metric.labs) <- c("impact",
                        "recovery",
                        "timeimpact",
                        "timerecovery")
biome_names <- c("BorealForestsTaiga" = "Boreal Forests Taiga", "TropicalSubtropicalMoistBroadleafForests" = "Tropical & Subtropical\nMoist Broadleaf Forests")

# Custom color palette
custom_colors <- c("ssp585" = "#ffab27", "ssp126" = "#99cc00")

# Updated plot (**FIGURE 1**)
figure1 <- stability_avg_long %>%
  dplyr::filter(metric %in% c("impact", "recovery")) %>%
  ggplot(aes(x = trophic_level , y = avg, fill = scenario)) +
  geom_bar(stat = "identity", position = position_dodge(0.6), width = 0.6) +
  geom_errorbar(aes(ymin = avg-sd, ymax = avg+sd), width = 0.2, colour = "black", alpha = 0.9, size = 0.4, position = position_dodge(0.6)) +
  facet_grid(metric ~ biome, scales = "free_y", labeller = labeller(metric = metric.labs, biome = biome_names), switch = "y") +
  geom_hline(yintercept = 0) +
  # use custom colors for taxa
  scale_fill_manual("Socio-economic\nscenario", values = custom_colors) +
  ylab("") +
  xlab("") +
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
    #legend.title = element_text(face = "bold"),
    # modify y & x-axis text
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 0.8),
    axis.title = element_text(face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
    # remove panel borders
    panel.border = element_blank(),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(2, "lines"))



figure1
invisible(gc())

ggsave(filename = "./output/Figure1_ResilienceMetrics_31Oct25.png", # path
       figure1, # plot
       bg = 'white', width = 230, height = 210, units = "mm", dpi = 1200, #compression = "lzw"
) # image parameters

##########
# STEP 4 # build supplementary plot for time to impact and to recovery
##########


# Updated plot (**SUPPLEMENTARY FIGURE 1**)
supfigure1 <- stability_avg_long %>%
  dplyr::filter(metric %in% c("timeimpact", "timerecovery")) %>%
  ggplot(aes(x = trophic_level , y = avg, fill = scenario)) +
  geom_bar(stat = "identity", position = position_dodge(0.6), width = 0.6) +
  geom_errorbar(aes(ymin = avg-sd, ymax = avg+sd), width = 0.2, colour = "black", alpha = 0.9, size = 0.4, position = position_dodge(0.6)) +
  facet_grid(metric ~ biome, scales = "free", labeller = labeller(metric = metric.labs, biome = biome_names), switch = "y") +
  geom_hline(yintercept = 0) +
  # use custom colors for taxa
  scale_fill_manual("Socio-economic\nscenario", values = custom_colors) +
  ylab("") +
  xlab("") +
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
    legend.title = element_text(face = "bold"),
    # modify y & x-axis text
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title = element_text(face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
    # remove panel borders
    panel.border = element_blank(),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(2, "lines"))
supfigure1
invisible(gc())

ggsave(filename = "./output/SupFigure1_TimeToImpactRecovery_31Oct25.png", # path
       supfigure1, # plot
       bg = 'white', width = 230, height = 210, units = "mm", dpi = 1200, #compression = "lzw"
) # image parameters