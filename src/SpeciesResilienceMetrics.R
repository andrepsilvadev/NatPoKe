####################################
# MULTI-SPECIES RESILIENCE METRICS #
####################################
# Inês Silva
# 23 April 2025 updated on 06 May 2025

source("./src/libraries.R")
source("./src/customFunctions.R")  

##########
# Step 1 # Get all the data
##########

# Boreal Forests ---------------------------------------------------------------

## Europe SSP5
europe_SSP5 <- fread("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/13Sep_Europe_ssp585/Outputs/TNIND_yr_13Sep_Europe_ssp585.csv")
## Europe SSP1
europe_SSP1 <- fread("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/13Sep_Europe_ssp126/Outputs/TNIND_yr_13Sep_Europe_ssp126.csv")

## North America SSP5
northamerica_SSP5 <- fread("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/13Sep_NorthAmerica_ssp585/Outputs/TNIND_yr_13Sep_NorthAmerica_ssp585.csv") 

## North America SSP1
northamerica_SSP1 <- fread("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/13Sep_NorthAmerica_ssp126/Outputs/TNIND_yr_13Sep_NorthAmerica_ssp126.csv")


# Tropical Moist Forests -------------------------------------------------------

## Asia SSP5
asia_SSP5 <- fread("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/13Sep_Asia_ssp585/Outputs/TNIND_yr_13Sep_Asia_ssp585.csv")
## Asia SSP1
asia_SSP1 <- fread("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/13Sep_Asia_ssp126/Outputs/TNIND_yr_13Sep_Asia_ssp126.csv")
## Africa SSP5
africa_SSP5 <- fread("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/13Sep_Africa_ssp585/Outputs/TNIND_yr_13Sep_Africa_ssp585.csv")
## Africa SSP1
africa_SSP1 <- fread("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/13Sep_Africa_ssp126/Outputs/TNIND_yr_13Sep_Africa_ssp126.csv")


## South America SSP5
southamerica_SSP5 <- fread("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/13Sep_SouthAmerica_ssp585/Outputs/TNIND_yr_13Sep_SouthAmerica_ssp585.csv")
## South America SSP1
southamerica_SSP1 <- fread("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/13Sep_SouthAmerica_ssp126/Outputs/TNIND_yr_13Sep_SouthAmerica_ssp126.csv")

invisible(gc())

##########
# Step 2 # Combining all regions data together
##########

datasets <- list(europe_SSP5, northamerica_SSP5, asia_SSP5, africa_SSP5, southamerica_SSP5,
                 europe_SSP1, northamerica_SSP1, asia_SSP1, africa_SSP1, southamerica_SSP1)

TNIND_yr <- do.call("rbind", datasets)
# check for species names
#unique(TNIND_yr$biome)

rm(europe_SSP5, northamerica_SSP5, asia_SSP5, africa_SSP5, southamerica_SSP5,
   europe_SSP1, northamerica_SSP1, asia_SSP1, africa_SSP1, southamerica_SSP1)
invisible(gc())


##########
# Step 3 # Get correspondence between species names and functional group
##########

# retrieve trait data for trophic level, continent and biome info
combined_traits_data <- read_csv(here("data", "mammalTraits_2025-03-17.csv")) %>% 
  dplyr::filter(BIOME_NAME %in%  gsub("[/& ]", "", c("Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga"))) 

TNIND_yr <- TNIND_yr %>% 
  mutate(species = pretty_species_names(species)) %>% # if running twice it throws a warning - It's ok!
  left_join(
    dplyr::select(combined_traits_data, sci_name, BIOME_NAME, CONTINENT, trophic_level),
    by = c("species" = "sci_name",
           "biome" = "BIOME_NAME", # keep biome & continent here or a many-to-many warning will appear
           "region" = "CONTINENT")) %>%
  # simplify replicates numbering
  mutate(rep_num = str_extract(rep, "^[0-9]+"))
  



# %>% 
#   group_by(biome, region, species, timestep, scenario, trophic_level) %>% 
#   summarise(across(height:mass, ~ mean(.x, na.rm = TRUE)))


# write complete dataset into .csv to facilitate usage downstream
write_csv(TNIND_yr, 
         file = "./output/completeRun13Sep2025.csv")

##########
# Step 4 # Calculate metrics
##########

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
                                   ifelse(any(timestep > t_policy & TNIND >= mean_post),
                                          min(timestep[timestep > t_policy & TNIND >= mean_post], na.rm = TRUE), 
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
          file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/resilienceMetricsPerSpecies.csv",
          row.names = FALSE)
invisible(gc())

# average stability metrics across functional groups ---------------------------
stability_avg <- stability_sps %>%
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
          file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/resilienceMetricsAveraged.csv",
          row.names = FALSE)
invisible(gc())

# transform into long format for plots -----------------------------------------
stability_avg_long <- stability_avg %>%
  pivot_longer(
    cols = matches("_avg$|_sd$"),
    names_to = c("metric", ".value"),
    names_sep = "_")

##########
# Step 5 # build plot for impact and recovery
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

ggsave(filename = "./output/Figure1_13Sep2025.png", # path
       figure1, # plot
       bg = 'white', width = 230, height = 210, units = "mm", dpi = 1200, #compression = "lzw"
       ) # image parameters

t_burnin <- 100
t_policy <- 110

scenarios <- c("ssp126", "ssp585")

# Loop through each scenario
for (sc in scenarios) {
  
  p <- TNIND_yr %>%
    filter(scenario == sc) %>%
    ggplot(aes(x = timestep, y = TNIND, group = species)) +
    geom_line() +
    facet_wrap(biome + region ~ species, scales = "free_y", ncol = 4) +
    labs(y = "Total number of individuals",
         title = paste("Scenario:", sc)) +
    theme_minimal() +
    geom_vline(xintercept = t_policy, linetype = "dotted", color = "red", size = 0.8)
  
  print(p)   # will display in the plotting window
  
  # Optionally save each scenario plot as a file
  #ggsave(filename = paste0("TNIND_", sc, ".png"), plot = p,
   #      width = 12, height = 8, dpi = 300)
}
head(TNIND_yr)

##########
# Step 6 # build supplementary plot for time to impact and to recovery
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

ggsave(filename = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/figures_20250427/SupFigure1.png", # path
       supfigure1, # plot
       bg = 'white', width = 230, height = 210, units = "mm", dpi = 1200, #compression = "lzw"
) # image parameters

