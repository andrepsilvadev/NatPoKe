####################################
# MULTI-SPECIES RESILIENCE METRICS #
####################################
# Inês Silva
# 23 April 2025

source("./src/libraries.R")
source("./src/customFunctions.R")

##########
# Step 1 # Get all the data
##########

# Boreal Forests ---------------------------------------------------------------

## Europe SSP5
europe_SSP5 <- fread("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/26Mar2025_Europe/Outputs/TNIND_yr_26Mar2025_Europe.csv") %>% 
  mutate(scenario = "SSP5",
         Biome = case_when(Biome == "Boreal Forests Taiga" ~ "BorealForestsTaiga"),
         Region = "Europe")

colnames(europe_SSP5) <- c("TNIND", "biome", "region", "species", "timestep", "scenario")

## Europe SSP1
europe_SSP1 <- fread("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/23April_Europe/Outputs/TNIND_yr_23April_Europe.csv") %>% 
  dplyr::select("TNIND", "biome", "region", "species", "timestep", "scenario")


## North America SSP5
northamerica_SSP5 <- fread("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/27Mar2025_NorthAmerica/Outputs/TNIND_yr_27Mar2025_NorthAmerica.csv") %>% 
  mutate(scenario = "SSP5",
         biome = case_when(biome == "oreal Forests Taiga" ~ "BorealForestsTaiga")) 

## North America SSP1
northamerica_SSP1 <- fread("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/23April_NorthAmerica/Outputs/TNIND_yr_23April_NorthAmerica.csv") %>% 
  dplyr::select("TNIND", "biome", "region", "species", "timestep", "scenario") %>% 
  mutate(scenario = "SSP1")


# Tropical Moist Forests -------------------------------------------------------

## Asia SSP5
asia_SSP5 <- fread("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/27Mar2025_Asia/Outputs/TNIND_yr_27Mar2025_Asia.csv") %>% 
  mutate(scenario = "SSP5",
         biome = case_when(biome == "Tropical Subtropical Moist Broadleaf Forests" ~ "TropicalSubtropicalMoistBroadleafForests"))

## Asia SSP1
asia_SSP1 <- fread("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/23April_Asia/Outputs/TNIND_yr_23April_Asia.csv") %>% 
  dplyr::select("TNIND", "biome", "region", "species", "timestep", "scenario")%>% 
  mutate(scenario = "SSP1")


## Africa SSP5
africa_SSP5 <- fread("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/27Mar2025_Africa/Outputs/TNIND_yr_27Mar2025_Africa.csv") %>% 
  mutate(scenario = "SSP5",
         biome = case_when(biome == "Tropical Subtropical Moist Broadleaf Forests" ~ "TropicalSubtropicalMoistBroadleafForests"))
## Africa SSP1
africa_SSP1 <- fread("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/23April_Africa/Outputs/TNIND_yr_23April_Africa.csv") %>% 
  dplyr::select("TNIND", "biome", "region", "species", "timestep", "scenario")%>% 
  mutate(scenario = "SSP1")



## South America SSP5
southamerica_SSP5 <- fread("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/27Mar2025_SouthAmerica/Outputs/TNIND_yr_27Mar2025_SouthAmerica.csv") %>% 
  mutate(scenario = "SSP5")
## South America SSP1
southamerica_SSP1 <- fread("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/23April_SouthAmerica/Outputs/TNIND_yr_23April_SouthAmerica.csv") %>% 
  dplyr::select("TNIND", "biome", "region", "species", "timestep", "scenario")%>% 
  mutate(scenario = "SSP1")


invisible(gc())

##########
# Step 2 # Combining all regions data together
##########

datasets <- list(europe_SSP5, northamerica_SSP5, asia_SSP5, africa_SSP5, southamerica_SSP5,
                 europe_SSP1, northamerica_SSP1, asia_SSP1, africa_SSP1, southamerica_SSP1)

TNIND_yr <- do.call("rbind", datasets)

# check for species names
#unique(TNIND_yr$biome)

##########
# Step 3 # Get correspondence between species names and functional group
##########
combined_traits_data <- read_csv("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/NatPoKe/data/mammalTraits_2025-03-17.csv") %>% 
  #read_csv(here("data", "mammalTraits_2025-03-17.csv")) %>% 
  dplyr::filter(BIOME_NAME %in% c("Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga")) %>% 
  mutate(
    BIOME_NAME = gsub("[/& ]", "", BIOME_NAME),
    CONTINENT = gsub("[/& ]", "", CONTINENT),
    Trophic = case_when(
    # based on Schloss 2012
    Diet.Meat >= 90 ~ "Carnivore",
    Diet.Plant >= 90 ~ "Herbivore",
    TRUE ~ NA_character_),
    trophic_level = case_when(
      # from original database
      trophic_level == 1 ~ "Herbivore",
      trophic_level == 2 ~ "Omnivore",
      trophic_level == 3 ~ "Carnivore",
      TRUE ~ as.character(trophic_level)))

TNIND_yr <- TNIND_yr %>% 
  mutate(species = pretty_species_names(species)) %>% # if running twice it throws a warning - It's ok!
  left_join(
    dplyr::select(combined_traits_data, sci_name, BIOME_NAME, CONTINENT, trophic_level),
    by = c("species" = "sci_name",
           "biome" = "BIOME_NAME", # keep biome & continent here or a many-to-many warning will appear
           "region" = "CONTINENT")
  )
#C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/NatPoKe/data/mammalTraits_2025-03-17.csv
# write complete dataset into .csv to facilitate usage downstream
write_csv(TNIND_yr, 
          file = "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/completeRunApril2025.csv")

##########
# Step 4 # Calculate metrics
##########

# define burn-in and policy start year
t_burnin <- 100
t_policy <- 110

  # Step 4.1 # calculate post policy mean value (for the recovery time metric)
post_disturbance_values <- TNIND_yr %>%
  #filter(timestep > t_burnin) %>% # remove burn-in period
  mutate(period = ifelse(timestep > t_burnin &
                           timestep <= t_policy, "Pre", "Post")) %>%  # code pre and post policy periods
  group_by(biome, species, scenario, period, trophic_level) %>%
  filter(period == "Post") %>% # filter for the post policy period only
  summarise(mean_post = mean(TNIND, na.rm = TRUE))
invisible(gc())

  # 4.2 # calculate metrics per SCENARIO, BIOME, FUCTIONAL GROUP, SPECIES
stability_sps <- TNIND_yr %>%
  filter(timestep > t_burnin) %>% # remove burn-in period
  mutate(period = ifelse(timestep >= t_burnin & timestep <= t_policy, "Pre", "Post")) %>%  # code pre and post policy
  left_join(post_disturbance_values,by = c("biome", "species", "scenario", "period", "trophic_level")) %>%
  group_by(biome, species, scenario, period, trophic_level) %>%
  summarise(mean = mean(TNIND, na.rm = TRUE),
            # find mean nº of individuals
            min = min(TNIND, na.rm = TRUE),
            # find min. nº of individuals
            max = max(TNIND, na.rm = TRUE),
            # find max. nº of individuals
            impact_year = timestep[which.min(TNIND)],
            # find the year the pop. reaches a min. value in the post policy period
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
invisible(gc())

  # 4.3 # average across functional group

# average stability metrics ACROSS TAXA
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

  # 4.4 # transform into long format
library(grr)
# stability metrics in long format for plots
stability_avg_long <- stability_avg %>%
  pivot_longer(
    cols = matches("_avg$|_sd$"),
    names_to = c("metric", ".value"),
    names_sep = "_")

##########
# Step 5 # build figure for impact and recovery
##########

# new facet label names
metric.labs <- c("Impact (units)", "Recovery (units)", "Time to Impact (years)" , "Time to recovery (years)")
names(metric.labs) <- c("impact",
                        "recovery",
                        "timeimpact",
                        "timerecovery")
biome_names <- c("BorealForestsTaiga" = "Boreal Forests Taiga", "TropicalSubtropicalMoistBroadleafForests" = "Tropical & Subtropical\nMoist Broadleaf Forests")

biome_names

# Custom color palette
custom_colors <- c("SSP5" = "#ffab27", "SSP1" = "#99cc00")

# Updated plot
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


ggsave(filename = "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/figures_20250427/Figure1.tiff", # path
       figure1, # plot
       bg = 'white', width = 230, height = 210, units = "mm", dpi = 1200, compression = "lzw") # image parameters



# maybe (just maybe) we can go bacj to get the radial plot??