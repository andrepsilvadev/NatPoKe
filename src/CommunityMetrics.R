#############################
# COMMUNITY METRICS FIGURES #
#############################
# Inês Silva
# 24 April 2025 updated on 06 May 2025

source("./src/libraries.R")
source("./src/customFunctions2.R")

##########
# Step 1 # Import data
##########

# all runs were previously compiled into one .csv file stored in the outputs folder

TNIND_yr <- read.csv("D:/metaRange_April26/completeMetaRangeRun_20260405.csv")


# clean up erroneous species
TNIND_yr <- TNIND_yr %>%
  filter(
    !(species == "Lynx lynx" & biome == "Tropical & Subtropical Moist Broadleaf Forests" & region == "Asia"),
    !(species == "Ursus arctos" & biome == "Tropical & Subtropical Moist Broadleaf Forests" & region == "Asia"))

# count number of unique species per biome and trophic level
species_count <- TNIND_yr %>%
  group_by(biome, region, trophic_level) %>%
  summarise(
    n_species = n_distinct(species),
    .groups = "drop")

##########
# Step 2 # Define burn-in and scenario start + other cosmetic arguments
##########

t_burnin <- 25
t_policy <- 136

# biome labels
biome_names <- c("Boreal Forests/Taiga" = "Boreal Forests/\nTaiga",
                 "Tropical & Subtropical Moist Broadleaf Forests" = "Tropical & Subtropical\nMoist Broadleaf Forests")

# biome labels
scenario_names <- c("ssp126" = "SSP1-2.6",
                 "ssp585" = "SSP5-8.5")


# prep custom color palette
trophic_cols <- c("Herbivore" = "#99cc00",
                  "Carnivore" = "#ffab27",
                  "Omnivore"  = "#377eb8")

##########
# Step 3 # Calculate Community metric (Shannon Diversity)
##########

#head(TNIND_yr)

Shannon_index <- TNIND_yr %>%
  group_by(biome, future_scenario, timestep, trophic_level, rep_num, species) %>%
  summarise(abundance = mean(TNIND, na.rm = TRUE), .groups = "drop") %>%
  group_by(biome, future_scenario, timestep, trophic_level, rep_num) %>%
  summarise(
    Shannon_Wiener_Index = {
      p <- abundance / sum(abundance)
      ln_p <- ifelse(p > 0, log(p), 0)
      -sum(p * ln_p, na.rm = TRUE)
    },
    .groups = "drop") %>%
  filter(timestep >= 25)

# clean up memory
invisible(gc())

##########
# Step 4 # Build Shannon Wienner Index through time plot
##########

icon_positions_shannon2 <- Shannon_index %>%
  group_by(future_scenario, biome, trophic_level) %>%
  arrange(timestep) %>%
  slice_tail(n = 5) %>%   # last 5 timesteps
  slice_head(n = 1) %>%   # max - 4
  transmute(
    x = timestep,
    y = Shannon_Wiener_Index + 0.03) %>%
  ungroup() %>% 
  left_join(species_count  %>%
              group_by(biome, trophic_level) %>% 
              summarise(n_species = sum(n_species)), by = c("biome", "trophic_level"))


ShannonOverTime <- ggplot(data = Shannon_index,
       aes(x = timestep, y = Shannon_Wiener_Index, colour = trophic_level, fill = trophic_level)) +
  # ribbon (mean ± SE)
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.25, colour = NA) +
  # mean line
  stat_summary(fun = mean, geom = "line", linewidth = 0.6) +
  # to deal with axis more freely (add axis on top row)
  ggh4x::facet_grid2(biome ~ future_scenario,
                     scales = "free", axes = "x", switch = "y", labeller = labeller(biome = as_labeller(biome_names), future_scenario = as_labeller(scenario_names))) +
  labs(x = "Time", y = "Shannon-Wiener index", caption = "Dotted line = Start of future scenarios") +
  scale_color_manual("Trophic levels", values = trophic_cols) +
  scale_fill_manual(values = trophic_cols) +
  scale_x_continuous(
    breaks = c(25, 40, 60, 110),
    labels = c("2015", "2030", "2050", "2100")) +
  # add label with number of species
  geom_text(data = icon_positions_shannon2, aes(x = x, y = y, label = paste0("n = ", n_species)), inherit.aes = FALSE, size = 2) +
  theme_minimal(base_size = 8) +
  theme(
    axis.line = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
    # modify facet labels
    strip.text = element_text(face = "bold", size = rel(1)),
    strip.placement = "outside",
    # adjust legend
    legend.position = "bottom",
    # modify x-axis text
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    # remove panel borders
    panel.border = element_blank(),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(2, "lines"),
    plot.margin = unit(c(0, 0.5, 0, 0.5), "cm")) +
  geom_vline(xintercept = c(40, 60, 110), linetype = "dashed", color = "gray50", size = 0.5)

# save plot
ggsave(filename = "D:/Figures/Figure2_ShannonWiener_troughTime.png", # path
      ShannonOverTime, # plot
     bg = 'white', width = 180, height = 120, units = "mm", dpi = 1200,
#compression = "lzw"
) # image parameters


# AUXILARY TABLE FOR FIGURE 2
Shannon_index_DF <- Shannon_index %>%
  group_by(future_scenario, biome, trophic_level) %>% 
  mutate(Shannon_Index_change_pct = ((Shannon_Wiener_Index - Shannon_Wiener_Index[timestep == 25])/Shannon_Wiener_Index[timestep == 25])*100) 
write.csv(Shannon_index_DF,
          file = "./output/ShannonIndexChange_31Oct2025.csv",
          row.names = FALSE)         

############
### LIXO ###
############
# get the top-right corner coordinates for each *TOP* facet only
icon_positions_shannon <- Shannon_index %>%
  group_by(biome, trophic_level) %>%
  summarise(x = max(timestep) - 2, # xx coordinate
            y = 0.45 ) %>% # yy coordinate, max(Shannon)
  ungroup() %>% 
  left_join(species_count  %>%
              group_by(biome, trophic_level) %>% 
              summarise(n_species = sum(n_species)), by = c("biome", "trophic_level"))


ShannonOverTime <- ggplot(data = Shannon_index,
                          aes(x = timestep, y = Shannon_Wiener_Index, color = future_scenario)) +
  geom_line() +
  # to deal with axis more freely (add axis on top row)
  ggh4x::facet_grid2(biome ~ trophic_level,
                     scales = "free", axes = "x", switch = "y", labeller = labeller(biome = as_labeller(biome_names))) +
  labs(x = "Time", y = "Shannon-Wiener index", caption = "Dotted line = Start of future scenarios") +
  scale_color_manual("Socio-economic\nscenario", values = custom_colors) +
  scale_x_continuous(
    breaks = c(25, 40, 60, 110),
    labels = c("2015", "2030", "2050", "2100")) +
  # add label with number of species
  geom_text(data = icon_positions_shannon, aes(x = x, y = 0.4, label = paste0("n = ", n_species)), inherit.aes = FALSE, size = 2.5) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
    # modify facet labels
    strip.text = element_text(face = "bold", size = rel(1)),
    strip.placement = "outside",
    # adjust legend
    legend.position = "bottom",
    # modify x-axis text
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    # remove panel borders
    panel.border = element_blank(),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(2, "lines"),
    plot.margin = unit(c(0, 0.5, 0, 0.5), "cm")) +
  geom_vline(xintercept = c(40, 60, 110), linetype = "dashed", color = "gray50", size = 0.5)

ShannonOverTime
