#############################
# COMMUNITY METRICS FIGURES #
#############################
# Inês Silva
# 24 April 2025 updated on 06 May 2025

source("./src/libraries.R")
source("./src/customFunctions.R")

##########
# Step 1 # Import data
##########

# all runs were previously compiled into one .csv file stored in the outputs folder
TNIND_yr <- fread("./output/13Sept_FinBioMeeting/completeRun13Sep2025.csv")


# count number of unique species per biome and trophic level
species_count <- TNIND_yr[, .(n_species = uniqueN(species)), by = .(biome, region, trophic_level)]
#View(species_count)

##########
# Step 2 # Define burn-in and scenario start + other cosmetic arguments
##########

t_burnin <- 100
t_policy <- 135

# get icons for each taxonomic group plot
uuid_carnivores <- get_uuid(name = "Panthera leo", n = 5)[[5]]
uuid_herbivores <- get_uuid(name = "Cervus elaphus", n = 1)
uuid_omnivores <- get_uuid(name = "Sus scrofa", n = 5)[[2]]

# prep labels
biome_names <- c("BorealForestsTaiga" = "Boreal Forests Taiga",
                 "TropicalSubtropicalMoistBroadleafForests" = "Tropical & Subtropical\nMoist Broadleaf Forests")

# prep custom color palette
custom_colors <- c("ssp585" = "#ffab27", "ssp126" = "#99cc00")

##########
# Step 3 # Calculate Community metric (Shannon Diversity)
##########

head(TNIND_yr)

# Shannon's index
Shannon_index <- TNIND_yr %>%
  group_by(biome, scenario, timestep, trophic_level, rep_num) %>% 
  summarise(across(c(TNIND, MNIND, mean_repRate, mean_carrCap, occupancy),
                   ~ mean(.x, na.rm = TRUE),
                   .names = "mean_{.col}"),
            .groups = "drop") %>% 
  # collapse across replicates
  group_by(biome, scenario, timestep, trophic_level) %>%
  summarise(mean_TNIND = mean(mean_TNIND, na.rm = TRUE)) %>% 
 
  dplyr::mutate(p_i = mean_TNIND / sum(mean_TNIND),
                # calculate proportion of individuals of species i
                ln_p_i = ifelse(p_i > 0, log(p_i), 0)) %>%  # in case pi is 0
  group_by(biome, scenario, timestep, trophic_level) %>%
  # up until here the table has values for each species, then info is summarised
  dplyr::summarize(Shannon_Wiener_Index = -sum(p_i * ln_p_i)) %>%   # calculate the Shannon-Wiener index
  dplyr::filter(timestep >= 100)
invisible(gc())

# if adding more variables we need to transform from wide to long format


##########
# Step 4 # Build plot
##########
Shannon_index <- Shannon_index %>% 
  dplyr::filter(!trophic_level == "Omnivore")
# get the top-right corner coordinates for each *TOP* facet only
icon_positions_shannon <- Shannon_index %>%
  group_by(biome, trophic_level) %>%
  summarise(x = max(timestep) - 2, # xx coordinate
            y = 0.45 ) %>% # yy coordinate, max(Shannon)
ungroup() %>% 
  # add the PhyloPic UUIDs to the positions
  mutate(phylopic = case_when(
    biome == "TropicalSubtropicalMoistBroadleafForests" ~ NA_character_,  # if Tropical biome, no icon (NA)
    trophic_level == "Carnivore" ~ uuid_carnivores,
    trophic_level == "Herbivore" ~ uuid_herbivores,
    trophic_level == "Omnivore" ~ uuid_omnivores
  )) %>%
  left_join(species_count  %>%
              group_by(biome, trophic_level) %>% 
              summarise(n_species = sum(n_species)), by = c("biome", "trophic_level"))

  
ShannonOverTime <- ggplot(data = Shannon_index,
       aes(x = timestep, y = Shannon_Wiener_Index, color = scenario)) +
  geom_line() +
  # to deal with axis more freely (add axis on top row)
  ggh4x::facet_grid2(biome ~ trophic_level,
                     scales = "free", axes = "x", switch = "y", labeller = labeller(biome = as_labeller(biome_names))) +
  labs(x = "Time", y = "Shannon-Wiener index", caption = "Dotted line = Start of future scenarios") +
  scale_color_manual("Socio-economic\nscenario", values = custom_colors) +
  scale_x_continuous(
    breaks = c(100, 115, 135, 185),
    labels = c("2015", "2030", "2050", "2100")) +
  # add PhyloPic icon for functional groups
  geom_phylopic(data = icon_positions_shannon,
               aes(x = x, y = y, uuid = phylopic), 
              size = c(0.06, 0.08), inherit.aes = FALSE) +  
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
    plot.margin = unit(c(0, 0.5, 0, 0.5), "cm"))# +
  #geom_vline(xintercept = c(115, 135, 185), linetype = "dotted", color = "black", size = 0.8)

ShannonOverTime

# save plot
ggsave(filename = "./output/jorinde/ShannonOverTime_02Oct2025.png", # path
       ShannonOverTime, # plot
      bg = 'white', width = 230, height = 210, units = "mm", dpi = 1200,
       #compression = "lzw"
       ) # image parameters

# AUXILARY TABLE FOR FIGURE 2
Shannon_index_DF <- Shannon_index %>%
  group_by(scenario, biome, trophic_level) %>% 
  mutate(Shannon_Index_change_pct = ((Shannon_Wiener_Index - Shannon_Wiener_Index[timestep == 100])/Shannon_Wiener_Index[timestep == 100])*100) %>% 
  dplyr::filter(timestep == 125)

#write.csv(Shannon_index_DF,
 #         file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/ShannonIndexChange.csv",
  #        row.names = FALSE)         
