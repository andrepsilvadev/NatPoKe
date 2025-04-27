#############################
# COMMUNITY METRICS FIGURES #
#############################
# Inês Silva
# 24 April 2025

##########
# Step 1 # Import data
##########

# all runs were previously compiled into one .csv file stored in the outputs folder
TNIND_yr <- fread("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/completeRunApril2025.csv")


# count number of unique species per biome and trophic level
species_count <- TNIND_yr[, .(n_species = uniqueN(species)), by = .(biome, trophic_level)]
# now build the caption
caption_list <- species_count[, {
  biome_caption <- paste0(n_species, " ", trophic_level, " species")
  list(biome_text = biome_caption)
}, by = biome]

# collapse it: one biome, then its trophic levels underneath
caption_final <- paste("Boreal Forests/ Taiga - ", caption_list[1,2], ";", caption_list[2,2], ";", caption_list[3,2], ";\n",
                       "Tropical & Subtropical Moist Broadleaf Forests - ", caption_list[4,2], ";", caption_list[5,2])


##########
# Step 2 # Define burn-in and scenario start + other cosmestic arguments
##########

t_burnin <- 100
t_policy <- 110

#img <- pick_phylopic(name = "Cervus elaphus", n = 10)

# get icons for each taxonomic group plot
uuid_carnivores <- get_uuid(name = "Panthera leo", n = 5)[[5]]
uuid_herbivores <- get_uuid(name = "Cervus elaphus", n = 1)
uuid_omnivores <- get_uuid(name = "Sus scrofa", n = 5)[[2]]

# prep labels
biome_names <- c("BorealForestsTaiga" = "Boreal Forests Taiga",
                 "TropicalSubtropicalMoistBroadleafForests" = "Tropical & Subtropical\nMoist Broadleaf Forests")

# prep custom color palette
custom_colors <- c("SSP5" = "#ffab27", "SSP1" = "#99cc00")


##########
# Step 3 # Calculate Community metrics (Shannon Diversity)
##########

# Shannon's index

Shannon_index <- TNIND_yr %>%
  group_by(scenario, biome, timestep, trophic_level) %>%
  dplyr::mutate(p_i = TNIND / sum(TNIND),
                # calculate proportion of individuals of species i
                ln_p_i = ifelse(p_i > 0, log(p_i), 0)) %>%  # in case pi is 0
  # up until here the table has values for each species, then info is summarised
  dplyr::summarize(Shannon_Wiener_Index = -sum(p_i * ln_p_i)) %>%   # calculate the Shannon-Wiener index
  dplyr::filter(timestep>=100)
invisible(gc())

# if adding more variables we need to transform from wide to long format

##########
# Step 4 # Build plot
##########

ShannonOverTime

############
# OPTION 1 #
############

# get the top-right corner coordinates for each facet
icon_positions_shannon <- Shannon_index %>%
  group_by(biome, trophic_level) %>%
  summarise(x = max(timestep) - 2,  # Add some padding to the max x
            y = 1.2 #max(Shannon_Wiener_Index)  # Add padding to the max y
            ) %>%
  ungroup() %>% 
  # add the PhyloPic UUIDs to the positions
  mutate(phylopic = case_when(
                              biome == "TropicalSubtropicalMoistBroadleafForests" ~ NA_character_,  # if Tropical biome, no icon (NA)
                              trophic_level == "Carnivore" ~ uuid_carnivores,
                              trophic_level == "Herbivore" ~ uuid_herbivores,
                              trophic_level == "Omnivore" ~ uuid_omnivores
                            ))
  
option1 <- ggplot(data = Shannon_index, aes(x = timestep, y = Shannon_Wiener_Index, color = scenario)) +
  geom_line() +
  ggh4x::facet_grid2(biome ~ trophic_level,
                     scales = "free",
                     axes = "x", switch = "y", labeller = labeller(biome = as_labeller(biome_names))) +
  labs(#title = "Shannon's Index over time",
       x = "Time",
       y = "Metric value",
       caption = caption_final) +
  scale_color_manual("Socio-economic\nscenario", values = custom_colors) +
  geom_phylopic(data = icon_positions_shannon, aes(x = x, y = y, uuid = phylopic), 
                           size = 0.2, inherit.aes = FALSE) +  # Add PhyloPic icons
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
    #plot title
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(size = ),
    # adjust legend
    legend.position = "bottom",
    # modify x-axis text
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    # remove panel borders
    panel.border = element_blank(),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(2, "lines"),
    plot.margin = unit(c(0, 0.5, 0, 0.5), "cm")) +
  geom_vline(xintercept = t_policy, linetype = "dotted", color = "black", size = 0.8)

ggsave(filename = "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/figures_20250427/Figure2.tiff", # path
       option1, # plot
       bg = 'white', width = 230, height = 210, units = "mm", dpi = 1200, compression = "lzw") # image parameters


############
# OPTION 2 #
############

# get the top-right corner coordinates for each facet
icon_positions_shannon <- Shannon_index %>%
  group_by(biome, trophic_level) %>%
  summarise(x = max(timestep) - 2,  # Add some padding to the max x
            y = 0.1 #max(Shannon_Wiener_Index)  # Add padding to the max y
  ) %>%
  ungroup() %>% 
  # add the PhyloPic UUIDs to the positions
  mutate(phylopic = case_when(
    #biome == "TropicalSubtropicalMoistBroadleafForests" ~ NA_character_,  # if Tropical biome, no icon (NA)
    trophic_level == "Carnivore" ~ uuid_carnivores,
    trophic_level == "Herbivore" ~ uuid_herbivores,
    trophic_level == "Omnivore" ~ uuid_omnivores
  ))

option2 <-  ggplot(data = Shannon_index, aes(x = timestep, y = Shannon_Wiener_Index, color = scenario)) +
  geom_line() +
  ggh4x::facet_grid2(trophic_level ~ biome, axes ="x",
             #scales = "free_x",
             switch = "y", labeller = labeller(biome = as_labeller(biome_names))) +
  labs(#title = "Shannon's Index over time",
    x = "Time",
    y = "Metric value",
    caption = caption_final) +
  scale_color_manual("Socio-economic\nscenario", values = custom_colors) +
  geom_phylopic(data = icon_positions_shannon, aes(x = x, y = y, uuid = phylopic), 
                size = 0.2, inherit.aes = FALSE) +  # Add PhyloPic icons
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
    #plot title
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(size = ),
    # adjust legend
    legend.position = "bottom",
    # modify x-axis text
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    # remove panel borders
    panel.border = element_blank(),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(2, "lines"),
    plot.margin = unit(c(0, 0.5, 0, 0.5), "cm")) +
  geom_vline(xintercept = t_policy, linetype = "dotted", color = "black", size = 0.8)

ggsave(filename = "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/figures_20250427/Figure2_option2.tiff", # path
       option2, # plot
       bg = 'white', width = 230, height = 210, units = "mm", dpi = 1200, compression = "lzw") # image parameters



