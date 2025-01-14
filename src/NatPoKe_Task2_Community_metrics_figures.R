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
library(rphylopic)

# import dummy dataset
dummy_dataset <- read_csv("data/dummy_dataset_Jan2025.csv")

# specify burn in timestep & policy start
t_burnin <- 2
t_policy <- 5

##############
# TAXA ICONS #
##############

# Get a single image uuid for a species
uuid_bird <- get_uuid(name = "Tinamus major", n = 1)
uuid_mammal <- get_uuid(name = "Vulpes vulpes", n = 1)
uuid_insect <- get_uuid(name = "Apolygus lucorum", n = 1)
# Get the image for that uuid
img_bird <- get_phylopic(uuid = uuid_bird)
img_mammal <- get_phylopic(uuid = uuid_mammal)
img_mammal <- get_phylopic(uuid = uuid_insect)


test <- data.frame("x" = c(18, 18, 18),
                   "y" = c(2.85, 2.85, 2.85),
                   "taxa" = c("Mammal", "Bird", "Insect"),
                   "phylopic" = c(uuid_mammal, uuid_bird, uuid_insect))



###############################
# CALCULATE COMMUNITY METRICS #
###############################

### Tpecies richness per cell in the landscape

community_df <- dummy_dataset %>%
  group_by(biome, scenario, time, cell_id, region, taxa) %>%
  dplyr::summarize(Sps_richness = n_distinct(species))# calculate species richness by counting the nº of species in each group
invisible(gc())

### Species Diversity (Shannon_Wiener_Index) -----------------------------------

# calculate the Shannon index
Shannon_index <- dummy_dataset %>%
  group_by(biome, scenario, time, cell_id) %>%
  dplyr::mutate(p_i = n_abundance / sum(n_abundance),
                # calculate proportion of individuals of species i
                ln_p_i = ifelse(p_i > 0, log(p_i), 0)) %>%  # in case pi is 0
  # up until here the table has values for each species, then info is summarised
  dplyr::summarize(Shannon_Wiener_Index = -sum(p_i * ln_p_i))  # calculate the Shannon-Wiener index
invisible(gc())

### Functional Diversity (Funct_diversity_Index) -------------------------------

# create trophic levels because dummy dataset did not have any 
# DELETE LATER when real data is used
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
invisible(gc())

# join all community metrics into one dataframe
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

# write this dataframe into a .csv to feed NatPoKe_Task2_Spatially_explicit_maps.R script
write.csv(community_df, "~/NatPoKe/data/community_df_Jan2025.csv")
invisible(gc())

# average community metrics per year 
community_df_year <- community_df %>%
  group_by(biome, scenario, time, taxa) %>%
  dplyr::summarise(
    mean_Sps_richness_yr = mean(Sps_richness, na.rm = TRUE),
    mean_Shannon_Index_yr = mean(Shannon_Wiener_Index, na.rm = TRUE),
    mean_Funct_Div_yr = mean(Funct_diversity_Index, na.rm = TRUE))
invisible(gc())

# change community metrics per year from wide to LONG format for plots
community_df_year_long <- community_df_year %>%
  pivot_longer(cols = c("mean_Sps_richness_yr", "mean_Shannon_Index_yr", "mean_Funct_Div_yr"),
               names_to = 'variables',
               values_to = 'values') %>%
  dplyr::filter(time >= t_burnin) # remove burn-in period
invisible(gc())

############
# FIGURE 2 # Community metrics per policy in both biomes per taxa (one figure per community metric)
############



taxas <- unique(dummy_dataset$taxa)
variables <- unique(c(community_df_year_long$variables))
biome_names <- c("Tropical forests" = "Tropical forests ", "Boreal forests" = "Boreal forests")
vars_names <- c("mean_Sps_richness_yr" = "Species \n Richness", "mean_Shannon_Index_yr" = "Shannon Wienner \nIndex", "mean_Funct_Div_yr" = "Functional \nDiversity")

unique(community_df_year_long$variables)

# create an empty list to store the plots
plot_list <- list()

# Loop through each variable
for (variable in variables) {
  
  # Filter data for the current variable
  variable_data <- community_df_year_long[community_df_year_long$variables == variable,]
  
  # Compute the top-right corner coordinates for each facet
  icon_positions <- variable_data %>%
    group_by(biome, taxa) %>%
    summarise(
      x = max(time) - 2,  # Add some padding to the max x
      y = max(values) + 0.1  # Add padding to the max y
    ) %>%
    ungroup()
  
  # Add the PhyloPic UUIDs to the positions (repeat as needed for each taxa)
  icon_positions <- icon_positions %>%
    mutate(
      phylopic = case_when(
        taxa == "Mammal" ~ uuid_mammal,
        taxa == "Bird" ~ uuid_bird,
        taxa == "Insect" ~ uuid_insect
      )
    )
  
  # Create the plot
  comm_composition_time <- ggplot(data = variable_data,
                                  aes(x = time, y = values, color = scenario)) +
    geom_line() +
    #facet_wrap(biome ~ taxa, scales = "free", 
              # labeller = labeller(biome = as_labeller(biome_names), taxa = as_labeller(taxas))) +
    facet_grid(biome ~ taxa, scales = "free",
                           labeller = labeller(
                             biome = as_labeller(biome_names),
                             taxa = as_labeller(taxas)),
               switch = "y") +
    xlab("Time") +
    ylab("Metric value") +
    #scale_color_discrete("Economic policy \nscenario") +
    coord_cartesian(clip = "off") +  # Allow plotting outside the panel
    geom_phylopic(data = icon_positions, aes(x = x, y = y, image = phylopic), 
                  size = 0.2, inherit.aes = FALSE) +  # Add PhyloPic icons
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
      panel.border = element_blank(),
      panel.spacing.x = unit(1, "lines"),
      panel.spacing.y = unit(2, "lines"),
      plot.margin = unit(c(0, 0.5, 0, 0.5), "cm")) +
    geom_vline(xintercept = t_policy, linetype = "dotted", color = "black", size = 0.8)
    
  # Save each plot in the list
  plot_list[[variable]] <- comm_composition_time
  
  # Optional: Save each scenario map as a separate image
  # ggsave(paste0("~/NatPoKe/output/dummy_figures/","Figure2_CommunityCompositionOverTime", variable, ".tiff"), 
  #        comm_composition_time, bg = 'white', width = 230, height = 210, units = "mm", dpi = 1200, compression = "lzw")
}

# see plot
#plot_list$mean_Funct_Div_yr 

#########################
# SUPPLEMENTARY FIGURES # 
#########################

# THESE FIGURES WERE FROM A TALK BEFORE JAN 14TH 2025 
# ON THIS DAY WE DECIDED WE DO NOT LIKE THIS FIGURES ANYMORE
# TEHSE WERE NOT THE COMPARISONS WE WANTED
# 
# 
# # create an empty list to store the plots
# plot_list2 <- list()
# 
# 
# # Loop through each scenario
# for (taxa in taxas) {
#   
#   # Filter data for the current taxa
#   taxa_data <- community_df_year_long[community_df_year_long$taxa == taxa,]
#   
#   comm_composition_time <- ggplot(data = taxa_data,
#                                   aes(x = time, y = values, color = scenario)) +
#     geom_line() +
#     # facet_grid(biome ~ variables, scales = "free",
#     #            labeller = labeller(
#     #              biome = as_labeller(biome_names),
#     #              variables = as_labeller(vars_names)
#     #            )) +
#     facet_wrap(biome~variables, scales = "free", labeller = labeller(biome = as_labeller(biome_names), variables = as_labeller(vars_names))) +
#     xlab("Time") +
#     ylab("Metric value") +
#     scale_color_discrete("Economic policy \nscenario") +
#     theme_minimal() +
#     theme(
#       # remove gridlines 
#       panel.grid = element_blank(),
#       # add subtle horizontal lines 
#       panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
#       # modify facet labels
#       strip.text = element_text(face = "bold", size = rel(1)),
#       strip.placement = "outside",
#       # adjust legend
#       legend.position = "bottom",
#       # ADD LEGEND FOR POLICY BEGGINING
#       # modify x-axis text
#       axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
#       # remove panel borders
#       panel.border = element_blank()) +
#     geom_vline(xintercept = t_policy, linetype = "dotted", color = "black", size = 0.8) 
#   
#   # save each plot in the list
#   plot_list2[[taxa]] <- comm_composition_time
#   
#   # save each scenario map as a separate image
#   #ggsave(paste0("~/NatPoKe/output/dummy_figures/","Figure2_CommunityCompositionOverTime", taxa, ".tiff"), comm_composition_time, bg = 'white', width = 230, height = 210, units = "mm", dpi = 1200, compression = "lzw")
# }
# 
# # check figures
# plot_list2$Mammal