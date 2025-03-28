## NatPoKe Figure 1 #
######## MIS ########
##### 13 JAN 25 #####

###############
# IMPORT DATA #
###############

# Boreal Forests ---------------------------------------------------------------

## Europe
TNIND_europe <- fread("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/26Mar2025_Europe/Outputs/TNIND_yr_26Mar2025_Europe.csv") %>% 
  mutate(scenario = "BAU",
         taxa = "Mammals") 
colnames(TNIND_europe) <- c("TNIND", "biome", "region", "species", "timestep", "scenario", "taxa")

## North America
TNIND_northamerica <- fread("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/27Mar2025_NorthAmerica/Outputs/TNIND_yr_27Mar2025_NorthAmerica.csv") %>% 
  mutate(scenario = "BAU",
         taxa = "Mammals",
         biome = case_when(biome == "oreal Forests Taiga" ~ "Boreal Forests Taiga")) 

# Tropical Moist Forests -------------------------------------------------------

## Asia
TNIND_asia <- fread("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/27Mar2025_Asia/Outputs/TNIND_yr_27Mar2025_Asia.csv") %>% 
  mutate(scenario = "BAU",
         taxa = "Mammals")

## Africa
TNIND_africa <- fread("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/27Mar2025_Africa/Outputs/TNIND_yr_27Mar2025_Africa.csv") %>% 
  mutate(scenario = "BAU",
         taxa = "Mammals")

## South America
TNIND_southamerica <- fread("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/27Mar2025_SouthAmerica/Outputs/TNIND_yr_27Mar2025_SouthAmerica.csv") %>% 
  mutate(scenario = "BAU",
         taxa = "Mammals") 

##################
# COMBINING DATA #
##################

datasets <- list(TNIND_europe, TNIND_northamerica, TNIND_asia, TNIND_africa, TNIND_southamerica)

TNIND_yr <- do.call("rbind", datasets)


mammalTraits <- read_csv(here("data", "mammalTraits_2025-03-17.csv")) %>% 
  dplyr::filter(BIOME_NAME %in% c("Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga")) %>% 
  mutate(Trophic = case_when(
    # based on Schloss 2012
    Diet.Meat >= 90 ~ "Carnivore",
    Diet.Plant >= 90 ~ "Herbivore",
    TRUE ~ NA_character_),
    trophic_level = case_when(
      # from original database
      trophic_level == 1 ~ "Herbivore",
      trophic_level == 2 ~ "Omnivore",
      trophic_level == 3 ~ "Carnivore",
      TRUE ~ as.character(trophic_level)),
    sci_name = stringr::str_replace_all(sci_name, " ", "")) %>% 
  distinct()

TNIND_yr <- TNIND_yr %>%
  left_join(mammalTraits, by = c("species" = "sci_name")) %>%
  dplyr::select(TNIND, biome, region, species, timestep, scenario, taxa, trophic_level) %>% 
  dplyr::group_by(species, biome, region, timestep, scenario, taxa, trophic_level) %>% # Group by the original unique identifiers
  dplyr::slice(1) %>% # Take only the first row within each group
  dplyr::ungroup()
  

t_burnin <- 100
t_policy <- 110

##############
# TAXA ICONS #
##############

# Get a single image uuid for a species
uuid_bird <- get_uuid(name = "Tinamus major", n = 1)
uuid_mammal <- get_uuid(name = "Vulpes vulpes", n = 1)
uuid_insect <- get_uuid(name = "Apolygus lucorum", n = 1)

# Get the image for that uuid
# img_bird <- get_phylopic(uuid = uuid_bird)
# img_mammal <- get_phylopic(uuid = uuid_mammal)
# img_mammal <- get_phylopic(uuid = uuid_insect)

###############################
# CALCULATE COMMUNITY METRICS #
###############################

### Tpecies richness per cell in the landscape

community_df <- TNIND_yr %>%
  group_by(biome, scenario, timestep, region, taxa) %>%
  dplyr::summarize(Sps_richness = n_distinct(species))# calculate species richness by counting the nº of species in each group
invisible(gc())

### Species Diversity (Shannon_Wiener_Index) -----------------------------------

# calculate the Shannon index
Shannon_index <- TNIND_yr %>%
  dplyr::filter(TNIND != 0) %>% # keep only cells where species exist 
  group_by(scenario, biome, timestep) %>%
  dplyr::mutate(p_i = TNIND / sum(TNIND),
                # calculate proportion of individuals of species i
                ln_p_i = ifelse(p_i > 0, log(p_i), 0)) %>%  # in case pi is 0
  # up until here the table has values for each species, then info is summarised
  dplyr::summarize(Shannon_Wiener_Index = -sum(p_i * ln_p_i))  # calculate the Shannon-Wiener index
invisible(gc())


### Functional Diversity (Funct_diversity_Index) -------------------------------

Funct_diversity <- TNIND_yr %>%
  dplyr::filter(TNIND != 0) %>% # keep only cells where species exist
  group_by(biome, scenario, timestep, trophic_level) %>%
  dplyr::summarise(Fmean_TNIND = mean(TNIND, na.rm = TRUE)) %>%
  group_by(biome, scenario, timestep) %>%
  dplyr::mutate(Fp_i = Fmean_TNIND / sum(Fmean_TNIND),
                # calculate proportion of individuals of fucntional group i
                Fln_p_i = ifelse(Fp_i > 0, log(Fp_i), 0)) %>%  # in case Fpi is 0
  # up until here the table has values for each functional group, then info is summarised
  dplyr::summarize(Funct_diversity_Index = -sum(Fp_i * Fln_p_i))  # calculate the functional diversity index
invisible(gc())

# join all community metrics into one dataframe
community_df <- community_df %>%
  group_by(biome, scenario, timestep, region) %>%
  left_join(Shannon_index, select(Shannon_Wiener_Index, biome, scenario, timestep, region),
            by = c("biome", "scenario", "timestep")) %>%
  left_join(Funct_diversity, select(Funct_diversity_Index,
                   biome,
                   scenario,
                   timestep, region),
            by = c("biome", "scenario", "timestep"))
invisible(gc())

# average community metrics per year -------------------------------------------
community_df_year <- community_df %>%
  group_by(biome, scenario, timestep, taxa) %>%
  dplyr::summarise(
    mean_Sps_richness_yr = mean(Sps_richness, na.rm = TRUE),
    mean_Shannon_Index_yr = mean(Shannon_Wiener_Index, na.rm = TRUE),
    mean_Funct_Div_yr = mean(Funct_diversity_Index, na.rm = TRUE))
invisible(gc())

# write this dataframe into a .csv to feed NatPoKe_Task2_Spatially_explicit_maps.R script
write.csv(community_df_year, "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/24Feb2025/community_df_peryear_24Feb2025.csv")

# change community metrics per year from wide to LONG format for plots
community_df_year_long <- community_df_year %>%
  pivot_longer(cols = c("mean_Sps_richness_yr", "mean_Shannon_Index_yr", "mean_Funct_Div_yr"),
               names_to = 'variables',
               values_to = 'values') %>%
  dplyr::filter(timestep >= t_burnin) # remove burn-in period
invisible(gc())

############
# FIGURE 2 # Community metrics per policy in both biomes per taxa (one figure per community metric)
############

taxas <- unique(TNIND_yr$biome)
variables <- unique(c(community_df_year_long$variables))
biome_names <- c("Boreal Forests Taiga" = "Boreal Forests Taiga", "Tropical Subtropical Moist Broadleaf Forests" = "Tropical & Subtropical Moist Broadleaf Forests")
vars_names <- c("mean_Sps_richness_yr" = "Species \n Richness", "mean_Shannon_Index_yr" = "Shannon Wienner \nIndex", "mean_Funct_Div_yr" = "Functional \nDiversity")

# split full dataframe per variable
variable_data <- split(community_df_year_long, community_df_year_long$variables)


# Shannon's index --------------------------------------------------------------

# get the top-right corner coordinates for each facet
icon_positions_shannon <- variable_data$mean_Shannon_Index_yr %>%
  group_by(biome, taxa) %>%
  summarise(
    x = max(timestep) - 2,  # Add some padding to the max x
    y = max(values)  # Add padding to the max y
  ) %>%
  ungroup()

# add the PhyloPic UUIDs to the positions
icon_positions_shannon <- icon_positions_shannon %>%
  mutate(
    phylopic = case_when(
      taxa == "Mammals" ~ uuid_mammal,
      taxa == "Bird" ~ uuid_bird,
      taxa == "Insect" ~ uuid_insect
    )
  )

# build the plot
shannon_over_time <- ggplot(data = variable_data$mean_Shannon_Index_yr,
                                aes(x = timestep, y = values, color = scenario)) +
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
  #geom_phylopic(data = icon_positions_shannon, aes(x = x, y = y, uuid = phylopic), 
   #            size = 0.0008, inherit.aes = FALSE) +  # Add PhyloPic icons
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

# save shannon_over_time plot
ggsave(filename = "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/24Feb2025/Figure2_ShannonWienerOverTime.tiff", # path
       shannon_over_time, # plot
       bg = 'white', width = 230, height = 210, units = "mm", dpi = 1200, compression = "lzw") # image parameters



# Functional diversity  --------------------------------------------------------

# get the top-right corner coordinates for each facet
icon_positions_functional <- variable_data$mean_Funct_Div_yr %>%
  group_by(biome, taxa) %>%
  summarise(
    x = max(timestep) - 2,  # Add some padding to the max x
    y = max(values)  # Add padding to the max y
  ) %>%
  ungroup()

# add the PhyloPic UUIDs to the positions
icon_positions_functional <- icon_positions_functional %>%
  mutate(
    phylopic = case_when(
      taxa == "Mammals" ~ uuid_mammal,
      taxa == "Bird" ~ uuid_bird,
      taxa == "Insect" ~ uuid_insect
    )
  )

# build the plot
functdiv_over_time <- ggplot(data = variable_data$mean_Funct_Div_yr,
                            aes(x = timestep, y = values, color = scenario)) +
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
  geom_phylopic(data = icon_positions_functional, aes(x = x, y = y, uuid = phylopic), 
               size = 0.0005, inherit.aes = FALSE) +  # Add PhyloPic icons
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

# save functdiv_over_time plot
ggsave(filename = "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/24Feb2025/Figure2_FunctionalDiversityOverTime.tiff", # path
      functdiv_over_time, # plot
      bg = 'white', width = 230, height = 210, units = "mm", dpi = 1200, compression = "lzw") # image parameters


# Species richness  ------------------------------------------------------------

# get the top-right corner coordinates for each facet
icon_positions_richness <- variable_data$mean_Sps_richness_yr %>%
  group_by(biome, taxa) %>%
  summarise(
    x = max(timestep) - 2,  # Add some padding to the max x
    y = max(values) +0.05 # Add padding to the max y
  ) %>%
  ungroup()

# add the PhyloPic UUIDs to the positions
icon_positions_richness <- icon_positions_richness %>%
  mutate(
    phylopic = case_when(
      taxa == "Mammals" ~ uuid_mammal,
      taxa == "Bird" ~ uuid_bird,
      taxa == "Insect" ~ uuid_insect
    )
  )

# build the plot
richness_over_time <- ggplot(data = variable_data$mean_Sps_richness_yr,
                             aes(x = timestep, y = values, color = scenario)) +
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
  geom_phylopic(data = icon_positions_richness, aes(x = x, y = y, uuid = phylopic), 
               size = 0.008, inherit.aes = FALSE) +  # Add PhyloPic icons
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

# save functdiv_over_time plot
ggsave(filename = "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/24Feb2025/Figure2_SpeciesRichnessOverTime.tiff", # path
        richness_over_time, # plot
        bg = 'white', width = 230, height = 210, units = "mm", dpi = 1200, compression = "lzw") # image parameters
 


# IF WE WANT A MORE AUTOMATED WAY THAT MIGHT NOT WORK WITH DIFFERENT SCALES 
# UNCOMMENT NEXT LINES


# 
# # create an empty list to store the plots
# plot_list <- list()
# 
# # Loop through each variable
# for (variable in variables) {
#   
#   # Filter data for the current variable
#   variable_data <- community_df_year_long[community_df_year_long$variables == variable,]
#   
#   # Compute the top-right corner coordinates for each facet
#   icon_positions <- variable_data %>%
#     group_by(biome, taxa) %>%
#     summarise(
#       x = max(time) - 2,  # Add some padding to the max x
#       y = max(values) + 0.1  # Add padding to the max y
#     ) %>%
#     ungroup()
#   
#   # Add the PhyloPic UUIDs to the positions (repeat as needed for each taxa)
#   icon_positions <- icon_positions %>%
#     mutate(
#       phylopic = case_when(
#         taxa == "Mammal" ~ uuid_mammal,
#         taxa == "Bird" ~ uuid_bird,
#         taxa == "Insect" ~ uuid_insect
#       )
#     )
#   
#   # Create the plot
#   comm_composition_time <- ggplot(data = variable_data,
#                                   aes(x = time, y = values, color = scenario)) +
#     geom_line() +
#     #facet_wrap(biome ~ taxa, scales = "free", 
#               # labeller = labeller(biome = as_labeller(biome_names), taxa = as_labeller(taxas))) +
#     facet_grid(biome ~ taxa, scales = "free",
#                            labeller = labeller(
#                              biome = as_labeller(biome_names),
#                              taxa = as_labeller(taxas)),
#                switch = "y") +
#     xlab("Time") +
#     ylab("Metric value") +
#     #scale_color_discrete("Economic policy \nscenario") +
#     coord_cartesian(clip = "off") +  # Allow plotting outside the panel
#     geom_phylopic(data = icon_positions, aes(x = x, y = y, uuid = phylopic), 
#                   size = 0.02, inherit.aes = FALSE) +  # Add PhyloPic icons
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
#       # modify x-axis text
#       axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
#       # remove panel borders
#       panel.border = element_blank(),
#       panel.spacing.x = unit(1, "lines"),
#       panel.spacing.y = unit(2, "lines"),
#       plot.margin = unit(c(0, 0.5, 0, 0.5), "cm")) +
#     geom_vline(xintercept = t_policy, linetype = "dotted", color = "black", size = 0.8)
#     
#   # Save each plot in the list
#   plot_list[[variable]] <- comm_composition_time
#   
#   # save each plot as a separate image
#   ggsave(paste0("~/NatPoKe/output/dummy_figures/","Figure2_CommunityCompositionOverTime", variable, ".tiff"), comm_composition_time, bg = 'white', width = 230, height = 210, units = "mm", dpi = 1200, compression = "lzw")
# }
# 
# # see plot
# plot_list$mean_Funct_Div_yr 






########################
# !!!! DISCLAIMER !!!! # -------------------------------------------------------
########################

# THESE FIGURES WERE FROM A TALK BEFORE JAN 14TH 2025 
# ON THIS DAY WE DECIDED WE DO NOT LIKE THIS FIGURES ANYMORE
# THESE WERE NOT THE COMPARISONS WE WANTED

 
 
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