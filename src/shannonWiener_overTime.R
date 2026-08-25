## Name: shannonWiener_overTime.R ##
## Author: Inês Silva ##
## Date: 24 April 2025 updated on 06 May 2025 re-updated on 27th April 2026
## Description: Calculate the Shannon Wiener Index per scenario, biome, region
## and trophic level for each time and build line plot over time (main manuscript figure).
## Supplementary Table with change values (%) between 2100 and 2015 also exists


source("./src/libraries.R")
source("./src/customFunctions2.R")

##########
# Step 1 # Import data
##########

# all runs were previously compiled into one .csv file stored in the outputs folder
TNIND_yr <- read.csv("E:/metaRange_May26/completeMetaRangeRun_20260517.csv")


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

# scenario labels
scenario_names <- c("ssp126" = "SSP1-2.6",
                 "ssp585" = "SSP5-8.5")

# region labels
region_names <- c("Asia" = "Asia",
                    "Africa" = "Africa",
                    "Europe+Asia" = "Europe &\nAsia",
                    "North America" = "North\nAmerica",
                    "South America" = "South\nAmerica")

# prep custom color palette
#trophic_cols <- c("Herbivore" = "#99cc00",
#                  "Carnivore" = "#ffab27",
#                  "Omnivore"  = "#377eb8")
trophic_cols <- c("Herbivore" = "#6A8F52", "Carnivore" = "#F2A65A", "Omnivore"  = "#5B8FA8")


##########
# Step 3 # Calculate Community metric (Shannon Diversity)
##########

#head(TNIND_yr)

Shannon_index <- TNIND_yr %>%
  
  # average abundance values (TNIND) per species
  group_by(future_scenario, biome, region, timestep, trophic_level, rep_num, species) %>%
  summarise(abundance = mean(TNIND, na.rm = TRUE), .groups = "drop") %>%
  
  # collapse replicates per species
  group_by(biome, future_scenario, region, timestep, trophic_level, species) %>%
  summarise(abundance = mean(abundance, na.rm = TRUE), .groups = "drop") %>%
  
  # group by trophic level 
  group_by(future_scenario, biome, region, timestep, trophic_level) %>%
  
  # calculate Shannon-Wiener index
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

### Shannon-Wiener Index per Biome ---------------------------------------------

icon_positions_shannon <- Shannon_index %>%
  group_by(future_scenario, biome, trophic_level, timestep) %>%
  summarise(Shannon_Wiener_Index = mean(Shannon_Wiener_Index, na.rm = TRUE)) %>% 
  arrange(timestep) %>%
  slice_tail(n = 5) %>%   # last 5 timesteps
  slice_head(n = 1) %>%   # max - 4
  transmute(
    x = timestep,
    y = Shannon_Wiener_Index + 0.02) %>%
  ungroup() %>% 
  left_join(species_count  %>%
              group_by(biome, trophic_level) %>% 
              summarise(n_species = sum(n_species)), by = c("biome", "trophic_level"))

ShannonOverTime_perbiome <- ggplot(data = Shannon_index,
       aes(x = timestep, y = Shannon_Wiener_Index, colour = trophic_level, fill = trophic_level)) +
  # ribbon (mean ± SE)
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.25, colour = NA, show.legend = FALSE) +
  # mean line
  stat_summary(fun = mean, geom = "line", linewidth = 0.6) +
  # to deal with axis more freely (add axis on top row)
  ggh4x::facet_grid2(biome ~ future_scenario,
                     scales = "free", axes = "x",
                     switch = "y", # switch labels from right to left side
                     labeller = labeller(biome = as_labeller(biome_names),
                                         future_scenario = as_labeller(scenario_names))) +
  labs(x = "Time", y = "Shannon-Wiener index", caption = "Dotted line = Start of future scenarios") +
  scale_color_manual("Trophic levels", values = trophic_cols) +
  scale_fill_manual(values = trophic_cols) +
  scale_x_continuous(
    breaks = c(25, 40, 60, 110),
    labels = c("2015", "2030", "2050", "2100")) +
  geom_vline(xintercept = c(40, 60, 110), linetype = "dashed", color = "gray50", size = 0.5)+
  # add label with number of species
  geom_text(data = icon_positions_shannon,
            aes(x = x, y = y,
                label = paste0("n = ", n_species),
                color = trophic_level),
            inherit.aes = FALSE, size = 2) +
  theme_minimal(base_size = 12) +
  theme(
    # background  -----
    panel.border = element_blank(),
    
    # grids -----
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dashed", size = 0.2),
    
    # axes -----
    axis.line = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.text.x = element_text(vjust = 1, hjust = 1),
    
    # facets -----
    strip.background.x = element_rect(fill = "grey90", colour = NA),
    strip.background.y = element_blank(),
    strip.text = element_text(face = "bold", size = rel(1)),
    strip.placement = "outside",
    
    # legend -----
    legend.position = "bottom",
    legend.justification = "right",
    
    # spacing ------
    plot.margin = unit(c(0, 0.5, 0, 0.5), "cm"),
    panel.spacing.x = unit(1, "lines"),
    #panel.spacing.y = unit(2, "lines")
    ) 

# save plot
ggsave(filename = "E:/metaRange_May26/FigureAndMetrics/Figure2_ShannonWienerTroughTime_perBIOME.png", # path
       ShannonOverTime_perbiome, # plot
       bg = 'transparent', width = 200, height = 160, units = "mm", dpi = 1200,
       #compression = "lzw"
) # image parameters


### Shannon-Wiener Index per Region --------------------------------------------

icon_positions_shannon2 <- Shannon_index %>%
  group_by(future_scenario, biome, region, trophic_level, timestep) %>%
  summarise(Shannon_Wiener_Index = mean(Shannon_Wiener_Index, na.rm = TRUE)) %>% 
  arrange(timestep) %>%
  slice_tail(n = 5) %>%   # last 5 timesteps
  slice_head(n = 1) %>%   # max - 4
  transmute(
    x = timestep,
    y = Shannon_Wiener_Index + 0.02) %>%
  ungroup() %>% 
  left_join(species_count  %>%
              group_by(biome, region, trophic_level) %>% 
              summarise(n_species = sum(n_species)), by = c("biome", "region", "trophic_level"))

# order region more logically
region_levels <- c("North America", "Europe+Asia", "South America", "Africa", "Asia")

Shannon_index$region <- factor(Shannon_index$region, levels = region_levels)
icon_positions_shannon2$region <- factor(icon_positions_shannon2$region, levels = region_levels)

Shannon_perRegion <- ggplot(data = Shannon_index,
       aes(x = timestep, y = Shannon_Wiener_Index, colour = trophic_level, fill = trophic_level)) +
  # ribbon (mean ± SE)
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.25, colour = NA, show.legend = FALSE) +
  # mean line
  stat_summary(fun = mean, geom = "line", linewidth = 0.6) +
  # to deal with axis more freely (add axis on top row)
  ggh4x::facet_grid2(region ~ future_scenario,
                     scales = "free_x", #axes = "x",
                     axes = "all",
                     #remove_labels = "y",
                     switch = "y",
                     labeller = labeller(biome = as_labeller(biome_names),
                                         future_scenario = as_labeller(scenario_names),
                                         region = as_labeller(region_names))) +
  labs(x = "Time", y = "Shannon-Wiener index", caption = "Dotted line = Start of future scenarios") +
  scale_color_manual("Trophic levels", values = trophic_cols) +
  scale_fill_manual(values = trophic_cols) +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = c(25, 40, 60, 110),
    labels = c("2015", "2030", "2050", "2100")) +
  # line limiting intro of new scenario
  geom_vline(xintercept = c(40, 60, 110), linetype = "dashed", color = "gray50", size = 0.5) +
  # add label with number of species
  geom_text(data = icon_positions_shannon2,
            aes(x = x, y = y + 0.02,
                label = paste0("n=", n_species),
                colour = trophic_level),
            inherit.aes = FALSE, size = 4) +
  theme_minimal(base_size = 10) +
  theme(
    # background  -----
    panel.border = element_blank(),
    
    # grids -----
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dashed", size = 0.2),
    
    # axes -----
    axis.line = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    #axis.text.x = element_text(vjust = 1, hjust = 1),
    
    # facets -----
    strip.background.x = element_rect(fill = "grey90", colour = NA),
    strip.background.y = element_blank(),
    strip.text = element_text(face = "bold", size = rel(1)),
    strip.placement = "outside",
    
    # legend -----
    legend.position = "bottom",
    legend.justification = "right",
    
    # spacing ------
    plot.margin = unit(c(0, 0.5, 0, 0.5), "cm"),
    panel.spacing.x = unit(1, "lines"),
    #panel.spacing.y = unit(2, "lines")
    )


# save plot
ggsave(filename = "E:/metaRange_May26/WBF_FIGURES/Figure2_ShannonWienerTroughTime_perREGION.png", # path
       Shannon_perRegion, # plot
       bg = 'transparent', width = 180, height = 240, units = "mm", dpi = 1200,
       #compression = "lzw"
) # image parameters


##########
# Step 5 # Build Supplementary Table with change values (2100-2015)
##########

ShannonIndex_DF_perBiome <- Shannon_index %>%
  group_by(future_scenario, biome, trophic_level) %>% 
  # calculate % of change in the Index
  mutate(Shannon_Index_change_pct = ((Shannon_Wiener_Index - Shannon_Wiener_Index[timestep == 25])/Shannon_Wiener_Index[timestep == 25])*100) 

write.csv(ShannonIndex_DF_perBiome,
          file = "D:/metaRange_April26/FigureAndMetrics/ShannonIndexChange_perBiome.csv",
          row.names = FALSE)         


ShannonIndex_DF_perRegion <- Shannon_index %>%
  dplyr::filter(timestep %in% c(25, 40, 60, 110)) %>%
  group_by(future_scenario, biome, region, trophic_level) %>%
  arrange(timestep) %>%
  mutate(baseline_Shannon = Shannon_Wiener_Index[timestep == 25],
                Shannon_Index_change = round((Shannon_Wiener_Index - baseline_Shannon) * 100,3)) %>%
  ungroup() %>%
  dplyr::select(future_scenario, biome, region, trophic_level,
                timestep, #Shannon_Wiener_Index, 
                Shannon_Index_change) %>%
  tidyr::pivot_wider(names_from = timestep, values_from = c(#Shannon_Wiener_Index,
    Shannon_Index_change),
    names_glue = "{.value}_{timestep}") %>% 
  dplyr::select(-Shannon_Index_change_25) %>% 
  rename("Scenario" = "future_scenario", 
         "Biome" = "biome",
         "Region" = "region",
         "Trophic Level" = "trophic_level",
         "Change 2030-2015" = "Shannon_Index_change_40",
         "Change 2050-2015" = "Shannon_Index_change_60",
         "Change 2100-2015" = "Shannon_Index_change_110")

#write.csv(ShannonIndex_DF_perRegion,
 #          file = "E:/metaRange_May26/FigureAndMetrics/ShannonIndexChange_perRegion.csv",
  #         row.names = FALSE)         

# save as a formatted word document
gt::gtsave(gt(ShannonIndex_DF_perRegion), file.path("E:/metaRange_May26/FigureAndMetrics",
                                                 paste0("ShannonIndexChange_perRegion", Sys.Date(), ".docx")))



# ################################################################################
# ######################## Shannon, Richness & Evenness ##########################
# ################################################################################
# 
# Diversity_components <- TNIND_yr %>%
#   
#   # average abundance values (TNIND) per species
#   group_by(future_scenario, biome, region, timestep, trophic_level, rep_num, species) %>%
#   summarise(abundance = mean(TNIND, na.rm = TRUE), .groups = "drop") %>%
#   
#   # collapse replicates per species
#   group_by(biome, future_scenario, region, timestep, trophic_level, species) %>%
#   summarise(abundance = mean(abundance, na.rm = TRUE), .groups = "drop") %>%
#   
#   # group by trophic level 
#   group_by(future_scenario, biome, region, timestep, trophic_level) %>%
#   
#   summarise(
#     # richness over Time
#     richness = sum(abundance > 0),
#     # Shannon-Wiener INdex over Time
#             Shannon = {
#               p <- abundance / sum(abundance)
#               -sum(p[p > 0] * log(p[p > 0]))
#               },
#     # Evenness over time
#     evenness = Shannon / log(richness),
#     
#     # total abundance across trophic level
#     total_abundance = sum(abundance),
#     
#     .groups = "drop"
#   ) %>%
#   filter(timestep >= t_burnin)
# 
# # order region more logically
# region_levels <- c("North America", "Europe+Asia", "South America", "Africa", "Asia")
# 
# Diversity_components$region <- factor(Diversity_components$region, levels = region_levels)
# #icon_positions_shannon2$region <- factor(icon_positions_shannon2$region, levels = region_levels)
# 
# 
# invisible(gc())
# 
# make_div_plot <- function(data, metric, ylab = NULL){
#   
#   ggplot(data,
#     aes(x = timestep, y = .data[[metric]], colour = trophic_level, fill = trophic_level)) +
#     # uncertainty ribbon
#     stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.25, colour = NA, show.legend = FALSE) +
#     # mean trajectory
#     stat_summary(fun = mean, geom = "line", linewidth = 0.6) +
#     # facets
#     ggh4x::facet_grid2(region ~ future_scenario,
#                        scales = "free_x", switch = "y",
#                        axes = "all",
#                        labeller = labeller(future_scenario = scenario_names)) +
#     # colors
#     scale_color_manual(name = "Trophic levels", values = trophic_cols) +
#     scale_fill_manual(values = trophic_cols) +
#     # time axis
#     scale_x_continuous(
#       expand =c(0,0),
#       breaks = c(25,40,60,110), 
#       labels = c("2015","2030","2050","2100")) +
#     # policy reference lines
#     geom_vline(xintercept = c(40,60,110),
#                linetype = "dashed", colour = "gray50", linewidth = 0.4) +
#     labs(x = NULL, y = ylab) +
#     theme_minimal(base_size = 10) +
#     theme(panel.grid = element_blank(), 
#           panel.grid.major.y = element_line(color="gray90", linetype="dashed"),
#           axis.line.x = element_line(color="black"),
#           axis.line.y = element_line(color="black"),
#           strip.text = element_text(face="bold"),
#           strip.placement = "outside",
#           legend.position = "bottom",
#           #axis.text.x = element_text(angle=45, hjust=1),
#           panel.spacing.x = unit(1,"lines"),
#           panel.spacing.y = unit(1.5,"lines")
#     )
# }
# 
# 
# 
# p_shannon <- make_div_plot(
#   Diversity_components,
#   "Shannon",
#   "Shannon diversity"
# )
# 
# p_richness <- make_div_plot(
#   Diversity_components,
#   "richness",
#   "Species richness"
# )
# 
# p_evenness <- make_div_plot(
#   Diversity_components,
#   "evenness",
#   "Evenness"
# )
# 
# final_fig <- p_shannon / (p_richness | p_evenness) +
#   plot_layout(heights = c(2.2, 1), guides = "collect") &
#   theme(legend.position = "bottom")
# 
# 
# # save plot
# ggsave(filename = "D:/metaRange_April26/FigureAndMetrics/Evenness_throughtime.png", # path
#        p_evenness, # plot
#        bg = 'white', width = 180, height = 240, units = "mm", dpi = 1200,
#        #compression = "lzw"
# ) # image parameters
# 
# ###############################
# ## EVENNESS CHANGE OVER TIME ##
# ###############################
# 
# years_keep <- c(25, 40, 60, 110, 136)
# 
# Evenness_change <- Diversity_components %>% 
#   filter(timestep %in% years_keep) %>% 
#   pivot_longer(cols = c(richness, Shannon, evenness, total_abundance), 
#                names_to = "metric", values_to = "value")%>% 
#   pivot_wider(names_from = timestep, values_from = value, names_prefix = "year_") %>%
#   dplyr::filter(metric == "evenness") %>% 
#   group_by(region, future_scenario) %>%
#   mutate(pct_change_region = 100 * (year_136 - year_25) / year_25) %>%
#   ungroup()
# 
# write.csv(Evenness_change,
#           file = "D:/metaRange_April26/FigureAndMetrics/EvennessChange_perRegion.csv",
#           row.names = FALSE)         
# 
# 
# ### DECOMPOSITOIN OF THE INDEX ### --------------------------------------------
# 
# df_decomp <- Shannon_index %>%
#   group_by(future_scenario, biome, region, timestep) %>%
#   mutate(p_i = Shannon_Wiener_Index / sum(Shannon_Wiener_Index),
#     shannon_component = -p_i * log(p_i)) %>%
#   ungroup()
# 
# 
# prop_trophicLevel <- ggplot(df_decomp, aes(x = timestep,
#                       y = p_i,
#                       fill = trophic_level)) +
#   geom_area() +
#   scale_fill_manual(values = trophic_cols) +
#   #coord_flip() +
#   ggh4x::facet_grid2(region ~ future_scenario, 
#                      switch = "y",
#                      labeller = labeller(future_scenario = as_labeller(scenario_names),
#                                          region = as_labeller(region_names))) +
#   scale_y_continuous(labels = scales::percent_format()) +
#   labs(x = "Timestep",
#        y = "Relative trophic composition",
#        fill = " ") +
#   theme_bw() + 
#   theme(legend.position = "bottom",
#         # facets -----
#         strip.background.x = element_rect(fill = "grey90", colour = NA),
#         strip.background.y = element_blank(),
#         strip.text = element_text(face = "bold", size = rel(1)),
#         strip.placement = "outside")
# 
# # save plot
# ggsave(filename = "D:/metaRange_April26/FigureAndMetrics/ProportionOfTrophicLevelsThroughTime.png", # path
#        prop_trophicLevel, # plot
#        bg = 'white', width = 180, height = 240, units = "mm", dpi = 1200,
#        #compression = "lzw"
# ) # image parameters
# 
# ###################################
# ## MEAN SPECIES ABUNDANCE CHANGE ##
# ###################################
# 
# aa <- TNIND_yr %>%
#   # average abundance values (TNIND) per species
#   group_by(future_scenario, biome, region, timestep, trophic_level, rep_num, species) %>%
#   summarise(abundance = mean(TNIND, na.rm = TRUE), .groups = "drop") %>%
#   # collapse replicates per species
#   group_by(biome, future_scenario, region, timestep, trophic_level, species) %>%
#   summarise(abundance = mean(abundance, na.rm = TRUE), .groups = "drop") %>% 
#   # filter for necessary timesteps
#   dplyr::filter(timestep %in% c(26,136)) %>% 
#   group_by(biome, future_scenario, region, trophic_level, species) %>% 
#   # calculate teh abundance change between the last adn current timestep
#   mutate(abundance_change = abundance - abundance[timestep == 26]) %>% 
#   # keep only results for th elast timestep
#   dplyr::filter(timestep == 136) %>% 
#   # average mean abundance change across species
#   group_by(biome, future_scenario, region, trophic_level) %>% 
#   summarise(mean_spsAbundanceChange = mean(abundance_change, na.rm = TRUE))
# 
# 
# 
# attempt2 <- Shannon_perRegion / (prop_trophicLevel + p_evenness)+
#   plot_layout(heights = c(2.2, 1), guides = "collect") &
#   theme(legend.position = "bottom")
# 
# # save plot
# ggsave(filename = "D:/metaRange_April26/FigureAndMetrics/TooBigAlterntaive2_DecompositionOfTheShannonWiener.png", # path
#        attempt2, # plot
#        bg = 'white', width = 180, height = 240, units = "mm", dpi = 1200,
#        #compression = "lzw"
# ) # image parameters
# 
# 
# ############
# ### LIXO ###
# ############
# # get the top-right corner coordinates for each *TOP* facet only
# icon_positions_shannon <- Shannon_index %>%
#   group_by(biome, trophic_level) %>%
#   summarise(x = max(timestep) - 2, # xx coordinate
#             y = 0.45 ) %>% # yy coordinate, max(Shannon)
#   ungroup() %>% 
#   left_join(species_count  %>%
#               group_by(biome, trophic_level) %>% 
#               summarise(n_species = sum(n_species)), by = c("biome", "trophic_level"))
# 
# 
# ShannonOverTime <- ggplot(data = Shannon_index,
#                           aes(x = timestep, y = Shannon_Wiener_Index, color = future_scenario)) +
#   geom_line() +
#   # to deal with axis more freely (add axis on top row)
#   ggh4x::facet_grid2(biome ~ trophic_level,
#                      scales = "free", axes = "x", switch = "y", labeller = labeller(biome = as_labeller(biome_names))) +
#   labs(x = "Time", y = "Shannon-Wiener index", caption = "Dotted line = Start of future scenarios") +
#   scale_color_manual("Socio-economic\nscenario", values = custom_colors) +
#   scale_x_continuous(
#     breaks = c(25, 40, 60, 110),
#     labels = c("2015", "2030", "2050", "2100")) +
#   # add label with number of species
#   geom_text(data = icon_positions_shannon, aes(x = x, y = 0.4, label = paste0("n = ", n_species)), inherit.aes = FALSE, size = 2.5) +
#   theme_minimal() +
#   theme(
#     axis.line = element_blank(),
#     axis.line.x = element_line(color = "black"),
#     axis.line.y = element_line(color = "black"),
#     panel.grid = element_blank(),
#     panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
#     # modify facet labels
#     strip.text = element_text(face = "bold", size = rel(1)),
#     strip.placement = "outside",
#     # adjust legend
#     legend.position = "bottom",
#     # modify x-axis text
#     axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
#     # remove panel borders
#     panel.border = element_blank(),
#     panel.spacing.x = unit(1, "lines"),
#     panel.spacing.y = unit(2, "lines"),
#     plot.margin = unit(c(0, 0.5, 0, 0.5), "cm")) +
#   geom_vline(xintercept = c(40, 60, 110), linetype = "dashed", color = "gray50", size = 0.5)
# 
# ShannonOverTime
