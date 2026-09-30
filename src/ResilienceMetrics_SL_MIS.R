## Name: ResilienceMetrics_SL_MIS.R ##
## Authors: Sarina Lincoln, Inês Silva ##
## Date: 15 April 2026
## Description: Calculate resilience metrics per species, namely invariability, 
## resistance, extent and rate of recovery and persistence, from the estar package
## Metrics are them averaged per biome and regions to produce two barplots, 
## one per biome and another per biome (included in main manuscript).
############ MANY LINES IN THE BOTTOM LEFT TO DELETE STILL #####################


source("./src/libraries.R")


##########
# STEP 1 # User-defined settings
##########

# pre-disturbance period
pre_tf <- c(25, 40)

# post disturbance period
post_tf <- c(110, 136)

# evaluate recovery at end of disturbed period
recovery_time <- 136   


##########
# STEP 2 # Load data
##########

TNIND_yr <- read.csv("E:/metaRange_May26/outputs/completeMetaRangeRun_20260517.csv")

# clean up erroneous species
TNIND_yr <- TNIND_yr %>%
  filter(
    !(species == "Lynx lynx" & biome == "Tropical & Subtropical Moist Broadleaf Forests" & region == "Asia"),
    !(species == "Ursus arctos" & biome == "Tropical & Subtropical Moist Broadleaf Forests" & region == "Asia")
  )
  
##########
# STEP 3 # Resilience Metric Function
##########

calc_metrics <- function(agg_data) {
  b_data <- agg_data %>%
  filter(timestep >= pre_tf[1], timestep <= pre_tf[2])
  
  d_data <- agg_data %>%
    filter(timestep >= post_tf[1], timestep <= post_tf[2])
  
  # ---------------------------
  # 1. INVARIABILITY
  # ---------------------------
  tibble(
    invariability = invariability(
      type      = "functional",
      response  = "v",
      mode      = "lm_res",
      vd_i      = "abundance",
      td_i      = "timestep",
      d_data    = d_data,
      vb_i      = "abundance",
      tb_i      = "timestep",
      b_data    = b_data,
      metric_tf = post_tf
    ),
    
    # ---------------------------
    # 2. RESISTANCE
    # ---------------------------
    
    resistance = resistance(
      type     = "functional",
      b        = "d", 
      res_mode = "lrr",
      res_time = "max",
      res_tf   = post_tf,
      b_tf     = pre_tf,
      vd_i     = "abundance",
      td_i     = "timestep",
      d_data   = agg_data
      
    ),
    
    # ---------------------------
    # 3. EXTENT OF RECOVERY
    # ---------------------------
    
    extent_recovery = recovery_extent(
      type = "functional",
      response = "lrr",        # Log-response ratio
      t_rec = recovery_time,   # Final timestep
      summ_mode = "mean",
      vd_i = "abundance",
      td_i = "timestep",
      d_data = agg_data,
      # vb_i = "abundance",
      # tb_i = "timestep",
      # b_data = b_data,
      b_tf     = pre_tf,
      b = "d"
    ),
    
    # ---------------------------
    # 4. RATE OF RECOVERY
    # ---------------------------
    
    rate_recovery = recovery_rate(
      type      = "functional",
      response  = "lrr",
      metric_tf = post_tf,
      vd_i      = "abundance",
      td_i      = "timestep",
      d_data    = agg_data,
      # vb_i      = "abundance",
      # tb_i      = "timestep",
      # b_data    = b_data,
      b_tf     = pre_tf,
      b         = "d"
    ),
    
    # ---------------------------
    # 5. PERSISTENCE
    # ---------------------------
    
    persistence = persistence(
      type      = "functional",
      metric_tf = post_tf,
      vd_i      = "abundance",
      td_i      = "timestep",
      d_data    = agg_data,
      b_tf     = pre_tf,
      b         = "d"
    ))
}

##########
# STEP 4 # Estimate metrics per species
##########

# changed from sum to mean values across replicates

results_sps <- tibble()

for (biome_name in unique(TNIND_yr$biome)) {
  for (scenario_name in unique(TNIND_yr$future_scenario)) {
    for (region_name in unique(TNIND_yr$region)) {
    
    filtered_data <- TNIND_yr %>%
      filter(
        biome == biome_name,
        future_scenario == scenario_name,
        region == region_name,
        timestep >= pre_tf[1] # removing burn-in
      )
    for (sp in unique(filtered_data$species)) {
      
      agg_data <- filtered_data %>%
        filter(species == sp) %>%
        group_by(timestep) %>%
        summarise(abundance = mean(TNIND, na.rm = TRUE), .groups = "drop")
      
      results_sps <- bind_rows(
        results_sps,
        calc_metrics(agg_data) %>%
          mutate(
            biome = biome_name,
            future_scenario = scenario_name,
            region = region_name,
            species = sp,
          )
      )
    }
  }
}
}


#print(results, n = Inf)

# Add trophic details to species aggregation 
results_sps <- results_sps %>%
  left_join(TNIND_yr %>% select(species, trophic_level) %>% distinct(),
            by = "species") %>% 
  relocate(future_scenario, biome, region, species, trophic_level)

# save metrics per species as .csv
write.csv(results_sps,
          file = file.path("E:/metaRange_May26/outputs", paste0("ResilienceMetricsPerSpecies", Sys.Date(), ".csv")),
          row.names = FALSE)

#unique(results_sps$region)

# save metrics per species as xlsx with a sheet for reach region
results_sps_by_region <- results_sps %>%
  dplyr::group_split(region)

region_names <- results_sps %>%
  dplyr::distinct(region) %>%
  dplyr::pull(region)

names(results_sps_by_region) <- region_names

# write an excel file with a sheet per region with species metrics values
write_xlsx(results_sps_by_region, file.path("E:/metaRange_May26/FigureAndMetrics",
                                            paste0("ResilienceMetricsPerSpecies", Sys.Date(), ".xlsx" )))

# average across biome, scenario and trophic level
results_region <- results_sps %>% 
  group_by(future_scenario, biome, region, trophic_level) %>% 
  summarise(
    mean_invariability   = mean(invariability, na.rm = TRUE),
    mean_resistance      = mean(resistance, na.rm = TRUE),
    mean_extentRecovery = mean(extent_recovery, na.rm = TRUE),
    mean_rateRecovery   = mean(rate_recovery, na.rm = TRUE),
    mean_persistence = mean(persistence, na.rm = TRUE),
    SD_invariability     = sd(invariability, na.rm = TRUE),
    SD_resistance        = sd(resistance, na.rm = TRUE),
    SD_extentRecovery   = sd(extent_recovery, na.rm = TRUE),
    SD_rateRecovery     = sd(rate_recovery, na.rm = TRUE),
    SD_persistence = sd(persistence, na.rm = TRUE),
    .groups = "drop")

# save summary table with metrics per region
results_region_rounded <- results_region %>%
  dplyr::select(!c("SD_invariability", "SD_resistance", "SD_extentRecovery", "SD_rateRecovery", "SD_persistence")) %>% 
  mutate(across(where(is.numeric), ~ signif(.x, 3))) %>% 
  rename("Scenario" = "future_scenario",
         "Biome" = "biome",
         "Trophic Level" = "trophic_level",
         "Mean Invariability" = "mean_invariability",
         "Mean Resistance" = "mean_resistance",
         "Mean Extent of Recovery" = "mean_extentRecovery",
         "Mean Rate of Recovery" = "mean_rateRecovery",
         "Mean Persistence" = "mean_persistence")

# save as a formatted word document
gt::gtsave(gt(results_region_rounded), file.path("E:/metaRange_May26/outputs/FigureAndMetrics",
                                             paste0("ResilienceMetricsPerRegion", Sys.Date(), ".docx")))

# save metrics per species as .csv
write.csv(results_region,
          file = file.path("E:/metaRange_May26/outputs", paste0("ResilienceMetricsPerRegion", Sys.Date(), ".csv")),
          row.names = FALSE)

##########
# STEP 5 # Build manuscripts main Figure
##########

results_sps_long <- results_sps %>%
  # pivot metrics values longer for plotting
  pivot_longer(cols = c(invariability, resistance, extent_recovery, rate_recovery,
                        persistence), names_to = "metric", values_to = "value") %>% 
  # remove extent of recovery
  dplyr::filter(!metric %in% "extent_recovery")

# set level to order the plots logically
metric_levels <- c("invariability","resistance", "rate_recovery", "persistence")
results_sps_long$metric <- factor(results_sps_long$metric,
                                  levels = metric_levels)
# define prettier metric names for plotting
metric.labs <- c("invariability" = "Invariability",
                 "resistance" = "Resistance",
                 # "extentRecovery" = "Recovery\nExtent",
                 "rate_recovery" = "Recovery\nRate",
                 "persistence" = "Persistence")

# define color scheme
trophic_cols <- c("Herbivore" = "#99cc00",
                  "Carnivore" = "#ffab27",
                  "Omnivore"  = "#377eb8")
# darker colors for the points
trophic_cols_dark <- colorspace::darken(trophic_cols, amount = 0.35)

# order region more logically
region_levels <- c("North America", "Europe+Asia",
                   "South America", "Africa", "Asia")

region_labs <- c("North America" = "North America",
                 "Europe+Asia" = "Eurasia",
                 "South America" = "South America",
                 "Africa" = "Africa",
                 "Asia" = "Asia")

# make pretties labels for scenarios
scenario_labs <- c("ssp126" = "SSP1-2.6",
                   "ssp585" = "SSP5-8.5")


# Main Manuscript figure #

metricsPerRegion <- ggplot(results_sps_long, 
                           aes(x = future_scenario, y = value, fill = trophic_level)) +
  # boxplot across species
  geom_boxplot(position = position_dodge(0.7), width = 0.55,
               colour = "grey40", linewidth = 0.2, outlier.shape = NA) +
  # one dot per species
  geom_jitter(aes(colour = trophic_level),
              position = position_jitterdodge(jitter.width = 0.12, dodge.width = 0.7),
              size = 1.1, alpha = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  facet_nested(metric ~ biome + region,
               scales = "free",
               switch = "y",
               labeller = labeller(metric = metric.labs, region = region_labs),
    strip = strip_nested(background_x = element_blank(),
                         by_layer_x = TRUE),
    nest_line = element_line(linewidth = 0.5, colour = "grey30")) +
  scale_x_discrete(labels = scenario_labs) +
  scale_fill_manual(values = trophic_cols, name = "") +
  scale_colour_manual(values = trophic_cols_dark, guide = "none") +
  ylab(NULL) +
  xlab(NULL) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    # increase spacing between pannels in yy axis
    panel.spacing.y = unit(1, "lines"),
    strip.placement = "outside")

# save plot
ggsave(filename = "E:/metaRange_May26/outputs/FigureAndMetrics/Figure1_ResilienceMetricsForEachRegion_updated.png", # path
       metricsPerRegion, # plot
       bg = 'white', width = 300, height = 200, units = "mm", dpi = 1200,
       #compression = "lzw"
) # image parameters


# Supplementary Table with Quantiles and Mean values

quantiles_table <- results_sps_long %>%
  group_by(metric, biome, region, future_scenario, trophic_level) %>%
  summarise(
    n = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    q1 = quantile(value, 0.25, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    q3 = quantile(value, 0.75, na.rm = TRUE),
    .groups = "drop") %>%
  # round values
  mutate(across(c(mean, q1, median, q3), ~ round(.x, 3)))


# write legends for word documents
metric_legends <- c(
  invariability = "Distribution of invariability values across species, summarised by biome, region, future scenario, and trophic level. Q1 represents the 25 percentile and Q3 the 75 percentile.",
  resistance = "Distribution of resistance values across species, summarised by biome, region, future scenario, and trophic level. Q1 represents the 25 percentile and Q3 the 75 percentile.",
  rate_recovery = "Distribution of recovery rate values across species, summarised by biome, region, future scenario, and trophic level. Q1 represents the 25 percentile and Q3 the 75 percentile.",
  #RECOVERY_EXTENT = "Distribution of recovery extent values across species, summarised by biome, region, future scenario, and trophic level. Q1 represents the 25 percentile and Q3 the 75 percentile.",
  persistence = "Distribution of persistence values across species, summarised by biome, region, future scenario, and trophic level. Q1 represents the 25 percentile and Q3 the 75 percentile."
)

# one document per metric
for (m in unique(quantiles_table$metric)) {
  
  # filter for  metric
  df <- quantiles_table %>%
    filter(metric == m) %>%
    dplyr::select(biome, region, future_scenario, trophic_level, n, mean, q1, median, q3)
  
  # Create gt table
  tbl <- df %>%
    gt() %>%
    # add spanner with metric name over summary statistics
    tab_spanner(label = m, columns = c(n, mean, q1, median, q3)) %>%
    # Column names
    cols_label(biome = "Biome", region = "Region", future_scenario = "Scenario",
               trophic_level = "Trophic level", n = "No. of species", mean = "Mean",
               q1 = "Q1", median = "Median", q3 = "Q3") %>%
    # Number formatting
    fmt_number(columns = c(mean, q1, median, q3),
               decimals = 3) %>%
    # Align summary statistics
    cols_align(align = "center", columns = c(n, mean, q1, median, q3)) %>%
    # Add legend
    tab_header(title = metric_legends[[m]]) %>%
    tab_options(table.font.size = px(9), source_notes.font.size = px(9))
  
  # File name
  output_file <- file.path("E:/metaRange_May26/outputs/FigureAndMetrics",
                           paste0(m, "_Quantiles_", Sys.Date(), ".docx"))
  
  # Save
  gtsave(tbl, output_file)
}

print(quantiles_table, n = Inf)
# Supplementary Materials Figure #


results_sps_aggRegion <- results_region %>% 
  # keep only relevant columns
  pivot_longer(
    cols = -c(future_scenario, biome, region, trophic_level),
    names_to = c(".value", "metric"),
    names_sep = "_") %>%
  mutate(
    # make metrics, trophic levels, scenarios and regions factors
    metric = factor(metric,
                    levels = c("invariability",
                               "resistance",
                               "extentRecovery",
                               "rateRecovery",
                               "persistence")),
    trophic_level = factor(trophic_level),
    future_scenario = factor(future_scenario),
    region = factor(region)) %>% 
  # remove extent of recovery
  dplyr::filter(!metric %in% "extentRecovery")

results_sps_aggRegion$region <- factor(results_sps_aggRegion$region,
                                       levels = region_levels)


supp_resilencePerRegions <- ggplot(results_sps_aggRegion,
       aes(x = future_scenario,
           y = mean,
           fill = trophic_level)) +
  geom_bar(stat = "identity",
           position = position_dodge(0.6),
           width = 0.55,
           colour = "grey40",
           linewidth = 0.1) +
  geom_errorbar(aes(ymin = mean - SD,
                    ymax = mean + SD),
                position = position_dodge(0.6),
                width = 0.2, linewidth = 0.2) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  facet_grid(metric ~ region,
             scales = "free",
             switch = "y",
             labeller = labeller(metric = metric.labs)) +
  scale_fill_manual(values = trophic_cols,
                    name = " ") +
  ylab(" ")+
  #ylab("Ecological Stability Metrics") +
  xlab(NULL) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    strip.placement = "outside"
  )

supp_resilencePerRegions

# save plot
ggsave(filename = "D:/metaRange_April26/FigureAndMetrics/SupplementaryFigure_ResilienceMetricsForEachRegion.png", # path
       supp_resilencePerRegions, # plot
       bg = 'white', width = 180, height = 160, units = "mm", dpi = 1200,
       #compression = "lzw"
) # image parameters


## OLD CODE ##
# metricsPerRegion <- ggplot(results_sps_aggRegion,
#                            aes(x = future_scenario, y = mean, fill = trophic_level)) +
#   # mean metric valus across species
#   geom_bar(stat = "identity", position = position_dodge(0.6), width = 0.55,
#            colour = "grey40", linewidth = 0.1) +
#   geom_errorbar(aes(ymin = mean - SD, ymax = mean + SD),
#                 position = position_dodge(0.6), width = 0.2, linewidth = 0.2) +
#   geom_hline(yintercept = 0, linewidth = 0.5) +
#   # nested facets per metric(rows) and biome/region (columns)
#   facet_nested(metric ~ biome + region, scales = "free",
#                switch = "y", labeller = labeller(metric = metric.labs, region = region_labs),
#                # add spanner line across biomes' regions
#                strip = strip_nested(background_x = element_blank(), by_layer_x = TRUE),
#                nest_line = element_line(linewidth = 0.5, colour = "grey30")) +
#   scale_x_discrete(labels = scenario_labs) +
#   # user defined color scale
#   scale_fill_manual(values = trophic_cols, name = " ") +
#   ylab(" ") +
#   xlab(NULL) +
#   theme_minimal(base_size = 11) +
#   theme(
#     legend.position = "bottom",
#     strip.text = element_text(face = "bold"),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.spacing = unit(0.5, "lines"),
#     strip.placement = "outside")

