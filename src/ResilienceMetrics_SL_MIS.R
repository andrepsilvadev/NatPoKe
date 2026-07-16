## Name: ResilienceMetrics_SL_MIS.R ##
## Authors: Sarina Lincoln, Inês Silva ##
## Description: 
## Date: 15 April 2026


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

TNIND_yr <- read.csv("E:/metaRange_May26/completeMetaRangeRun_20260517.csv")

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
  left_join(
    TNIND_yr %>% select(species, trophic_level) %>% distinct(),
    by = "species"
  )

#unique(results_sps$region)

results_by_region <- results_sps %>%
  group_split(region)

write_xlsx(
  results_by_region,
  file.path(
    "E:/metaRange_May26/FigureAndMetrics", "ResilienceMetricsPerSpeciesAndRegion_SuppTable.xlsx" ))

# average across biome, scenario and trophic level
results_sps_aggRegion <- results_sps %>% 
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
    .groups = "drop") %>% 
  pivot_longer(
    cols = -c(future_scenario, biome, region, trophic_level),
    names_to = c(".value", "metric"),
    names_sep = "_") %>%
  mutate(
    metric = factor(metric,
                    levels = c("invariability",
                               "resistance",
                               "extentRecovery",
                               "rateRecovery",
                               "persistence")),
    trophic_level = factor(trophic_level),
    future_scenario = factor(future_scenario),
    region = factor(region))

trophic_cols <- c("Herbivore" = "#99cc00", "Carnivore" = "#ffab27", "Omnivore"  = "#377eb8")

metric.labs <- c("invariability" = "Invariability", "resistance" = "Resistance",
                 "extentRecovery" = "Extent of\nrecovery", "rateRecovery" = "Rate of\nrecovery",
                 "persistence" = "Persistence")

# order region more logically
region_levels <- c("North America", "Europe+Asia", "South America", "Africa", "Asia")

results_sps_aggRegion$region <- factor(results_sps_aggRegion$region, levels = region_levels)


### ORIGINAL PLOT ###
orig_plot <- ggplot(results_sps_aggRegion,
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

orig_plot

# save plot
ggsave(filename = "D:/metaRange_April26/FigureAndMetrics/Figure1_ResilienceMetricsForEachRegion.png", # path
       orig_plot, # plot
       bg = 'white', width = 180, height = 160, units = "mm", dpi = 1200,
       #compression = "lzw"
) # image parameters



### OPTION 1 - DOT PLOT ###

dot_plot <- ggplot(results_sps_aggRegion,
            aes(x = future_scenario, y = mean, colour = trophic_level, group = trophic_level)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.8) +
  geom_errorbar(aes(ymin = mean - SD,
                    ymax = mean + SD),
                width = 0.2,
                position = position_dodge(width = 0.5)) +
  facet_grid(metric ~ region,
             scales = "free_y",
             switch = "y",
             labeller = labeller(metric = metric.labs)) +
  labs(x = "Climate scenario",
       y = "Mean stability metric (± SD)",
       colour = "Trophic level") +
  scale_color_manual(values = trophic_cols,
                     name = "Trophic level") +
  theme_bw(base_size = 11) +
  #theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank(),
    #panel.spacing = unit(1.2, "lines")
    strip.placement = "outside"
  )

dot_plot

# save plot
ggsave(filename = "D:/metaRange_April26/FigureAndMetrics/Figure1_ResilienceMetricsForEachRegion_dotPlot.png", # path
       dot_plot, # plot
       bg = 'white', width = 180, height = 160, units = "mm", dpi = 1200,
       #compression = "lzw"
) # image parameters


## OPTION 2 - BOXPLOT + VIOLOIN PLOT

results_long <- results_sps %>%
  pivot_longer(
    cols = c(invariability,
             resistance,
             extent_recovery,
             rate_recovery,
             persistence),
    names_to = "metric",
    values_to = "value")
# order region more logically
region_levels <- c("North America", "Europe+Asia", "South America", "Africa", "Asia")

results_long$region <- factor(results_long$region, levels = region_levels)


boxplot <- ggplot(results_long,
       aes(trophic_level, value, fill = trophic_level)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.15,
               outlier.shape = NA) +
  ggh4x::facet_grid2(metric ~ region,
             scales = "free_y", 
             switch = "y",
             labeller = labeller(metric = metric.labs)) +
  scale_fill_manual(values = trophic_cols,
                     name = "Trophic level") +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank(),
    #panel.spacing = unit(1.2, "lines")
    strip.placement = "outside"
  )


# save plot
ggsave(filename = "D:/metaRange_April26/FigureAndMetrics/Figure1_ResilienceMetricsForEachRegion_boxPlot.png", # path
       boxplot, # plot
       bg = 'white', width = 180, height = 160, units = "mm", dpi = 1200,
       #compression = "lzw"
) # image parameters








# per scenario

results_scen <- tibble()
for (biome_name in unique(TNIND_yr$biome)) {
  for (scenario_name in unique(TNIND_yr$future_scenario)) {
    
    filtered_data <- TNIND_yr %>%
      filter(
        biome == biome_name,
        future_scenario == scenario_name,
        timestep >= pre_tf[1]
      )
    agg_data <- filtered_data %>%
      group_by(timestep) %>%
      summarise(
        abundance = mean(TNIND, na.rm = TRUE),
        .groups = "drop"
  )
results_scen <- bind_rows(
  results_scen,
  calc_metrics(agg_data) %>%
    mutate(
      biome = biome_name,
      future_scenario = scenario_name))
  }
}

############
# OUTPUT 2 # Analysis aggregated by species
############



# ---------------------------
# Analysis aggregated by trophic
# ---------------------------

results_trop <- tibble()

for (biome_name in unique(TNIND_yr$biome)) {
  for (scenario_name in unique(TNIND_yr$future_scenario)) {
    #for (rep_n in unique(TNIND_yr$rep_num)) {

    filtered_data <- TNIND_yr %>%
      filter(
        biome == biome_name,
        future_scenario == scenario_name,
     #   rep_num == rep_n,
        timestep >= pre_tf[1]
      )
    
    for (tl in unique(filtered_data$trophic_level)) {
  
  agg_data <- filtered_data %>%
    filter(trophic_level == tl) %>%
    group_by(timestep
             #, rep_num
             ) %>%
    summarise(abundance = mean(TNIND, na.rm = TRUE), .groups = "drop")
  
  results_trop <- bind_rows(
    results_trop,
    calc_metrics(agg_data) %>%
      mutate(
        biome = biome_name,
        future_scenario = scenario_name,
        trophic_level = tl,
       # rep_num = rep_n
      )
  )
    }}}
#}



df_long <- results_trop %>%
  dplyr::select(invariability, resistance, extent_recovery,
                rate_recovery, biome, future_scenario, trophic_level) %>% 
  pivot_longer(
    cols = c(invariability, resistance, extent_recovery,
             rate_recovery#, persistence
    ),
    names_to = "metric",
    values_to = "value") %>%
  mutate(metric = factor(metric,
                         levels = c("invariability",
                                    "resistance",
                                    "extent_recovery",
                                    "rate_recovery"
                                    #,"persistence"
                         )),
         trophic_level = factor(trophic_level),
         future_scenario = factor(future_scenario))

trophic_cols <- c(
  "Herbivore" = "#99cc00",
  "Carnivore" = "#ffab27",
  "Omnivore" = "#377eb8"#,
  #"Top predator" = "#984ea3"
)

# new facet label names
metric.labs <- c("Invariability",
                 "Resistance",
                 "Extent of recovery" ,
                 "Rate of recovery")
names(metric.labs) <- c("invariability",
                        "resistance",
                        "extent_recovery",
                        "rate_recovery")



ggplot(df_long, aes(x = future_scenario, y = value, fill = trophic_level)) +
  geom_bar(stat = "identity", position = position_dodge(0.6), width = 0.55, colour = "grey30", linewidth = 0.2) +
  geom_hline(yintercept = 0, linewidth = 0.5, colour = "black") +
  facet_grid(metric ~ biome,
             scales = "free_y",
             switch = "y",
             labeller = labeller(metric = metric.labs)) +
  scale_fill_manual(values = trophic_cols, name = "Trophic level") +
  ylab("Ecological Stability Metrics") +
  xlab(NULL) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.2, "lines")
  )


# ---------------------------
# Analysis aggregated by trophic AND region 
# ---------------------------

results_trop_reg <- tibble()
for (biome_name in unique(TNIND_yr$biome)) {
  for (region_name in unique(TNIND_yr$region)) {
    for (scenario_name in unique(TNIND_yr$future_scenario)) {
    
    filtered_data <- TNIND_yr %>%
      filter(
        biome == biome_name,
        future_scenario == scenario_name,
        timestep >= pre_tf[1]
      )
    
      for (tl in unique(filtered_data$trophic_level)) {
      
      agg_data <- filtered_data %>%
        filter(trophic_level == tl) %>%
        group_by(timestep) %>%
        summarise(abundance = sum(TNIND, na.rm = TRUE), .groups = "drop")
      
      results_trop_reg <- bind_rows(
        results_trop_reg,
        calc_metrics(agg_data) %>%
          mutate(
            biome = biome_name,
            future_scenario = scenario_name,
            trophic_level = tl,
            region = region_name
            
          )
      )
    }}}}


# save diagnostics in .xlsx

wb <- createWorkbook()
addWorksheet(wb, "perScenario")
writeData(wb, "perScenario", results_scen)
addWorksheet(wb, "perSpecies")
writeData(wb, "perSpecies", results_sps)
addWorksheet(wb, "perTrophicLevel")
writeData(wb, "perTrophicLevel", results_trop)
addWorksheet(wb, "perTrophicLevelAndRegion")
writeData(wb, "perTrophicLevelAndRegion", results_trop_reg)
saveWorkbook(wb, file.path("D:/metaRange_April26", paste0("resilienceMetrics_", 
                                                   "20260405",
                                                   #Sys.Date(),
                                                   ".xlsx")), overwrite = TRUE)
invisible(gc())


# 
# write.csv(results, "metric_results_region&trophic.csv", row.names = FALSE)

