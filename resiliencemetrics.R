# ============================================================
# Assemblage-level stability metrics (estar)
# Author: Sarina Lincoln
# Date: 02/10/2026
# ============================================================
setwd("C:/Users/Admin/OneDrive - Stockholm University/FinBio/R_scripts/NatPoKe-dev/NatPoKe-dev_Ines")

library(dplyr)
library(estar)
library(readr)


# ---------------------------
# User-defined settings
# ---------------------------

pre_tf        <- c(85, 115)
post_tf       <- c(115, 235)
recovery_time <- 235    # evaluate recovery at end of disturbed period


# ---------------------------
# Load data
# ---------------------------

TNIND_yr <- read.csv("./data/sarina/sarina/completeMetaRangeRun_31Oct25.csv")

# ---------------------------
# Resilience Metric Function
# ---------------------------

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

# load & reset tibble ----

results <- tibble()

# ---------------------------
# Analysis aggregated by scenario
# ---------------------------


for (biome_name in unique(TNIND_yr$biome)) {
  for (scenario_name in unique(TNIND_yr$scenario)) {
    
filtered_data <- TNIND_yr %>%
  filter(
    biome == biome_name,
    scenario == scenario_name,
    timestep >= pre_tf[1]
  )
agg_data <- filtered_data %>%
  group_by(timestep) %>%
  summarise(
    abundance = sum(TNIND, na.rm = TRUE),
    .groups = "drop"
  )
results <- bind_rows(
  results,
  calc_metrics(agg_data) %>%
    mutate(
      biome = biome_name,
      scenario = scenario_name
))
}}
# ---------------------------
# Analysis aggregated by species
# ---------------------------

for (biome_name in unique(TNIND_yr$biome)) {
  for (scenario_name in unique(TNIND_yr$scenario)) {
    
    filtered_data <- TNIND_yr %>%
      filter(
        biome == biome_name,
        scenario == scenario_name,
        timestep >= pre_tf[1]
      )
    for (sp in unique(filtered_data$species)) {
  
  agg_data <- filtered_data %>%
    filter(species == sp) %>%
    group_by(timestep) %>%
    summarise(abundance = sum(TNIND, na.rm = TRUE), .groups = "drop")
  
  results <- bind_rows(
    results,
    calc_metrics(agg_data) %>%
      mutate(
        biome = biome_name,
        scenario = scenario_name,
        species = sp,
      )
  )
  }
  }
}

# Add trophic details to species aggregation 

results <- results %>%
  left_join(
    TNIND_yr %>% select(species, trophic_level) %>% distinct(),
    by = "species"
  )
# ---------------------------
# Analysis aggregated by trophic
# ---------------------------

for (biome_name in unique(TNIND_yr$biome)) {
  for (scenario_name in unique(TNIND_yr$scenario)) {

    filtered_data <- TNIND_yr %>%
      filter(
        biome == biome_name,
        scenario == scenario_name,
        timestep >= pre_tf[1]
      )
    
    for (tl in unique(filtered_data$trophic_level)) {
  
  agg_data <- filtered_data %>%
    filter(trophic_level == tl) %>%
    group_by(timestep) %>%
    summarise(abundance = sum(TNIND, na.rm = TRUE), .groups = "drop")
  
  results <- bind_rows(
    results,
    calc_metrics(agg_data) %>%
      mutate(
        biome = biome_name,
        scenario = scenario_name,
        trophic_level = tl
      )
  )
}}}


# ---------------------------
# Analysis aggregated by trophic AND region 
# ---------------------------

for (biome_name in unique(TNIND_yr$biome)) {
  for (region_name in unique(TNIND_yr$region)) {
    for (scenario_name in unique(TNIND_yr$scenario)) {
    
    filtered_data <- TNIND_yr %>%
      filter(
        biome == biome_name,
        scenario == scenario_name,
        timestep >= pre_tf[1]
      )
    
      for (tl in unique(filtered_data$trophic_level)) {
      
      agg_data <- filtered_data %>%
        filter(trophic_level == tl) %>%
        group_by(timestep) %>%
        summarise(abundance = sum(TNIND, na.rm = TRUE), .groups = "drop")
      
      results <- bind_rows(
        results,
        calc_metrics(agg_data) %>%
          mutate(
            biome = biome_name,
            scenario = scenario_name,
            trophic_level = tl,
            region = region_name
            
          )
      )
    }}}}


results
# 
# write.csv(results, "metric_results_region&trophic.csv", row.names = FALSE)
