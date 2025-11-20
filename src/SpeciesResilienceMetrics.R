####################################
# MULTI-SPECIES RESILIENCE METRICS #
####################################
# Inês Silva
# 23 April 2025 updated on 06 May 2025, 06 Nov 2025

source("./src/libraries.R")
source("./src/customFunctions.R")  

##########
# STEP 1 # Load all runs data
##########

# Boreal Forests ---------------------------------------------------------------

## Europe SSP5
europe_SSP1 <- fread("./output/metaRangeRuns/Europe_ssp126_31Oct25/Outputs/TNIND_yr_Europe_ssp126_31Oct25.csv")
## Europe SSP1
europe_SSP5 <- fread("./output/metaRangeRuns/Europe_ssp585_31Oct25/Outputs/TNIND_yr_Europe_ssp585_31Oct25.csv")

## North America SSP5
northamerica_SSP1 <- fread("./output/metaRangeRuns/NorthAmerica_ssp126_31Oct25/Outputs/TNIND_yr_NorthAmerica_ssp126_31Oct25.csv") 

## North America SSP1
northamerica_SSP5 <- fread("./output/metaRangeRuns/NorthAmerica_ssp585_31Oct25/Outputs/TNIND_yr_NorthAmerica_ssp585_31Oct25.csv")


# Tropical Moist Forests -------------------------------------------------------

## South America SSP5
southamerica_SSP1 <- fread("./output/metaRangeRuns/SouthAmerica_ssp126_31Oct25/Outputs/TNIND_yr_SouthAmerica_ssp126_31Oct25.csv")
## South America SSP1
southamerica_SSP5 <- fread("./output/metaRangeRuns/SouthAmerica_ssp585_31Oct25/Outputs/TNIND_yr_SouthAmerica_ssp585_31Oct25.csv")

## Africa SSP5
africa_SSP1 <- fread("./output/metaRangeRuns/Africa_ssp126_31Oct25/Outputs/TNIND_yr_Africa_ssp126_31Oct25.csv")
## Africa SSP1
africa_SSP5 <- fread("./output/metaRangeRuns/Africa_ssp585_31Oct25/Outputs/TNIND_yr_Africa_ssp585_31Oct25.csv")

## Asia SSP5
asia_SSP1 <- fread("./output/metaRangeRuns/Asia_ssp126_31Oct25/Outputs/TNIND_yr_Asia_ssp126_31Oct25.csv")
## Asia SSP1
asia_SSP5 <- fread("./output/metaRangeRuns/Asia_ssp585_31Oct25/Outputs/TNIND_yr_Asia_ssp585_31Oct25.csv")

invisible(gc())

##########
# STEP 2 # Combining all regions data together
##########

datasets <- list(europe_SSP5, northamerica_SSP5, asia_SSP5, africa_SSP5, southamerica_SSP5,
                 europe_SSP1, northamerica_SSP1, asia_SSP1, africa_SSP1, southamerica_SSP1)

TNIND_yr <- do.call("rbind", datasets)
# check for species names
#unique(TNIND_yr$species)

rm(europe_SSP5, northamerica_SSP5, asia_SSP5, africa_SSP5, southamerica_SSP5,
   europe_SSP1, northamerica_SSP1, asia_SSP1, africa_SSP1, southamerica_SSP1)
invisible(gc())

# get correspondence between species names and functional group
# retrieve trait data for trophic level, continent and biome info
combined_traits_data <- read_csv(here("data", "mammalTraits_2025-03-17.csv")) %>% 
  dplyr::filter(BIOME_NAME %in%  gsub("[/& ]", "", c("Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga"))) 

TNIND_yr <- TNIND_yr %>% 
  mutate(species = pretty_species_names(species)) %>% # if running twice it throws a warning - It's ok!
  left_join(
    dplyr::select(combined_traits_data, sci_name, BIOME_NAME, CONTINENT, trophic_level),
    by = c("species" = "sci_name",
           "biome" = "BIOME_NAME", # keep biome & continent here or a many-to-many warning will appear
           "region" = "CONTINENT")) %>%
  # simplify replicates numbering
  mutate(rep_num = str_extract(rep, "^[0-9]+")) %>% 
  # correction for tigers that are from asia but asian boreal forest are modelled together with europe
  mutate(trophic_level = replace(trophic_level, species== "Panthera tigris", "Carnivore"))%>%
  # deal with integer 64 columns (=big big numbers)
  mutate(across(where(bit64::is.integer64), as.numeric))


# write complete dataset into .csv (RAW DATA)
write_csv(TNIND_yr, 
          file = "./output/completeMetaRangeRun_31Oct25.csv")

##########
# STEP 3 # Build "diagnostics" pop. trends
##########

# create folder to save diagnostics
diagnostics <- file.path("./output/metaRangeRuns/diagnostics2")
dir.create(diagnostics, showWarnings = TRUE)

n_sps <- TNIND_yr %>% 
  group_by(scenario, biome, region) %>% 
  summarise(n_species = n_distinct(species), .groups = "drop")

# plot population trends -------------------------------------------------------
## differences within EACH replicate
TNIND_diff <- TNIND_yr %>%
  arrange(species, biome, rep_num, timestep) %>%
  group_by(scenario, biome, region, species, rep_num) %>%
  mutate(diff_TNIND = TNIND - lag(TNIND)) %>%
  ungroup() %>% 
  dplyr::select(scenario, biome, region, species, timestep, rep_num, TNIND, diff_TNIND) %>% 
  arrange(scenario, biome, region, species)

## mean TNIND and diff ACROSS replicates
TNIND_mean <- TNIND_diff %>%
  group_by(scenario, biome, region, species, timestep) %>%
  summarise(mean_TNIND = round(mean(TNIND, na.rm = TRUE), 0)) %>% 
  mutate(mean_diff_TNIND = mean_TNIND - lag(mean_TNIND, n = 1),
         rep_num = "Mean") %>%
  ungroup() %>% 
  dplyr::select(scenario, biome, region, species, timestep, mean_TNIND, mean_diff_TNIND, rep_num)

# save diagnostics in .xlsx
# create workbook
wb <- createWorkbook()
addWorksheet(wb, "spsModelled")
writeData(wb, "spsModelled", n_sps)
addWorksheet(wb, "perReplicate")
writeData(wb, "perReplicate", TNIND_diff)
# add across-replicate sheet 
addWorksheet(wb, "acrossReplicates")
writeData(wb, "acrossReplicates", TNIND_mean)
saveWorkbook(wb, file.path(diagnostics, "metaRangeRun_31Oct_diagnostics.xlsx"), overwrite = TRUE)
invisible(gc())


# pop trends in plot format ----------------------------------------------------

### per biome × region × scenario (all species together) -------------------------
combo_list <- TNIND_diff %>%
  distinct(biome, region, scenario)

for (i in seq_len(nrow(combo_list))) {
  
  b <- combo_list$biome[i]
  r <- combo_list$region[i]
  s <- combo_list$scenario[i]
  
  # filter data for this combination
  df <- TNIND_mean %>%
    filter(biome == b,
           region == r,
           scenario == s)
  
  # skip if nothing there
  if (nrow(df) == 0) next
  
  # nice title
  biome_title <- gsub("([A-Z])", " \\1", b) |> trimws()
  
  # plot (all species together)
  p <- ggplot(df, aes(x = timestep, y = mean_TNIND)) +
    geom_line() +
    facet_wrap(~species, scales = "free_y") +
    labs(title = paste0(biome_title, " – ", r, " – ", s),
         subtitle = "Species population trends") +
    theme_minimal() +
    theme(strip.text = element_text(face = "italic"))
  
  # filename
  fname <- paste0("./output/metaRangeRuns/diagnostics2/",
                  gsub(" ", "", b), "_", gsub(" ", "", r), "_", gsub(" ", "", s),
                  "_speciesPopulationTrends.png")
  
  ggsave(filename = fname, plot = p,
         bg = "white", width = 350, height = 210, units = "mm", dpi = 300)
  rm(df, p)
  gc()
}


### per species
# get unique combination to plot (biome + region + species + scenario)
combo_list <- TNIND_diff %>%
  distinct(biome, region, species, scenario)

# go through each combination
for (i in seq_len(nrow(combo_list))) {
  biome_to_plot   <- combo_list$biome[i]
  region_to_plot  <- combo_list$region[i]
  sp              <- combo_list$species[i]
  scen_to_plot    <- combo_list$scenario[i]
  
  # filter for that combo
  sp_mean <- TNIND_mean %>%
    filter(biome == biome_to_plot,
           region == region_to_plot,
           species == sp,
           scenario == scen_to_plot,
           timestep > 100)       # remove burn in
  
  sp_data <- TNIND_diff %>%
    filter(biome == biome_to_plot,
           region == region_to_plot,
           species == sp,
           scenario == scen_to_plot)
  
  # skip if data missing
  if (nrow(sp_data) == 0 | nrow(sp_mean) == 0) next
  
  # right plot - mean TNIND across replicates
  p_right <- ggplot(sp_mean, aes(x = timestep, y = mean_TNIND)) +
    geom_line(color = "black", size = 1) +
    labs(
      title = paste(sp, "- Mean across replicates"),
      x = "Timestep", y = "Mean TNIND") +
    theme_minimal()
  
  # left plot - TNIND per replicate separately
  p_left <- ggplot(sp_data, aes(
    x = timestep, y = TNIND,
    group = factor(rep_num),
    color = factor(rep_num))) +
    geom_line(size = 0.8, alpha = 0.7) +
    geom_vline(xintercept = 100, linetype = "dashed") +
    labs(
      title = paste(sp, "- Replicates"),
      x = "Timestep", y = "TNIND", color = "Replicate") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # combine both
  combined_plot <- p_right + p_left +
    plot_annotation(
      title = paste(
        "Temporal dynamics of TNIND",
        "\nBiome:", biome_to_plot,
        "| Region:", region_to_plot,
        "| Scenario:", scen_to_plot))
  
  # filename (scenario added)
  filename <- paste0(
    "TNIND_",
    gsub(" ", "_", sp), "_",
    gsub(" ", "_", biome_to_plot), "_",
    gsub(" ", "_", region_to_plot), "_",
    gsub(" ", "_", scen_to_plot),
    ".png")
  
  # save plots
  ggsave(filename, combined_plot,
         path = diagnostics,
         width = 16, height = 6, dpi = 300)
  
  message("Saved: ", filename)
  
  gc(rm(sp_data, sp_mean, p_left, p_right, combined_plot,
        biome_to_plot, region_to_plot, scen_to_plot, sp))
}


##########
# STEP 4 # Calculate metrics
##########

length(unique(TNIND_yr$timestep))

t_burnin <- 100 # burn-in years
t_policy <- 135 # final timestep

# calculate post policy mean value --------------------------------------------- 
# for the recovery time metric
post_disturbance_values <- TNIND_yr %>%
  dplyr::filter(timestep > t_burnin) %>% # remove burn-in period
  mutate(period = ifelse(timestep > t_burnin &
                           timestep <= t_policy, "Pre", "Post")) %>%  # code pre and post policy periods
  dplyr::filter(period == "Post") %>% # filter for the post policy period only
  group_by(biome, scenario, period, trophic_level, species, rep_num) %>% 
  summarise(across(c(TNIND, MNIND, mean_repRate, mean_carrCap, occupancy),
                   ~ mean(.x, na.rm = TRUE),
                   .names = "mean_{.col}"),
            .groups = "drop") %>% 
  # collapse across replicates
  group_by(biome, scenario, period, trophic_level, species) %>%
  summarise(mean_post = mean(mean_TNIND, na.rm = TRUE))
invisible(gc())

# calculate metrics per scenario, biome, functional group & sps ----------------
stability_sps <- TNIND_yr %>%
  dplyr::filter(timestep > t_burnin) %>% # remove burn-in period
  mutate(period = ifelse(timestep >= t_burnin & timestep <= t_policy, "Pre", "Post")) %>%  # code pre and post policy
  left_join(post_disturbance_values,by = c("biome", "species", "scenario", "period", "trophic_level")) %>%
  group_by(biome, scenario, period, trophic_level, species) %>%
  summarise(
    # find mean nº of individuals
    mean = mean(TNIND, na.rm = TRUE),
    # find min. nº of individuals
    min = min(TNIND, na.rm = TRUE),
    # find max. nº of individuals
    max = max(TNIND, na.rm = TRUE),
    # find the year the pop. reaches a min. value in the post policy period
    impact_year = timestep[which.min(TNIND)],
    # find year the pop bounces back to the same value in the pre period or even surpasses it
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

# write metrics per species to csv file (SUPPLEMENTARY TABLE X)
write.csv(stability_sps %>% 
            dplyr::select(biome, scenario, species, trophic_level, mean_Post, mean_Pre, impact, time_impact, recovery, time_recovery),
          file = "./output/resilienceMetricsPerSpecies.csv",
          row.names = FALSE)
invisible(gc())

# average stability metrics across functional groups ---------------------------
stability_avg <- stability_sps %>%
  dplyr::filter(!species == "Bison bonasus") %>% 
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

# new column labels
lookup <- c("impact_avg" = "Mean Impact",
            "impact_sd" = "Impact SD",
            "recovery_avg" = "Mean Recovery",
            "recovery_sd" = "Recovery SD",
            "timeimpact_avg" = "Mean Time to Impact",
            "timeimpact_sd" = "Time to Impact SD",
            "timerecovery_avg" = "Mean Time to Recovery",
            "timerecovery_sd" = "Time to Recovery SD")

# (Table x to support **FIGURE 1**)
write.csv(stability_avg %>%
            rename_with(~ lookup[.x], .cols = names(lookup)),
          file = "./output/resilienceMetricsAveraged.csv",
          row.names = FALSE)
invisible(gc())

# transform into long format for plots -----------------------------------------
stability_avg_long <- stability_avg %>%
  pivot_longer(
    cols = matches("_avg$|_sd$"),
    names_to = c("metric", ".value"),
    names_sep = "_")

##########
# STEP 5 # build plot for impact and recovery
##########

# new facet label names
metric.labs <- c("Impact\n (proportion of individuals lost)", "Recovery\n (proportion of individuals recovered)", "Time to Impact (years)" , "Time to recovery (years)")
names(metric.labs) <- c("impact",
                        "recovery",
                        "timeimpact",
                        "timerecovery")
biome_names <- c("BorealForestsTaiga" = "Boreal Forests Taiga", "TropicalSubtropicalMoistBroadleafForests" = "Tropical & Subtropical\nMoist Broadleaf Forests")

# Custom color palette
custom_colors <- c("ssp585" = "#ffab27", "ssp126" = "#99cc00")

# Updated plot (**FIGURE 1**)
figure1 <- stability_avg_long %>%
  dplyr::filter(metric %in% c("impact", "recovery")) %>%
  ggplot(aes(x = trophic_level , y = avg, fill = scenario)) +
  geom_bar(stat = "identity", position = position_dodge(0.6), width = 0.6) +
  geom_errorbar(aes(ymin = avg-sd, ymax = avg+sd), width = 0.2, colour = "black", alpha = 0.9, size = 0.4, position = position_dodge(0.6)) +
  facet_grid(metric ~ biome, scales = "free_y", labeller = labeller(metric = metric.labs, biome = biome_names), switch = "y") +
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
    #legend.title = element_text(face = "bold"),
    # modify y & x-axis text
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 0.8),
    axis.title = element_text(face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
    # remove panel borders
    panel.border = element_blank(),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(2, "lines"))



figure1
invisible(gc())

ggsave(filename = "./output/Figure1_ResilienceMetrics_31Oct25.png", # path
       figure1, # plot
       bg = 'white', width = 230, height = 210, units = "mm", dpi = 1200, #compression = "lzw"
) # image parameters

##########
# STEP 6 # build supplementary plot for time to impact and to recovery
##########


# Updated plot (**SUPPLEMENTARY FIGURE 1**)
supfigure1 <- stability_avg_long %>%
  dplyr::filter(metric %in% c("timeimpact", "timerecovery")) %>%
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
supfigure1
invisible(gc())

ggsave(filename = "./output/SupFigure1_TimeToImpactRecovery_31Oct25.png", # path
       supfigure1, # plot
       bg = 'white', width = 230, height = 210, units = "mm", dpi = 1200, #compression = "lzw"
) # image parameters

