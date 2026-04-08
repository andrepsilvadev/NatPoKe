## Name: readMetaRangeOutput.R ##
## Author: Inês Silva ##
## Description: read metaRange ouputs and perform basic diagnostics
## Date: November 25th 2025 ##

source("./src/libraries.R")
source("./src/customFunctions2.R")  

################
# FULL DATASET # 
################
TNIND_paths <- list()

##########
# STEP 1 # collect TNIND paths if not already provided
##########

if (length(TNIND_paths) == 0) {
  
  runs <- read.csv("./data/run_table.csv", stringsAsFactors = FALSE)
  
  TNIND_paths <- character(0)
  
  for (i in seq_len(nrow(runs))) {
    
    target_region <- runs$region[i]
    target_biome <- runs$biome[i]
    future_scenario <- runs$scenario[i]
    
    runname <- paste(
      target_region,
      future_scenario,
      "20260405",
      #format(Sys.time(), "%Y%m%d"),
      sep = "_"
    )

    tnind_file <- file.path(
      "D:/metaRange_April26", 
      runname, "Outputs",
      paste0("TNIND_yr_", runname, ".csv")
    )
    
    if (!file.exists(tnind_file)) {
      warning("TNIND file not found (skipping): ", tnind_file)
      next
    }
    
    TNIND_paths[runname] <- tnind_file
  }
}

# read all dfs
TNIND_all_runs <- lapply(TNIND_paths, data.table::fread)

# combine into unique df
TNIND_yr <- data.table::rbindlist(TNIND_all_runs, use.names = TRUE, fill = TRUE)
# clean up
rm(TNIND_all_runs)
invisible(gc())

# get correspondence between species names and functional group
combined_traits_data <- read_csv(here("data", "externaldata", "mammalTraits_2025-12-11.csv")) %>% 
  dplyr::filter(BIOME_NAME %in% c("Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga")) %>% 
  mutate(
    CONTINENT = case_when(
      BIOME_NAME == "Boreal Forests/Taiga" & CONTINENT == "Europe" ~ "Europe+Asia",
      TRUE ~ CONTINENT
    )
  )

# clean up the dataset
TNIND_yr <- TNIND_yr %>% 
  mutate(species = pretty_species_names(species)) %>% # if running twice it throws a warning - It's ok!
  left_join(
    dplyr::select(combined_traits_data, sci_name, BIOME_NAME, CONTINENT, trophic_level),
    by = c("species" = "sci_name",
           "biome" = "BIOME_NAME", # keep biome & continent here or a many-to-many warning will appear
           "region" = "CONTINENT"
           )) %>%
  # simplify replicates numbering
  mutate(rep_num = str_extract(rep, "^[0-9]+")) %>% 
  # correction for tigers that are from asia but asian boreal forest are modelled together with europe
  mutate(trophic_level = replace(trophic_level, species== "Panthera tigris", "Carnivore"))%>%
  # deal with integer 64 columns (=big big numbers)
  mutate(across(where(bit64::is.integer64), as.numeric))

##########
# STEP 3 #  Write complete dataset into .csv (RAW DATA)
##########

write_csv(TNIND_yr, 
          file = file.path(output_root, paste0("completeMetaRangeRun_", #format(Sys.time(), "%Y%m%d"),
                                               "20260405",
                                               ".csv")))

################################
# DIAGNOSTIC POPULATION TRENDS # 
################################

# STEP 1 # Build and excel file

# create folder to save diagnostics
diagnostics_dir <- file.path("D:/metaRange_April26", "diagnostics")
dir.create(diagnostics_dir, showWarnings = TRUE)

# get number of sps per combin
n_sps <- TNIND_yr %>% 
  group_by(future_scenario, biome, region) %>% 
  summarise(n_species = n_distinct(species), .groups = "drop")

# population trends ------------------------------------------------------------
## differences within EACH replicate
TNIND_diff <- TNIND_yr %>%
  arrange(species, biome, rep_num, timestep) %>%
  group_by(future_scenario, biome, region, species, rep_num) %>%
  mutate(diff_TNIND = TNIND - lag(TNIND)) %>%
  ungroup() %>% 
  dplyr::select(future_scenario, biome, region, species, timestep, rep_num, TNIND, diff_TNIND) %>% 
  arrange(future_scenario, biome, region, species)

## mean TNIND and diff ACROSS replicates
TNIND_mean <- TNIND_diff %>%
  group_by(future_scenario, biome, region, species, timestep) %>%
  summarise(mean_TNIND = round(mean(TNIND, na.rm = TRUE), 0)) %>% 
  mutate(mean_diff_TNIND = mean_TNIND - lag(mean_TNIND, n = 1),
         rep_num = "Mean") %>%
  ungroup() %>% 
  dplyr::select(future_scenario, biome, region, species, timestep, mean_TNIND, mean_diff_TNIND, rep_num)

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
saveWorkbook(wb, file.path(diagnostics_dir, paste0("metaRangeRun_", 
                                                   "20260405",
                                                   #Sys.Date(),
                                                   "_diagnostics.xlsx")), overwrite = TRUE)
invisible(gc())


# STEP 2 # Plot population trends over time 

### per biome × region × scenario (all species together)
combo_list <- TNIND_diff %>%
  distinct(biome, region, future_scenario)

for (i in seq_len(nrow(combo_list))) {
  
  b <- combo_list$biome[i]
  r <- combo_list$region[i]
  s <- combo_list$future_scenario[i]
  
  # filter data for this combination
  df <- TNIND_mean %>%
    filter(biome == b,
           region == r,
           future_scenario == s)
  
  # skip if nothing there
  if (nrow(df) == 0) next
  
  # nice title
  biome_title <- gsub("([A-Z])", " \\1", b) |> trimws()
  
  # plot (all species together)
  p <- ggplot(df, aes(x = timestep, y = mean_TNIND)) +
    geom_line() +
    # add line for burn-in
    geom_vline(xintercept = 25, linetype="dotted", 
               color = "red") +
    facet_wrap(~species, scales = "free_y") +
    labs(title = paste0(biome_title, " – ", r, " – ", s),
         subtitle = "Species population trends") +
    theme_minimal() +
    theme(strip.text = element_text(face = "italic"))
  
  # filename
  fname <- file.path(diagnostics_dir,
                  paste0(gsub(" ", "", b), "_", gsub(" ", "", r), "_", gsub(" ", "", s),
                  "_speciesPopulationTrends.png"))
  # save plot
  ggsave(filename = fname, plot = p,
         bg = "white", width = 350, height = 210, units = "mm", dpi = 300)
  rm(df, p)
  gc()
}


### per species ----------------------------------------------------------------
# get unique combination to plot (biome + region + species + scenario)
combo_list <- TNIND_diff %>%
  distinct(biome, region, species, future_scenario)

# go through each combination
for (i in seq_len(nrow(combo_list))) {
  biome_to_plot   <- combo_list$biome[i]
  region_to_plot  <- combo_list$region[i]
  sp              <- combo_list$species[i]
  scen_to_plot    <- combo_list$future_scenario[i]
  
  # filter for that combo
  sp_mean <- TNIND_mean %>%
    filter(biome == biome_to_plot,
           region == region_to_plot,
           species == sp,
           future_scenario == scen_to_plot,
           timestep > 25)       # remove burn in
  
  sp_data <- TNIND_diff %>%
    filter(biome == biome_to_plot,
           region == region_to_plot,
           species == sp,
           future_scenario == scen_to_plot)
  
  # skip if data missing
  if (nrow(sp_data) == 0 | nrow(sp_mean) == 0) next
  
  # right plot - mean TNIND across replicates
  p_right <- ggplot(sp_mean, aes(x = timestep, y = mean_TNIND)) +
    geom_line(color = "black", size = 0.8) +
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
    plot_annotation(title = paste("Temporal dynamics of TNIND", "\nBiome:", biome_to_plot,
                                  "| Region:", region_to_plot, "| Scenario:", scen_to_plot))
  
  # filename 
  filename <- paste0("TNIND_", gsub(" ", "_", sp), "_", gsub(" ", "_", biome_to_plot), "_",
                     gsub(" ", "_", region_to_plot), "_", gsub(" ", "_", scen_to_plot), ".png")
  
  # save plots
  ggsave(filename, combined_plot, path = diagnostics_dir,
         width = 16, height = 6, dpi = 300)
  
  message("Saved: ", filename)
  
  gc(rm(sp_data, sp_mean, p_left, p_right, combined_plot,
        biome_to_plot, region_to_plot, scen_to_plot, sp))
}

############################
# COMPLETE TRAIT DATAFRAME #
############################

traitdf_paths <- character(0)

for (i in seq_len(nrow(runs))) {
  
  target_region <- runs$region[i]
  target_biome <- runs$biome[i]
  future_scenario <- runs$scenario[i]
  
  runname <- paste(
    target_region,
    future_scenario,
    "20260405",
    #format(Sys.time(), "%Y%m%d"),
    sep = "_"
  )
  
  traitdf_file <- file.path(
    "D:/metaRange_April26", 
    runname, "Inputs",
    "metaRangeSpeciesDataframe.csv")
  
  
  if (!file.exists(traitdf_file)) {
    warning("Trait Dataframe file not found (skipping): ", traitdf_file)
    next
  }
  
  traitdf_paths[runname] <- traitdf_file
}


# read all dfs
traits_all_runs <- lapply(traitdf_paths, data.table::fread)

# combine into unique df
all_traits <- data.table::rbindlist(traits_all_runs, use.names = TRUE, fill = TRUE) %>% 
  select(!Index) %>%   # remove Index column if present 
  distinct(Species, .keep_all = TRUE) %>% # ensure one row per species
  mutate(Species = pretty_species_names(as.character(Species)))

# clean up
rm(traits_all_runs)
invisible(gc())

# Save merged .xlsx
write_xlsx(all_traits, file.path("D:/metaRange_April26", "completeTraitDataframe_allSps.xlsx"))

#################################
# AVERAGE SUITABILITY OVER TIME #
#################################

inputFolder_paths <- character(0)

for (i in seq_len(nrow(runs))) {
  
  target_region <- runs$region[i]
  target_biome <- runs$biome[i]
  future_scenario <- runs$scenario[i]
  
  runname <- paste(
    target_region,
    future_scenario,
    "20260405",
    #format(Sys.time(), "%Y%m%d"),
    sep = "_"
  )
  
  input_folder <- file.path(
    "D:/metaRange_April26", 
    runname, "Inputs")
  
  
  if (!file.exists(input_folder)) {
    warning("Input folder not found (skipping): ", input_folder)
    next
  }
  
  inputFolder_paths[runname] <- input_folder
}

# clean up
rm(input_folder)
invisible(gc())

# loop over folders and produce one plot per folder
for (dir in inputFolder_paths) {
  
  # list tif files in this folder
  files <- list.files(dir, pattern = "_reprojectedm\\.tif$", full.names = TRUE)
  if (length(files) == 0) next
  
  message("Average Suitability Over Time plot for: ", dir)
  
  # process all species inside this folder
  folder_df <- lapply(files, function(f) {
    
    fname <- basename(f)
    
    # get species name 
    species_name <- str_extract(fname, "^(.*?)_(?=(boreal|tropical))")
    species_name <- gsub("_$", "", species_name)  # remove trailing _
    
    # load raster
    r <- rast(f)
    
    # get global mean suitability value per layer
    df <- global(r, "mean", na.rm = TRUE) %>%
      as.data.frame()
    
    # add years & sps names
    df$year <- as.numeric(names(r))
    df$species <- suppressMessages(pretty_species_names(species_name)) #pretty_species_names() is custom function
    df
    
  }) %>% bind_rows()
  
  # folder name for saving plot
  folder_name <- basename(dirname(dir))
  
  # plot average suitability
  p <- ggplot(folder_df, aes(x = year, y = mean, group = species)) +
    geom_line() +
    facet_wrap(~ species, scales = "free_y", ncol = 3) +
    scale_x_continuous(
      breaks = seq(min(folder_df$year), max(folder_df$year), by = 20)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(face = "italic")) +
    labs(x = "Year", y = "Mean suitability", title = paste("Average suitability –", folder_name))
  
  print(p)
  n_species <- length(unique(folder_df$species))
  ncol <- 3
  nrow <- ceiling(n_species / ncol)
  
  ggsave(filename = paste0(folder_name, "_suitabilityOverTime.png"),
         path = diagnostics_dir,
         plot = p, width = 12,
         height = nrow * 3,
         dpi = 300)
}

message("✅ All diagnostic plots & files were created successfully!")

