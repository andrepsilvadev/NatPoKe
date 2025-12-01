## Name: readMetaRangeOutput.R ##
## Author: Inês Silva ##
## Description: read metaRange ouputs and perform basic diagnostics
## Date: November 25th 2025 ##

source("./src/libraries.R")
source("./src/customFunctions.R")  

################
# FULL DATASET # 
################

# STEP 1 # Load all runs data

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

# STEP 2 # Combine all regions data together

datasets <- list(europe_SSP5, northamerica_SSP5, asia_SSP5, africa_SSP5, southamerica_SSP5,
                 europe_SSP1, northamerica_SSP1, asia_SSP1, africa_SSP1, southamerica_SSP1)

TNIND_yr <- do.call("rbind", datasets)
# check for species names
#unique(TNIND_yr$species)

# clean up
rm(europe_SSP5, northamerica_SSP5, asia_SSP5, africa_SSP5, southamerica_SSP5,
   europe_SSP1, northamerica_SSP1, asia_SSP1, africa_SSP1, southamerica_SSP1)
invisible(gc())

# get correspondence between species names and functional group
combined_traits_data <- read_csv(here("data", "mammalTraits_2025-03-17.csv")) %>% 
  dplyr::filter(BIOME_NAME %in%  gsub("[/& ]", "", c("Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga"))) 

# clean up the dataset
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

# STEP 3 #  Write complete dataset into .csv (RAW DATA)

write_csv(TNIND_yr, 
          file = "./output/completeMetaRangeRun_31Oct25.csv")

################################
# DIAGNOSTIC POPULATION TRENDS # 
################################

# STEP 1 # Build and excel file

# create folder to save diagnostics
diagnostics <- file.path("./output/metaRangeRuns/diagnostics2")
dir.create(diagnostics, showWarnings = TRUE)

# get number of sps per combin
n_sps <- TNIND_yr %>% 
  group_by(scenario, biome, region) %>% 
  summarise(n_species = n_distinct(species), .groups = "drop")

# population trends ------------------------------------------------------------
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


# STEP 2 # Plot population trends over time 

### per biome × region × scenario (all species together)
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
  # save plot
  ggsave(filename = fname, plot = p,
         bg = "white", width = 350, height = 210, units = "mm", dpi = 300)
  rm(df, p)
  gc()
}


### per species ----------------------------------------------------------------
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
    plot_annotation(title = paste("Temporal dynamics of TNIND", "\nBiome:", biome_to_plot,
                                  "| Region:", region_to_plot, "| Scenario:", scen_to_plot))
  
  # filename 
  filename <- paste0("TNIND_", gsub(" ", "_", sp), "_", gsub(" ", "_", biome_to_plot), "_",
    gsub(" ", "_", region_to_plot), "_", gsub(" ", "_", scen_to_plot), ".png")
  
  # save plots
  ggsave(filename, combined_plot, path = diagnostics,
         width = 16, height = 6, dpi = 300)
  
  message("Saved: ", filename)
  
  gc(rm(sp_data, sp_mean, p_left, p_right, combined_plot,
        biome_to_plot, region_to_plot, scen_to_plot, sp))
}

############################
# COMPLETE TRAIT DATAFRAME #
############################

runs_path <- "/mnt/data/maria/NatPoKe/output/metaRangeRuns"

# Europe
EuropeSSP1_input = file.path(runs_path, "Europe_ssp126_31Oct25/Inputs")
EuropeSSP5_input = file.path(runs_path, "Europe_ssp585_31Oct25/Inputs")

# North America
NorthAmericaSSP1_input = file.path(runs_path, "NorthAmerica_ssp126_31Oct25/Inputs")
NorthAmericaSSP5_input = file.path(runs_path, "NorthAmerica_ssp585_31Oct25/Inputs")

# South America
SouthAmericaSSP1_input = file.path(runs_path, "SouthAmerica_ssp126_31Oct25/Inputs")
SouthAmericaSSP5_input = file.path(runs_path, "SouthAmerica_ssp585_31Oct25/Inputs")

# Africa
AfricaSSP1_input = file.path(runs_path, "Africa_ssp126_31Oct25/Inputs")
AfricaSS5_input = file.path(runs_path, "Africa_ssp585_31Oct25/Inputs")

# Asia
AsiaSSP1_input = file.path(runs_path, "Asia_ssp126_31Oct25/Inputs")
AsiaSSP5_input = file.path(runs_path, "Asia_ssp585_31Oct25/Inputs")

input_dirs <- c(EuropeSSP1_input, NorthAmericaSSP1_input, SouthAmericaSSP1_input, AfricaSSP1_input, AsiaSSP1_input,
                EuropeSSP5_input, NorthAmericaSSP5_input, SouthAmericaSSP5_input, AfricaSSP5_input, AsiaSSP5_input)

# Read all CSVs, merge, and keep only one row per species
all_traits <- map_dfr(input_dirs, function(input_dir) {
  csv_file <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)[1]
  read_csv(csv_file, show_col_types = FALSE) %>% 
    select(!Index)   # remove Index column if present
}) %>% 
  distinct(Species, .keep_all = TRUE)  # ensure one row per species

# Save merged .xlsx
write_xlsx(all_traits, file.path(runs_path, "completeTraitDataframe_allSps.xlsx"))

#################################
# AVERAGE SUITABILITY OVER TIME #
#################################


# ---- list input directories ----
input_dirs <- c(
  "C:/Users/maria/Desktop/Inputs/",
  "C:/Users/maria/Desktop/Inputs2/"
)

# loop over folders and produce one plot per folder
for (dir in input_dirs) {
  
  # list tif files in this folder
  files <- list.files(dir, pattern = "_reprojectedKm\\.tif$", full.names = TRUE)
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
  folder_name <- basename(normalizePath(dir))
  
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
  
  #print(p)
  
  ggsave(filename = paste0(folder_name, "_species_timeseries.png"),
         plot = p, width = 10, height = 7, dpi = 300)
}




###############################################################33

directory_pairs <- list(
  # North America SSP1
  northamerica_SSP1 = c(input = "./output/metaRangeRuns/NorthAmerica_ssp126_31Oct25/Inputs/",
                        output = "./output/metaRangeRuns/NorthAmerica_ssp126_31Oct25/Outputs/"),
  # North America SSP5
  northamerica_SSP5 = c(input = "./output/metaRangeRuns/NorthAmerica_ssp585_31Oct25/Inputs/",
                        output = "./output/metaRangeRuns/NorthAmerica_ssp585_31Oct25/Outputs/")
  
  # Europe SSP1
  europe_SSP1 = c(input = "./output/metaRangeRuns/Europe_ssp126_31Oct25/Inputs/",
                  output = "./output/metaRangeRuns/Europe_ssp126_31Oct25/Outputs/"),
  # Europe SSP5
  europe_SSP5 = c(input = "./output/metaRangeRuns/Europe_ssp585_31Oct25/Inputs/",
                  output = "./output/metaRangeRuns/Europe_ssp585_31Oct25/Outputs/"),
  #
  
  
)