## Name: SDMfigures.R ##
## Author: Jorinde-M. Rieger & Inês Silva ##
## Description: Create all suplementary material's figure from the SDM's runs ##
## Date: 30th Aug 2025 ##

# Settings & libraries ---------------------------------------------------------
source("src/libraries.R") # libraries
source("src/customFunctions2.R") # functions
gc()

# paths for outputs
pathSMDoutputs <- c(# Tropical region
  "./data/sdm/tropical_SDMS",
  # Boreal region
  "C:/Users/maria/Desktop/test/boreal_SDMS")

# folder to save figure and tables on SDM outputs
SDMsFigures_dir <- file.path(output_root, "SDMsFigures")
if (!dir.exists(SDMsFigures_dir)) {
  dir.create(SDMsFigures_dir, recursive = TRUE)
}
# create subfolder for presence plots
presencePlotBase <- file.path(SMDsFigures_dir, "presencePlots")
if (!dir.exists(presencePlotBase)) {
  dir.create(presencePlotBase, recursive = TRUE)
}

# scenarios names (used for files)
scenarios <- c("Current", "ssp126_2030_tropical", "ssp126_2050_tropical", "ssp126_2100_tropical",
               "ssp585_2030_tropical", "ssp585_2050_tropical", "ssp585_2100_tropical",
               "ssp126_2030_boreal", "ssp126_2050_boreal", "ssp126_2100_boreal",
               "ssp585_2030_boreal", "ssp585_2050_boreal", "ssp585_2100_boreal")

# scenarios prettier labels
scenario_labels <- c(
  Current = "Current",
  ssp126_2030_tropical = "SSP1-2.6 (2030)",
  ssp126_2050_tropical = "SSP1-2.6 (2050)",
  ssp126_2100_tropical = "SSP1-2.6 (2100)",
  ssp585_2030_tropical = "SSP5-8.5 (2030)",
  ssp585_2050_tropical = "SSP5-8.5 (2050)",
  ssp585_2100_tropical = "SSP5-8.5 (2100)",
  ssp126_2030_boreal = "SSP1-2.6 (2030)",
  ssp126_2050_boreal = "SSP1-2.6 (2050)",
  ssp126_2100_boreal = "SSP1-2.6 (2100)",
  ssp585_2030_boreal = "SSP5-8.5 (2030)",
  ssp585_2050_boreal = "SSP5-8.5 (2050)",
  ssp585_2100_boreal = "SSP5-8.5 (2100)"
)

species_table <- read.csv("./data/traitData/CompleteMammalSpsDataframe_2025-12-20.csv", stringsAsFactors = FALSE)
target_species <- gsub(" ", ".", unique(species_table$sci_name))

# get basemaps objects
world <- ne_countries(scale = "medium", returnclass = "sf")

############
# OUTPUT 1 # Presence maps
############

# load complete GBIF occurrence data
gbif_all <- readr::read_csv("./data/sdm/GBIF_occurrences_mammals_2026-02-04.csv", show_col_types = FALSE)
gbif_all <- gbif_all[,2:5]
# carefull here not to swithc axis!!
colnames(gbif_all)[1:3] <- c("species", "Latitude", "Longitude")
gbif_all$species <- gsub(" ", ".", gbif_all$species)

presencePlots <- list()

for (path in pathSMDoutputs) {
  
  # Infer region name from folder name
  region <- if (grepl("Boreal", path, ignore.case = TRUE)) "boreal" else "tropical"
  
  # Create subfolder for this region
  region_output_dir <- file.path(presencePlotBase, region)
  if (!dir.exists(region_output_dir)) dir.create(region_output_dir, recursive = TRUE)
  
  for (sps in target_species) {
    message(paste0("Building Presence Plot for ", sps, " (", region, ")"))
    
    # presence points used in SDMs
    csv_file <- file.path(path, paste0("PresencePoints_", gsub(" ", ".", sps), "_", region, ".csv"))
    if (!file.exists(csv_file)) next
    
    presence_df <- readr::read_csv(csv_file, show_col_types = FALSE)
    colnames(presence_df)[1:2] <- c("Longitude", "Latitude")
    presence_df$Type <- "SDM Presences"
    
    # total GBIF occ records
    gbif_species <- gbif_all %>%
      dplyr::filter(species == sps) %>%
      dplyr::mutate(Type = "GBIF Occurrences")
    
    # color-blind palette
    color_map <- c("GBIF" = "#0072B2", "SDM" = "#E69F00")
    
    # plot
    p <- ggplot() +
      geom_sf(data = world, fill = "grey95", color = "black") +
      geom_point(data = gbif_species, aes(x = Longitude, y = Latitude, color = "GBIF"),
                 size = 0.4, alpha = 0.6) +
      geom_point(data = presence_df, aes(x = Longitude, y = Latitude, color = "SDM"),
                 size = 0.7, alpha = 0.8) +
      scale_color_manual(values = color_map, breaks = c("GBIF", "SDM"),
                         labels = c("GBIF occurrences", "SDM presences"), drop = FALSE) +
      ggtitle(bquote(italic(.(pretty_species_names(sps))) ~ " occurrences (" ~ .(region) ~ ")")) +
      theme_minimal(base_size = 12) +
      theme(
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill = "white", color = "grey80"),
        panel.grid = element_line(color = "grey90")
      )
    invisible(gc())
    
    # Save to region subfolder
    ggsave(
      plot = p,
      file = file.path(region_output_dir, paste0("PresencesPlot_", gsub(" ", ".", sps), ".png")),
      bg = 'white', width = 300, height = 150, units = "mm", dpi = 1200
    )
  }
}

################
# EXTRA OUTPUT # Number of available presences over time
################

species_list <- sort(unique(gbif_all$species))
species_groups <- split(
  species_list,
  # adjust number of breaks according to teh number of species being included
  cut(seq_along(species_list), breaks = 4, labels = FALSE))

for (i in seq_along(species_groups)) {
  
  sp_subset <- species_groups[[i]]
  
  p <- gbif_all %>%
    dplyr::filter(year >= 2015,
                  species %in% sp_subset) %>%
    count(species, year) %>%
    ggplot(aes(x = year, y = n)) +
    geom_col(fill = "#0072B2") +
    facet_wrap(~ species, scales = "free_y") +
    labs(title = paste("Occurrences over time per species (Part", i, ")"), 
         x = "Year", y = "Count") +
    theme_minimal(base_size = 10) +
    theme(strip.text = element_text(face = "italic"))
  
  ggsave(plot = p, file = file.path(SDMsFigures_dir, paste0("gbifMammalOccurrencesOverTime_part_", i, ".png")),
    bg = "white", width = 300, height = 200, units = "mm", dpi = 300)
}

############
# OUTPUT 2 # Variable Importance for Ensemble Models Table
############

# create data frame for all variable importance of EM
all_var_importance_em <- data.frame() 

for (path in pathSMDoutputs) {
  for (species in target_species) {
    
    # Infer region name from folder name
    region <- if (grepl("Boreal", path, ignore.case = TRUE)) "boreal" else "tropical"
    
    # find teh path for the .csv with metrics value
    file_path <- file.path(path,
                           paste0("VarImportanceEM_", gsub(" ", ".", species), "_", region, ".csv"))
    if (file.exists(file_path)) {
      var_imp <- read.csv(file_path)
      # add species column 
      var_imp$species <- species  
      # add biome column
      var_imp$biome <- ifelse(grepl("Boreal", path, ignore.case = TRUE), "Boreal Forests/Taiga",
                              ifelse(grepl("Tropical", path, ignore.case = TRUE), "Tropical & Subtropical Moist Broadleaf Forests", NA))
      # put everything together
      all_var_importance_em <- rbind(all_var_importance_em, var_imp)
    }
  }
}

#unique(all_var_importance_em$species)
# filter for algo == "EMmean"
var_importance_em_filtered <- all_var_importance_em %>%
  filter(algo == "EMmean")

# summarize mean and SD for each species and variable
importance_summary <- var_importance_em_filtered %>%
  group_by(biome, species, expl.var) %>%
  dplyr::summarize(
    mean_importance = mean(var.imp, na.rm = TRUE), # Calculate mean
    sd_importance = sd(var.imp, na.rm = TRUE), # Calculate standard deviation
    .groups = "drop") %>%
  pivot_longer(cols = c(mean_importance, sd_importance), names_to = "metrics", values_to = "importance") %>%
  mutate(metrics = ifelse(metrics == "mean_importance", "Mean", "SD")) %>%
  pivot_wider(names_from = expl.var, values_from = importance) %>%
  dplyr::arrange(biome, species, metrics)

# write table to .csv and .xslx (for easy copy paste later)
write.csv(importance_summary,
          file = file.path(SDMsFigures_dir,
                           paste0("VariableImportanceSummaryTable", Sys.Date(), ".csv")), row.names = FALSE)
writexl::write_xlsx(importance_summary,
                    path = file.path(SMDsFigures_dir,
                                     paste0("VariableImportanceSummaryTable", Sys.Date(), ".xlsx")))

############
# OUTPUT 3 # Evaluation metrics table for Ensemble Models
############

all_EvalScoresEM <- data.frame() 

for (path in pathSMDoutputs) {
  for (species in target_species) {
    
    # get region name from folder name
    region <- if (grepl("Boreal", path, ignore.case = TRUE)) "boreal" else "tropical"
    
    # find teh path for the .csv with metrics value
    file_path <- file.path(path, paste0("EvalScoresEM_", gsub(" ", ".", species), "_", region, ".csv"))
    if (file.exists(file_path)) {
      eval_scores <- read.csv(file_path)
      # add species column 
      eval_scores$species <- species
      # add biome column
      eval_scores$biome <- ifelse(grepl("Boreal", path, ignore.case = TRUE), "Boreal Forests/Taiga",
                                  ifelse(grepl("Tropical", path, ignore.case = TRUE), "Tropical & Subtropical Moist Broadleaf Forests", NA))
      # bind all together
      all_EvalScoresEM <- rbind(all_EvalScoresEM, eval_scores)
      rm(eval_scores)
    }
  }
}

# summarize calibration scores 
eval_table_em <- all_EvalScoresEM %>%
  group_by(biome, species, metric.eval) %>%
  # get mean value for the calibration metric
  summarise(
    mean_validation = mean(validation, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  pivot_wider(names_from = metric.eval, values_from = mean_calibration)

# write table to .csv and .xslx (for easy copy paste later)
write.csv(eval_table_em,
          file = file.path(SDMsFigures_dir,
                           paste0("EnsembleCalibrationScoresSummaryTable", Sys.Date(), ".csv")), row.names = FALSE)
writexl::write_xlsx(eval_table_em,
                    path = file.path(SDMsFigures_dir,
                                     paste0("EnsembleCalibrationScoresSummaryTable", Sys.Date(), ".xlsx")))

############
# OUTPUT 4 # Continuous landscapes 
############

# Tropical Biome
extent_tropical_name <- "Tropical & Subtropical Moist Broadleaf Forests" # full name of the biome
extent_tropical_sf <- load_biome(extent_tropical_name)
extent_tropical_sp <- terra::vect(extent_tropical_sf) 

# Boreal Biome
extent_boreal_name <- "Boreal Forests/Taiga" # full name of the biome
extent_boreal_sf <- load_biome(extent_boreal_name)
extent_boreal_sp <- terra::vect(extent_boreal_sf) # Convert the sf to a spatial object


# define base folder for continuous outputs
continuous_base <- file.path(SDMsFigures_dir, "ContinuousLandscapes")
if (!dir.exists(continuous_base)) dir.create(continuous_base, recursive = TRUE)

# Storage for plots
continuous_list <- list()

# first loop to produce a continuous map per sps and scenario
for (path in pathSMDoutputs) {
  for (species in target_species) {
    for (scenario in scenarios) {
      
      # prep rasters' paths
      if (scenario == "Current") {
        tif_path <- file.path(
          path,
          paste0("proj_Current_EM_", gsub(" ", ".", species), "_continuous.tif")) } 
      else {
        tif_path <- file.path(
          path,
          paste0("proj_", scenario, "_", gsub(" ", ".", species), "_continuous.tif")) }
      
      # continue if the file exists
      if (!file.exists(tif_path)) {
        next  # otherwise skip to next iteration
      }
      
      # load raster
      rast_obj <- rast(tif_path)
      
      # choose mask based on path (either boreal or tropical)
      if (grepl("boreal", path, ignore.case = TRUE)) {
        rast_crop <- crop(rast_obj, extent_boreal_sp)
        rast_mask <- mask(rast_crop, extent_boreal_sp)}
      else if (grepl("tropical", path, ignore.case = TRUE)) {
        rast_crop <- crop(rast_obj, extent_tropical_sp)
        rast_mask <- mask(rast_crop, extent_tropical_sp)}
      else {
        rast_crop <- rast_obj
        rast_mask <- rast_obj}
      
      # normalize raster by 1000 and clamp to [0,1]
      rast_norm <- rast_mask / 1000
      rast_norm <- clamp(rast_norm, lower = 0, upper = 1, values = TRUE)   # terra::clamp
      rm(rast_mask)
      invisible(gc())
      
      # convert to df
      cont_df <- as.data.frame(rast_norm, xy = TRUE, na.rm = TRUE)
      names(cont_df)[3] <- "suitability"
      
      # progress message
      message(paste0("Building individual plots for ", species, " in ", scenario, " scenario"))
      
      # plot SDM results
      p <- ggplot() +
        geom_sf(data = world, fill = "grey90", color = "black", linewidth = 0.2) +
        geom_tile(data = cont_df, aes(x = x, y = y, fill = suitability)) +
        tidyterra::scale_fill_terrain_c(name = "Suitability", limits = c(0, 1)
        ) +
        labs(x = "Longitude", y = "Latitude", 
             title = scenario_labels[scenario]) +
        theme_minimal()
      
      # figure out region hint from path
      if (grepl("boreal", path, ignore.case = TRUE)) {
        region_hint <- "boreal"} else if (grepl("tropical", path, ignore.case = TRUE)) {
          region_hint <- "tropical"} else {
            region_hint <- "unknown"}
      
      # Store plot with region in the name
      continuous_list[[paste(species, scenario, region_hint, sep = "_")]] <- p
    }
  }
}

species_layouts <- list()

for (species in target_species) {
  
  message("Building continuous SDM landscapes for ", species)
  
  # detect which regions exist for this species
  species_keys <- names(continuous_list)[grepl(species, names(continuous_list))]
  regions <- unique(sub(".*_(tropical|boreal)$", "\\1", species_keys))
  
  for (region in regions) {
    
    # safely extract plots for this species × region
    p_current   <- continuous_list[[paste(species, "Current", region, sep = "_")]]
    p_126_2030  <- continuous_list[[paste(species, "ssp126_2030", region, sep = "_", region)]]
    p_126_2050  <- continuous_list[[paste(species, "ssp126_2050", region, sep = "_", region)]]
    p_126_2100  <- continuous_list[[paste(species, "ssp126_2100", region, sep = "_", region)]]
    p_585_2030  <- continuous_list[[paste(species, "ssp585_2030", region, sep = "_", region)]]
    p_585_2050  <- continuous_list[[paste(species, "ssp585_2050", region, sep = "_", region)]]
    p_585_2100  <- continuous_list[[paste(species, "ssp585_2100", region, sep = "_", region)]]
    
    # if any key plots are missing, skip
    if (is.null(p_current)) {
      message("Skipping ", species, " (", region, ") because SDM outputs are missing.")
      next
    }
    
    # use patchwork to layout plots
    species_plot <- 
      (p_current | (p_126_2030 / p_585_2030) | (p_126_2050 / p_585_2050) | (p_126_2100 / p_585_2100))  +
      plot_layout(widths = c(1, 1, 1, 1),
                  guides = "collect") +
      plot_annotation(title = bquote(italic(.(species)) ~ "-" ~ .(region))) &
      theme(legend.position = "bottom")
    
    # store a per-species × region patchwork
    species_layouts[[paste(species, region, sep = "_")]] <- species_plot
    
    # create subfolder for region if missing
    region_dir <- file.path(continuous_base, region)
    if (!dir.exists(region_dir)) dir.create(region_dir, recursive = TRUE)
    
    # save as .png file
    ggsave(
      plot = species_plot,
      file = file.path(region_dir,
                       paste0("SDMlandscapes_", species, "_", region, ".png")),
      bg = 'white', width = 400, height = 150, units = "mm", dpi = 1200
    )
  }
}
