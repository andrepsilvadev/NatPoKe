## Name: SDMfigures.R ##
## Author: Jorinde-M. Rieger & Inês Silva ##
## Description: Create all suplementary material's figure from the SDM's runs ##
## Date: 30th Aug 2025 ##

# Settings & libraries ---------------------------------------------------------
source("src/libraries.R") # libraries
source("src/customFunctions2.R") # functions


# paths for outputs
pathSMDoutputs <- c(# Tropical region
                    "C:/Users/User/OneDrive - Universidade de Lisboa (1)/NatPokeTropical",
                    # Boreal region
                    "C:/Users/User/OneDrive - Universidade de Lisboa (1)/NatPokeBoreal")

# new folder to save figures
outputPathSDMfigures <- "output/SDMoutputs_09Sept25" # adapt if needed
if (!dir.exists(outputPathSDMfigures)) {
  dir.create(outputPathSDMfigures, recursive = TRUE)
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

# target species
targetSpecies <- c("Alces alces", "Bison bonasus", "Cervus elaphus", 
                   "Sus scrofa","Lynx rufus", "Canis lupus", "Rangifer tarandus",
                   "Gorilla gorilla", "Orycteropus afer", "Pan troglodytes", 
                   "Panthera onca", "Crocuta crocuta", "Syncerus caffer",
                   "Panthera leo","Loxodonta africana")

# get basemaps objects
extent <- "GlobalTerrestrial"
world <- ne_countries(scale = "medium", returnclass = "sf")

############
# OUTPUT 1 # Presence maps
############

presencePlots <- list()

for (path in pathSMDoutputs) {
  
  for (species in targetSpecies) {
    
    message(paste0("Building Presence Plot for ", species))
    # file path for the presence .csv
    csv_file <- file.path(path,
                          paste0("PresencePoints_", gsub(" ", ".", species), "_", extent, ".csv"))
    
    if (file.exists(csv_file)) {
      # read presence points
      presence_df <- readr::read_csv(csv_file, show_col_types = FALSE)
      colnames(presence_df)[1:2] <- c("Longitude", "Latitude")
      presence_df$Type <- "Presence Points"
      
      # plot occurrences over world map
      p <- ggplot() +
        geom_sf(data = world, fill = "grey95", color = "black") +
        geom_point(
          data = presence_df,
          aes(x = Longitude, y = Latitude),
          color = "red", size = 0.3, alpha = 0.7
        ) +
        ggtitle(bquote("Presence points of " * italic(.(species)) * " since 2015")) +
        theme_minimal()
      
      # save all  species plots inside a list
      presencePlots[[gsub(" ", ".", species)]] <- p
      
      # save plot as image
      ggsave(plot = p,
             file = file.path(outputPathSDMfigures, "presencePlots", paste0("PresencesPlot_", gsub(" ", ".", species), ".png")),
             bg = 'white', width = 300, height = 150, units = "mm", dpi = 1200, 
             #compression ="lzw"
      )
    }
  }
}

############
# OUTPUT 2 # Variable Importance for Ensemble Models Table
############

# create data frame for all variable importance of EM
all_var_importance_em <- data.frame() 

for (path in pathSMDoutputs) {
  for (species in targetSpecies) {
    # find teh path for the .csv with metrics value
    file_path <- file.path(path,
                           paste0("VarImportanceEM_", gsub(" ", ".", species), "_", extent, ".csv"))
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
          file = file.path(outputPathSDMfigures,
                           paste0("VariableImportanceSummaryTable", Sys.Date(), ".csv")), row.names = FALSE)
writexl::write_xlsx(importance_summary,
                    path = file.path(outputPathSDMfigures,
                                     paste0("VariableImportanceSummaryTable", Sys.Date(), ".xlsx")))

############
# OUTPUT 3 # Evaluation metrics table for Ensemble Models
############

all_EvalScoresEM <- data.frame() 

for (path in pathSMDoutputs) {
  for (species in targetSpecies) {
    # find teh path for the .csv with metrics value
    file_path <- file.path(path, paste0("EvalScoresEM_", gsub(" ", ".", species), "_", extent, ".csv"))
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
    mean_calibration = mean(calibration, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  pivot_wider(names_from = metric.eval, values_from = mean_calibration)

# write table to .csv and .xslx (for easy copy paste later)
write.csv(eval_table_em,
          file = file.path(outputPathSDMfigures,
                           paste0("EnsembleCalibrationScoresSummaryTable", Sys.Date(), ".csv")), row.names = FALSE)
writexl::write_xlsx(eval_table_em,
                    path = file.path(outputPathSDMfigures,
                                     paste0("EnsembleCalibrationScoresSummaryTable", Sys.Date(), ".xlsx")))



#################### OLD CODE - DELETE LATER IF UNECESSARY #####################
# all_EvalScoresEM <- data.frame() 
# 
# for (path in pathSMDoutputs) {
#   
#   for (species in targetSpecies) {
#     
#     file_path <- file.path(path, paste0("EvalScoresEM_", gsub(" ", ".", species), "_", extent, ".csv"))
#     
#     if (file.exists(file_path)) {
#       eval_scores <- read.csv(file_path)
#       eval_scores$species <- species  # Add species column if not present
#       all_EvalScoresEM <- rbind(all_EvalScoresEM, eval_scores)
#     }
#   }
# }
# 
# eval_plot_em <- ggplot(all_EvalScoresEM, aes(x = species, y = calibration, fill = metric.eval)) +
#   geom_boxplot() +
#   labs(title = "Evaluation Metrics for Ensemble Models (EM)",
#        x = "Species",
#        y = "Calibration Score",
#        fill = "Metric") +
#   scale_fill_viridis_d(name = "Metric") +  
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
# 
# ggsave(plot = eval_plot_em,
#        file = file.path(outputPathSDMfigures, "EvaluationMetrics_EM_exampleSpecies.tif"),
#        bg = 'white', width = 300, height = 200, units = "mm", dpi = 1200, compression ="lzw")
#################### OLD CODE - DELETE LATER IF UNECESSARY #####################

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



# Storage for plots
continuous_list <- list()

# first loop to produce a continuous map per sps and scenario
for (path in pathSMDoutputs) {
  for (species in targetSpecies) {
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
      
      # convert to df
      cont_df <- as.data.frame(rast_mask, xy = TRUE, na.rm = TRUE)
      names(cont_df)[3] <- "suitability"
      
      # plot SDM results
      p <- ggplot() +
        geom_sf(data = world, fill = "grey90", color = "black", linewidth = 0.2) +
        geom_tile(data = cont_df, aes(x = x, y = y, fill = suitability)) +
        tidyterra::scale_fill_terrain_c(name = "Suitability", limits = c(0, 1000)) +
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

#################### OLD CODE - DELETE LATER IF UNECESSARY #####################
# # Storage for plots
# continuous_list <- list()
# 
# # first for loop to produce a continuos map per sps and scenario
# for (path in pathSMDoutputs) {
#   
#   for (species in targetSpecies) {
#     
#     for (scenario in scenarios) {
#       
#       # prep rasters' paths
#       if (scenario == "Current") {
#         tif_path <- file.path(
#           path,
#           paste0("proj_Current_EM_", gsub(" ", ".", species), "_continuous.tif"))
#       } else {
#         tif_path <- file.path(
#           path,
#           paste0("proj_", scenario, "_", gsub(" ", ".", species), "_continuous.tif"))
#       }
#       
#       # only continue if the file exists
#       if (!file.exists(tif_path)) {
#         #message("File not found: ", tif_path)
#         next  # skip to next iteration
#       }
#       
#       # prep the rasters
#       rast_obj <- rast(tif_path)
#       rast_crop <- crop(rast_obj, extent_tropical_sp)
#       rast_mask <- mask(rast_crop, extent_tropical_sp)
#       
#       cont_df <- as.data.frame(rast_mask, xy = TRUE, na.rm = TRUE)
#       names(cont_df)[3] <- "suitability"
#       
#       # plot SDM results
#       p <- ggplot() +
#         geom_sf(data = world, fill = "grey90", color = "black", linewidth = 0.2) +
#         geom_tile(data = cont_df, aes(x = x, y = y, fill = suitability)) +
#         tidyterra::scale_fill_terrain_c(name = "Suitability", limits = c(0, 1000)) +
#         labs(x = "Longitude", y = "Latitude", 
#              title = scenario_labels[scenario]) +
#         theme_minimal()
#       
#       # Store plot
#       continuous_list[[paste(species, scenario, sep = "_")]] <- p
#     }
#   }
# }
#################### OLD CODE - DELETE LATER IF UNECESSARY #####################

species_layouts <- list()

for (species in targetSpecies) {
  
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
    
    # If any key plots are missing, skip
    if (is.null(p_current)) {
      message("Skipping ", species, " (", region, ") because SDM outputs are missing.")
      next
    }
    
    # Patchwork layout (same structure as before)
    species_plot <- 
      (p_current | (p_126_2030 / p_585_2030) | (p_126_2050 / p_585_2050) | (p_126_2100 / p_585_2100))  +
      plot_layout(widths = c(1, 1, 1, 1),
                  guides = "collect") +
      plot_annotation(title = bquote(italic(.(species)) ~ "-" ~ .(region))) &
      theme(legend.position = "bottom")
    
    # Store per-species × region patchwork
    species_layouts[[paste(species, region, sep = "_")]] <- species_plot
    
    # Save to file
    ggsave(
      plot = species_plot,
      file = file.path(outputPathSDMfigures, "ContinuousLandscapes",
                       paste0("SDMlandscapes_", species, "_", region, ".png")),
      bg = 'white', width = 400, height = 150, units = "mm", dpi = 1200
    )
  }
}