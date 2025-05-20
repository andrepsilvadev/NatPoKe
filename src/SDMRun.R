## Name: SDMRun.R ##
## Author: Jorinde-M. Rieger ##
## Description: Creates output of SDM results in R ##
## Date: May 19th 2025 ##

# Settings & libraries -----------------------------------------------------------------
source("~/NatPoKe9/src/libraries.R") # libraries
# When using the pipeline for the first time run taxaOcccurence.R and adapt species and user-login for GBIF Database:
#source("~/NatPoKe9/src/TaxaOccurence.R") # downloads taxa occurences from GBIF Database
source("~/NatPoKe9/src/customFunctions2.R") # functions
source("~/NatPoKe9/src/inputClimate.R") # format and reads input raster landscapes, adapt: scenarios, years & variables
source("~/NatPoKe9/src/inputSpeciesData.R") # format and reads input data based on TaxaOccurence.R output
source("~/NatPoKe9/src/SDM.R") # function to format data and SDM

# Load dataset and format species occurence -----------------------------------------------------------------
# Define the biome and continents
# Tropical Biome
biome_name <- "Tropical & Subtropical Moist Broadleaf Forests"
biome_name_short <- "Tropical Biome"
continent_names <- c("Central & South America", "Africa", "Asia")
continent_title <- c("Central & South America", "Africa", "Asia")

# Boreal Biome
biome_name <- "Boreal Forests/Taiga"
biome_name_short <- "Boreal Biome"
continent_names <- c("North America", "Europe")
continent_title <- c("North America", "Europe & Asia")

# Load and select the biome shapefile
biome_sf <- load_select_biome(biome_name)
biome_sp <- vect(biome_sf)

# Load and select the continents
continents <- load_select_continents(continent_names)

# Transform continent CRS to match the raster CRS
continents <- st_transform(continents, crs = st_crs(biome_sf))

# Define continent geometries
continent_geoms <- setNames(lapply(continent_names, function(continent) {
  continents %>% dplyr::filter(continent == !!continent)
}), continent_names)

# Crop and mask trainingLandscapes to biome and selected continents
trainingLandscapesContinents <- lapply(continent_geoms, function(continent_geom) {
  cropped_training <- crop_mask_raster(trainingLandscapes, biome_sp)
  crop_mask_raster(cropped_training, continent_geom)
})

# Example access a training landscape
plot(trainingLandscapesContinents[["Europe"]])

# Loop through each predictionLandscapes raster in the list and mask and crop to biome and continent
predictionLandscapesContinents <- list()
for (i in seq_along(predictionLandscapes)) {
  landscape_name<- names(predictionLandscapes)[i]
  predictionLandscapesContinents[[landscape_name]] <- lapply(continent_geoms, function(continent_geom){
    cropped_prediction <- crop_mask_raster(predictionLandscapes[[i]], biome_sp)
    crop_mask_raster(cropped_prediction, continent_geom)
  })
}

# Example access a training landscape
plot(predictionLandscapesContinents[["ssp126_2071-2100"]][["Europe"]])

# Run the SDMensembleMultiSpecies function -----------------------------------------------------------------
# Select the name of the studied species
targetSpecies <- c("Alces alces", "Canis lupus")

# Initialize a list to store results for each continent
resultsByContinents <- list()
for (continent_name in names(trainingLandscapesContinents)){
  print(paste("Processing continent:", continent_name))
  training_landscape <- trainingLandscapesContinents[[continent_name]]
  prediction_landscape <- lapply(predictionLandscapesContinents, function(x) x[[continent_name]])
  
  print(paste("Training landscape for", continent_name, ":"))
  print(training_landscape)
  print(paste("Prediction landscapes for", continent_name, ":"))
  print(prediction_landscape)
  
  # Format species occurence data for the current continent
  speciesData <- formatInputDataFrame(
    speciesData = speciesDataOcc,
    targetSpecies = targetSpecies,
    landscape = training_landscape
  )
  # Run the SDM ensembleMulitSpecies function for the current continent
  resultsByContinents[[continent_name]] <- SDMensembleMultiSpecies(
    targetSpecies = targetSpecies,
    speciesData = speciesData,
    trainingLandscapes = training_landscape,
    predictionLandscapes = prediction_landscape,
    biome_name = continent_name
  )
}

# Example access of results
resultsByContinents$`North America`$biomodData[["Alces alces"]]

# SDM evaluation metrics ------------------------------------------------
# Plot Evaluation Metrics for single models
# Filter for TSS metric
eval_scores_tss <- results$evaluationScores[eval_scores_combined$metric.eval == "TSS", ]

# Plot evaluation metrics
eval_plot <- ggplot(eval_scores_tss, aes(x = species, y = validation, fill = algo)) +
  geom_boxplot() +
  labs(
    title = "Evaluation Metrics (TSS) for Target Species",
    x = "Species",
    y = "TSS",
    fill = "Algorithm"
  ) +
  scale_fill_viridis_d(name = "Metric") +  
  theme_minimal()
print(eval_plot)
ggsave(file.path(output_folder, "EvaluationMetrics_TSS.png"), plot = eval_plot, width = 10, height = 6, dpi = 300)


# Plot evaluation metrics for ensemble models
eval_plot_em <- ggplot(results$evaluationScoresEM, aes(x = species, y = calibration, fill = metric.eval)) +
  geom_boxplot() +
  labs(
    title = "Evaluation Metrics for Ensemble Models (EM)",
    x = "Species",
    y = "Value",
    fill = "Metric"
  ) +
  scale_fill_viridis_d(name = "Metric") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
print(eval_plot_em)
ggsave(file.path(output_folder, "EvaluationMetrics_EnsembleModels.png"), plot = eval_plot_em, width = 10, height = 6, dpi = 300)



# Plot evaluation metrics for Run1 and Run2
# Filter the evaluation scores for the metrics ROC and TSS
eval_scores_filtered <- results$evaluationScores %>%
  filter(metric.eval %in% c("ROC", "TSS")) %>%  # Keep only ROC and TSS metrics
  mutate(run = ifelse(grepl("RUN1", full.name), "RUN1", 
                      ifelse(grepl("RUN2", full.name), "RUN2", NA)))  # Extract run information

# Ensure the run column is not NA
eval_scores_filtered <- eval_scores_filtered %>% filter(!is.na(run))

# Create the boxplot
eval_plot <- ggplot(eval_scores_filtered, aes(x = species, y = validation, fill = metric.eval)) +
  geom_boxplot(position = position_dodge(1)) +
  facet_wrap(~ run, ncol = 2) +  # Create separate facets for RUN1 and RUN2
  labs(
    title = "Evaluation Metrics (ROC and TSS) for Single Models",
    x = "Species",
    y = "Validation Score",
    fill = "Metric"
  ) +
  scale_fill_viridis_d(name = "Metric") +  
  theme_minimal()
print(eval_plot)

# Save the plot
ggsave(file.path(output_folder, "EvaluationMetrics_SingleModels_RUN1_RUN2.png"), plot = eval_plot, width = 12, height = 8, dpi = 300)

# Variable importance ------------------------------------------------  
# Extract variable importance data
var_importance_em <- results$variableImportanceEM
# Summarize the data to calculate mean and SD for each species and variable
importance_summary <- var_importance_em %>%
  group_by(species, algo, expl.var) %>% # filter for EMmean #
  summarize(
    mean_importance = mean(var.imp, na.rm = TRUE),
    sd_importance = sd(var.imp, na.rm = TRUE),
    .groups = "drop")
# Reshape the data to create a table with species as rows and variables as columns
# Add a row for mean and a row for SD for each species
importance_table <- importance_summary %>%
  pivot_longer(cols = c(mean_importance, sd_importance), names_to = "metrics", values_to = "importance") %>%
  mutate(metrics = ifelse(metrics == "mean_importance", "Mean", "SD")) %>%
  pivot_wider(names_from = expl.var, values_from = importance) %>%
  arrange(species, metrics)
print(importance_table)
# Save the table as a CSV file
#write.csv(importance_table, file = file.path(output_folder, "VariableImportanceSummary.csv"), row.names = FALSE)


# Filter the data for algo == "EMmean"
var_importance_em_filtered <- results$variableImportanceEM %>%
  filter(algo == "EMmean")  # Keep only rows where algo is EMmean

# Summarize the data to calculate mean and SD for each species and variable
importance_summary <- var_importance_em_filtered %>%
  group_by(species, expl.var) %>%  # Group by species and variable
  summarize(
    mean_importance = mean(var.imp, na.rm = TRUE),  # Calculate mean
    sd_importance = sd(var.imp, na.rm = TRUE),      # Calculate standard deviation
    .groups = "drop"
  )

# Reshape the data to create a table with species as rows and variables as columns
# Add a row for mean and a row for SD for each species
importance_table <- importance_summary %>%
  pivot_longer(cols = c(mean_importance, sd_importance), names_to = "metrics", values_to = "importance") %>%
  mutate(metrics = ifelse(metrics == "mean_importance", "Mean", "SD")) %>%
  pivot_wider(names_from = expl.var, values_from = importance) %>%
  arrange(species, metrics)

print(importance_table)

# Save the table as a CSV file and Excel file
write.csv(importance_table, file = file.path(output_folder, "VariableImportanceSummary.csv"), row.names = FALSE)
write_xlsx(importance_table, path = file.path(output_folder, "VariableImportanceSummary.xlsx"))


# Current Landscapes ------------------------------------------------
# Loop through each species and plot the current ensemble forecast (EMmean)
for (species in names(results$biomodEC)) {
  # Extract the BIOMOD_EnsembleForecasting object for the current conditions
  ensemble_current <- results$biomodEC[[species]]
  
  # Extract the SpatRaster for the `EMmean` ensemble model
  raster <- get_predictions(ensemble_current)
  
  # Ensure only the `EMmean` layer is selected
  emmean_layer <- raster[[grep("EMmean", names(raster))]]
  
  # Plot the raster with a title
  plot(emmean_layer,
       main = paste(species, "- EMmean - Current Conditions"),
       col = terrain.colors(100))
  
  # Optionally save the plot as a PNG file
  png(
    filename = file.path(output_folder, paste0("CurrentSuitability_", species, ".png")),
    width = 2000,
    height = 1500,
    res = 300
  )
  plot(emmean_layer,
       main = paste(species, "- EMmean - Current Conditions"),
       col = terrain.colors(100))
  dev.off()
}

# Initialize a list to store plots for each species
current_plots <- list()

# Loop through each species to create current suitability plots
for (species in names(results$biomodEC)) {
  # Extract the BIOMOD_EnsembleForecasting object for the current conditions
  ensemble_current <- results$biomodEC[[species]]
  
  # Extract the SpatRaster for the `EMmean` ensemble model
  raster <- get_predictions(ensemble_current)
  emmean_layer <- raster[[grep("EMmean", names(raster))]]
  
  # Convert the raster to a data frame for ggplot
  raster_df <- as.data.frame(emmean_layer, xy = TRUE, na.rm = TRUE)
  colnames(raster_df)[3] <- "value"  # Rename the value column
  
  # Create a ggplot object for current suitability
  current_plot <- ggplot(raster_df, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    scale_fill_terrain_c(name = "Prediction") +
    labs(
      title = paste(species, "- Current Conditions"),
      x = "Longitude",
      y = "Latitude"
    ) +
    coord_fixed() +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 10),
      legend.position = "right"
    )
  
  # Add the plot to the list
  current_plots[[species]] <- current_plot
}

# Combine all current plots into a single grid
combined_current_plot <- grid.arrange(grobs = current_plots, ncol = 2)

# Save the combined plot
ggsave(
  filename = file.path(output_folder, "CurrentSuitability_AllSpecies.png"),
  plot = combined_current_plot,
  width = 16,
  height = 8,
  dpi = 300
)

# Plot Presence Points for multiple species ------------------------------------------------
# Initialize a list to store ggplot objects for each species
presencePlots <- list()

# Loop through each species to create presence point plots
for (species in names(results$biomodData)) {
  # Extract the myBiomodData object for the current species
  biomod_data <- results$biomodData[[species]]
  
  # Extract the presence points (coordinates where response variable is 1)
  presence_points <- biomod_data@coord[biomod_data@data.species == 1, ]
  presence_df <- as.data.frame(presence_points)
  colnames(presence_df) <- c("Longitude", "Latitude")
  
  # Add a column to indicate presence points for the legend
  presence_df$Type <- "Presence Points"
  
  # Remove rows with missing values
  presence_df <- na.omit(presence_df)
  
  # Extract the current suitability raster for the species
  ensemble_current <- results$biomodEC[[species]]
  raster <- get_predictions(ensemble_current)
  emmean_layer <- raster[[grep("EMmean", names(raster))]]
  
  # Convert the current suitability raster to a data frame for ggplot
  current_landscape_df <- as.data.frame(emmean_layer, xy = TRUE, na.rm = TRUE)
  colnames(current_landscape_df) <- c("Longitude", "Latitude", "Value")
  
  # Create a ggplot object for the species
  p <- ggplot() +
    geom_raster(data = current_landscape_df, aes(x = Longitude, y = Latitude, fill = Value), alpha = 0.8) +
    geom_point(data = presence_df, aes(x = Longitude, y = Latitude, color = Type),size = 0.5) +
    scale_color_manual(name = "Legend", values = c("Presence Points" = "blue")) +
    scale_fill_terrain_c(name = "Suitability") +
    labs(
      title = paste(species),
      x = "Longitude",
      y = "Latitude"
    ) +
    coord_fixed() +  # Preserve aspect ratio
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 10),
      legend.position = "none"  # Remove individual legends
    )
  
  # Add the plot to the list
  presencePlots[[species]] <- p
}

# Extract the legend from one of the plots
example_plot <- ggplot() +
  geom_raster(data = current_landscape_df, aes(x = Longitude, y = Latitude, fill = Value), alpha = 0.8) +
  geom_point(data = presence_df, aes(x = Longitude, y = Latitude, color = Type), size = 0.5) +
  scale_color_manual(name = "Legend", values = c("Presence Points" = "blue")) +
  scale_fill_terrain_c(name = "Suitability") +
  guides(
    fill = guide_colorbar(order = 2),  # Suitability color scale comes second
    color = guide_legend(order = 1)   # Presence points legend comes first
  ) +
  labs(
    title = paste(species),
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_fixed() +  # Preserve aspect ratio
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )
shared_legend <- cowplot::get_legend(example_plot)

# Arrange the plots in a grid
combined_presence_plot <- grid.arrange(
  grobs = presencePlots,
  ncol = length(presencePlots)  # Number of columns corresponds to the number of species
)

# Combine the plot and legend side by side
final_plot <- grid.arrange(
  combined_presence_plot,
  arrangeGrob(
    grobs = list(shared_legend),
    ncol = 1
  ),
  ncol = 2,  # Two columns: one for the plot and one for the legend
  widths = unit(c(15, 3), "null"),
  top = textGrob("Presence of Multiple Species in Current Suitability Landscape", gp = gpar(fontsize = 16))
)

# Save the combined plot
ggsave(filename = file.path(output_folder, "PresencePoints_WithCurrentSuitability.png"),
       plot = final_plot,
       width = 12, height = 6, dpi = 300)

# Current and Predicted Landscapes ------------------------------------------------
# Define the scenarios and species to plot
scenarios <- c("ssp126", "ssp585")
scenario_names <- c("SSP1-RCP2.6", "SSP5-RCP8.5")
years <- c("2011-2040", "2041-2070", "2071-2100")

species_list <- names(results$biomodEF)

# Create a mapping function for scenario names
scenario_names <- function(scenario) {
  if (grepl("ssp126", scenario)) {
    return("SSP1-RCP2.6")
  } else if (grepl("ssp585", scenario)) {
    return("SSP5-RCP8.5")
  } else {
    return(scenario)  # Default to the original name if no match
  }
}

# Initialize a list to store current plots for each species
current_plots <- list()

# Loop through each species to create current suitability plots
for (species in species_list) {
  # Extract the BIOMOD_EnsembleForecasting object for the current conditions
  ensemble_current <- results$biomodEC[[species]]
  
  # Extract the SpatRaster for the `EMmean` ensemble model
  raster <- get_predictions(ensemble_current)
  emmean_layer <- raster[[grep("EMmean", names(raster))]]
  
  # Convert the raster to a data frame for ggplot
  raster_df <- as.data.frame(emmean_layer, xy = TRUE, na.rm = TRUE)
  colnames(raster_df)[3] <- "value"  # Rename the value column
  
  # Create a ggplot object for current suitability
  currentPlot <- ggplot(raster_df, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    scale_fill_terrain_c(name = "Prediction") +
    labs(
      title = NULL,  # Remove individual titles
      x = "Longitude",
      y = "Latitude"
    ) +
    coord_sf(expand = FALSE) +  # Ensure correct aspect ratio
    theme_bw() +  # Use a theme with grid lines
    theme(
      axis.title = element_text(size = 22),
      axis.text = element_text(size = 20),
      axis.ticks = element_line(),
      panel.grid.major = element_line(color = "gray"),
      panel.grid.minor = element_blank(),
      legend.position = "none"  # Remove individual legends
    )
  
  # Add the plot to the list
  current_plots[[species]] <- currentPlot
}

# Initialize a list to store plots for each species, scenario, and year
future_plots <- list()

# Loop through each year, scenario, and species to create plots
for (year in years) {
  for (scenario in scenarios) {
    for (species in species_list) {
      # Combine scenario and year to match the naming convention in results
      scenario_year <- paste0(scenario, "_", year)
      
      # Extract the BIOMOD_EnsembleForecasting object
      ensemble_forecast <- results$biomodEF[[species]][[scenario_year]]
      
      # Extract the SpatRaster for the `EMmean` ensemble model
      raster <- get_predictions(ensemble_forecast)
      
      # Ensure only the `EMmean` layer is selected
      emmean_layer <- raster[[grep("EMmean", names(raster))]]
      
      # Convert the raster to a data frame for ggplot
      raster_df <- as.data.frame(emmean_layer, xy = TRUE, na.rm = TRUE)
      colnames(raster_df)[3] <- "value"  # Rename the value column
      
      # Create a ggplot object
      futurePlot <- ggplot(raster_df, aes(x = x, y = y, fill = value)) +
        geom_raster() +
        scale_fill_terrain_c(name = "Suitability",                  # Legend title
                             guide = guide_colorbar(
                               title.position = "top",              # Position the title at the top
                               title.theme = element_text(size = 22),  # Increase legend title size
                               label.theme = element_text(size = 20)   # Increase legend label size
                             )
        )+
        labs(
          title = NULL,  # Remove individual titles
          x = "Longitude",
          y = "Latitude"
        ) +
        coord_sf(expand = FALSE) +  # Ensure correct aspect ratio
        theme_bw() +  # Use a theme with grid lines
        theme(
          axis.title = element_text(size = 22),
          axis.text = element_text(size = 20),
          axis.ticks = element_line(),
          panel.grid.major = element_line(color = "gray"),
          panel.grid.minor = element_blank(),
          legend.position = "none"  # Remove individual legends
        )
      
      # Add the plot to the list
      future_plots[[paste0(scenario, "_", year, "_", species)]] <- futurePlot
    }
  }
}

# Extract the legend from one of the plots
example_plot <- ggplot(raster_df, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  scale_fill_terrain_c(name = "Suitability",                  # Legend title
                       guide = guide_colorbar(
                         title.position = "top",              # Position the title at the top
                         title.theme = element_text(size = 22),  # Increase legend title size
                         label.theme = element_text(size = 20),   # Increase legend label size
                         barwidth = 20,
                         barheight = 1.5
                       )
  ) +
  theme_bw() +
  theme(
    #legend.position = "bottom",  # Position the legend at the bottom
    legend.direction = "horizontal",  # Make the legend horizontal
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 20)
  )
shared_legend <- cowplot::get_legend(example_plot)

# Loop through each year to create separate plots
for (year in years) {
  # Filter scenarios for the current year
  year_scenarios <- scenarios[grepl(year, scenarios)]
  
  title_grobs <- list(nullGrob())  # Start with a null grob for the row labels
  title_grobs <- append(title_grobs, list(textGrob("Current", gp = gpar(fontsize = 26), just = "centre")))
  
  # Add titles for each scenario and year combination
  for (scenario in scenarios) {
    title_grobs <- append(title_grobs, list(
      textGrob(
        label = paste(scenario_names(scenario), "\n", year),
        gp = gpar(fontsize = 26),
        just = "centre"
      )
    ))
  }
  
  # Initialize layout matrix
  ncols <- 1 + 1 + length(scenarios)  # 1 for row labels, 1 for current, and columns for scenarios
  nrows <- length(species_list) + 1  # +1 for the title row
  layout_matrix <- matrix(NA, nrow = nrows, ncol = ncols)
  layout_matrix[1, ] <- 1:ncols  # Fill the first row with title indices
  
  # Initialize grobs list
  all_grobs <- title_grobs
  counter <- ncols + 1  # Start counter after the title grobs
  
  # Loop through each species to add row labels and plots
  for (i in seq_along(species_list)) {
    species <- species_list[i]
    
    # Add species label as a vertical title
    label_grob <- textGrob(species, rot = 90, gp = gpar(fontface = "italic", fontsize = 24))
    all_grobs <- append(all_grobs, list(label_grob))
    
    # Add the current plot
    current_grob <- gtable_trim(ggplotGrob(current_plots[[species]]))
    all_grobs <- append(all_grobs, list(current_grob))
    
    # Add future plots for each scenario
    for (scenario in scenarios) {
      future_grob <- gtable_trim(ggplotGrob(future_plots[[paste0(scenario, "_", year, "_", species)]]))
      all_grobs <- append(all_grobs, list(future_grob))
    }
    
    # Update layout matrix for this species
    layout_matrix[i + 1, ] <- counter:(counter + ncols - 1)
    counter <- counter + ncols
  }
  
  # Define column widths dynamically
  col_widths <- unit.c(unit(1, "cm"), unit(1, "null"), unit(1, "null"), unit(1, "null"))
  # Adjust heights to minimize distance between titles and plots
  row_heights <- unit(c(1.5, rep(5, length(species_list))), "null")  # Small height for title row
  
  # Draw the final plot for the current year
  final_plot <- grid.arrange(
    grobs = all_grobs,
    layout_matrix = layout_matrix,
    widths = col_widths,
    heights = row_heights,
    bottom = shared_legend
  )
  
  # Save the plot for the current year
  ggsave(
    filename = file.path(output_folder, paste0("CurrentFutureSuitability_", year, ".png")),
    plot = final_plot,
    width = 24, height = 13,
    dpi = 300
  )
}


### All years arranged after another ###
# Create a function to extract years and map scenario names
extract_years <- function(scenario) {
  sub(".*_(\\d{4}-\\d{4})$", "\\1", scenario)  # Extract the year range (e.g., "2071-2100")
}
# Create title grobs dynamically
title_grobs <- list(nullGrob())  # Start with a null grob for the row labels
title_grobs <- append(title_grobs, list(textGrob("Current", gp = gpar(fontsize = 14, fontface = "bold"), just = "centre")))

# Add titles for each scenario and year combination
for (scenario in scenarios) {
  title_grobs <- append(title_grobs, list(
    textGrob(
      label = paste(scenario_names(scenario), "\n", extract_years(scenario)),
      gp = gpar(fontsize = 14, fontface = "bold"),
      just = "centre"
    )
  ))
}

# Initialize layout matrix
ncols <- 1 + 1 + length(scenarios)  # 1 for row labels, 1 for current, and columns for scenarios
nrows <- length(species_list) + 1  # +1 for the title row
layout_matrix <- matrix(NA, nrow = nrows, ncol = ncols)
layout_matrix[1, ] <- 1:ncols  # Fill the first row with title indices

# Initialize grobs list
all_grobs <- title_grobs
counter <- ncols + 1  # Start counter after the title grobs

# Loop through each species to add row labels and plots
for (i in seq_along(species_list)) {
  species <- species_list[i]
  
  # Add species label as a vertical title
  label_grob <- textGrob(species, rot = 90, gp = gpar(fontface = "italic", fontsize = 12))
  all_grobs <- append(all_grobs, list(label_grob))
  
  # Add the current plot
  current_grob <- gtable_trim(ggplotGrob(current_plots[[species]]))
  all_grobs <- append(all_grobs, list(current_grob))
  
  # Add future plots for each scenario
  for (scenario in scenarios) {
    future_grob <- gtable_trim(ggplotGrob(future_plots[[paste0(scenario, "_", species)]]))
    all_grobs <- append(all_grobs, list(future_grob))
  }
  
  # Update layout matrix for this species
  layout_matrix[i + 1, ] <- counter:(counter + ncols - 1)
  counter <- counter + ncols
}

# Define column widths dynamically
col_widths <- unit.c(unit(1, "cm"), unit(1, "null"), rep(unit(1, "null"), length(scenarios)))

# Draw the final plot
final_plot <- grid.arrange(
  grobs = all_grobs,
  layout_matrix = layout_matrix,
  widths = col_widths
)
