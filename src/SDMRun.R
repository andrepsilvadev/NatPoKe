## Name: SDMRun.R ##
## Author: Jorinde-M. Rieger ##
<<<<<<< HEAD
## Description: Creates output of SDM results in R ##
## Date: April 30th 2025 ##

# Settings & libraries -----------------------------------------------------------------
source("~/NatPoKe9/src/libraries.R") # libraries
source("~/NatPoKe9/src/customFunctions.R") # functions
source("~/NatPoKe9/src/inputClimate.R") # format and reads input raster landscapes
source("~/NatPoKe9/src/inputSpeciesData.R") # format and reads input data
source("~/NatPoKe9/src/SDM.R") # function to format data and SDM

# Load dataset and format species occurence -----------------------------------------------------------------
# Crop the landscapes to the extent of the biome
biome_name <- "Tropical & Subtropical Moist Broadleaf Forests"
biome_name <- "Boreal Forests/Taiga"

# Function to load and select the biome shapefile
load_select_biome <- function(biome_name) {
  biome_sf <- st_read("~/data/data/Ecoregions2017/Ecoregions2017/Ecoregions2017.shp")
  biome_sf[biome_sf$BIOME_NAME == biome_name, ]}

biome_sf <- load_select_biome(biome_name)
biome_sp <- vect(biome_sf)

# Function to crop and mask rasters to biome
crop_mask_raster <- function(raster, biome_sp) {
  mask(crop(raster, biome_sp), biome_sp)}

# Crop and mask trainingLandscapes to biome extent
trainingLandscapes <- crop_mask_raster(trainingLandscapes, biome_sp)
plot(trainingLandscapes)

#### Test with Sweden ####

biome_name <- "Sweden"
# Load Sweden's shapefile using rnaturalearth
sweden_sf <- ne_countries(scale = "medium", country = "Sweden", returnclass = "sf")
sweden_sp <- vect(sweden_sf)  # Convert to SpatVector for terra compatibility
# Function to crop and mask rasters to Sweden
crop_mask_raster <- function(raster, sweden_sp) {
  mask(crop(raster, sweden_sp), sweden_sp)
}
# Crop and mask trainingLandscapes to Sweden's extent
trainingLandscapes <- crop_mask_raster(trainingLandscapes, sweden_sp)
plot(trainingLandscapes)

# Loop through each predictionLandscapes raster in the list and mask and crop to biome (test extent)
for (i in seq_along(predictionLandscapes)) {
  # Crop and mask the raster
  predictionLandscapes[[i]] <- mask(crop(predictionLandscapes[[i]], sweden_sp), sweden_sp)
}

print(predictionLandscapes)
plot(predictionLandscapes[["ssp126_2071-2100"]])

####### 

# Select the name of the studied species
targetSpecies <- c("Alces alces", "Canis lupus")

# Format species occurence to true presence and NAs with corresponding coordinates
# test with trainingLandscape
speciesData <- formatInputDataFrame(
  speciesData = speciesDataOcc,
  targetSpecies = targetSpecies, 
  landscape = trainingLandscapes)
head(speciesData)

# Run the SDMensembleMultiSpecies function -----------------------------------------------------------------

results <- SDMensembleMultiSpecies(targetSpecies = targetSpecies,
                                   speciesData = speciesData,
                                   trainingLandscapes = trainingLandscapes,
                                   predictionLandscapes = predictionLandscapes,
                                   biome_name = biome_name)

# Example of accessing the results
results$biomodData[["Alces alces"]]

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
=======
## Description: The main pipeline for the SDMs creates the output of the SDM.R function ##
## Date: August 5th 2025 ##

# Settings & libraries -----------------------------------------------------------------
# set working directory for maxent.jar file
source("src/libraries.R") # libraries
# When using the pipeline for the first time, run taxaOcccurrence.R and adapt species and user-login for GBIF Database:
#source("input/TaxaOccurence.R") # downloads taxa occurences from GBIF Database
source("src/customFunctions2.R") # functions
source("src/SDM.R") # function to format data and SDM

# Define scenarios and environmental variables -----------------------------------------------------------------
scenarios <- c("ssp126", "ssp585") # define socio-economic pathways
scenarios_des <- c("rcp26_ssp1", "rcp85_ssp5") # scenario names in land-use raster
scenario_names <- c("SSP1-RCP2.6", "SSP5-RCP8.5") # define socio-economic pathways names

# Define a mapping for scenario names
scenario_name_mapping <- c(
  "rcp26_ssp1" = "ssp126",
  "rcp85_ssp5" = "ssp585")

# Define climatologies
variables <- c("bio1", # mean annual air temperature
               "bio10", # mean daily mean air temperatures of the warmest quarter
               "bio11", # mean daily mean air temperatures of the coldest quarter
               "bio12", # mean annual precipitation amount
               "bio16", # mean monthly precipitation amount of the wettest quarter
               "bio17") # mean monthly precipitation amount of the driest quarter

# Define years
years <- c(2030, 2050, 2100)
baseline_year <- 2015

# Define the target resolution
target_resolution <- 0.04166 # approx. 5km resolution

# Global extent as initial extent for SDMs
extent <- "Global Terrestrial"
extent_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Define output path
outputPathLandscapes <- "output/Landscapes"
if (!dir.exists(outputPathLandscapes)) {
  dir.create(outputPathLandscapes, recursive = TRUE)
}

# When using the pipeline for the first time run on global extent:-----------------------------------------------------------------
# Format training and prediction Landscape
source("input/inputClimate.R") # format and reads input climate raster landscapes, adapt: scenarios, years & variables
source("input/inputLandUse.R") # format and reads input land-use raster landscapes, adapt: scenarios, years & variables
source("input/inputElev.R") # format and reads input land-use raster landscapes, adapt: scenarios, years & variables

# Define file paths and load training landscapes
trainingLandscapesClim <- file.path(outputPathLandscapes, paste0("trainingLandscapesClim_", baseline_year, "_5km.tif"))
trainingLandscapesLandUse <- file.path(outputPathLandscapes,paste0("trainingLandscapesLandUse_",  baseline_year, "_5km.tif"))
trainingLandscapesElev <- file.path(outputPathLandscapes, paste0("trainingLandscapesElev_",  baseline_year, "_5km.tif"))
trainingLandscapesClim <- terra::rast(trainingLandscapesClim)
trainingLandscapesLandUse <- terra::rast(trainingLandscapesLandUse)
trainingLandscapesElev <- terra::rast(trainingLandscapesElev)

# Resample the extent of the training landscapes to the land-use training Landscape
trainingLandscapesClim <- terra::resample(trainingLandscapesClim, trainingLandscapesLandUse)
trainingLandscapesElev <- terra::resample(trainingLandscapesElev, trainingLandscapesLandUse)

# Merge the climate, elevation and land-use rasters
trainingLandscapes <- c(trainingLandscapesElev, trainingLandscapesLandUse, trainingLandscapesClim)

# Save the merged training landscape
output_file <- file.path(outputPathLandscapes, paste0("trainingLandscapes_", baseline_year, "_5km.tif"))
writeRaster(trainingLandscapes, output_file, overwrite = TRUE)

# Create a list to store the merged prediction landscapes in parallelization
# Register parallel backend
num_cores <- min(parallel::detectCores() - 1, 10)  # Use up to 10 cores
cl <- parallel::makeCluster(num_cores)
doParallel::registerDoParallel(cl)

# Create an empty list to store merged prediction landscapes
predictionLandscapes <- list()

# Parallelized loop using foreach
foreach::foreach(scenario = scenarios, .combine = 'c', .packages = c("terra", "sf")) %:%
  foreach::foreach(year = years, .combine = 'c') %dopar% {
    # Define file paths for climate and land-use prediction landscapes & load them in the environment, if needed
    predictionLandscapesClim <- file.path(outputPathLandscapes, paste0("predictionLandscapesClim_", scenario, "_", year, "_5km.tif"))
    predictionLandscapesLandUse <- file.path(outputPathLandscapes, paste0("predictionLandscapesLandUse_", scenario, "_", year, "_5km.tif"))
    predictionLandscapesElev <- file.path(outputPathLandscapes, paste0("predictionLandscapesElev_", scenario, "_", year, "_5km.tif"))
    predictionLandscapesClim <- terra::rast(predictionLandscapesClim)
    predictionLandscapesLandUse <- terra::rast(predictionLandscapesLandUse)
    predictionLandscapesElev <- terra::rast(predictionLandscapesElev)
    
    # Ensure CRS, extent, and resolution consistency
    predictionLandscapesClim <- terra::resample(predictionLandscapesClim, predictionLandscapesLandUse)
    predictionLandscapesElev <- terra::resample(predictionLandscapesElev, predictionLandscapesLandUse)
    
    # Merge the climate and land-use rasters
    merged_prediction <- c(predictionLandscapesElev, predictionLandscapesLandUse, predictionLandscapesClim)
    
    # Save the merged prediction landscape
    output_file <- file.path(outputPathLandscapes, paste0("predictionLandscapes_", scenario, "_", year, "_5km.tif"))
    writeRaster(merged_prediction, output_file, overwrite = TRUE)
    
    # Store the merged prediction landscape in the list
    predictionLandscapes[[paste0(scenario, "_", year)]] <- merged_prediction
  }

# Stop the cluster
stopCluster(cl)

# Load formatted training and prediction Landscape -----------------------------------------------------------------
# Load formatted trainingLandscapes at global extent
trainingLandscapes <- file.path(outputPathLandscapes, paste0("trainingLandscapes_", baseline_year, "_5km.tif")) # with Antarctica
trainingLandscapes <- terra::rast(trainingLandscapes)

# Define extent for the predicitionLandscapes
# Put Biomes together in one landscape?
# Tropical Biome
extent <- "Tropical Biome"
extent_name <- "Tropical & Subtropical Moist Broadleaf Forests" # full name of the biome
extent_sf <- load_biome(extent_name)
extent_crs <- sf::st_transform(extent_sf, crs = crs(trainingLandscapes)) # Ensure CRS consistency
extent_sp <- terra::vect(extent_crs) # Convert the sf to a spatial object

# Boreal Biome
extent <- "Boreal Biome"
extent_name <- "Boreal Forests/Taiga" # full name of the biome
extent_sf <- load_biome(extent_name)
extent_crs <- sf::st_transform(extent_sf, crs = crs(trainingLandscapes)) # Ensure CRS consistency
extent_sp <- terra::vect(extent_crs) # Convert the sf to a spatial object

# Load predictionLandscapes and crop to defined extent
predictionLandscapes <- list()
# Loop through scenarios and years
for (scenario in scenarios) {
  for (year in years) {
    raster_path <- file.path(outputPathLandscapes, paste0("predictionLandscapes_", scenario, "_", year, "_5km.tif"))# with Antarctica
    raster <- terra::rast(raster_path)
    raster <- crop_mask_raster(raster, extent_sp)
    predictionLandscapes[[paste0(scenario, "_", year)]] <- raster
  }
}
rm(raster)

# When using the pipeline for the first time: check variable correlation and select suitable variables for the landscapes -----------------------------------------------------------------
# Calculate the correlation matrix for all layers in the trainingLandscapes with Pearson's correlation coefficient
cor_matrix <- terra::layerCor(trainingLandscapes, fun = "cor", use = "complete.obs", maxcell = 0.5*ncell(trainingLandscapes), na.rm = TRUE) # pearson correlation coefficient
write.csv(cor_matrix, file = "output/cor_matrix.csv", row.names = TRUE)
cor_mat <- cor_matrix$correlation

# Plot the correlation coefficients as percentages
png(filename = file.path(outputPathLandscapes, "CorrelationMatrix.png"),  width = 2000, height = 1500, res = 300)
corrplot::corrplot.mixed(
  cor_mat, tl.pos = 'lt', tl.cex = 0.6, number.cex = 0.5, addCoefasPercent = TRUE, tl.col = "black")
dev.off()

# Find highly correlated pairs (absolute correlation > 0.7) and select suitable variables
high_cor_pairs <- which(abs(cor_mat) > 0.7 & upper.tri(cor_mat), arr.ind = TRUE)

# Print pairs for selection process
for(i in seq_len(nrow(high_cor_pairs))) {
  cat(
    rownames(cor_mat)[high_cor_pairs[i, 1]], "and",
    colnames(cor_mat)[high_cor_pairs[i, 2]],
    "correlation:",
    round(cor_mat[high_cor_pairs[i, 1], high_cor_pairs[i, 2]], 2), "\n"
  )
}

# Filter suitable variables for the landscapes -----------------------------------------------------------------
# Remove highly correlated variables from training and prediction Landscapes
vars_to_remove <- c("Elevation", "bio10", "bio11", "bio16", "bio17")

# Subset trainingLandscapes to remove unwanted variables
trainingLandscapes <- trainingLandscapes[[!names(trainingLandscapes) %in% vars_to_remove]]

# Subset each predictionLandscapes raster to remove unwanted variables
for (name in names(predictionLandscapes)) {
  predictionLandscapes[[name]] <- predictionLandscapes[[name]][[!names(predictionLandscapes[[name]]) %in% vars_to_remove]]
}

# Format species occurrence input data -----------------------------------------------------------------
# Select target species (from TaxaOccurence.R)
targetSpecies <- c("Alces alces", "Bison bonasus", "Cervus elaphus", "Sus scrofa", "Vulpes vulpes", "Canis latrans",
"Lynx rufus", "Martes americana", "Taxidea taxus", "Ursus americanus", "Leontopithecus caissara", # hase only 4 occurences
"Leopardus pardalis", "Nasua nasua", "Aepyceros melampus", "Colobus angolensis", "Daubentonia madagascariensis",
"Diceros bicornis", "Erythrocebus patas", "Gorilla beringei", "Gorilla gorilla", "Orycteropus afer",
"Pan paniscus", "Pan troglodytes", "Papio anubis", "Papio ursinus", "Cervus nippon", "Cuon alpinus",
"Felis chaus", "Macaca fuscata", "Pongo abelii", "Pongo pygmaeus", "Panthera tigris", "Lynx lynx",
"Ursus arctos", "Canis lupus", "Rangifer tarandus", "Puma concolor", "Bison bison", "Panthera onca",
"Crocuta crocuta", "Mandrillus sphinx", "Panthera pardus", "Syncerus caffer", "Acinonyx jubatus",
"Panthera leo", "Connochaetes taurinus", "Loxodonta africana")

# Implement the for loop for single species apply to SDM function
SDMresults <- list() # Initialize an empty list to store results for each species
for (species in targetSpecies){
  print(paste("Processing species:", species))
  
  # Select the species data based on TaxaOccurence.R output
  species_group <- "NatPoKeMammals"
  
  # Load the species data occurrences based on TaxaOccurence.R output
  speciesData <- read.csv(file = paste0("data/trait_datasets/GBIF_",species_group, "_30+occurrences_.csv"))
  
  # Select single species data
  DataSingleSpecies <- speciesData %>%
    dplyr::filter(species == !!species)
  
  # Remove NAs and filter out records older than 2015
  # this might reduce the number of presence data to <30 occurences
  DataSingleSpecies <- DataSingleSpecies %>%
    drop_na(decimalLongitude,decimalLatitude, year)%>%
    filter(year >= 2015)
  
  # Load raster to define grid cells
  env_raster <- trainingLandscapes[[1]]
  
  # Extract cell ID for each occurrence
  DataSingleSpecies$cell <- terra::cellFromXY(env_raster, cbind(DataSingleSpecies$decimalLongitude, DataSingleSpecies$decimalLatitude))
  
  # Keep only one occurrence per unique grid cell
  speciesDataOcc <- removeSpeciesDuplicatesbyCellID(DataSingleSpecies) # Remove duplicate records per cell
  
  speciesPresence <- speciesDataOcc %>%
    mutate("{species}" := 1)%>%
    rename(
      x = decimalLongitude,
      y = decimalLatitude
    ) %>%
    select(all_of(species), x, y) 

  # Run the SDMensembleMultiSpecies function for the current species  
  cat("\n", species, "modeling started...")
  results <- SDMensembleMultiSpecies(
    targetSpecies = species,
    speciesData = speciesPresence,
    myExpl = trainingLandscapes,
    myExplFuture = predictionLandscapes,
    extent = extent
  )
  SDMresults[[species]] <- results  # Save results for each species
  cat("\n", species, "modeling finished.")
}
>>>>>>> c6a946ef84e6cbe6619ddb2264eed0d3d45ce642
