############################
# MAMMAL PLOTS PER SPECIES #
############################
# Inês Silva
# 20 March 2025


# Define target species to plot
species_names <- c("Alcesalces", "Lynxlynx") 

species_names_mapping <- c("Alcesalces" = "Alces alces",
                           "Lynxlynx" = "Lynx lynx")
  
# loop through each species and create a combined figure
for (species in species_names) {
  
  # file patterns for each raster type
  suitability_pattern <- paste0(species, "_suitability_cropped_modified_reprojectedKm\\.tif$")
  abundance_pattern <- paste0(".*_", species, "_abundance\\.tif$")
  reproduction_pattern <- paste0(".*_", species, "_reproductionRate\\.tif$")
  dispersal_pattern <- paste0(".*_", species, "_dispersal_change\\.tif$")
  
  # get correct species names for titles
  species_title <- species_names_mapping[species]
  
  ###############
  # Suitability #
  ###############
  
  # list suitability files
  suitability_file <- list.files(path = dirinput, pattern = suitability_pattern, full.names = TRUE)
  if (length(suitability_file) > 0) {
    # read rasters
    suitability_raster <- rast(suitability_file)
    # average rasters across layers
    suitability_avg <- mean(suitability_raster, na.rm = TRUE)
    # transform into dataframe
    suitability_df <- as.data.frame(suitability_avg, xy = TRUE)
    
    # plot suitability index
    suitability_plot <- ggplot() +
      geom_raster(data = suitability_df, aes(x = x, y = y, fill = mean)) +
      scale_fill_viridis_c(name = "Suitability\nIndex") +
      labs(title = "Suitability Index") +
      theme_minimal() +
      theme(legend.title = element_text(size = 10))
  } else {
    suitability_plot <- ggplot() + labs(title = paste(species, "Suitability data not found")) + theme_void()
  }
  
  #############
  # Abundance #
  #############
  
  # list abundance files
  abundance_files <- list.files(path = dirout, pattern = abundance_pattern, full.names = TRUE)
  if (length(abundance_files) > 0) {
    # read rasters & stack them
    abundance_stack <- c(rast(abundance_files))
    # average rasters across ALL years
    abundance_avg <- mean(abundance_stack, na.rm = TRUE)
    # transform into dataframe
    abundance_df <- as.data.frame(abundance_avg, xy = TRUE)
    
    # plot average abundance
    abundance_plot <- ggplot() +
      geom_raster(data = abundance_df, aes(x = x, y = y, fill = mean)) +
      scale_fill_viridis_c(name = "Nº of\nindividuals") +
      labs(title = "Average abundance", x = "Latitude", y = "Longitude") +
      theme_minimal() +
      theme(legend.title = element_text(size = 10))
  } else {
    abundance_plot <- ggplot() + labs(title = paste(species, "Abundance data not found")) + theme_void()
  }
  
  #####################
  # Reproduction Rate #
  #####################
  
  # list reproduction Rate files
  reproduction_files <- list.files(path = dirout, pattern = reproduction_pattern, full.names = TRUE)
  if (length(reproduction_files) > 0) {
    # read rasters & stack them
    reproduction_stack <- c(rast(reproduction_files))
    # average rasters across ALL years
    reproduction_avg <- mean(reproduction_stack, na.rm = TRUE)
    # transform into dataframe
    reproduction_df <- as.data.frame(reproduction_avg, xy = TRUE)
    
    # plot average reproduction rate
    reproduction_plot <- ggplot() +
      geom_raster(data = reproduction_df, aes(x = x, y = y, fill = mean)) +
      scale_fill_viridis_c(name = "Nº of\nindividuals") +
      labs(title = "Average\nnet reproduction rate", x = "Latitude", y = "Longitude") +
      theme_minimal() +
      theme(legend.title = element_text(size = 10))
  } else {
    reproduction_plot <- ggplot() + labs(title = paste(species, "Reproduction data not found")) + theme_void()
  }
  
  ####################
  # Dispersal Change #
  ####################
  
  # list dispersal change files
  dispersal_files <- list.files(path = dirout, pattern = dispersal_pattern, full.names = TRUE)
  if (length(dispersal_files) > 0) {
    # read rasters & stack them
    dispersal_stack <- c(rast(dispersal_files))
    # average rasters across ALL years
    dispersal_avg <- mean(dispersal_stack, na.rm = TRUE)
    # transform into dataframe
    dispersal_df <- as.data.frame(dispersal_avg, xy = TRUE)
    
    # plot dispersal change 
    dispersal_plot <- ggplot() +
      geom_raster(data = dispersal_df, aes(x = x, y = y, fill = mean)) +
      labs(title = "Average\ndispersal change", x = "Latitude", y = "Longitude") +
      scale_fill_gradient2(low = "green", mid = "white", high = "red", midpoint = 0, name = "Nº of\nindividuals)") +
      theme_minimal() +
      theme(legend.title = element_text(size = 10))
  } else {
    dispersal_plot <- ggplot() + labs(title = paste(species, "Dispersal data not found")) + theme_void()
  }
  
  ########################
  # Combine & save plots #
  ########################
  
  
  final_plot <- (suitability_plot + abundance_plot) / (reproduction_plot + dispersal_plot) +
    plot_annotation(title = species_title,
                    theme = theme(plot.title = element_text(size = 12, face = "italic")))
  
  ggsave(plot = final_plot,
         file = file.path(dirout, paste0("combined_", species, "_", runname, ".tif")),
         bg = 'white', width = 300, height = 400, units = "mm", dpi = 1200, compression = "lzw")
}
