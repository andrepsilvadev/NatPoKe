## checkup for weird lines in maps ##
## Inês Silva ##
## 18 May 2026 ##


# Set up, load needed packages & functions
library(here)
source(here("src", "libraries.R"))
source(here("src", "customFunctions2.R"))

outputFolder_paths <- "D:/metaRange_May26/Asia_ssp585_20260405/Outputs"


suitability_files <- c(
  "Capreolus.pygargus" =
    "D:/metaRange_April26/Asia_ssp585_20260405/Inputs/Capreolus.pygargus_tropical_ssp585_cropped_reprojectedm.tif",
  
  "Axis.axis" =
    "D:/metaRange_April26/Asia_ssp585_20260405/Inputs/Axis.axis_tropical_ssp585_cropped_reprojectedm.tif",
  
  "Boselaphus.tragocamelus" =
    "D:/metaRange_April26/Asia_ssp585_20260405/Inputs/Boselaphus.tragocamelus_tropical_ssp585_cropped_reprojectedm.tif"
)

plot_list <- list()

for(sps in names(suitability_files)){
  
  message("Working on ", sps)
  
  # load suitability raster
  r <- rast(suitability_files[[sps]])
  
  # ---- MAP 1 (layer 1) ----
  df1 <- as.data.frame(r[[1]], xy = TRUE, na.rm = TRUE)
  
  p_map1 <- ggplot(df1) +
    geom_raster(aes(x, y, fill = df1$`2015`)) +
    coord_equal() +
    theme_void() +
    scale_fill_viridis_c(option = "viridis") +
    labs(title = paste("Year 2015"), , fill = "Suitability") +
    theme(legend.position = "bottom")
  
  # ---- MAP 2 (layer 111) ----
  df111 <- as.data.frame(r[[111]], xy = TRUE, na.rm = TRUE)
  
  p_map111 <- ggplot(df111) +
    geom_raster(aes(x, y, fill = df111$`2125`)) +
    coord_equal() +
    theme_void() +
    scale_fill_viridis_c(option = "viridis") +
    labs(title = "Year 2125", fill = "Suitability") +
    theme(legend.position = "bottom")
  
  # ---- MEAN SUITABILITY THROUGH TIME ----
  df_mean <- global(r, "mean", na.rm = TRUE) %>%
    as.data.frame()
  
  df_mean$year <- as.numeric(names(r))
  df_mean$species <- suppressMessages(
    pretty_species_names(sps)
  )
  
  p_time <- ggplot(df_mean,
                   aes(year, mean)) +
    geom_line(linewidth = 1) +
    theme_bw() +
    labs(x="Year",
         y="Mean suitability",
         title="Average suitability")
  
  # ---- COMBINE IN ONE ROW ----
  combined_plot <- p_map1 | p_map111 | p_time
  
  # store plot
  plot_list[[sps]] <- combined_plot
}

plot_list$Capreolus.pygargus


abundance_rasters <- list()

for (target_sps in target_species) {
  
  message("Working on ", target_sps)
  
  for (timestep in c(26,136)) {
    
    sps_files <- list.files(
      path = "D:/metaRange_May26/Asia_ssp585_20260405/Outputs",
      pattern = paste0(".*", timestep, "_", target_sps, "_abundance\\.tif"),
      full.names = TRUE
    )
    
    if(length(sps_files)==0) next
    
    # mean across replicates
    r_stack <- rast(sps_files)
    r_mean  <- app(r_stack, mean, na.rm=TRUE)
    
    abundance_rasters[[paste(target_sps,timestep,sep="_")]] <- r_mean
  }
}

for(sps in target_species){
  
  r26  <- abundance_rasters[[paste0(sps,"_26")]]
  r136 <- abundance_rasters[[paste0(sps,"_136")]]
  
  # convert to dataframes
  df26 <- as.data.frame(r26, xy=TRUE, na.rm=TRUE)
  df136 <- as.data.frame(r136, xy=TRUE, na.rm=TRUE)
  
  p26 <- ggplot(df26) +
    geom_raster(aes(x, y, fill = mean)) +
    coord_equal() +
    theme_void() +
    scale_fill_viridis_c(option="viridis"#, limits=lims
                         ) +
    labs(title = paste(sps, "- t26"))
  
  p136 <- ggplot(df136) +
    geom_raster(aes(x, y, fill = mean)) +
    coord_equal() +
    theme_void() +
    scale_fill_viridis_c(option="viridis", na.value = "transparent"#, limits=lims
                         ) +
    labs(title = paste(sps, "- t136"))
  
  # store side-by-side
  abundance_change_plots[[sps]] <- p26 | p136
}

abundance_change_plots$Capreolus.pygargus

abundance_change_maps <- list()
abundance_change_plots <- list()

for(sps in target_species){
  
  r26  <- abundance_rasters[[paste0(sps,"_26")]]
  r136 <- abundance_rasters[[paste0(sps,"_136")]]
  
  if(is.null(r26) | is.null(r136)) next
  
  change_raster <- (r136 - r26) / r26
  
  abundance_change_maps[[sps]] <- change_raster
  
  df <- as.data.frame(change_raster, xy=TRUE, na.rm=TRUE)
  
  p <- ggplot(df) +
    geom_raster(aes(x, y, fill = mean)) +
    coord_equal() +
    theme_void() +
    scale_fill_viridis_c(
      option="viridis",
      name = "Abundance\nchange", limits = c(-1,1)
    ) +
    labs(title = paste("Abundance change:", sps))
  
  abundance_change_plots[[sps]] <- p
}

abundance_change_plots$Boselaphus.tragocamelus









# Combine all species & timesteps into a single dataframe
final_df <- bind_rows(dir_data)

head(final_df)





                    ## carnivores
                       "Canis.lupus", "Felis.chaus", "Acinonyx.jubatus",
                       ## omnivores
                       "Arctictis.binturong", "Macaca.nigra", "Macaca.fuscata"
                       )


# Initialize an empty list to store final dataframes
all_final_data <- list()

# Loop through each directory
for (dir in outputFolder_paths) {
  
  
  

  
  # Combine all species & timesteps into a single dataframe
  final_df <- bind_rows(dir_data)
  
  # Extract second-to-last folder name as key
  folder_names <- strsplit(dir, "/")[[1]]
  short_dir_name <- folder_names[length(folder_names) - 1]
  
  # Store in the master list
  all_final_data[[short_dir_name]] <- final_df
}
