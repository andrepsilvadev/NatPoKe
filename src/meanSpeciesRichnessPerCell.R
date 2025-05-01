####################
# Species richness #
####################
# Inês Silva
# 18 Apr 2025

# GOAL: build a plot with mean species richness per cell over time

##########
# Step 1 # Find raster for all timesteps per species, saved tehm per timestep
##########

# Define target species
target_species <- c("Alcesalces", "Lynxlynx", "Canislupus", "Susscrofa",
                    "Rangifertarandus", "Odocoileusvirginianus", "Cervuselaphus",
                    "Damadama", "Lynxrufus", "Crocutacrocuta", "Pantheraleo",
                    "Pantheratigris", "Pumaconcolor", "Callithrix jacchus",
                    "Nasua nasua")

# List of directories to search through
dirouts_list <- c("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/23April_Europe/Outputs",
                  "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/23April_NorthAmerica/Outputs",
                  "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/23April_SouthAmerica/Outputs",
                  "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/23April_Africa/Outputs",
                  "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/23April_Asia/Outputs")  # update with your actual paths



species_files <- list()

# Loop through each directory
for (dir_path in dirouts_list) {
  
  # Initialize a vector to store matching files for this directory
  dir_matches <- c()
  # Loop through each target species
  for (target_sps in target_species) {
    
    # Find all matching files for this species in the current directory
    sps_files <- list.files(dir_path,
                            pattern = paste0(".*(10[1-9]|11[0-9]|12[0-5])_", target_sps, "_abundance\\.tif$"),
                            full.names = TRUE)
    # Add them to the directory's file list
    dir_matches <- c(dir_matches, sps_files)
  }
  # Store all matched files under the directory name
  species_files[[dir_path]] <- dir_matches
}

##########
# Step 2 # stack them per timestep
##########

# start an empty list
stacked_rasters <- list()

# go through each directory
for (dir_path in names(species_files)) {
  dir_files <- species_files[[dir_path]]
  
  if (length(dir_files) == 0) {
    message("No files to process in ", dir_path)
    next
  }
  
  # get a shorter directory name
  short_dir_name <- basename(dirname(dir_path))
  
  # get timestep and species name
  file_info <- data.frame(
    file = dir_files,
    timestep = sub(".*(10[1-9]|11[0-9]|12[0-5])_.*", "\\1", basename(dir_files)),
    species = sub(".*_(.*?)_abundance\\.tif$", "\\1", basename(dir_files)),
    stringsAsFactors = FALSE
  )
  
  # start a sublist for the directory names
  if (!short_dir_name %in% names(stacked_rasters)) {
    stacked_rasters[[short_dir_name]] <- list()
  }
  
  # group rasters and stack them by timestep
  for (ts in unique(file_info$timestep)) {
    ts_files <- file_info[file_info$timestep == ts, ]
    ts_stack <- rast(ts_files$file)
    names(ts_stack) <- ts_files$species
    
    # store each stack in a list inside the directory list "nested list"
    stacked_rasters[[short_dir_name]][[ts]] <- ts_stack
    invisible(gc())
  }
}

# check results
#stacked_rasters$`23April_Europe`$`117`
#plot(stacked_rasters$`23April_Europe`$`101`)

##########
# Step 3 # modify each one where if (abund >0) replace value with 1
##########

# Initialize output list
binarized_rasters <- list()

# Loop over each directory (short name)
for (dir_name in names(stacked_rasters)) {
  binarized_rasters[[dir_name]] <- list()
  
  # Loop over each timestep
  for (timestep in names(stacked_rasters[[dir_name]])) {
    r <- stacked_rasters[[dir_name]][[timestep]]
    
    # Binarize: values > 0 become 1, values == 0 stay 0
    r_bin <- r
    r_bin[r_bin > 0] <- 1
    
    # Store result
    binarized_rasters[[dir_name]][[timestep]] <- r_bin
    invisible(gc())
  }
}

# cehck results
#binarized_rasters$`23April_Europe`$`101`
#plot(binarized_rasters$`23April_Europe`$`101`)

##########
# Step 4 # sum each stack into one raster (shoudl hava 25 rasters)
##########

# start and empty list
richness_rasters <- list()

# go through each directory
for (dir_name in names(binarized_rasters)) {
  richness_rasters[[dir_name]] <- list()
  
  # over each timestep stack
  for (ts in names(binarized_rasters[[dir_name]])) {
    bin_stack <- binarized_rasters[[dir_name]][[ts]]
    
    # sum all layers in a stack to get the number of sps per cell (richness)
    richness <- sum(bin_stack, na.rm = TRUE)
    
    # store richness as one raster per time step
    richness_rasters[[dir_name]][[ts]] <- richness
    invisible(gc())
  }
}
# cehck reults
#plot(richness_rasters$`23April_Europe`$`101`)

##########
# Step 5 # use global function to get mean species richness value per cell
##########

# get an empty list
richness_mean_rows <- list()

# go through each directory
for (dir_name in names(richness_rasters)) {
  for (timestep in names(richness_rasters[[dir_name]])) {
    
    # extract the raster
    r <- richness_rasters[[dir_name]][[timestep]]
    
    # get cell values
    v <- values(r)
    
    # filter out cells wher eno species exist
    v_nonzero <- v[!is.na(v) & v > 0]
    
    # calculate the mean using only cells where species exist
    mean_val <- mean(v_nonzero)
    
    # store results
    richness_mean_rows[[length(richness_mean_rows) + 1]] <- data.frame(
      dir_name = dir_name,
      timestep = as.integer(timestep),
      mean_richness = mean_val
    )
  }
}

# check results
#richness_mean_rows
   
##########
# Step 6 # get those results into a dataframe with timestep and sps richness column
##########

richness_mean_df <- do.call(rbind, richness_mean_rows)

##########
# Step 7 # plot
##########

region_names <- c("23April_Africa" = "Africa",
                 "23April_Asia" = "Asia",
                 "23April_Europe" = "Europe",
                 "23April_NorthAmerica" = "North America",
                 "23April_SouthAmerica" = "South America")


mean_spsRichness <- ggplot(data = richness_mean_df, aes(x = timestep, y = mean_richness, color = dir_name)) + 
  geom_line() +
  labs(x = "Time (yrs)", y = "Mean species richness per cell", color = "Region") +
  theme_minimal() + 
  theme(legend.position = "bottom") +
  scale_color_viridis_d(labels = region_names)

ggsave(plot = mean_spsRichness,
       file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/figures_20250427/Supfigure_meanSpeciesRichness.png",
       bg = 'white', width = 250, height = 250, units = "mm", dpi = 1200, #compression = "lzw"
       )

