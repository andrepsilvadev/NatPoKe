################################
# DISPERSAL CHANGE ACROSS TIME #
################################
# Inês Silva
# 11 Mar. 2025

# packages
library(data.table)
library(ggplot2)
library(gridExtra) # to arraange plots
library(dplyr)
library(terra)

###############
# import data #
###############

## rasters ##

species_names <- c("Alcesalces", "Cervuselaphus", "Lynxlynx", "Rangifertarandus")
species_avg <- list()

for (species in species_names) {
  # list all dispersal chnage rasters for a species
  species_disp_rast <- list.files(path = "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/11Mar2025_Abund10_dispDist1.05/Outputs/",
                             pattern = paste0(species, "_dispersal_change.tif"), full.names = TRUE)
  
  # list all abundance rasters for a species
  species_abund_rast <- list.files(path = "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/11Mar2025_Abund10_dispDist1.05/Outputs/",
                                   pattern = paste0(species, "_abundance.tif"), full.names = TRUE)
  
  # stack all rasters for a species
  species_stack <- c(rast(species_rast))
  
  # average all rasters
  species_avg[[species]] <- mean(species_stack, na.rm = TRUE)
}


## dataframe ##

outputs <- fread(file.path(dirout, paste0("metaRangeOutputs", runname, ".csv")))

# change columns format to factor
outputs <- outputs %>% 
  mutate(scenario = as.factor(scenario),
         region = as.factor(region),
         species = as.factor(species),
         TrophicLevel = as.factor(TrophicLevel),
         Taxa = as.factor(Taxa))
# check data
summary(outputs)
gc()

###################################################
# calculate average dispersal change across years #
###################################################

# checking what happens in eahc timestep
chekup <- outputs %>% 
  dplyr::filter(abundance !=0) %>% 
  group_by(species, timestep) %>% 
  summarise(avg_dispChange = mean(dispersal_change, na.rm = TRUE),
            avg_abund = mean(abundance, na.rm = TRUE)) 
  

# averaging everything
outputs_avg <- outputs %>%
  dplyr::filter(abundance != 0) %>% # use only cells where the species exists
  group_by(species, x, y) %>%
  summarise(avg_disp_change = mean(dispersal_change, na.rm = TRUE), .groups = "drop") %>%
  mutate(species = recode(species,
                           "Alcesalces" = " Alces alces (Moose)",
                           "Cervuselaphus" = "Cervus elaphus (Red deer)",
                           "Lynxlynx" = "Lynx lynx (Eurasian lynx)",
                           "Rangifertarandus" = "Rangifer tarandus (Reindeer)")) 

############################
# plot results per species #
############################

## using the rasters ##

par(mfrow = c(2,2))
plot(species_avg$Alcesalces)
plot(species_avg$Cervuselaphus)
plot(species_avg$Lynxlynx)
plot(species_avg$Rangifertarandus)

## using the dataframe ##

# get species names in vector
species_list <- unique(outputs_avg$species)

# create list to store plots
plot_list <- list()

# go through each species to create a plot
for (sp in species_list) {
  species_data <- outputs_avg %>% filter(species == sp)
  
  # calculate the min and max values to have a speciefic color scale for each species
  min_val <- min(species_data$avg_disp_change, na.rm = TRUE)
  max_val <- max(species_data$avg_disp_change, na.rm = TRUE)
  
  p <- ggplot(species_data, aes(x = x, y = y, fill = avg_disp_change)) +
    geom_tile() +
    scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0,
                         limits = c(min_val, max_val)) + # Set custom color scale
    theme_minimal() +
    labs(x = "Latitude", y = "Longitude", fill = "Average Dispersal\nChange Over Time", title = sp) + 
    theme(strip.text = element_text(size = 9),
          plot.title = element_text(size = 9),)
  
  plot_list[[sp]] <- p
}


# arrange all the plots
dispChange_sps <- grid.arrange(grobs = plot_list, ncol = 2)
dispChange_sps

# save plot
ggsave(plot = dispChange_sps,
       #file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/11Mar2025_Abund10/dispersalChange11Mar2025.tif",
       file = file.path(dirout, paste0("dipersalChange", runname, ".tif")),
       bg = 'white', width = 300, height = 180, units = "mm", dpi = 1200, compression = "lzw")


## just to check what happens in a few timesteps for lynx ##
# Load your raster
my_raster <- rast("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/11Mar2025_Abund10_dispDist1.05/Outputs/BAU_Boreal_regionalExtent_002_Alcesalces_dispersal_change.tif")

# Plot the raster with the desired scale
plot(my_raster, range = c(-4000, 1000))

## t = 21
plot(rast("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/11Mar2025_Abund10_dispDist1.05/Outputs/BAU_Boreal_regionalExtent_002_Alcesalces_dispersal_change.tif"))
## t = 30
plot(rast("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/11Mar2025_Abund10_dispDist1.05/Outputs/BAU_Boreal_regionalExtent_030_Alcesalces_dispersal_change.tif"))
## t = 40
plot(rast("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/11Mar2025_Abund10_dispDist1.05/Outputs/BAU_Boreal_regionalExtent_040_Alcesalces_dispersal_change.tif"))
## t = 50
plot(rast("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/11Mar2025_Abund10_dispDist1.05/Outputs/BAU_Boreal_regionalExtent_050_Alcesalces_dispersal_change.tif"))
## t = 125
plot(rast("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/11Mar2025_Abund10_dispDist1.05/Outputs/BAU_Boreal_regionalExtent_125_Alcesalces_dispersal_change.tif"))


########################################
## dispersal change between time steps #
########################################

## for all species ##

library(ggplot2)
library(gridExtra)

outputs_subset <- outputs %>% 
  dplyr::filter(abundance != 0) %>% # use only cells where the species exists
  dplyr::filter(timestep %in% c(021,125))

outputs_split <- split(outputs_subset, outputs_subset$species)

plot_list2 <- list()

for (sps_name in names(outputs_split)) { # Iterate over names of split list
  sps <- outputs_split[[sps_name]]
  
  p <- ggplot(sps, aes(x = x, y = y, fill = dispersal_change)) +
    geom_raster() +
    scale_fill_gradient2(
      low = "red",
      mid = "white",
      high = "green",
      midpoint = mean(sps$dispersal_change, na.rm = TRUE)
    ) +
    labs(title = paste(sps_name, "dispersal change"), x = "Longitude", y = "Latitude") + #dynamic title
    theme_minimal() +
    facet_wrap(~timestep)
  
  plot_list2[[sps_name]] <- p # Use the species name as the list index
}

all_sps <- grid.arrange(grobs = plot_list2, ncol = 2) # grid.arrange works well with ggplot objects.
ggsave(plot = all_sps,
       file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/11Mar2025_Abund10/Allsps_dispersalChange11Mar2025.tif",
       #file = file.path(dirout, paste0("dipersalChange", runname, ".tif")),
       bg = 'white', width = 400, height = 180, units = "mm", dpi = 1200, compression = "lzw")


## for one sps ##

LynxlynxdispersalChange <- ggplot(outputs_subset[outputs_subset$species == "Lynxlynx",], aes(x = x, y = y, fill = dispersal_change)) +
  geom_raster() +
  facet_wrap(~timestep, labeller = labeller(timestep = c("21" = "Year 21", "125" = "Year 125"))) +
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "green",
    midpoint = mean(outputs_subset[outputs_subset$species == "Lynxlynx",]$dispersal_change, na.rm = TRUE) # Individual midpoint
  ) +
  labs(title = "Eurasian lynx dispersal change", x = "Longitude", y = "Latitude", fill = "Dispersal\nchange\n(nº indiv.)") +
  theme_minimal() +
  theme(strip.text = element_text(size = 10), 
        panel.background = element_blank())

ggsave(plot = LynxlynxdispersalChange,
       file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/11Mar2025_Abund10/Lynxlynx_dispersalChange11Mar2025.tif",
       #file = file.path(dirout, paste0("dipersalChange", runname, ".tif")),
       bg = 'white', width = 400, height = 180, units = "mm", dpi = 1200, compression = "lzw")


#########################################################

# ATTEMPT FROM MARCH 14TH
## SAME BAD RESULTS

library(terra)

species_avg <- list()

for (species in species_names) {
  # List all dispersal change rasters for a species
  disp_files <- list.files(
    path = "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/11Mar2025_Abund10_dispDist1.05/Outputs/",
    pattern = paste0(species, "_dispersal_change.tif"),
    full.names = TRUE
  )
  
  # List all abundance rasters for a species
  abund_files <- list.files(
    path = "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/11Mar2025_Abund10_dispDist1.05/Outputs/",
    pattern = paste0(species, "_abundance.tif"),
    full.names = TRUE
  )
  
  if (length(disp_files) > 0 && length(abund_files) > 0) {
    # Extract timesteps from filenames
    disp_timesteps <- as.numeric(gsub(".*_(\\d+)_.*", "\\1", basename(disp_files)))
    abund_timesteps <- as.numeric(gsub(".*_(\\d+)_.*", "\\1", basename(abund_files)))
    
    # Find common timesteps
    common_timesteps <- intersect(disp_timesteps, abund_timesteps)
    
    # Initialize a list to store dispersal rasters that meet the condition
    valid_disp_rasters <- list()
    
    for (timestep in common_timesteps) {
      # Find corresponding dispersal and abundance files
      disp_file <- disp_files[disp_timesteps == timestep]
      abund_file <- abund_files[abund_timesteps == timestep]
      
      if (length(disp_file) == 1 && length(abund_file) == 1) {
        # Load abundance raster and calculate mean
        abund_raster <- rast(abund_file)
        abund_mean <- global(abund_raster, fun = "mean", na.rm = TRUE)$mean
        
        # Check if mean abundance is greater than zero
        if (abund_mean > 0) {
          # Load dispersal raster and add to the list
          valid_disp_rasters <- c(valid_disp_rasters, rast(disp_file))
        }
      }
    }
    
    # Average the valid dispersal rasters
    if (length(valid_disp_rasters) > 0) {
      species_avg[[species]] <- mean(rast(valid_disp_rasters), na.rm = TRUE)
    } else {
      cat(paste("No dispersal rasters met the abundance condition for", species, ".\n"))
      species_avg[[species]] <- NULL
    }
  } else {
    cat(paste("Missing dispersal or abundance rasters for", species, ". Skipping.\n"))
    species_avg[[species]] <- NULL
  }
}
plot(species_avg$Cervuselaphus) 
