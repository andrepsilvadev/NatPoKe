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

outputs <- fread(file.path(dirout, "metaRangeOutputs11Mar2025_Abund10_repRate1.05.csv"))

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

outputs_avg <- outputs %>%
  group_by(species, x, y) %>%
  summarise(avg_disp_change = mean(dispersal_change, na.rm = TRUE), .groups = "drop")

############################
# plot results per species #
############################

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
    theme(strip.text = element_text(size = 9))
  
  plot_list[[sp]] <- p
}


# arrange all the plots
dispChange_sps <- grid.arrange(grobs = plot_list, ncol = 2)
dispChange_sps

# save plot
ggsave(plot = dispChange_sps,
       file = "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/11Mar2025_Abund10/Outputs/dispersalChange11Mar2025.tif",
       #file = file.path(dirout, paste0("dipersalChange", runname, ".tif")),
       bg = 'white', width = 300, height = 180, units = "mm", dpi = 1200, compression = "lzw")


## just to check what happens in a few timesteps for lynx ##

## t = 21
plot(rast("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/11Mar2025_Abund10/Outputs/BAU_Boreal_regionalExtent_021_Lynxlynx_dispersal_change.tif"))
## t = 30
plot(rast("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/11Mar2025_Abund10/Outputs/BAU_Boreal_regionalExtent_030_Lynxlynx_dispersal_change.tif"))
## t = 40
plot(rast("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/11Mar2025_Abund10/Outputs/BAU_Boreal_regionalExtent_040_Lynxlynx_dispersal_change.tif"))
## t = 50
plot(rast("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/11Mar2025_Abund10/Outputs/BAU_Boreal_regionalExtent_050_Lynxlynx_dispersal_change.tif"))
## t = 125
plot(rast("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/11Mar2025_Abund10/Outputs/BAU_Boreal_regionalExtent_125_Lynxlynx_dispersal_change.tif"))

##########
## LIXO ##
##########

# Create a list of species names
species_names <- unique(combined_df$raster_name)

# Create a list to store ggplot objects for each species
plot_list <- list()

# Loop through each species and create a plot with its own color scale
for (species in species_names) {
  species_df <- subset(combined_df, raster_name == species)
  
  p <- ggplot(species_df, aes(x = x, y = y, fill = abundance)) +
    geom_raster() +
    scale_fill_gradient2(
      low = "red",
      mid = "white",
      high = "green",
      midpoint = mean(species_df$abundance, na.rm = TRUE) # Individual midpoint
    ) +
    labs(title = species, x = "Longitude", y = "Latitude") +
    theme_minimal() +
    facet_wrap(~timestep) # Use facet_wrap for individual plots
  plot_list[[species]] <- p
}

# Combine the plots using grid.arrange from the gridExtra package
library(gridExtra)

# Arrange the plots in a grid
grid.arrange(grobs = plot_list, ncol = length(unique(combined_df$timestep)))