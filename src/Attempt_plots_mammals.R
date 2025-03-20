#################################
# TEST PLOTS TO PRESENT RESULTS #
#################################
# Inês Silva
# 20 Mar 2025

# have a plot to show sps suitability, reproduction rate, dispersal change
# and abundance spatially

library(patchwork)

###############
# SUITABILITY #
###############

# import suitability
alces <- rast(file.path(dirinput, "Alcesalces_suitability_cropped_modified_reprojectedKm.tif"))
moose_suit_avg <- mean(alces, na.rm = TRUE)

# convert raster to data frame
alces_suit <- as.data.frame(moose_suit_avg, xy = TRUE)

# plotting suitability
alcesSuitability <- ggplot() +
  geom_raster(data = alces_df, aes(x = x, y = y, fill = mean)) +
  scale_fill_viridis_c(name = "Suitability\nIndex") +
  labs(title = "Moose (Alces alces) suitability index") +
  theme_minimal() + 
  theme(legend.title = element_text(size = 10))

#############
# ABUNDANCE #
#############

# list abundance rasters
alces_abund_rast <- list.files(path = dirout,
                                 pattern = "Alcesalces_abundance.tif", full.names = TRUE)
# stack abundance rasters
alces_abund_stack <- c(rast(alces_abund_rast))
# average rasters across ALL timesteps
alces_abund_avg <- mean(alces_abund_stack, na.rm = TRUE)

# convert raster to data frame
alces_abund <- as.data.frame(alces_abund_avg, xy = TRUE)

alcesAbund <- ggplot() +
  geom_raster(data = alces_abund, aes(x = x, y = y, fill = mean)) +
  scale_fill_viridis_c(name = "Nº individuals") +
  labs(title = "Moose (Alces alces) average abundance") +
  theme_minimal() + 
  theme(legend.title = element_text(size = 10))
#alcesAbund

#####################
# REPRODUCTION RATE #
#####################

# list reproduction rate rasters
alces_repRate_rast <- list.files(path = dirout,
                               pattern = "Alcesalces_reproductionRate.tif", full.names = TRUE)
# stack reproduction rate rasters
alces_repRate_stack <- c(rast(alces_repRate_rast))
# average rasters across ALL timesteps
alces_repRate_avg <- mean(alces_repRate_stack, na.rm = TRUE)

# convert raster to data frame
alces_repRate <- as.data.frame(alces_repRate_avg, xy = TRUE)

alcesRepRate <- ggplot() +
  geom_raster(data = alces_repRate, aes(x = x, y = y, fill = mean)) +
  scale_fill_viridis_c(name = "Nº individuals") +
  labs(title = "Moose (Alces alces) average\nnet reproduction rate") +
  theme_minimal() + 
  theme(legend.title = element_text(size = 10))
#alcesRepRate

####################
# DISPERSAL CHANGE #
####################

# list reproduction rate rasters
alces_dispChange_rast <- list.files(path = dirout,
                                 pattern = "Alcesalces_dispersal_change.tif", full.names = TRUE)
# stack reproduction rate rasters
alces_dispChange_stack <- c(rast(alces_dispChange_rast))
# average rasters across ALL timesteps
alces_dispChange_avg <- mean(alces_dispChange_stack, na.rm = TRUE)

# convert raster to data frame
alces_dispChange <- as.data.frame(alces_dispChange_avg, xy = TRUE)

alces_dispersalChange <- ggplot() +
  geom_raster(data = alces_dispChange, aes(x = x, y = y, fill = mean)) +
  labs(x = "Latitude", y = "Longitude", fill = "Average\nDispersal Change\n(nº indiv)", title = "Moose (Alces alces) average\ndispersal change") +
  scale_fill_gradient2(low = "green", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() + 
  theme(legend.title = element_text(size = 10))
#alces_dispersalChange

finalMoose <- (alcesSuitability + alcesAbund) / (alcesRepRate + alces_dispersalChange)

ggsave(plot = finalMoose,
       #file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/11Mar2025_Abund10/dispersalChange11Mar2025.tif",
       file = file.path(dirout, paste0("combinedMoose", runname, ".tif")),
       bg = 'white', width = 300, height = 400, units = "mm", dpi = 1200, compression = "lzw")
