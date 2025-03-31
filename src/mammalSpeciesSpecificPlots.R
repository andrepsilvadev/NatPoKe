###################################
# SPECIES SPECIFIC OUTPUT FIGURES #
###################################
# Inês Silva
# 21 March 2025 - 29 March 2025

# This script will produce 2 figures per species 
## Figure 1 - Species suitability maps over time (timesteps 101-125)
## Figure 2 - Species overview figure with:
              ### abundance and model validation
              ### average abundance over time, proportion of abundance & average dispersal change over time)

##########
# Step 1 # Set up, load packages & necessary functions 
##########

# load all necessary packages
source(here("src","libraries.R"))

# custom function (validateModel_1sps is crucial here)
source(here("src", "customFunctions.R"))

# model validation function relies external data - LOAD IT HERE
santini2022 <- read_excel("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/SRIT_ANDRE/external_data/geb13476-sup-0002-tables1.xls") %>%
  mutate(Species = str_replace_all(Species, " ", ""))
# model validation also needs species traits dataframe
spData <- read.csv(file.path(dirinput, "metaRangeSpeciesDataframe.csv"))

# specificy directories (if not done already)
dirinput
dirout

# get a list of species from the inputs folder
species_files <- list.files(dirinput, pattern = "_suitability_cropped_modified_reprojectedKm.tif")
species_names <- unique(gsub("_suitability_cropped_modified_reprojectedKm.tif", "", species_files))

# nicer names for plotting
names_replace <- c("Alcesalces" = "Alces alces",
                   "Lynxlynx" = "Lynx lynx",
                   "Cervuselaphus" = "Cervus elaphus",
                   "Rangifertarandus" = "Rangifer tarandus",
                   "Susscrofa" = "Sus scrofa",
                   "Damadama" = "Dama dama",
                   "Canislupus" = "Canis lupus")
 
# define target region
target_region <- "Sweden"

# Timestep labels
timestep_labels <- paste0("Timestep ", 101:125)


##########
# Step 2 # Build suitability over time figures 
##########

# needs access to model simulation input folder
# make sure to use tidyterra's package geom_spatRaster, or it won't work

# go through each species
for (target_sps in species_names) {
  
  # find reprojected raster for target sps (inside INPUTS directory)
  sps_suitability <- rast(list.files(dirinput,
                                     pattern = paste0(target_sps, "_suitability_cropped_modified_reprojectedKm.tif"),
                                     full.names = TRUE))
  # replace layers names with timesteps labels
  names(sps_suitability) <- timestep_labels
  
  if (length(sps_suitability) > 0) {
  suitability_plot <- ggplot() +
    # use tidyterra's geom_spatrater function to plot spatraster layers 
    tidyterra::geom_spatraster(data = sps_suitability) +
    facet_wrap(.~lyr, ncol = 10) +
    # colorscale
    scale_fill_gradientn(colors = brewer.pal(11, "RdYlGn"), na.value = "transparent",
                         breaks = c(min(terra::global(sps_suitability, min, na.rm = TRUE)[,1]),
                                   max(terra::global(sps_suitability, max, na.rm = TRUE)[,1])),
                         labels = c("Low\n 0.0", "High\n 1.0"),
                         name = "Suitability\nIndex") +
    labs(x = "Latitude", y = "Longitude", title = paste0(names_replace[[target_sps]], " Suitability Maps")) +
    theme_minimal() +
    theme(legend.title = element_text(size = 9, face = "bold", vjust = 0.9),
          panel.background = element_blank(),
          panel.spacing.y = unit(0.5, "lines"),
          legend.position = c(.55, .15),
          legend.direction = "vertical",
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0))
  } else {
    suitability_plot <- ggplot() + labs(title = paste(target_sps, "Suitability map not found")) + theme_void()
  }
  
  # save figure 1
  ggsave(plot = suitability_plot,
         filename = file.path(dirout, paste0("suitabilityOverTime_", target_sps, ".tiff")),
         bg = 'white', width = 400, height = 400, units = "mm", dpi = 1200, compression = "lzw")
}
  
##########
# Step 3 # Build model overview figure per species
##########

# contains final abundance timestep (A), model validation (B),
# average abundance over time, 101-125 (C), proportion of abundance change (D)
# and average dispersal change, 101-125 (E)


# go through each species
for (target_sps in species_names) {
  
  #############
  # ICON PLOT #
  #############
  
  # get sps name with spaces
  names <- names_replace[[target_sps]]
  # get sps icon
  sps_uid <- get_uuid(names, n = 3)
  
  # plot icon and title
  icon <- ggplot() +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    add_phylopic(uuid = ifelse(is.na(sps_uid[3]), sps_uid[1], sps_uid[3]), x = 0.7, y = 0.9, height = 0.2) +
    geom_text(aes(x = 0.7, y = 0.75, label = paste0(names, " overview")), fontface = "italic", size = 6)+
    theme_void()
  
  ##########################
  # ABUNDANCE TIMESTEP 125 #
  ##########################
  
  # find raster for sps final abundance
  abund125 <- rast(list.files(dirout,
                              pattern = paste0("125_", target_sps, "_abundance\\.tif"), full.names = TRUE))
  
  if (length(abund125) > 0) {
  # convert to dataframe
  abundance125_df <- as.data.frame(abund125, xy = TRUE)
  
  # plot final abundance values for sps
  abundance125_plot <- ggplot() +
    geom_raster(data = abundance125_df, aes(x = x, y = y, fill = lyr1)) +
    scale_fill_gradientn(colors = rev(brewer.pal(11, "RdYlBu")), na.value = "transparent", name = "Abundance\n(nº indiv.)") +
    #scale_fill_viridis_c(name = "Nº of\nindividuals", na.value = NA) +
    # remove space between axis and plot area
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(x = "Latitude", y = "Longitude") +
    theme_minimal() +
    theme(legend.title = element_text(size = 10, face = "bold", vjust = 0.9),
          legend.position = c(0.9, 0.15),
          legend.box.background = element_rect(fill = "white", color = NA),
          axis.ticks = element_line(color = "black", linewidth = 0.5))
  } else {
    abundance125_plot <- ggplot() + labs(title = paste(target_sps, "abundance for timestep 125 not found")) + theme_void()
  }
  
  ####################
  # MODEL VALIDATION #
  ####################
  
  # find raster for timestep to validate
  abund101 <- rast(list.files(dirout,
                              pattern = paste0("101_", target_sps, "_abundance\\.tif"), full.names = TRUE))
  if (length(abund101) > 0) {
  # convert to dataframe
  abundance101_df <- as.data.frame(abund101, xy = TRUE)
  # aplly model validate function
  validationList <- validateModel_1sps(# target species to validate
                                       targetspecies = target_sps,
                                       # target sps abundance estimates from external source
                                       independentDensity = santini2022,
                                       # target sps abundance estimated with our model
                                       estimatedDensity = abundance101_df,
                                       # target sps trait data
                                       spData = spData)
  # replace sps names to better looking
  validationList <- lapply(validationList, function(df) {df$species <- names_replace[df$species]; return(df) })
  # plot model validation
  validation_plot <- ggplot(validationList$independentDensity, aes(species)) +
    geom_boxplot(aes(ymin = lw95, lower = lw75, middle = meanDensity, upper = up75, ymax = up95), stat = "identity") +
    geom_point(data = validationList$estimatedDensity,
               aes(x = species, y = estimatedDensity), color = "firebrick3", position = "jitter", size = 1) +
    labs(y = "Independent density estimate", x = element_blank(),
         #title = "Model validation",
         subtitle =  "Estimated densities in red", 
         #caption = "performed for timestep 100"
         ) +
    theme_minimal() +
    theme(plot.caption = element_text(size = 8),
          axis.text.x = element_text(size = 10)) 
  } else {
    validation_plot <- ggplot() + labs(title = paste(target_sps, "validation did not go well")) + theme_void()
          }
  
  ## WHAT WE CAN DO WITH METARANGE OUTPUTS ##
  
  ###############################
  # AVERAGE ABUNDANCE OVER TIME #
  ###############################
  
  # find all abundance raster for the sps
  abundance_files <- list.files(path = dirout,
                                pattern = paste0(".*(10[1-9]|11[0-9]|12[0-5])_", target_sps, "_abundance\\.tif$"),
                                full.names = TRUE)
  if (length(abundance_files) > 0) {
    # stack all rasters
    abundance_stack <- c(rast(abundance_files))
    # average rasters across all layers (this is different from a global mean!!)
    abundance_avg <- mean(abundance_stack, na.rm = TRUE)
    # convert to dataframe
    abundance_df <- as.data.frame(abundance_avg, xy = TRUE)
    
    # plot mean abundance over time
    abundance_overTime <- ggplot() +
      geom_raster(data = abundance_df, aes(x = x, y = y, fill = mean)) +
      scale_fill_viridis_c(name = "Nº of\nindividuals") +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      labs(x = "Latitude", y = "Longitude") +
      theme_minimal() +
      theme(legend.title = element_text(size = 10, face = "bold", vjust = 0.9),
            legend.position = c(0.9, 0.15),
            legend.box.background = element_rect(fill = "white", color = NA),
            axis.ticks = element_line(color = "black", linewidth = 0.5))
  } else {
    abundance_overTime <- ggplot() + labs(title = paste(target_sps, "Abundance data not found")) + theme_void()
  }
  
  ##################################
  # PROPORTION OF ABUNDANCE CHANGE #
  ##################################
  
  if (exists("abundance125_df") && exists("abundance101_df")) {
  # go back for the final timestep abundance raster for the sps
  abundance125_df <- abundance125_df %>% 
    # add timestep info
    mutate(timestep = 125)
  
  # go back for the 1st timestep after the burn-in
  abundance101_df <- abundance101_df %>% 
    # add timestep info
    mutate(timestep = 101)
  
  # bind dataframes from both timesteps needed
  abund_change <- rbind(abundance101_df, abundance125_df) %>% 
    # rename to more understandable
    rename("abundance" = lyr1) %>% 
    # calculate the proportion of abundance change
    mutate(abund_change_prop = (abundance - abundance[timestep == 101])/abundance[timestep == 101])
  
  # proportion of abundance change plot
  prop_abund_change <- ggplot() +
    geom_raster(data = abund_change, aes(x = x, y = y, fill = abund_change_prop)) +
    scale_fill_gradientn(colors = brewer.pal(11, "RdBu"), na.value = "transparent",
                         #limits = c(-1, 1), # Set limits to -1 and 1
                         #values = scales::rescale(c(-1, 0, 1))
                        ) + # Ensure 0 is in the middle
    labs(x = "Latitude", y = "Longitude", fill = "Proportion of\nAbundance\nChange") +
    theme_minimal() +
    theme(legend.title = element_text(size = 10, face = "bold", vjust = 0.9),
          legend.position = c(0.8, 0.2),
          # remove background and line from legend box
          legend.box.background = element_rect(fill = NA, color = NA),
          # remove grid lines
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # remove coordinates and axis labels
          axis.title = element_blank(),
          axis.text = element_blank())
  } else {
    prop_abund_change <- ggplot() + labs(title = paste(target_sps, "Proportion of abundance change\ndata not found")) + theme_void()
  }
  
  ####################
  # DISPERSAL CHANGE #
  ####################
  
  # list dispersal change files
  dispersal_files <- list.files(path = dirout,
                                pattern = paste0(".*(10[1-9]|11[0-9]|12[0-5])_", target_sps, "_dispersal_change\\.tif"),
                                full.names = TRUE)
  
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
      labs(x = "Latitude", y = "Longitude") +
      scale_fill_gradient2(low = "chartreuse3", mid = "white", high = "firebrick3", midpoint = 0,
                           breaks = c(min(dispersal_df$mean, na.rm = TRUE), mean(dispersal_df$mean, na.rm = TRUE), max(dispersal_df$mean, na.rm = TRUE)),
                           labels = c(paste0(round(min(dispersal_df$mean, na.rm = TRUE)), "\n(emigration)"),
                                      paste(round(mean(dispersal_df$mean, na.rm = TRUE)), "\n(no change)"),
                                      paste(round(max(dispersal_df$mean, na.rm = TRUE)), "\n(immigration)")),
                           name = "Mean Dispersal\nChange Over\nTime (nº of indiv.)") +
      theme_minimal() +
      theme(legend.title = element_text(size = 10, face = "bold", vjust = 1.9),
            legend.position = c(0.9, 0.2),
            legend.text = element_text(vjust = 0.8),
            # remove background and line from legend box
            legend.box.background = element_rect(fill = NA, color = NA),
            # remove grid lines
            panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # remove coordinates and axis labels
            axis.title = element_blank(),
            axis.text = element_blank())
  } else {
    dispersal_plot <- ggplot() + labs(title = paste(target_sps, "Dispersal data not found")) + theme_void()
  }
  
  ########################################
  # COMBINE ALL PLOTS FOR MODEL OVERVIEW #
  ########################################
  
  # define plots layout
  layout <- "AABBCC
             DDEEFF"
  
  # build figure 2
  figure2 <-abundance125_plot + validation_plot + icon + abundance_overTime + prop_abund_change + dispersal_plot +
    plot_layout(design = layout) +
    plot_annotation(tag_levels = list(c('A', 'B', ' ', 'C', 'D', 'E', 'F'))) +
    plot_annotation(
      caption = "(A) Abundance for timestep 125, (B) Model validation for timestep 101, (C) Mean abundance over time,\n(D) Proportion of abundance change between timesteo 125 and 101, (E) Mean dispersal change over time") & 
    theme(plot.subtitle = element_text(size = 12),
          plot.caption = element_text(size = 12))
  
  # save figure 2
  ggsave(plot = figure2,
         filename = file.path(dirout, paste0("modelOverview_", target_sps, ".tiff")),
         bg = 'white', width = 400, height = 400, units = "mm", dpi = 1200, compression = "lzw")

  # print Finished for message
  message(paste0("Finished for ", target_sps))  
}  
