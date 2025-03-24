###########################
# MAMMALS OUTPUTS FIGURES #
###########################
# Inês Silva
# 21 March 2025

# This script contains code to produce 2 figures per species and one figure for the taxa

## Figure 1 - Species suitability maps over time
## Figure 2 - Species overview figure with:
                    ### models initial values (abund & reproduction rate)
                    ### models final rasters (abund & dispersal change)
                    ### analysis we can do with model results (abundance over time, proportion of abundance & dispersal chnage over time)
## Figure 3 - Taxa overview with :
                    ### mean taxa abundance over time
                    ### taxa's proportion of abundance change
                    ### mean taxa dispersal change


## ADD FUNCTION TO CUSTIM FUNCTIONS ##

validateModel_1sps <- function(
    targetspecies, independentDensity, estimatedDensity, spData) {
  # compares mean density estimated by model per cell with
  # predicted density from independent model extract predicted abundance 
  # and join with observed abundance
  # based on validateModel1 from MechSpatCons but uses estimated
  # number of individuals from rangeshifter output dataframe
  # instead from raster
  
  ## species density estimates by an independent source (akin to observed density)
  independentDensity <- independentDensity %>% 
    dplyr::filter(Species %in% target_sps) %>%
    dplyr::select(Species, lw95, lw75, PredMd, up75, up95) %>%
    mutate(
      lw95 = as.numeric(lw95),
      lw75 = as.numeric(lw75),
      PredMd = as.numeric(PredMd), # Predicted population density (individuals/km2)
      up75 = as.numeric(up75),
      up95 = as.numeric(up95)) %>%
    rename(
      species = Species,
      meanDensity = PredMd
    )
  
  ## species density estimated by metaRange
  predicted <- estimatedDensity %>%
    mutate(species = target_sps) %>% 
    dplyr::group_by(species, x, y) %>%
    dplyr::summarise(
      meanNInd = mean(lyr1),
      .groups = 'drop') %>%
    as.data.frame()
  
  spData2 <- spData %>%
    dplyr::select(Species, ModellingRes) %>%
    dplyr::filter(Species == target_sps) %>% 
    rename(species = Species) %>%
    as.data.frame()
  
  estimatedDensityJoin <- dplyr::inner_join(predicted, spData2, by = "species") %>%
    mutate(estimatedDensity = meanNInd/ModellingRes)
  
  ## compare observed with predicted density
  list <- list(independentDensity, estimatedDensityJoin)
  names(list) <- c("independentDensity", "estimatedDensity")
  return(list)
}

################################################################################

library(rnaturalearth)
library(ggplot2)
library(sf)
library(tidyterra)
library(RColorBrewer)
library(readxl)
library(dplyr)
library(stringr)
library(terra)
library(patchwork)


# directories
dirinput
dirout
#output_folder <- "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE"

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

# import external data for model validation
santini2022 <- read_excel("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/SRIT_ANDRE/external_data/geb13476-sup-0002-tables1.xls") %>%
  mutate(Species = str_replace_all(Species, " ", ""))

# import species traits df for model validation
spData <- read.csv(file.path(dirinput, "metaRangeSpeciesDataframe.csv"))


# Next big for loop builds two figures per species:
  ## Figure 1 - Species suitability maps over time
  ## Figure 2 - Species overview figure with:
    ### models initial values (abund & reproduction rate)
    ### models final rasters (abund & dispersal change)
    ### analysis we can do with model results (abundance over time, proportion of abundance & dispersal chnage over time)


# go through each species
for (target_sps in species_names) {
  
  #####################
  # SUITABILITY PLOTS #
  #####################
  
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
         filename = file.path(dirout, paste0(target_sps, "_suitabilityOverTime.tiff")),
         bg = 'white', width = 400, height = 400, units = "mm", dpi = 1200, compression = "lzw")
  
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
  
  ## METARANGE FINAL TIMESTEP PLOTS ##
  
  #######################
  # ABUNDANCE OVER TIME #
  #######################
  
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
  
  ############################
  # MEAN ABUNDANCE OVER TIME #
  ############################
  
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
    scale_fill_gradientn(colors = brewer.pal(11, "RdBu"), na.value = "transparent") +
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
  
  ####################
  # COMBINE FIGURE 2 #
  ####################
  
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
         filename = file.path(dirout, paste0(target_sps, "_modelOverview.tiff")),
         bg = 'white', width = 400, height = 400, units = "mm", dpi = 1200, compression = "lzw")

  # print Finished for message
  message(paste0("Finished for ", target_sps))  
}  

################################################################################


###############################
# MAMMALS ABUNDANCE OVER TIME #
###############################

# list for abundance raster for all timesteps & species
abundance_lists <- list()
# list for abundance rasters averaged per species
averaged_species_rasters <- list()
# go through each species
for (target_sps in species_names) {
  
  # Step 1 - Find abundance rasters for all species
  
  abundance_files <- list.files(path = dirout,
                                # import only raster for timesteps 101 to 125
                                pattern = paste0(".*(10[1-9]|11[0-9]|12[0-5])_", target_sps, "_abundance\\.tif"),
                                full.names = TRUE)
  if (length(abundance_files) > 0) {
    # read all abundance rasters for the species
    abundance_rasters <- lapply(abundance_files, rast)
    # store all rasters into a list
    abundance_lists[[target_sps]] <- abundance_rasters
  } else {
    # print a warning if no rasters are found a target species
    abundance_lists[[target_sps]] <- NULL
    print(paste("No abundance files found for", target_sps))
  }
  
  # Step 2 - Average rasters per species
  
  # stack species rasters
  abundance_stack <- terra::rast(abundance_lists[[target_sps]])
  # calculate the mean across all timesteps (layers)
  averaged_raster <- terra::mean(abundance_stack, na.rm = TRUE)
  # store the averaged raster in a list
  averaged_species_rasters[[target_sps]] <- averaged_raster
}

# Step 3 - Resample each raster according to a template raster
## since each species was modelled with a different resolution we need to RESAMPLE
## rasters back to a common resolution so we average everyone into one raster for
## all mammals

# list for species resampled rasters (all with same resolution)
resampled_rasters <- list()
# go through each species
for (target_sps in species_names) {
  # extract current raster from the list
  current_raster <- averaged_species_rasters[[target_sps]]
  # extract template raster
  template_raster <- averaged_species_rasters$Cervuselaphus
  
# resample each raster
resampled_rasters[[target_sps]] <- resample(# raster to change resolution
         x = current_raster,
         y = template_raster,
         # method to use for resampling (nearest neighbor is not the best option for continuous data)
         method = "bilinear")
}

# Step 4 - Average all resampled rasters into one for mammals
all_mammals_stack <- terra::rast(unlist(resampled_rasters))
mammals_meanAbundance <- terra::mean(all_mammals_stack, na.rm = TRUE)
# transform into dataframe
mammals_meanAbundance_df <- as.data.frame(mammals_meanAbundance, xy = TRUE)

# Step 5 - Plot mean abundance over time
mammals_abundance <- ggplot() +
  geom_raster(data = mammals_meanAbundance_df, aes(x = x, y = y, fill = mean)) +
  scale_fill_viridis_c(name = "Nº of\nindividuals") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Latitude", y = "Longitude") +
  theme_minimal() +
  theme(legend.title = element_text(size = 10, face = "bold", vjust = 0.9),
        legend.position = c(0.9, 0.15),
        legend.box.background = element_rect(fill = "white", color = NA),
        axis.ticks = element_line(color = "black", linewidth = 0.5))

##########################################
# MAMMALS PROPORTION OF ABUNDANCE CHANGE #
##########################################

# Step 1 - Find abundance rasters for 1st and last timestep for each sps & calculate
## propotion of abundance change

# list for prop abundance chnage rasters for all sps
prop_abund_change <- list()
# go through each species
for (target_sps in species_names) {
  # import abundance raster after the burn-in for sps
  sps101 <- rast(list.files(path = dirout,
            pattern = paste0(".*101_", target_sps, "_abundance\\.tif"),
            full.names = TRUE))
  # import final abundance raster for sps
  sps125 <- rast(list.files(path = dirout,
                       pattern = paste0(".*125_", target_sps, "_abundance\\.tif"),
                       full.names = TRUE))
  
  # calculate the prop abundance change raster for each sps
  prop_abund_change[[target_sps]] <- (sps125 - sps101) / sps101
}

# Step 2 - Resample raster so all have the same resolution

# list to save resampled prop abund change rasters
resampled_prop_rasters <- list()
# go through each species
for (target_sps in species_names) {
  # extract current raster from the list
  current_raster <- prop_abund_change[[target_sps]]
  # extract template raster
  template_raster <- prop_abund_change$Cervuselaphus
  
  # resample each raster
  resampled_prop_rasters[[target_sps]] <- resample(# raster to change resolution
                                                    x = current_raster,
                                                    y = template_raster,
                                                    # method to use for resampling (nearest neighbor is not the best option for continuous data)
                                                    method = "bilinear")
}

# Step 3 - Average across all species to get proportion of abund change for MAMMALS
# stack resampled prop abund change rasters for all sps
all_props_stack <- terra::rast(unlist(resampled_prop_rasters))
# average across all layers (species)
mammals_propAbundChange <- terra::mean(all_props_stack, na.rm = TRUE)
# convert to df
mammals_propAbundChange_df <- as.data.frame(mammals_propAbundChange, xy = TRUE)

# Step 4 - Plot mean propotion of abundance change
mammals_abundChange <- ggplot() +
  geom_raster(data = mammals_propAbundChange_df, aes(x = x, y = y, fill = mean)) +
  scale_fill_gradientn(colors = brewer.pal(11, "RdBu"), na.value = "transparent") +
  labs(x = "Latitude", y = "Longitude", fill = "Proportion of\nAbundance\nChange") +
  theme_minimal() +
  theme(legend.title = element_text(size = 10, face = "bold",vjust = 0.9),
        legend.position = c(0.9, 0.2),
        # remove background and line from legend box
        legend.box.background = element_rect(fill = NA, color = NA),
        # remove grid lines
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove coordinates and axis labels
        axis.title = element_blank(),
        axis.text = element_blank())

######################################
# MAMMALS DISPERSAL CHANGE OVER TIME #
######################################

# list for dispersal change raster for all timesteps & species
dispChange_list <- list()
# list for dispersal change rasters averaged per species
averaged_dispChange_rasters <- list()
# go through each species
for (target_sps in species_names) {
  
  # Step 1 - Find dispersal change rasters for all species
  
  dispChange_files <- list.files(path = dirout,
                                # import only raster for timesteps 101 to 125
                                pattern = paste0(".*(10[1-9]|11[0-9]|12[0-5])_", target_sps, "_dispersal_change\\.tif"),
                                full.names = TRUE)
  if (length(dispChange_files) > 0) {
    # read all dispersal change rasters for the species
    dispChange_rasters <- lapply(dispChange_files, rast)
    # store all rasters into a list
    dispChange_list[[target_sps]] <- dispChange_rasters
  } else {
    # print a warning if no rasters are found a target species
    dispChange_list[[target_sps]] <- NULL
    print(paste("No abundance files found for", target_sps))
  }
  
  # Step 2 - Average rasters per species
  
  # stack species rasters
  dispChange_stack <- terra::rast(dispChange_list[[target_sps]])
  # calculate the mean across all timesteps (layers)
  averaged_dispChange_raster <- terra::mean(dispChange_stack, na.rm = TRUE)
  # store the averaged raster in a list
  averaged_dispChange_rasters[[target_sps]] <- averaged_dispChange_raster
}

# Step 3 - Resample each raster according to a template raster
## since each species was modelled with a different resolution we need to RESAMPLE
## rasters back to a common resolution so we average everyone into one raster for
## all mammals

# list for species resampled rasters (all with same resolution)
resampled_dispChange_rasters <- list()
# go through each species
for (target_sps in species_names) {
  # extract current raster from the list
  current_raster <- averaged_dispChange_rasters[[target_sps]]
  # extract template raster
  template_raster <- averaged_dispChange_rasters$Cervuselaphus
  
  # resample each raster
  resampled_dispChange_rasters[[target_sps]] <- resample(# raster to change resolution
    x = current_raster,
    y = template_raster,
    # method to use for resampling (nearest neighbor is not the best option for continuous data)
    method = "bilinear")
}

# Step 4 - Average all resampled rasters into one for mammals
dispChange_mammals_stack <- terra::rast(unlist(resampled_dispChange_rasters))
mammals_meanDispChange <- terra::mean(dispChange_mammals_stack, na.rm = TRUE)
# transform into dataframe
mammals_meanDispChange_df <- as.data.frame(mammals_meanDispChange, xy = TRUE)

# Step 5 - Plot mean abundance over time
mammals_dispChange <- ggplot() +
  geom_raster(data = mammals_meanDispChange_df, aes(x = x, y = y, fill = mean)) +
  scale_fill_gradient2(low = "chartreuse3", mid = "white", high = "firebrick3", midpoint = 0, name = "Mean Dispersal\nChange Over\nTime (nº of indiv.)") +
  theme_minimal() +
  theme(legend.title = element_text(size = 10, face = "bold",vjust = 0.9),
        legend.position = c(0.9, 0.2),
        # remove background and line from legend box
        legend.box.background = element_rect(fill = NA, color = NA),
        # remove grid lines
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove coordinates and axis labels
        axis.title = element_blank(),
        axis.text = element_blank())


mammals_overview <- mammals_abundance + mammals_abundChange + mammals_dispChange


mammals_overview +
  plot_annotation(tag_levels = 'A') +
  plot_annotation(
  title = "Mammal species results",
  subtitle = "(A) Mean abundance over time, (B) Proportion of abundance change and (C) Mean dispersal chnage over",
  caption = paste0("Based on ", length(species_names), " species")) & 
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 9),
        plot.caption = element_text(size = 9))










# LIXO #


#################
# REFERENCE MAP #
#################

world <- ne_countries(scale = "medium", returnclass = "sf")
continent <- world[world$region_un == "Europe", ]
region_sf <- world[world$name == target_region, ]

reference_map <- ggplot() +
  geom_sf(data = continent, fill = "gray95", color = "black") +
  geom_sf(data = region_sf, fill = "tan1", color = "black") +
  labs(caption = "Data: Natural Earth") +
  coord_sf(xlim = c(-10, 45), ylim = c(37, 70)) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())

###########################################
# COMBINED SUITBAILITY WITH REFERENCE MAP #
###########################################

final_suitability <- suitability_plot 
#inset_element(reference_map, 0.6, 0, 1.15, 0.2)




# # original raster layer's names
# raster_layer_names <- names(sps_suitability)
# # correspond raster layers names to new names
# label_vector <- setNames(timestep_labels, raster_layer_names)
# 
# suit_overTime <- gplot(rast(suitability_file)) +
#   geom_tile(aes(fill = value)) +
#   facet_wrap(~ variable, ncol = 10, labeller = as_labeller(label_vector)) +
#   labs(x = "Latitude", y = "Longitude") + 
#   coord_equal() +
#   scale_fill_viridis_c(name = "Suitability\nIndex", na.value=NA) +
#   labs(title = "Suitability Index") +
#   theme_minimal() +
#   theme(legend.title = element_text(size = 10),
#         panel.background =  element_blank(),
#         panel.spacing.y = unit(0.5, "lines")) 
