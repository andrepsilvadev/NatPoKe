
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

target_sps <- "Cervuselaphus"

# since names do have a species in between words to look nice we have to replace names before plotting
names_replace <- c("Alcesalces" = "Alces alces",
                   "Lynxlynx" = "Lynx lynx", 
                   "Cervuselaphus" = "Cervus elaphus",
                   "Rangifertarandus" = "Rangifer tarandus")

# new names for the timsteps
timestep_labels <- paste0("Timestep ", 101:125)


#####################
# SUITABILITY PLOTS #
#####################


# read raster 
sps_suitability <- rast(list.files(dirinput, pattern = "Cervuselaphus_suitability_cropped_modified_reprojectedKm.tif", full.names = TRUE))
library(tidyterra)
names(sps_suitability) <- timestep_labels
ggplot() +
  geom_spatraster(data = sps_suitability) +
  facet_wrap(.~lyr, ncol = 10) +
  scale_fill_viridis_c(name = "Suitability\nIndex", na.value=NA) +
  labs(title = "Suitability Index") +
  theme_minimal() +
  theme(legend.title = element_text(size = 10),
        panel.background =  element_blank(),
        panel.spacing.y = unit(0.5, "lines")) 

  

# original raster layer's names
raster_layer_names <- names(sps_suitability)
# correspond raster layers names to new names
label_vector <- setNames(timestep_labels, raster_layer_names)

suit_overTime <- gplot(rast(suitability_file)) +
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable, ncol = 10, labeller = as_labeller(label_vector)) +
  labs(x = "Latitude", y = "Longitude") + 
  coord_equal() +
  scale_fill_viridis_c(name = "Suitability\nIndex", na.value=NA) +
  labs(title = "Suitability Index") +
  theme_minimal() +
  theme(legend.title = element_text(size = 10),
        panel.background =  element_blank(),
        panel.spacing.y = unit(0.5, "lines")) 

########################
# INITIAL VALUES PLOTS #
########################

############################
# METARANGE FINAL TIMESTEP #
############################

## ABUNDANCE

# import final abundance raster
abund125 <- rast(list.files(dirout, pattern = "125_Cervuselaphus_abundance\\.tif", full.names = TRUE))

# transform into dataframe
abundance125_df <- as.data.frame(abund125, xy = TRUE)
# plot average abundance
abundance125_plot <- ggplot() +
  geom_raster(data = abundance125_df, aes(x = x, y = y, fill = lyr1)) +
  scale_fill_viridis_c(name = "Nº of\nindividuals", na.value = NA) +
  labs(title = "Abundance in Timestep 125", x = "Latitude", y = "Longitude") +
  theme_minimal() +
  theme(legend.title = element_text(size = 10))

## MODEL VALIDATION

# import final abundance raster
abund101 <- rast(list.files(dirout, pattern = "101_Cervuselaphus_abundance\\.tif", full.names = TRUE))
# transform into dataframe
abundance101_df <- as.data.frame(abund101, xy = TRUE)

# import independentDensity
santini2022 <- read_excel("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/SRIT_ANDRE/external_data/geb13476-sup-0002-tables1.xls") %>% 
  # santini's dataframe has species names with spaces but metaRange does not like spaces
  # remove spaces again
  mutate(Species = str_replace_all(Species, " ", ""))
# species trait data
spData <- read.csv(file.path(dirinput, "metaRangeSpeciesDataframe.csv")) 

# applying model validation function
validationList <- validateModel_1sps(
  targetspecies = target_sps,
  independentDensity = santini2022,
  estimatedDensity = abundance101_df,
  spData = spData
) 

# replace sps names to be better looking
validationList <- lapply(validationList, function(df) {
  df$species <- names_replace[df$species]
  return(df)
})

pvalidation1 <- ggplot(validationList$independentDensity, aes(species)) +
  geom_boxplot(
    aes(ymin = lw95, lower = lw75, middle = meanDensity, upper = up75, ymax = up95),
    stat = "identity") +
  #ylim(0, 8)+
  geom_point(data = validationList$estimatedDensity,
             aes(x = species, y = estimatedDensity),
             color = "red",
             position = "jitter",
             size = 1) +
  labs(y = "Independent density estimate", x = "Species", title = "Model validation - estimated densities in red", caption = "performed for timestep 100") +
  theme_minimal() +
  theme(plot.caption = element_text(size = 8))

# MODEL RESULTS FIGURES
abundance125_plot + pvalidation1

####################################
# WHAT WE CAN DO WITH THIS FIGURES #
####################################

## ABUNDANCE OVER TIME 

# list abundance files
abundance_files <- list.files(path = dirout, pattern = "Cervuselaphus_abundance\\.tif", full.names = TRUE)
if (length(abundance_files) > 0) {
  # read rasters & stack them
  abundance_stack <- c(rast(abundance_files))
  # average rasters across ALL years
  abundance_avg <- mean(abundance_stack, na.rm = TRUE)
  # transform into dataframe
  abundance_df <- as.data.frame(abundance_avg, xy = TRUE)
  
  # plot average abundance
  abundance_overTime <- ggplot() +
    geom_raster(data = abundance_df, aes(x = x, y = y, fill = mean)) +
    scale_fill_viridis_c(name = "Nº of\nindividuals") +
    labs(title = "Mean Abundance over time", x = "Latitude", y = "Longitude") +
    theme_minimal() +
    theme(legend.title = element_text(size = 10))
} else {
  abundance_overTime <- ggplot() + labs(title = paste(species, "Abundance data not found")) + theme_void()
}

## ABUNDANCE CHANGE 

# abund_df final timestep
abundance125_df <- abundance125_df %>% 
  mutate(timestep = 125)

# abund_df initial timesteps after burn in
abund100 <- rast(list.files(dirout, pattern = "100_Cervuselaphus_abundance\\.tif", full.names = TRUE))
# transform into dataframe
abundance100_df <- as.data.frame(abund101, xy = TRUE) %>% 
  mutate(timestep = 100)

abund_change <- rbind(abundance100_df, abundance125_df) %>% # bind df from both timesteps needed
  # rename to more understandable
  rename("abundance" = lyr1) %>% 
  # calculate the proportion of abundance change
  mutate(abund_change_prop = (abundance - abundance[timestep == 100])/abundance[timestep == 100])

library(RColorBrewer)
prop_abund_change <- ggplot()+
  geom_raster(data = abund_change, aes(x = x, y = y, fill = abund_change_prop)) +
  scale_fill_gradientn(colors = brewer.pal(11, "RdBu"), na.value = "transparent") +
  labs(x = "Latitude", y = "Longitude", fill = "Proportion of\nAbundance Change") +
  theme_minimal() +
  theme(legend.title = element_text(size = 10))

## DISPERSAL CHANGE

# list dispersal change files
dispersal_files <- list.files(path = dirout, pattern = "Cervuselaphus_dispersal_change\\.tif", full.names = TRUE)
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
    labs(title = "Mean Dispersal Change Over Time", x = "Latitude", y = "Longitude") +
    scale_fill_gradient2(low = "green", mid = "white", high = "red", midpoint = 0, name = "Nº of\nindividuals)") +
    theme_minimal() +
    theme(legend.title = element_text(size = 10))
} else {
  dispersal_plot <- ggplot() + labs(title = paste(species, "Dispersal data not found")) + theme_void()
}


Fragmento do código

library(ggplot2)

# Sample data with a color variable
data <- data.frame(
  x = 1:100,
  y = rnorm(100),
  category = rep(c("A", "B", "C", "D"), each = 25),
  color_var = rnorm(100) # Variable for color mapping
)

# Facet plot with a color scale bar
aa <- ggplot(data, aes(x = x, y = y, color = color_var)) +
  geom_point() +
  facet_wrap(~ category) +
  labs(
    title = "Facet Plot with Color Scale",
    x = "X-axis",
    y = "Y-axis",
    color = "Color Variable" # Label for the color scale
  ) +
  scale_color_viridis_c(option = "viridis") + # Use viridis color scale (or other options)
  theme_minimal()

aa / (abundance_plot + pvalidation1) / (abundance_overTime + prop_abund_change + dispersal_plot)
