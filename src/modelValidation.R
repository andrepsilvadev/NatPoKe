########################
# MODEL VALIDATION FIG #
######### MIS ##########
# 24 Jan 2025

# GOAL: Compare mean species densities estimated from two sources.


# WHAT IS MODEL VALIDATION?
# Model validation is the process of determining whether the model accurately
# represents the behavior of the system (Aumann, 2007). Model validity should be
# evaluated both operationally (i.e., by determining if model output agrees with
# observed data) and conceptually (i.e., by determining whether the theory and
# assumptions underlying the model are justifiable; Sargent, 1984; Rykiel, 1996).

# Kerr LA, Goethel DR. Simulation Modeling as a Tool for Synthesis of Stock Identification Information. In: Stock Identification Methods, 2014, 501-533

start.time <- Sys.time() # start the clock
# packages
library(readxl)
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(terra)
library(data.table)


###################
# DATASETS NEEDED #
###################

# To validate the metaRange model we need:
  # (1) targetspecies: Vector of species names for which the validation will be performed
  # (2) independentDensity: dataframe containing species density estimates from an Santini 2022
  # (3) estimatedDensity: dataframe containing species abundance data derived from the model output
  # (4) spData: dataframe with species traits (with ModellingRes) to calculate density from abundance
        # for now, 20250130, ModellingRes will be the pixel size of one of the rasters BUT THIS WILL CHANGE WHENEVER SOMEONE THINKS OF THIS
  # (5) validationYear: The specific year (or time step) used for validation

# (1) targetspecies
targetspecies <- read.csv(file.path(dirinput, "metaRangeSpeciesDataframe.csv")) %>% 
  dplyr::pull(Species)

# (2) independentDensity
santini2022 <- read_excel("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/SRIT_ANDRE/external_data/geb13476-sup-0002-tables1.xls") %>% 
  # santini's dataframe has species names with spaces but metaRange does not like spaces
  # remove spaces again
  mutate(Species = str_replace_all(Species, " ", ""))

# (3) estimatedDensity
estimatedDensity <- fread(file.path(dirout, paste0("metaRangeOutputs", runname, ".csv"))) 

# import a raster to get cell size
size <- res(terra::rast(file.path(dirinput, "Lynxlynx_suitability_cropped_modified_reprojectedKm.tif")))

# (4) spData
spData <- read.csv(file.path(dirinput, "metaRangeSpeciesDataframe.csv")) 
#IF WE WANT TO GO BACK TO THE ORIGINAL IDEA OF USING SANTINI'S "MEASUREMENTS" OF PREDICTED DENSITIES
# to get the PredMd which is Starting density per cell (individuals/cell) from santini 2022
#left_join(dplyr::select(santini2022, Species, PredMd), by = c("species" = "Species")) %>%
# create ModellingRes variable
#mutate(ModellingRes = ceiling(sqrt(2/as.numeric(PredMd)))) # change to a specific value 
# ADD NOTE TO USE VALUES FROM SPEPS TRAITS DATASET

# (5) validationYear
# defined directly in the function

#############################
# MODEL VALIDATION FUNCTION #
#############################

# this model validation uses independent estimates
# André's comments are in lowercase letters within the function

validateModel1.1 <- function(
    targetspecies, independentDensity, estimatedDensity, spData, validationYear) {
  # compares mean density estimated by model per cell with
  # predicted density from independent model extract predicted abundance 
  # and join with observed abundance
  # based on validateModel1 from MechSpatCons but uses estimated
  # number of individuals from rangeshifter output dataframe
  # instead from raster
  
  ## species density estimates by an independent source (akin to observed density)
  independentDensity <- independentDensity %>% 
    dplyr::filter(Species %in% targetspecies) %>%
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
    dplyr::filter(species %in% targetspecies) %>%
    dplyr::filter(timestep %in% validationYear) %>% # validate model at the equilibrium (burn-in years)
    dplyr::group_by(species, x,y) %>%
    dplyr::summarise(
      meanNInd = mean(abundance),
      .groups = 'drop') %>%
    as.data.frame()
  
  spData2 <- spData %>%
    dplyr::select(Species, ModellingRes) %>%
    rename(species = Species) %>%
    as.data.frame()
  
  estimatedDensityJoin <- dplyr::inner_join(predicted, spData2, by = "species") %>%
    mutate(estimatedDensity = meanNInd/ModellingRes)
  
  ## compare observed with predicted density
  list <- list(independentDensity, estimatedDensityJoin)
  names(list) <- c("independentDensity", "estimatedDensity")
  return(list)
}



# applying the function
validationList <- validateModel1.1(
  targetspecies = targetspecies,
  independentDensity = santini2022,
  estimatedDensity = estimatedDensity,
  spData = spData,
  validationYear = 101
) 

#########################
# MODEL VALIDATION PLOT #
#########################

# since names do have a species in between words to look nice we have to replace names before plotting
names_replace <- c("Alcesalces" = "Alces alces",
                   "Lynxlynx" = "Lynx lynx", 
                   "Cervuselaphus" = "Cervus elaphus",
                   "Rangifertarandus" = "Rangifer tarandus")

validationList <- lapply(validationList, function(df) {
  df$species <- names_replace[df$species]
  return(df)
})

pvalidation1 <- ggplot(validationList$independentDensity, aes(species)) +
  geom_boxplot(
    aes(ymin = lw95, lower = lw75, middle = meanDensity, upper = up75, ymax = up95),
    stat = "identity") +
  ylim(0, 8)+
  geom_point(data = validationList$estimatedDensity,
             aes(x = species, y = estimatedDensity),
             color = "red",
             position = "jitter",
             size = 1) +
  ylab("Independent density estimate") +
  xlab("Species") +
  ggtitle(label = "Model validation - estimated densities in red") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

pvalidation1

# saving the plot
ggsave(filename = file.path(dirout, paste0("ModelValidation", runname, ".tiff")),
       plot = pvalidation1,
       bg = 'white', width = 300, height = 230, units = "mm", dpi = 1200, compression = "lzw")

pvalidation2 <- ggplot(validationList$independentDensity, aes(x = "", y = meanDensity)) +
  geom_boxplot(
    aes(ymin = lw95, lower = lw75, middle = meanDensity, upper = up75, ymax = up95),
    stat = "identity"
  ) +
  geom_point(data = validationList$estimatedDensity,
             aes(x = "", y = estimatedDensity),
             color = "red",
             position = position_jitter(width = 0.2),
             size = 1) +
  facet_wrap(~ species, scales = "free_y") + 
  ylab("Independent density estimate") +
  xlab("Species") +
  ggtitle("Model validation - estimated densities in red") + 
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
# saving the plot
ggsave(filename = file.path(dirout, paste0("ModelValidation2", runname, ".tiff")),
       plot = pvalidation2,
       bg = 'white', width = 300, height = 230, units = "mm", dpi = 1200, compression = "lzw")





end.time <- Sys.time() # end the clock
time.taken <- round(end.time - start.time) # calculate time taken to run the complete script
time.taken


