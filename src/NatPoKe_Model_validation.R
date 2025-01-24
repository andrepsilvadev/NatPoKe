########################
# MODEL VALIDATION FIG #
######### MIS ##########
# 24 Jan 2025

# packages
library(readxl)
library(stringr)

###################
# DATASETS NEEDED #
###################

santini2022 <- read_excel("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/SRIT_ANDRE/external_data/geb13476-sup-0002-tables1.xls") %>% 
  # santini's dataframe has species names with spaces but metaRange does not like spaces
  # remove spaces again
  mutate(Species = str_replace_all(Species, " ", ""))

targetspecies <- c("Alcesalces", "Lynxlynx")

estimatedDensity <- read.csv("~/NatPoKe/example/mammals_try2/results/all_data_together_22Jan.csv") %>% 
  mutate(cell_id = paste0(x,y))

spData <- read_csv("~/NatPoKe/example/mammals_try2/target_metarange_mammals20250110.csv") %>% 
  # to get the PredMd which is Starting density per cell (individuals/cell) from santini 2022
  left_join(dplyr::select(santini2022, Species, PredMd), by = c("species" = "Species")) %>%
  # create ModellingRes variable
  mutate(ModellingRes = ceiling(sqrt(2/as.numeric(PredMd))))

#############################
# MODEL VALIDATION FUNCTION #
#############################

# this model validation uses independent estimates
# André's comments are in lowercase letters
# MY COMMENTS ARE IN CAPS LOCK

validateModel1.1 <- function(
    targetspecies, independentDensity, estimatedDensity, spData, validationYear) {
  # compares mean density estimated by model per cell with
  # predicted density from independent model extract predicted abundance 
  # and join with observed abundance
  # based on validateModel1 from MechSpatCons but uses estimated
  # number of individuals from rangeshifter output dataframe
  # instead from raster
  
  ## species density estimates by an independent source (akin to observed density)
  ### THIS IS THE SANTINI DATAFRAME FROM SUP MATERIALS
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
  
  ## species density estimated by rangeshifter
  ### THIS IS THE OUTPUT DATAFRAME OF THE MODEL THTA NEEDS TO HAVE AT LEAST SPS NAMES, YEAR, CELLID & ABUNDANCE
  predicted <- estimatedDensity %>%
    dplyr::filter(species %in% targetspecies) %>%
    dplyr::filter(time %in% validationYear) %>% # validate model at the equilibrium (burn-in years)
    dplyr::group_by(species, cell_id) %>%
    dplyr::summarise(
      meanNInd = mean(abundance),
      .groups = 'drop') %>%
    as.data.frame()
  
  ### THIS IS THE TRAIT DATAFRAME BUILT FOR RANGESHIFTER FROM THE combined_traits_data DF
  spData2 <- spData %>%
    dplyr::select(species, ModellingRes) %>%
    #rename(species = Species) %>%
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
  validationYear = 8
) 

#########################
# MODEL VALIDATION PLOT #
#########################

#since names do have a species in between words to look nice we have to replace names before plotting
names_replace <- c("Alcesalces" = "Alces alces",
                   "Lynxlynx" = "Lynx lynx")

validationList <- lapply(validationList, function(df) {
  df$species <- names_replace[df$species]
  return(df)
})

pvalidation1 <- ggplot(validationList$independentDensity, aes(species)) +
  geom_boxplot(
    aes(ymin = lw95, lower = lw75, middle = meanDensity, upper = up75, ymax = up95),
    stat = "identity") +
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

#pvalidation1

# ggsave(path = paste0("./output/", runname, "/Outputs"),
#        filename = "ComparisonToIndependentModel.png",
#        plot = pvalidation1,
#        dpi = 600,
#        width = 25,
#        height = 10,
#        units = "cm",
#        bg = "white")

