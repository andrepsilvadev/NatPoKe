############################
# TESTING VALIDATEMODEL1.2 #
############################
# Ines Silva
# 15 april 2025


dirinput <- "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/26Mar2025_tutorial/Inputs"
dirout <- "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/26Mar2025_tutorial/Outputs"
runname <- "26Mar2025_tutorial"


# (1) targetspecies
species_names <- read.csv(file.path(dirinput, "metaRangeSpeciesDataframe.csv")) %>% 
  dplyr::pull(Species)

# (2) independentDensity
santini2022 <- read_excel("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/SRIT_ANDRE/external_data/geb13476-sup-0002-tables1.xls") %>% 
  # santini's dataframe has species names with spaces but metaRange does not like spaces
  # remove spaces again
  mutate(Species = str_replace_all(Species, " ", ""))

# (4) spData
spData <- read.csv(file.path(dirinput, "metaRangeSpeciesDataframe.csv")) 


aa <- validateModel1.2(targetspecies = c("Alcesalces", "Cervuselaphus"),
                 independentDensity = santini2022,
                 dirouts = dirout,
                 spData = spData,
                 validationYear = 101)

# plotting the results
ggplot(aa$independentDensity, aes(x = "", y = meanDensity)) +
  geom_boxplot(aes(ymin = lw95, lower = lw75, middle = meanDensity, upper = up75, ymax = up95), stat = "identity") +
  geom_point(data = aa$estimatedDensity, aes(x = "", y = estimatedDensity), color = "red", position = position_jitter(width = 0.2), size = 1) +
  facet_wrap(~ species, scales = "free_y") + 
  ylab("Independent density estimate (individuals/km2)") +
  xlab(" ") +
  ggtitle("Model validation - estimated densities in red") + 
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
