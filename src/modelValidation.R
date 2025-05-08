########################################
# VALIDATING ALL SPECIES FROM ALL RUNS #
########################################
# Inês Silva
# 27 April 2025

runs_path <- "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs"

# Define multiple pairs of input and output directories
directory_pairs <- list(
  # Europe
  EuropeSSP1 = c(input = file.path(runs_path, "23April_Europe/Inputs"), output = file.path(runs_path, "23April_Europe/Outputs")),
  EuropeSSP5 = c(input = file.path(runs_path, "26Mar2025_Europe/Inputs"), output = file.path(runs_path, "26Mar2025_Europe/Outputs")),
  # North America
  NorthAmericaSSP1 = c(input = file.path(runs_path, "23April_NorthAmerica/Inputs"), output = file.path(runs_path, "23April_NorthAmerica/Outputs")),
  NorthAmericaSSP5 = c(input = file.path(runs_path, "27Mar2025_NorthAmerica/Inputs"), output = file.path(runs_path, "27Mar2025_NorthAmerica/Outputs")),
  # South America
  SouthAmericaSSP1 = c(input = file.path(runs_path, "23April_SouthAmerica/Inputs"), output = file.path(runs_path, "23April_SouthAmerica/Outputs")),
  SouthAmericaSSP5 = c(input = file.path(runs_path, "27Mar2025_SouthAmerica/Inputs"), output = file.path(runs_path, "27Mar2025_SouthAmerica/Outputs")),
  # Africa
  AfricaSSP1 = c(input = file.path(runs_path, "23April_Africa/Inputs"), output = file.path(runs_path, "23April_Africa/Outputs")),
  AfricaSS5 = c(input = file.path(runs_path, "27Mar2025_Africa/Inputs"), output = file.path(runs_path, "27Mar2025_Africa/Outputs")),
  # Asia
  AsiaSSP1 = c(input = file.path(runs_path, "23April_Asia/Inputs"), output = file.path(runs_path, "23April_Asia/Outputs")),
  AsiaSSP5 = c(input = file.path(runs_path, "27Mar2025_Asia/Inputs"), output = file.path(runs_path, "27Mar2025_Asia/Outputs"))
  )

# start an empty list (for results dfs)
plot_data_list <- list()

# go through each pair of directories
for (name in names(directory_pairs)) {
  dirs <- directory_pairs[[name]]
  dirinput <- dirs[["input"]]
  dirout <- dirs[["output"]]
  
  # (1) get targetspecies
  species_names <- read.csv(file.path(dirinput, "metaRangeSpeciesDataframe.csv")) %>%
    dplyr::pull(Species)
  
  # (2) get independentDensity
  santini2022 <- read_excel("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/SRIT_ANDRE/external_data/geb13476-sup-0002-tables1.xls") %>%
    mutate(Species = str_replace_all(Species, " ", ""))
  
  # (3) get spData (modelling resolution)
  spData <- read.csv(file.path(dirinput, "metaRangeSpeciesDataframe.csv"))
  
  
  # (4) apply the function for the current pair of directories
  aa <- validateModel1.2(targetspecies = species_names,
                         independentDensity = santini2022,
                         dirouts = dirout,
                         spData = spData,
                         validationYear = 101)
  
  # store results in list
  if (
      is.list(aa) && !is.null(aa$independentDensity) && !is.null(aa$estimatedDensity) &&
      is.data.frame(aa$independentDensity) && is.data.frame(aa$estimatedDensity)) {
    plot_data_list[[name]] <- list(independentDensity = aa$independentDensity,
                                   estimatedDensity = aa$estimatedDensity,
                                   name = name) # Store the data and the name
  } else {
    cat("Warning: 'aa' for", name, "SOMETHING WENT WRONG! Check origin data or function.\n")
  }
}

# check results
plot_data_list$EuropeSSP1

# plot model validation for each dataset
for (plot_data in plot_data_list) {
  # independent estimates
  independent_density <- plot_data$independentDensity
  # model outputs
  estimated_density <- plot_data$estimatedDensity
  # name
  plot_name <- plot_data$name
  
  # actual plot
  plot_output <- ggplot(independent_density, aes(x = "", y = meanDensity)) +
    geom_boxplot(aes(ymin = lw95, lower = lw75, middle = meanDensity, upper = up75, ymax = up95), stat = "identity") +
    geom_point(data = estimated_density, aes(x = "", y = estimatedDensity), color = "red", position = position_jitter(width = 0.2), size = 1) +
    facet_wrap(~ species, scales = "free_y", labeller = labeller(species = pretty_species_names)) +
    ylab(expression("Independent density estimate (individuals/km"^2*")")) +
    xlab(" ") +
    ggtitle(" ") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          strip.text = element_text(face = "italic"))
  
  print(plot_output) # see the plot
  
  # to save the plots
 # ggsave(filename = paste0("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/figures_20250427/SupplementaryFigure_validation_plot_", plot_name, ".png"),
  #       plot_output, # plot
   #      bg = 'white', width = 230, height = 210, units = "mm", dpi = 1200,
         #compression = "lzw"
    #     ) # image parameters
}


################################################################################
############# IF WE WANT TO HAVE ALL SPECIES IN THE SAME PLOT ##################

# empty lists
all_estimated <- list()
all_independent <- list()

# get all dfs together
for (data in plot_data_list) {
  all_estimated[[data$name]] <- data$estimatedDensity
  all_independent[[data$name]] <- data$independentDensity
}

# bind everything together
combined_estimated <- dplyr::bind_rows(all_estimated, .id = "Dataset")
combined_independent <- dplyr::bind_rows(all_independent, .id = "Dataset")

# ployt everyone form everywhere
ggplot(combined_independent, aes(x = "", y = meanDensity)) +
  geom_boxplot(aes(ymin = lw95, lower = lw75, middle = meanDensity, upper = up75, ymax = up95), stat = "identity") +
  geom_point(data = combined_estimated, aes(x = "", y = estimatedDensity), color = "red", position = position_jitter(width = 0.2), size = 1) +
  facet_wrap(Dataset ~ species, scales = "free_y", labeller = labeller(species = pretty_species_names)) +
  ylab(expression("Independent density estimate (individuals/km"^2*")")) +
  xlab(" ") +
  ggtitle(paste("Model validation", plot_name, "scenario")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "italic"))



