## Name: modelValidation.R ##
## Authors: Inês Silva ##
## Description: Validates metaRang outputs for multiple species in multiple scenarios&regions ##
## Date: 27 April 2025 updated on 10 Nov. 2025

source("./src/customFunctions.R")

##########
# STEP 1 # Define run output's directories
##########

runs_path <- "/mnt/data/maria/NatPoKe/output/metaRangeRuns"

# Define multiple pairs of input and output directories
directory_pairs <- list(
  # Europe
  EuropeSSP1 = c(input = file.path(runs_path, "Europe_ssp126_31Oct25/Inputs"), output = file.path(runs_path, "Europe_ssp126_31Oct25/Outputs")),
  EuropeSSP5 = c(input = file.path(runs_path, "Europe_ssp585_31Oct25/Inputs"), output = file.path(runs_path, "Europe_ssp585_31Oct25/Outputs")),
  # North America
  NorthAmericaSSP1 = c(input = file.path(runs_path, "NorthAmerica_ssp126_31Oct25/Inputs"), output = file.path(runs_path, "NorthAmerica_ssp126_31Oct25/Outputs")),
  NorthAmericaSSP5 = c(input = file.path(runs_path, "NorthAmerica_ssp585_31Oct25/Inputs"), output = file.path(runs_path, "NorthAmerica_ssp585_31Oct25/Outputs")),
  # South America
  SouthAmericaSSP1 = c(input = file.path(runs_path, "SouthAmerica_ssp126_31Oct25/Inputs"), output = file.path(runs_path, "SouthAmerica_ssp126_31Oct25/Outputs")),
  SouthAmericaSSP5 = c(input = file.path(runs_path, "SouthAmerica_ssp585_31Oct25/Inputs"), output = file.path(runs_path, "SouthAmerica_ssp585_31Oct25/Outputs")),
  # Africa
  AfricaSSP1 = c(input = file.path(runs_path, "Africa_ssp126_31Oct25/Inputs"), output = file.path(runs_path, "Africa_ssp126_31Oct25/Outputs")),
  AfricaSS5 = c(input = file.path(runs_path, "Africa_ssp585_31Oct25/Inputs"), output = file.path(runs_path, "Africa_ssp585_31Oct25/Outputs")),
  # Asia
  AsiaSSP1 = c(input = file.path(runs_path, "Asia_ssp126_31Oct25/Inputs"), output = file.path(runs_path, "Asia_ssp126_31Oct25/Outputs")),
  AsiaSSP5 = c(input = file.path(runs_path, "Asia_ssp585_31Oct25/Inputs"), output = file.path(runs_path, "Asia_ssp585_31Oct25/Outputs"))
)

##########
# STEP 2 # Run validation function
##########

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
  santini2022 <- read_excel("./data/geb13476-sup-0002-tables1.xls") %>%
    mutate(Species = str_replace_all(Species, " ", "."))

  # (3) get spData (modelling resolution)
  spData <- read.csv(file.path(dirinput, "metaRangeSpeciesDataframe.csv"))


  # (4) apply the function for the current pair of directories
  aa <- validateModel1.2(
    targetspecies = species_names,
    independentDensity = santini2022,
    dirouts = dirout,
    spData = spData,
    validationYear = 101
  )

  # store results in list
  if (
    is.list(aa) && !is.null(aa$independentDensity) && !is.null(aa$estimatedDensity) &&
      is.data.frame(aa$independentDensity) && is.data.frame(aa$estimatedDensity)) {
    plot_data_list[[name]] <- list(
      independentDensity = aa$independentDensity,
      estimatedDensity = aa$estimatedDensity,
      name = name
    ) # Store the data and the name
  } else {
    cat("Warning: 'aa' for", name, "SOMETHING WENT WRONG! Check origin data or function.\n")
  }
}

# check results
# plot_data_list$EuropeSSP1

##########
# STEP 3 # Plot each sps validation per region & scenario separately
##########

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

# WARNING # There is a problem with the santini2022 file for Orycteropus.afer
# there are two entries in the file, I am going to remove the absurd one mannually
# and check with André after
combined_independent <- dplyr::bind_rows(all_independent, .id = "Dataset") %>%
  na.omit()

# make sure sps is a factor
combined_estimated$species <- as.factor(combined_estimated$species)
combined_independent$species <- as.factor(combined_independent$species)

# get unique dataset names
datasets <- unique(combined_independent$Dataset)

# start empty list
plot_list <- list()

for (ds in datasets) {
  # subset current dataset
  df_indep <- combined_independent %>% filter(Dataset == ds)
  df_est <- combined_estimated %>% filter(Dataset == ds)

  p <- ggplot(df_indep, aes(x = species, y = meanDensity)) +
    # independent densities from santini
    geom_boxplot(
      aes(
        ymin = lw95, lower = lw75,
        middle = meanDensity,
        upper = up75, ymax = up95
      ),
      stat = "identity", fill = "lightgray", color = "black"
    ) +
    # dependent densities estimates from metaRange
    geom_point(
      data = df_est, aes(x = species, y = estimatedDensity),
      color = "red",
      position = position_jitter(width = 0.2),
      size = 1.5
    ) +
    ylab(expression("Independent density estimate (individuals/km"^2 * ")")) +
    xlab("") +
    ggtitle(paste("Model validation:", ds)) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(face = "italic")
    )

  # store plots in list
  plot_list[[ds]] <- p
}

##########
# STEP 4 # Save validtaion figures
##########

# Europe SSP1
ggsave(
  filename = "./output/metaRangeRuns/modelValidation/Europe_SSP1.png",
  plot_list[["EuropeSSP1"]] + plot_list[["EuropeSSP1"]] + scale_y_continuous(limits = c(0, 25)),
  bg = "white", width = 300, height = 150, units = "mm", dpi = 300 # , compression = "lzw"
)

# Europe SSP5
ggsave(
  filename = "./output/metaRangeRuns/modelValidation/Europe_SSP5.png",
  plot_list[["EuropeSSP5"]] + plot_list[["EuropeSSP5"]] + scale_y_continuous(limits = c(0, 25)),
  bg = "white", width = 300, height = 150, units = "mm", dpi = 300 # , compression = "lzw"
)

# North America SSP1
ggsave(
  filename = "./output/metaRangeRuns/modelValidation/NorthAmerica_SSP1.png",
  plot_list[["NorthAmericaSSP1"]] + plot_list[["NorthAmericaSSP1"]] + scale_y_continuous(limits = c(0, 25)),
  bg = "white", width = 300, height = 150, units = "mm", dpi = 300 # , compression = "lzw"
)

# North America SSP5
ggsave(
  filename = "./output/metaRangeRuns/modelValidation/NorthAmerica_SSP5.png",
  plot_list[["NorthAmericaSSP5"]],
  bg = "white", width = 300, height = 150, units = "mm", dpi = 300 # , compression = "lzw"
)

# South America SSP1
ggsave(
  filename = "./output/metaRangeRuns/modelValidation/SouthAmerica_SSP1.png",
  plot_list[["SouthAmericaSSP1"]] + plot_list[["SouthAmericaSSP1"]] + scale_y_continuous(limits = c(0, 15)),
  bg = "white", width = 300, height = 150, units = "mm", dpi = 300 # , compression = "lzw"
)


# South America SSP5
ggsave(
  filename = "./output/metaRangeRuns/modelValidation/SouthAmerica_SSP5.png",
  plot_list[["SouthAmericaSSP5"]] + plot_list[["SouthAmericaSSP5"]] + scale_y_continuous(limits = c(0, 20)),
  bg = "white", width = 300, height = 150, units = "mm", dpi = 300 # , compression = "lzw"
)


# Africa SSP1
ggsave(
  filename = "./output/metaRangeRuns/modelValidation/Africa_SSP1.png",
  plot_list[["AfricaSSP1"]] / plot_list[["AfricaSSP1"]] + scale_y_continuous(limits = c(0, 30)),
  bg = "white", width = 300, height = 200, units = "mm", dpi = 300 # , compression = "lzw"
)

# Africa SSP5
ggsave(
  filename = "./output/metaRangeRuns/modelValidation/Africa_SSP5.png",
  plot_list[["AfricaSS5"]] / plot_list[["AfricaSS5"]] + scale_y_continuous(limits = c(0, 30)),
  bg = "white", width = 300, height = 200, units = "mm", dpi = 300 # , compression = "lzw"
)

# Asia SSP1
ggsave(
  filename = "./output/metaRangeRuns/modelValidation/Asia_SSP1.png",
  plot_list[["AsiaSSP1"]] / plot_list[["AsiaSSP1"]] + scale_y_continuous(limits = c(0, 35)),
  bg = "white", width = 300, height = 200, units = "mm", dpi = 300 # , compression = "lzw"
)

# Asia SSP5
ggsave(
  filename = "./output/metaRangeRuns/modelValidation/Asia_SSP5.png",
  plot_list[["AsiaSSP5"]] / plot_list[["AsiaSSP5"]] + scale_y_continuous(limits = c(0, 35)),
  bg = "white", width = 300, height = 200, units = "mm", dpi = 300 # , compression = "lzw"
)
