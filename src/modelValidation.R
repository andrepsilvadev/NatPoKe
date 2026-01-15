## Name: modelValidation.R ##
## Authors: Inês Silva ##
## Description: Validates metaRang outputs for multiple species in multiple scenarios&regions ##
## Date: 27 April 2025 updated on 10 Nov. 2025

source("./src/customFunctions.R")
source("./src/customFunctions2.R")

##########
# STEP 1 # Define run output's directories
##########

runs <- read.csv("./data/run_table.csv", stringsAsFactors = FALSE)

# build input & output folder paths

inputFolder_paths <- character(0)
outputFolder_paths <- character(0)

for (i in seq_len(nrow(runs))) {
  
  target_region <- runs$region[i]
  target_biome <- runs$biome[i]
  future_scenario <- runs$scenario[i]
  
  runname <- paste(
    target_region,
    future_scenario,
    format(Sys.time(), "%Y%m%d"),
    sep = "_"
  )
  # save input path
  input_folder <- file.path(
    getwd(), "outputs",
    runname, "Inputs")
  if (!file.exists(input_folder)) {
    warning("Input folder not found (skipping): ", input_folder)
    next
  }
    # save output path
    output_folder <- file.path(
      getwd(), "outputs",
      runname, "Outputs")
    if (!file.exists(output_folder)) {
      warning("Output folder not found (skipping): ", output_folder)
      next
  }
  # append input path to list
  inputFolder_paths[runname] <- input_folder
  # append output path to list
  outputFolder_paths[runname] <- output_folder
}

##########
# STEP 2 # Run validation function
##########

# start an empty list (for results dfs)
plot_data_list <- list()

# go through each pair of directories
for (i in seq_len(nrow(runs))) {
  
  target_region <- runs$region[i]
  target_biome <- runs$biome[i]
  future_scenario <- runs$scenario[i]
  
  runname <- paste(
    target_region,
    future_scenario,
    format(Sys.time(), "%Y%m%d"),
    sep = "_"
  )
  
  # select correct folders 
  dirinput <- inputFolder_paths[[runname]]
  dirout <- outputFolder_paths[[runname]]

  if (length(list.files(dirinput, pattern = "\\.tif$", full.names = TRUE)) == 0) {
    warning("Input folder is empty! Someone shoudl go check what went wrong: ")
    next
  }
  
  # (1) get targetspecies
  species_names <- read.csv(file.path(dirinput, "metaRangeSpeciesDataframe.csv")) %>%
    dplyr::pull(Species)
  

  # (2) get independentDensity (from Santini et al. 2022)
  santini2022 <- read_excel("./data/externaldata/geb13476-sup-0002-tables1.xls") %>%
    mutate(Species = str_replace_all(Species, " ", "."))

  # (3) get spData (modelling resolution)
  spData <- read.csv(file.path(dirinput, "metaRangeSpeciesDataframe.csv"))

  # (4) apply the function for the current pair of directories
  aa <- validateModel1.2(
    targetspecies = species_names,
    independentDensity = santini2022,
    dirouts = dirout,
    spData = spData,
    validationYear = 26 # make sure this year is the timestep after the burn-in period ends
  )

  # store results in list
  if (
    is.list(aa) && !is.null(aa$independentDensity) && !is.null(aa$estimatedDensity) &&
      is.data.frame(aa$independentDensity) && is.data.frame(aa$estimatedDensity)) {
    plot_data_list[[runname]] <- list(
      independentDensity = aa$independentDensity,
      estimatedDensity = aa$estimatedDensity,
      runname = runname
    ) 
  } else {
    cat("Warning: 'aa' for", runname, "SOMETHING WENT WRONG! Check origin data or function.\n")
  }
}

# check results
#plot_data_list$`Europe+Asia_ssp126_20260102`

##########
# STEP 3 # Plot each sps validation per region & scenario separately
##########

# empty lists
all_estimated <- list()
all_independent <- list()

# get all dfs together
for (data in plot_data_list) {
  all_estimated[[data$runname]] <- data$estimatedDensity
  all_independent[[data$runname]] <- data$independentDensity
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
  df_indep <- combined_independent %>%
    filter(Dataset == ds) %>% 
    mutate(species = reorder(species, meanDensity))
  
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
      size = 1
    ) +
    #coord_flip() +
    ylab(expression("Independent density estimate (individuals/km"^2 * ")")) +
    xlab("") +
    ggtitle(paste("Model validation:", ds)) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(face = "italic")
    )
  
  # save each run's plot (organise based on n of species)
  n_species <- n_distinct(df_indep$species)
  #validation_dir <- file.path(output_root, "modelValidation")
  #dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(
    filename = file.path(validation_dir, paste0("validation_", ds, ".png")),
    plot = p,
    bg = "white",
    width = max(8, n_species * 0.25),  # 0.25–0.35 works well
    height = 6,
    units = "in",
    dpi = 300
  )

  # store plots in list
  #plot_list[[ds]] <- p
}