## Name: modelValidation.R ##
## Authors: Inês Silva ##
## Date: 27 April 2025 updated on 10 Nov. 2025
## Description: Validate metaRange outputs for multiple species in multiple
## scenarios&regions. Validation is against the Santini et al. 2022 dataset. Scatter
## plots are produced to show agreement between metaRange and Santini as well as 
## supplementary table with % of modelled cells within or outside confidence
## intervals available in Santini et al. 2022

#source("./src/customFunctions.R")
source("./src/customFunctions2.R")
source("./src/libraries.R")

##########
# STEP 1 # Define run output's directories
##########

runs <- read.csv("./data/run_table.csv", stringsAsFactors = FALSE)

validation_dir <- file.path("E:/metaRange_May26/", "modelValidation")
dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

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
    "20260517",
    #format(Sys.time(), "%Y%m%d"),
    sep = "_"
  )
  # save input path
  input_folder <- file.path(
    "E:/metaRange_May26",
    runname, "Inputs")
  if (!file.exists(input_folder)) {
    warning("Input folder not found (skipping): ", input_folder)
    next
  }
    # save output path
    output_folder <- file.path(
      "E:/metaRange_May26",
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
    "20260517",
    #format(Sys.time(), "%Y%m%d"),
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
  set.seed(123)
  aa <- validateModel1.2(
    targetspecies = species_names,
    independentDensity = santini2022,
    dirouts = dirout,
    spData = spData,
    validationYear = 26, # make sure this year is the timestep after the burn-in period ends
    subsample = FALSE
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
#plot_data_list$`Europe+Asia_ssp126_20260405`
#plot_data_list$Asia_ssp126_20260517$independentDensity

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


TNIND_yr <- read.csv("E:/metaRange_May26/completeMetaRangeRun_20260517.csv") %>%
  mutate(species = str_replace(species, " ", "."))

combined_estimated <- combined_estimated %>%
  left_join(TNIND_yr %>% select(species, trophic_level) %>% distinct(),
            by = "species")


combined_independent <- combined_independent %>%
  left_join(TNIND_yr %>% select(species, trophic_level) %>% distinct(),
            by = "species")
# 
# # start empty list
# plot_list <- list()
# 
# ylims_list <- list(
#   Asia_ssp126_20260405 = c(0, 10),
#   Asia_ssp585_20260405 = c(0, 5),
#   Europe_ssp126_20260405 = c(0, 1)
# )
# 
# for (ds in datasets) {
#   # subset current dataset
#   df_indep <- combined_independent %>%
#     filter(Dataset == ds) %>% 
#     mutate(species = reorder(species, meanDensity))
#   
#   df_est <- combined_estimated %>% filter(Dataset == ds)
#   
#   p <- ggplot(df_indep, aes(x = species, y = meanDensity)) +
#     # independent densities from santini
#     geom_boxplot(
#       aes(
#         ymin = lw95, lower = lw75,
#         middle = meanDensity,
#         upper = up75, ymax = up95),
#       stat = "identity", fill = "lightgray", color = "black") +
#     # dependent densities estimates from metaRange
#     geom_point(
#       data = df_est, aes(x = species, y = estimatedDensity),
#       color = "red",
#       position = position_jitter(width = 0.2),
#       size = 1) +
#     facet_wrap(~ trophic_level, scales = "free") +
#     #coord_flip() +
#     ylab(expression("Independent density estimate (individuals/km"^2 * ")")) +
#     xlab("") +
#     ggtitle(paste("Model validation:", ds)) +
#     theme_minimal() +
#     theme(
#       axis.text.x = element_text(angle = 45, hjust = 1),
#       strip.text = element_text(face = "italic")
#     )
#   
#   # save each run's plot (organise based on n of species)
#   n_species <- n_distinct(df_indep$species)
#   
#   ggsave(filename = file.path(validation_dir, paste0("validation_", ds, ".png")),
#     plot = p, bg = "white",
#     width = max(8, n_species * 0.25),  # 0.25–0.35 works well
#     height = 6, units = "in", dpi = 300)
#   
#   # store plots in list
#   plot_list[[ds]] <- p
# }


##########
# STEP 4 # Proportion outside known density estimates
##########

# Join CI to cell-level data first
estimated_with_ci <- combined_estimated %>%
  dplyr::left_join(
    combined_independent %>%
      dplyr::select(Dataset, species, lw95, up95, lw75, up75, meanDensity),
    by = c("Dataset", "species")) %>%
  dplyr::mutate(up95,lw95,
                below95 = estimatedDensity < lw95,
                above95 = estimatedDensity > up95,
                outside95 = below95 | above95,
                outside75 = estimatedDensity < lw75 | estimatedDensity > up75)

# Now summarise per species
estimated_summary_by_species <- estimated_with_ci %>%
  dplyr::group_by(Dataset, species) %>%
  dplyr::summarise(up95 = mean(up95, na.rm = TRUE),
                   lw95 = mean(lw95, na.rm = TRUE),
                   estimated_mean = mean(estimatedDensity, na.rm = TRUE),
                   observed_mean = mean(meanDensity, na.rm = TRUE),
                   prop_outside95 = mean(outside95, na.rm = TRUE),
                   prop_below95 = mean(below95, na.rm = TRUE),
                   prop_above95 = mean(above95, na.rm = TRUE),
                   prop_outside75 = mean(outside75, na.rm = TRUE),
                   n_cells = dplyr::n(), .groups = "drop") %>% 
  # remove erroneous species in asia
  dplyr:: filter(!(Dataset %in% c("Asia_ssp126_20260517", "Asia_ssp585_20260517") &
                     species %in% c("Lynx.lynx", "Ursus.arctos"))) %>% 
  separate(Dataset, into = c("region", "scenario", "date"), sep = "_", remove = TRUE) %>% 
  mutate(species = gsub(".", " ", species, fixed = TRUE),
         scenario = case_when(scenario == "ssp126" ~ "SSP1-2.6",
                              scenario == "ssp585" ~ "SSP5-8.5",
                              TRUE ~ scenario)) %>% 
  dplyr::select(-date) %>%
  # flag those sps above 70% (these are the worst models we might have)
  mutate(flag95 = prop_outside95 > 0.70, 
         flag75 = prop_outside75 > 0.70)

#print(estimated_summary_by_species, n = Inf)

validationTable <- estimated_summary_by_species %>%
  dplyr::select(region, scenario, species, prop_outside95, prop_outside75, 
         prop_below95, prop_above95, n_cells) %>%
  gt::gt() %>%
  # keep only one decimal place
  fmt_percent(columns = c(prop_outside95, prop_outside75, prop_below95, prop_above95),
              decimals = 1) %>%
  # highligh bad density estimates
  data_color(columns = prop_outside95, rows = prop_outside95 > 0.70, 
             palette = c("white", "red")) %>%
  data_color(columns = prop_outside75, rows = prop_outside75 > 0.70, 
             palette = c("white", "red")) %>%
  tab_header(title = "Validation table, highlighted cells represent proportions outside 95% and 75% confidence intervals")

# save as a formatted word document
gt::gtsave(validationTable, file.path(validation_dir,
                             paste0("modelValidationTable", Sys.Date(), ".docx")))

# save as a .csv
fwrite(as.data.frame(estimated_summary_by_species),
       file = file.path(validation_dir, paste0("modelValidationTable", Sys.Date(), ".csv")))

# calculate the proportion of cells outside percentiles
prop_outside_species <- estimated_summary_by_species %>%
  #group_by(scenario, region) %>% 
  dplyr::summarise(min_prop_outside95 = min(prop_outside95, na.rm = TRUE),
                   mean_prop_outside95 = mean(prop_outside95, na.rm = TRUE),
                   max_prop_outside95 = max(prop_outside95, na.rm = TRUE),
                   prop_outside75 = mean(prop_outside75, na.rm = TRUE),
                   prop_below95 = mean(prop_below95, na.rm = TRUE),
                   prop_above95 = mean(prop_above95, na.rm = TRUE),
                   n_species = dplyr::n()) %>% 
  dplyr::select(-n_species) %>% 
  pivot_longer(cols = c("min_prop_outside95", "mean_prop_outside95",
                        "max_prop_outside95", "prop_outside75", "prop_below95", 
                        "prop_above95"),
               names_to = "Category",
               values_to = "Value")

# save output as .csv
write.csv(prop_outside_species,
          file = file.path(validation_dir, "ProportionOfCellOutsideIntervals.csv"),
          row.names = FALSE)


# number of species underestimating
length(unique(estimated_summary_by_species %>% 
  dplyr::filter(prop_outside95 > 0.70) %>% 
  pull(species)))


validation_plot_species <- estimated_summary_by_species %>%
  mutate(
    validation = case_when(
      estimated_mean < lw95 ~ "Below 95% interval",
      estimated_mean > up95 ~ "Above 95% interval",
      TRUE ~ "Within 95% interval"
    )
  )


val_plots <- list()

for (sc in unique(validation_plot_species$scenario)) {
  
  for (reg in unique(validation_plot_species$region)) {
    
    validation_plot_species_sc_reg <- validation_plot_species %>%
      dplyr::filter(scenario == sc, region == reg)
    
    label_data <- validation_plot_species_sc_reg %>%
      dplyr::filter(validation == "Below 95% interval")
    
    p <- ggplot(validation_plot_species_sc_reg, 
                aes(x = observed_mean, y = estimated_mean)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.8) +
      geom_point(aes(fill = validation), shape = 21, size = 3, alpha = 0.8, colour = "black") +
      geom_text(data = label_data, aes(label = species),
                vjust = -0.5, hjust = -0.1, nudge_x = 0.05, size = 2) +
      scale_x_log10() +
      scale_y_log10() +
      scale_fill_manual(values = c("Below 95% interval" = "#D55E00",
                                   "Within 95% interval" = "#56B4E9",
                                   "Above 95% interval" = "#009E73")) +
      labs(title = reg,
           x = "Independent population density (log individuals/km²)",
           y = "MetaRange population density (log individuals/km²)",
           fill = "Model estimate") +
      theme_classic() +
      theme(legend.position = "bottom",
            label
            #axis.line = element_line(size = 1, colour = "black")
            )
    
    val_plots[[paste(sc, reg, sep = "_")]] <- p
  }
}


############
# SSP1-2.6 #
############

layout <- "AB#
           CDE"

validationssp126 <- patchwork::wrap_plots(val_plots[c("SSP1-2.6_North America", "SSP1-2.6_Europe+Asia",
                                  "SSP1-2.6_South America", "SSP1-2.6_Africa", "SSP1-2.6_Asia")]) +
  patchwork::plot_layout(design = layout,
                         guides = "collect", axis_titles = "collect") & theme(legend.position = "bottom")

# save plot
ggsave(filename = file.path(validation_dir, "ValidationPerSpecies_SSP1.png"), # path
       validationssp126, # plot
       bg = 'white', width = 250, height = 160, units = "mm", dpi = 1200,
       #compression = "lzw"
) # image parameters


############
# SSP5-8.5 #
############

validationssp585 <- patchwork::wrap_plots(val_plots[c("SSP5-8.5_North America", "SSP5-8.5_Europe+Asia",
                                                      "SSP5-8.5_South America", "SSP5-8.5_Africa", "SSP5-8.5_Asia")]) +
  patchwork::plot_layout(design = layout,
                         guides = "collect", axis_titles = "collect") & theme(legend.position = "bottom")

# save plot
ggsave(filename = file.path(validation_dir, "ValidationPerSpecies_SSP5.png"), # path
       validationssp585, # plot
       bg = 'white', width = 250, height = 160, units = "mm", dpi = 1200,
       #compression = "lzw"
) # image parameters
