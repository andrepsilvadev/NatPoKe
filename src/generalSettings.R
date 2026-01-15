## Name: generalSettings.R
## Author: Inês Silva ##
## Date: December 20th, 2025 ##
## Description: Define general directory names and create needed folders ##

# get project root
project_root <- getwd()

# data paths (shared across runs)
data_dir <- file.path(project_root, "data")
## folder with sdm related inputs
dir.create(file.path(data_dir, "sdm"), showWarnings = FALSE)
sdm_dir <- file.path(data_dir, "sdm")
## folder with processed SDM landscapes
dir.create(file.path(sdm_dir, "SDMlandscapes_October25"), showWarnings = FALSE)
processedSDM_dir <- file.path(sdm_dir, "SDMlandscapes_October25")


# output root (all runs live here)
output_root <- file.path(project_root, "outputs")
# create directories for extra outputs
## sensitivity runs & analysis
dir.create(file.path(output_root, "sensitivity_runs"), showWarnings = FALSE)
sens_output_root <- file.path(output_root, "sensitivity_runs")
## diagnostic analysis
dir.create(file.path(output_root, "diagnostics"), showWarnings = FALSE)
diagnostics_dir <- file.path(output_root, "diagnostics")
## model validation
dir.create(file.path(output_root, "modelValidation"), showWarnings = FALSE)
validation_dir <- file.path(output_root, "modelValidation")
## SDM Figures
dir.create(file.path(output_root, "SDMsFigures"), showWarnings = FALSE)
SMDsFigures_dir <- file.path(output_root, "SDMsFigures")
# Main figure & metrics
dir.create(file.path(output_root, "FigureAndMetrics"), showWarnings = FALSE)
figureAndMetrics <- file.path(output_root, "FigureAndMetrics")

# create run-specific directory
runpath <- file.path(output_root, runname)

dir.create(runpath, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(runpath, "Inputs"), showWarnings = FALSE)
dir.create(file.path(runpath, "Outputs"), showWarnings = FALSE)

# shortcuts used by all scripts
dirinput <- file.path(runpath, "Inputs")
dirout   <- file.path(runpath, "Outputs")

# -----------------------
# Logging
# -----------------------
logfile <- file.path(runpath, "run.log")
sink(logfile, split = TRUE)
on.exit(sink(), add = TRUE)

cat("Started run name:", runname, "\n")
cat("Region:", target_region, "\n")
cat("Biome:", target_biome, "\n")
#cat("Scenario:", future_scenario, "\n\n")


