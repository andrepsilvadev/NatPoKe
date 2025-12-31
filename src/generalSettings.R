## Name: generalSettings.R
## Author: Inês Silva ##
## Date: December 20th, 2025 ##
## Description: Define general directory names and create needed folders ##

# get project root
project_root <- getwd()

# data paths (shared across runs)
data_dir <- file.path(project_root, "data")

# output root (all runs live here)
output_root <- file.path(project_root, "outputs")

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

cat("Started run name:", runname, "\n")
cat("Region:", target_region, "\n")
cat("Biome:", target_biome, "\n")
cat("Scenario:", future_scenario, "\n\n")


