# NatPoKe — Nature Policy Effects on Keystone Species

[![DOI](https://img.shields.io/badge/DOI-coming_soon-blue?logo=doi&logoColor=white)](https://doi.org/10.0000/placeholder) [![Project Page](https://img.shields.io/badge/Project_Website-MISTRAFinBio-green?logo=leaflet&logoColor=white)](https://finbio.org/)

A research pipeline to explore how nature policy interventions affect keystone species in Boreal and Tropical forests.<br>
The project leverages process-based modeling (via [metaRange](https://metarange.github.io/metaRange/#)) and resilience metrics to evaluate ecological responses under various policy scenarios.


> 🚧 **Under active development** 🚧 
<br>



## Repository Overview

This repository contains all scripts and supporting materials used in the modeling and analysis pipeline.<br>

| Folder      | Description                                                                                                                                                                                                                                                                                      |
|:----------- |:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| **data**    | Raw data, including trait data for XX mammal species, sourced from multiple sources.                                                                                                                                                                                                             |
| **input**   | Intermediate files and data transformations used during pre-processing. ⚠️ *Currently not in use.*                                                                                                                                                                                               |
| **output**  | Generated results: figures, tables, and model outputs. *Currently with dummy figure only.*                                                                                                                                                                                                       |
| **src**     | All R scripts used in data analysis, modeling, and visualization.<br>➡️ For full details, see the [`src/README.md`](https://github.com/andrepsilvadev/NatPoKe/tree/4295ecc339433ef3d49ab62a288edb9beb6d765f/src#readme).<br><ul><li>model input data preparation</li><li>species models</li><li>visualizations</li><li>Validation & Sensitivity Analysis</li></ul> |
| **reports** | R Markdown files producing reports from model runs (single and multispecies), and main and supplementary project figures.                                                                                                                                                                        |
<br>

 ## 🛠 How to Run the Pipeline - Quick Guide
 <br>
 
```r
#  1️⃣ Load Settings & Libraries
source("./src/libraries.R")            # Load necessary packages
source("./src/customFunctions.R")      # Load customized functions

# Set a unique run name (e.g., date_region_scenario)
runname <- "09May_Europe_Robinson"

# Load general paths and spatial settings
source("./src/generalSettings.R")


# 2️⃣ Select Input Parameters

# Choose the target biome (select one)
target_biome <- "Boreal Forests/Taiga"  
# Options: "Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga"

# Choose the target region (select one)
target_region <- "Europe"  
# Options: "North America", "South America", "Europe", "Asia", "Africa"

# Choose the scenario name
scenario <- "SSP1"

# Choose target species (can include multiple species and names must have spaces)
target_species <- c(
  "Alces alces",    # Moose
  "Lynx lynx"       # Eurasian lynx
)


# 3️⃣ Prepare & Load Species Data

# Build the species dataframe and metadata
source("./src/mammalMetaRangeSpeciesDataframe.R")

# Load input files (Robinson projection version)
# Note: Jorinde's scripts are still missing here, but I think they go here also


# 4️⃣ Run the Mammal Model
source("./src/mammalModel.R")

```
