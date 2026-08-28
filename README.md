# NatPoKe — Nature Policy Effects on Keystone Species

[![DOI](https://img.shields.io/badge/DOI-coming_soon-blue?logo=doi&logoColor=white)](https://doi.org/10.0000/placeholder) [![Project Page](https://img.shields.io/badge/Project_Website-MISTRAFinBio-green?logo=leaflet&logoColor=white)](https://finbio.org/)

A research pipeline using the [metaRange](https://metarange.github.io/metaRange/#) framework to couple species distribution models with population demography and dispersal, to quantify species resilience (invariability, resistance, recovery and persistence), temporal change in community diversity (Shannon-Wiener) and composition (Bray-Curtis dissimilarity), alongside their spatial heterogeneity.

> 🚧 **Under active development** 🚧<br>
>
> For questions, clarifications, or collaborations regarding this project, please contact:<br> **André P. Silva** & **Inês Silva**<br> [Institution or Department Name]<br> Email: [andre.pinto.da.silva@su.se](andre.pinto.da.silva@su.se) & [misilva@ciencias.ulisboa.pt](misilva@ciencias.ulisboa.pt)

## Repository Overview

![description](repoOverview2_black.png)

This repository contains all scripts and supporting materials used in the modeling and analysis pipeline.<br>

| Folder | Description |
|:---|:---|
| **data** | Raw data, including trait data for XX mammal species, sourced from multiple sources. |
| **input** | Intermediate files and data transformations used during pre-processing. ⚠️ *Currently not in use.* |
| **output** | Generated results: figures, tables, and model outputs. *Currently with dummy figure only.* |
| **src** | All R scripts used in data analysis, modeling, and visualization.<br>➡️ For full details, see the [`src/README.md`](https://github.com/andrepsilvadev/NatPoKe/blob/ines_silva/src/README.md).<br> |
| **reports** | R Markdown files producing reports from model runs (single and multispecies), and main and supplementary project figures. |

<br>

## Scripts description <br>

### 1. Raw data

`selectSpecies.R`  
Selects and filters the mammal species included available from the [SRIT-database](https://zenodo.org/records/18455098) datasets, based on the necessary traits (e.g. reproduction rate, body mass, etc...)

`taxaOccurrence.R`  
Retrieves and processes species occurrence records from GBIF to generate the occurrence dataset used for species distribution modelling.

`ClimateChange.R`
Processes current and projected climate layers from CHELSA and prepares the environmental data for the different future scenarios and time periods.

`LandUseChange.R`
Processes land-use/land-cover data and generates the corresponding current and future environmental layers used in the species distribution models.

### 2. Species Distribution Models

`SDM.R`  
Builds species distribution models using species occurrences, climate and land-use as predictorsthrough [biomod2](https://biomodhub.github.io/biomod2/), to estimate current and future species suitability. Custom function is created to iterate across multiple species and scenarios.

`SDMfigures.R`  
Generates presence maps vs total available records maps, variable importance tables, evaluation metrics plots and current and projected distribution maps for all species, commonly used for quality control and interpretation of results before moving forward.

### 3. Build metaRange inputs

`metaRangeSpeciesDataframe.R`  
Compiles and formats species traits into a metaRange ready dataframe required for all species within a region to be modelled.

`speciesSuitabilityLayers.R`  
Prepares species-specific environmental suitability layers across time periods to serve as environment in the metaRange simulations. Interpolation is used to fill in timesteps between future projections (e.g. between 2030 and 2050).

### 4. Run metaRange simulations

`mammalModel.R`  
Code for the metaRange population dynamics simulations, incorporating process like suitability influenc eon population parameters, demography following Beverton&Holt and dispersal to simulate mammal population dynamics under alternative scenarios.

### 5. Analysis, validation & evaluation

`readMetaRangeOutputs.R`  
Reads and performs basic diagnotic (saving plots and dataframes) from the raw metaRange simulation outputs for downstream analyses. Compiles multiple metaRange (e.g. different regions) into one dataframe.

`modelValidation.R`  
Evaluates model performance by comparing simulated outputs against an independent dataset (Santini et al. 2022) to assess the reliability of model predictions.

`sensitivityAnalysis.R`  
Tests the sensitivity of model outcomes to key parameters and assumptions to identify which factors most influence the results.

`speciesResilienceMetrics.R`  
Calculates species-level resilience metrics, using the [estar](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210x.70265) package, from the model outputs to quantify changes in population persistence and responses to environmental change.

`shannonWiener_overTime.R`  
Calculates temporal changes in community diversity using the Shannon–Wiener diversity index.

`shannonWiener_spatiallyExplicit.R`    
Calculates spatially explicit Shannon–Wiener diversity across the study area to identify geographic patterns in community diversity.

`brayCurtis_overTime.R`  
Quantifies temporal changes in community composition using Bray–Curtis dissimilarity.

`brayCurtis_spatiallyExplicit.R`  
Calculates spatially explicit Bray–Curtis dissimilarity to assess geographic variation and turnover in community composition.   








<br>

\## 🛠 How to Run the Pipeline - Quick Guide <br>

Use the `run.R` to run the complete pipeline from creating the inputs for a specific species, region and scenario up to running the metaRange model and producing results.

``` r
#  1️⃣ Load Settings & Libraries
source("./src/libraries.R")            # Load necessary packages
source("./src/customFunctions.R")      # Load customized functions

# Set a unique run name (e.g., date_region_scenario)
runname <- "13Sep_SouthAmerica_ssp585"

# Create folder to save pipeline inputs and outputs
source("./src/generalSettings.R")


# 2️⃣ Select Input Parameters

# Choose the target biome (select one)
target_biome <- "Boreal Forests/Taiga"  
# Options: "Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga"

# Choose the target region (select one)
target_region <- "Europe"  
# Options: "North America", "South America", "Europe", "Asia", "Africa"

# Choose the scenario name
scenario <- "ssp126" # Options: "ssp126" or "ssp585

# Choose target species (can include multiple species and names must have spaces)
target_species <- c(
  "Alces alces",    # Moose
  "Lynx lynx"       # Eurasian lynx
)


# 3️⃣ Prepare & Load Input Data

# create species traits dataframe
source("./src/mammalMetaRangeSpeciesDataframe.R")

# build Species Distribution Models & save output rasters
#source("./src/SDM.R") - this script should be run separetly & at the moment outputs from this script exist for 16 species

## transform SDM outputs into input data for MetaRange model
source("./src/specieSuitabilityLayers.R")

# 4️⃣ Run the Mammal Model

source("./src/mammalModel.R")
```
